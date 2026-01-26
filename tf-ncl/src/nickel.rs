use crate::intermediate::{self, FieldDescriptor, GoSchema, Providers, WithProviders};
use nickel_lang_parser::{
    ast::{builder, typ::Type, Ast, AstAlloc, MergePriority, Node},
    typ::{DictTypeFlavour, TypeF},
};

pub trait AsNickel {
    fn as_nickel<'a>(&self, alloc: &'a AstAlloc) -> Ast<'a>;
}

impl AsNickel for WithProviders<GoSchema> {
    fn as_nickel<'a>(&self, alloc: &'a AstAlloc) -> Ast<'a> {
        as_nickel_record(alloc, &self.data.schema)
            .path(["terraform", "required_providers"])
            .value(alloc, self.providers.as_nickel(alloc))
            .build(alloc)
    }
}

impl AsNickel for Providers {
    fn as_nickel<'a>(&self, alloc: &'a AstAlloc) -> Ast<'a> {
        use builder::*;
        Record::from_iterator(
            alloc,
            self.0.iter().map(|(name, provider)| {
                Field::name(name).value(
                    Record::from_iterator(
                        alloc,
                        [
                            Field::name("source")
                                .priority(MergePriority::Bottom)
                                .value(alloc.string(&provider.source)),
                            Field::name("version")
                                .priority(MergePriority::Bottom)
                                .value(alloc.string(&provider.version)),
                        ],
                    )
                    .build(alloc),
                )
            }),
        )
        .build(alloc)
    }
}

impl AsNickel for Vec<String> {
    fn as_nickel<'a>(&self, alloc: &'a AstAlloc) -> Ast<'a> {
        alloc
            .array(self.iter().map(|s| alloc.string(s).into()))
            .into()
    }
}

impl AsNickel for Vec<FieldDescriptor> {
    fn as_nickel<'a>(&self, alloc: &'a AstAlloc) -> Ast<'a> {
        alloc.array(self.iter().map(|x| x.as_nickel(alloc))).into()
    }
}

impl AsNickel for FieldDescriptor {
    fn as_nickel<'a>(&self, alloc: &'a AstAlloc) -> Ast<'a> {
        use builder::*;

        let priority_tag = if self.force { "Force" } else { "Default" };
        let priority = alloc.enum_variant(priority_tag.into(), None);
        Record::new()
            .field("prio")
            .value(alloc, priority)
            .field("path")
            .value(alloc, self.path.as_nickel(alloc))
            .build(alloc)
    }
}

pub trait AsNickelField {
    fn as_nickel_field<'a>(
        &self,
        alloc: &'a AstAlloc,
        field: builder::Field<'a, builder::Incomplete>,
    ) -> builder::Field<'a, builder::Complete<'a>>;
}

impl AsNickelField for &intermediate::Attribute {
    fn as_nickel_field<'a>(
        &self,
        alloc: &'a AstAlloc,
        field: builder::Field<'a, builder::Incomplete>,
    ) -> builder::Field<'a, builder::Complete<'a>> {
        let intermediate::Attribute {
            description,
            optional,
            computed,
            type_,
        } = self;
        let (t, computed_fields) = type_.as_nickel_contracts(alloc);
        let field = field.some_doc(description.clone()).optional(*optional);
        let field = if let Some(fs) = computed_fields {
            field.contracts([t, fs])
        } else {
            field.contract(t)
        };
        if *computed {
            field
                .priority(MergePriority::Bottom)
                .value(Node::Var("TfNcl.undefined".into()))
        } else {
            field.no_value()
        }
    }
}

pub trait AsNickelContracts {
    fn as_nickel_contracts<'a>(&self, alloc: &'a AstAlloc) -> (Type<'a>, Option<Type<'a>>);
}

enum PrimitiveType {
    Dyn,
    Str,
    Num,
    Bool,
}

impl AsNickel for PrimitiveType {
    fn as_nickel<'a>(&self, _alloc: &'a AstAlloc) -> Ast<'a> {
        use PrimitiveType::*;
        match self {
            Dyn => Node::Var("Dyn".into()).into(),
            Str => Node::Var("String".into()).into(),
            Num => Node::Var("Number".into()).into(),
            Bool => Node::Var("Bool".into()).into(),
        }
    }
}

impl AsNickelContracts for &intermediate::Type {
    fn as_nickel_contracts<'a>(&self, alloc: &'a AstAlloc) -> (Type<'a>, Option<Type<'a>>) {
        use intermediate::Type::*;

        fn primitive<'ast>(
            alloc: &'ast AstAlloc,
            inner: PrimitiveType,
        ) -> (Type<'ast>, Option<Type<'ast>>) {
            use PrimitiveType::*;
            let arg = match inner {
                Dyn => Node::Var("Dyn".into()),
                Str => Node::Var("String".into()),
                Num => Node::Var("Number".into()),
                Bool => Node::Var("Bool".into()),
            };
            let ty = TypeF::Contract(
                alloc.alloc(
                    alloc
                        .app(Node::Var("TfNcl.Tf".into()).into(), [arg.into()])
                        .into(),
                ),
            );
            (ty.into(), None)
        }

        match self {
            Dynamic => primitive(alloc, PrimitiveType::Dyn),
            String => primitive(alloc, PrimitiveType::Str),
            Number => primitive(alloc, PrimitiveType::Num),
            Bool => primitive(alloc, PrimitiveType::Bool),
            //TODO(vkleen): min and max should be represented as a contract
            //TODO(vkleen): tfvar wrapping is unclear
            List {
                min: _,
                max: _,
                content,
            } => (
                TypeF::Array(alloc.alloc(content.as_ref().as_nickel_contracts(alloc).0)).into(),
                None,
            ),
            Object { open, content } => (
                TypeF::Contract(
                    alloc.alloc(
                        builder::Record::from_iterator(
                            alloc,
                            content
                                .iter()
                                .map(|(k, v)| v.as_nickel_field(alloc, builder::Field::name(k))),
                        )
                        .set_open(*open)
                        .build(alloc),
                    ),
                )
                .into(),
                None,
            ),
            Dictionary {
                inner,
                prefix,
                computed_fields,
            } => {
                let inner_contract = TypeF::Dict {
                    type_fields: alloc.alloc(inner.as_ref().as_nickel_contracts(alloc).0),
                    flavour: DictTypeFlavour::Contract,
                }
                .into();
                (
                    inner_contract,
                    Some(
                        TypeF::Contract(
                            alloc.alloc(
                                alloc
                                    .app(
                                        Node::Var("TfNcl.ComputedFields".into()).into(),
                                        [prefix.as_nickel(alloc), computed_fields.as_nickel(alloc)],
                                    )
                                    .into(),
                            ),
                        )
                        .into(),
                    ),
                )
            }
        }
    }
}

fn as_nickel_record<'a, K, V, It>(alloc: &'a AstAlloc, r: It) -> builder::Record<'a>
where
    K: AsRef<str>,
    V: AsNickelField,
    It: IntoIterator<Item = (K, V)>,
{
    builder::Record::from_iterator(
        alloc,
        r.into_iter()
            .map(|(k, v)| v.as_nickel_field(alloc, builder::Field::name(k))),
    )
}
