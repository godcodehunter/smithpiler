    // As described in 6.7.2.2 extract C type in stream of specifiers 
    /*
    __________________________________________________________________________________________
    | void | char | short | long | signed | unsigned | int | float | double | result type    |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  1   |  0   |  0    |  0   |   0    |    0     |  0  |   0   |   0    | void           |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  1   |  0    |  0   |   0    |    0     |  0  |   0   |   0    | char           |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  1   |  0    |  0   |   1    |    0     |  0  |   0   |   0    | char signed    |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  1   |  0    |  0   |   0    |    1     |  0  |   0   |   0    | char unsigned  |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  1    |  0   |   0    |    0     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|                |
    |  0   |  0   |  1    |  0   |   1    |    0     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------| short          |
    |  0   |  0   |  1    |  0   |   0    |    0     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|                |
    |  0   |  0   |  1    |  0   |   1    |    0     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  1    |  0   |   0    |    1     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------| short unsigned |
    |  0   |  0   |  1    |  0   |   0    |    1     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  0    |  0   |   0    |    0     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|                |
    |  0   |  0   |  0    |  0   |   1    |    0     |  1  |   0   |   0    | int            |
    |------|------|-------|------|--------|----------|-----|-------|--------|                |
    |  0   |  0   |  0    |  0   |   1    |    0     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  0    |  0   |   0    |    1     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------| int unsigned   |
    |  0   |  0   |  0    |  0   |   0    |    1     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  0    |  1   |   0    |    0     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|                |
    |  0   |  0   |  0    |  1   |   1    |    0     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------| long           |
    |  0   |  0   |  0    |  1   |   0    |    0     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|                |
    |  0   |  0   |  0    |  1   |   1    |    0     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  0    |  1   |   0    |    1     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------| long unsigned  |
    |  0   |  0   |  0    |  1   |   0    |    1     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  0    |  2   |   0    |    0     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|                |
    |  0   |  0   |  0    |  2   |   1    |    0     |  0  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------| long long      |
    |  0   |  0   |  0    |  2   |   0    |    0     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|                |
    |  0   |  0   |  0    |  2   |   1    |    0     |  1  |   0   |   0    |                |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  0    |  0   |   0    |    0     |  0  |   1   |   0    | float          |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  0    |  0   |   0    |    0     |  0  |   0   |   1    | double         |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    |  0   |  0   |  0    |  1   |   0    |    0     |  0  |   0   |   1    | double long    |
    |------|------|-------|------|--------|----------|-----|-------|--------|----------------|
    */
    // TODO:
    // _Bool
    // float _Complex
    // double _Complex
    // long double _Complex
    // atomic type specifier
    
    // struct or union specifier
    // enum specifier
    
    // typedef name
    use std::convert::TryInto;
    use multimap::MultiMap;
    use crate::r#type;
    use lang_c::{ast::*, span::*};
    use crate::diagnostics::*;

    #[derive(Copy, Clone, Eq, PartialEq, Hash)]
    enum TypeSpecifierKey {
        Void,
        Char,
        Short,
        Int,
        Long,
        Float,
        Double,
        Signed,
        Unsigned,
        Bool,
        Complex,
        Atomic,
        Struct,
        Enum,
        TypedefName,
        TypeOf,
        TS18661Float,
    }

    #[derive(Clone)]
    pub struct TypeHolder<'a>(MultiMap::<TypeSpecifierKey, &'a Node<TypeSpecifier>>);

    impl<'a> TypeHolder<'a> {
        pub fn new() -> Self {
            Self(Default::default())
        }

        pub fn process_specifier(&mut self, item: &'a Node<TypeSpecifier>) {
            let key = match &item.node {
                TypeSpecifier::Void => TypeSpecifierKey::Void,
                TypeSpecifier::Char => TypeSpecifierKey::Char,
                TypeSpecifier::Short => TypeSpecifierKey::Short,
                TypeSpecifier::Int => TypeSpecifierKey::Int,
                TypeSpecifier::Long => TypeSpecifierKey::Long,
                TypeSpecifier::Float => TypeSpecifierKey::Float,
                TypeSpecifier::Double => TypeSpecifierKey::Double,
                TypeSpecifier::Signed => TypeSpecifierKey::Signed,
                TypeSpecifier::Unsigned => TypeSpecifierKey::Unsigned,
                TypeSpecifier::Bool => TypeSpecifierKey::Bool,
                TypeSpecifier::Complex => TypeSpecifierKey::Complex,
                TypeSpecifier::Atomic(_) => TypeSpecifierKey::Atomic,
                TypeSpecifier::Struct(_) => todo!(),
                TypeSpecifier::Enum(_) => todo!(),
                TypeSpecifier::TypedefName(_) => todo!(),
                TypeSpecifier::TypeOf(_) => todo!(),
                TypeSpecifier::TS18661Float(_) => todo!(),
            };
            self.0.insert(key, item);
        }
    }

    impl<'a> TryInto<r#type::Type> for TypeHolder<'a> {
        type Error = Diagnostic;
        
        fn try_into(self) -> Result<r#type::Type, Self::Error> {
            if self.0.len() > 3 {
                let iter = self.0.into_iter()
                    .flat_map(|item| item.1.into_iter());
                return Err(wrong_type_declaration(iter));
            }

            if self.0.len() == 1 {
                if self.0.get(&TypeSpecifierKey::Void).is_some() {
                    return Ok(r#type::Type::new_void());
                }
                if self.0.get(&TypeSpecifierKey::Char).is_some() {
                    return Ok(r#type::Type::new_unsigned_char());
                }
                if self.0.get(&TypeSpecifierKey::Short).is_some() {
                    return Ok(r#type::Type::new_signed_short_int());
                }

                if self.0.get(&TypeSpecifierKey::Int).is_some() 
                || self.0.get(&TypeSpecifierKey::Signed).is_some() {
                    return Ok(r#type::Type::new_signed_int());
                }
                if self.0.get(&TypeSpecifierKey::Unsigned).is_some() {
                    return Ok(r#type::Type::new_unsigned_int());
                }
                if self.0.get(&TypeSpecifierKey::Long).is_some() {
                    return Ok(r#type::Type::new_signed_long_int());
                }
                if self.0.get(&TypeSpecifierKey::Float).is_some() {
                    return Ok(r#type::Type::new_float());
                } 
                if self.0.get(&TypeSpecifierKey::Double).is_some() {
                    return Ok(r#type::Type::new_double());
                } 
            }
            todo!()
        }
    }
