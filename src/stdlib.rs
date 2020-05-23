use crate::mir;
use mir::Ty;
// Standard library function names

pub const STDLIB_SCAN_CHAR: &'static str = "__std_scan_char";
pub const STDLIB_SCAN_INT: &'static str = "__std_scan_int";
pub const STDLIB_SCAN_DOUBLE: &'static str = "__std_scan_double";

pub const STDLIB_PUT_CHAR: &'static str = "__std_put_char";
pub const STDLIB_PUT_INT: &'static str = "__std_put_int";
pub const STDLIB_PUT_DOUBLE: &'static str = "__std_put_double";
pub const STDLIB_PUT_STR: &'static str = "__std_put_str";

// Standard library function types
pub fn scan_char_ty() -> Ty {
    Ty::function_of(Ty::int(), [], true)
}
pub fn scan_int_ty() -> Ty {
    Ty::function_of(Ty::int(), [], true)
}
pub fn scan_double_ty() -> Ty {
    Ty::function_of(Ty::double(), [], true)
}
pub fn put_char_ty() -> Ty {
    Ty::function_of(Ty::Void, [Ty::int()], true)
}
pub fn put_int_ty() -> Ty {
    Ty::function_of(Ty::Void, [Ty::int()], true)
}
pub fn put_double_ty() -> Ty {
    Ty::function_of(Ty::Void, [Ty::double()], true)
}
pub fn put_str_ty() -> Ty {
    Ty::function_of(Ty::Void, [Ty::ptr_of(Ty::byte()), Ty::int()], true)
}

pub mod c0 {
    use crate::{
        c0::ast::{FunctionType, PrimitiveType, PrimitiveTypeVar, RefType, TypeDef},
        prelude::Ptr,
    };
    pub fn scan_char_ty() -> TypeDef {
        TypeDef::Function(FunctionType {
            params: Vec::new(),
            return_type: Ptr::new(TypeDef::Primitive(PrimitiveType {
                occupy_bytes: 4,
                var: PrimitiveTypeVar::SignedInt,
            })),
            body: None,
            is_extern: true,
        })
    }
    pub fn scan_int_ty() -> TypeDef {
        TypeDef::Function(FunctionType {
            params: Vec::new(),
            return_type: Ptr::new(TypeDef::Primitive(PrimitiveType {
                occupy_bytes: 4,
                var: PrimitiveTypeVar::SignedInt,
            })),
            body: None,
            is_extern: true,
        })
    }
    pub fn scan_double_ty() -> TypeDef {
        TypeDef::Function(FunctionType {
            params: Vec::new(),
            return_type: Ptr::new(TypeDef::Primitive(PrimitiveType {
                occupy_bytes: 8,
                var: PrimitiveTypeVar::Float,
            })),
            body: None,
            is_extern: true,
        })
    }
    pub fn put_char_ty() -> TypeDef {
        TypeDef::Function(FunctionType {
            params: vec![Ptr::new(TypeDef::Primitive(PrimitiveType {
                occupy_bytes: 4,
                var: PrimitiveTypeVar::SignedInt,
            }))],
            return_type: Ptr::new(TypeDef::Unit),
            body: None,
            is_extern: true,
        })
    }
    pub fn put_int_ty() -> TypeDef {
        TypeDef::Function(FunctionType {
            params: vec![Ptr::new(TypeDef::Primitive(PrimitiveType {
                occupy_bytes: 4,
                var: PrimitiveTypeVar::SignedInt,
            }))],
            return_type: Ptr::new(TypeDef::Unit),
            body: None,
            is_extern: true,
        })
    }
    pub fn put_double_ty() -> TypeDef {
        TypeDef::Function(FunctionType {
            params: vec![Ptr::new(TypeDef::Primitive(PrimitiveType {
                occupy_bytes: 8,
                var: PrimitiveTypeVar::Float,
            }))],
            return_type: Ptr::new(TypeDef::Unit),
            body: None,
            is_extern: true,
        })
    }
    pub fn put_str_ty() -> TypeDef {
        TypeDef::Function(FunctionType {
            params: vec![Ptr::new(TypeDef::Ref(RefType {
                target: Ptr::new(TypeDef::Primitive(PrimitiveType {
                    occupy_bytes: 1,
                    var: PrimitiveTypeVar::SignedInt,
                })),
            }))],
            return_type: Ptr::new(TypeDef::Unit),
            body: None,
            is_extern: true,
        })
    }
}
