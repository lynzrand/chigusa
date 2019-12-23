use super::codegen::*;
use super::err::*;
use super::*;
use crate::c0::ast::{self, *};
use crate::prelude::*;
use indexmap::{map::Entry, IndexMap};

/// Generate type conversion for `a` and `b` to match their types.
///
/// If both a and b are primitive types, they are implicitly converted
/// according to rules. If either side is void, the code will return an error.
/// Otherwise, the right hand side (b) is converted to left hand side (a).
///
/// # Returns
///
/// 0 - Operations for lhs
/// 1 - Operations for rhs
/// 2 - Resulting type
pub(super) fn flatten_ty(
    a: Type,
    a_sink: &mut InstSink,
    b: Type,
    b_sink: &mut InstSink,
) -> CompileResult<Type> {
    use TypeDef::*;

    if *a.borrow() == Unit || *b.borrow() == Unit {
        return Err(CompileError::AssignVoid);
    }

    if let Primitive(p) = &*a.borrow() {
        if let Primitive(q) = &*b.borrow() {
            use ast::PrimitiveTypeVar::*;
            if p.var == Float {
                if q.var != Float {
                    conv(b.cp(), a.cp(), b_sink)
                } else {
                    Ok(a.cp())
                }
            } else {
                if q.var != Float {
                    conv(b.cp(), a.cp(), b_sink)
                } else {
                    if p.occupy_bytes > q.occupy_bytes {
                        conv(b.cp(), a.cp(), a_sink)
                    } else {
                        conv(a.cp(), b.cp(), a_sink)
                    }
                }
            }
        } else {
            conv(b.cp(), a.cp(), b_sink)
        }
    } else {
        conv(b.cp(), a.cp(), b_sink)
    }
}

/// Generate implicit conversion for `val` to match `tgt` type
pub(super) fn conv(from: Type, to: Type, sink: &mut InstSink) -> CompileResult<Type> {
    use TypeDef::*;
    match &*to.borrow() {
        Unit => {
            pop(from, sink)?;
            Ok(to.cp())
        }
        Unknown | TypeErr => Err(CompileError::ErrorType),
        Primitive(t) => match &*from.borrow() {
            Primitive(f) => {
                use ast::PrimitiveTypeVar::*;
                match (f.var, t.var) {
                    (Float, UnsignedInt) | (Float, SignedInt) => sink.push(Inst::D2I),

                    (UnsignedInt, Float) | (SignedInt, Float) => sink.push(Inst::I2D),

                    (SignedInt, UnsignedInt) if t.occupy_bytes == 1 => sink.push(Inst::I2C),
                    _ => (),
                };

                Ok(to.cp())
            }
            Ref(..) => Err(CompileError::MakePrimitiveFromRef),
            _ => Err(CompileError::UnsupportedType),
        },
        Ref(r) => match &*from.borrow() {
            Ref(r1) => {
                if r == r1 {
                    Ok(to.cp())
                } else {
                    log::warn!("Implicit ref type change: {:?} -> {:?}", from, to);
                    Ok(to.cp())
                }
            }
            _ => Err(CompileError::MakeRefFromPrimitive),
        },
        ast::TypeDef::NamedType(..) => Err(CompileError::InternalError(
            "Named types shouldn't appear in type calculation".into(),
        )),
        _ => Err(CompileError::UnsupportedType),
    }
}

pub(super) fn pop(ty: Type, sink: &mut InstSink) -> CompileResult<()> {
    let slots = ty
        .borrow()
        .occupy_slots()
        .ok_or(CompileError::RequireSized(format!("{:?}", ty.cp())))?;
    match slots {
        0 => (),
        1 => sink.push(Inst::Pop1),
        2 => sink.push(Inst::Pop2),
        n @ _ => sink.push(Inst::PopN(n as u32)),
    }
    Ok(())
}

pub(super) fn store(ty: Type, sink: &mut InstSink) -> CompileResult<()> {
    let slots = ty
        .borrow()
        .occupy_slots()
        .ok_or(CompileError::RequireSized(format!("{:?}", ty.cp())))?;
    match slots {
        0 => (),
        1 => sink.push(Inst::IStore),
        2 => sink.push(Inst::DStore),
        n @ _ => Err(CompileError::UnsupportedType)?,
    }
    Ok(())
}
