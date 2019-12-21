use super::codegen::*;
use super::err::*;
use super::*;
use crate::c0::ast::{self, *};
use crate::prelude::*;
use indexmap::{map::Entry, IndexMap};

/// Implementation for smaller, instruction-wise structures
impl<'b> FnCodegen<'b> {
    /// Generate implicit conversion for `a` and `b` to match their types.
    fn flatten_ty(&mut self, a: Type, b: Type) -> CompileResult<Type> {
        todo!()
    }

    /// Generate implicit conversion for `val` to match `tgt` type
    fn implicit_conv(&mut self, val: Type, tgt: Ptr<ast::TypeDef>) -> CompileResult<Type> {
        use TypeDef::*;
        match &*tgt.borrow() {
            Unit | Unknown | TypeErr => {
                // self.inst_sink().push(Ins::Pop1)
                Ok(tgt.cp())
            }
            _ => match &*a.borrow() {
                Primitive(p) => {
                    if let Primitive(o) = a {
                        if o.occupy_bytes >= p.occupy_bytes && o.var == p.var {
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                Ref(r) => {
                    // TODO: r.target == Unit
                    false
                }
                _ => false,
            },
        }
        // match &*val.borrow() {
        //     ast::TypeDef::Unit => Err(CompileError::AssignVoid),
        //     ast::TypeDef::Primitive(p) => {
        //         todo!()
        //         //
        //     }
        //     ast::TypeDef::Ref(r) => {
        //         todo!()
        //         //
        //     }
        //     ast::TypeDef::NamedType(..) => Err(CompileError::InternalError(
        //         "Named types shouldn't appear in type calculation".into(),
        //     )),
        //     _ => Err(CompileError::UnsupportedType),
        // }
    }

    fn add(&mut self, lhs: Type, rhs: Type) -> CompileResult<Type> {
        todo!()
    }

    fn pop(&mut self, ty: Type) -> CompileResult<()> {
        let slots = ty
            .borrow()
            .occupy_slots()
            .ok_or(CompileError::RequireSized(format!("{:?}", ty.cp())))?;
        match slots {
            0 => (),
            1 => self.inst_sink().push(Inst::Pop1),
            2 => self.inst_sink().push(Inst::Pop2),
            n @ _ => self.inst_sink().push(Inst::PopN(n as u32)),
        }
        Ok(())
    }
}
