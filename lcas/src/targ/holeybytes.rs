use std::str::FromStr;

use arch_ops::holeybytes::{
    Address, Instruction, Opcode, Operands, OpsType, Register, Relative16, Relative32,
};

use super::TargetMachine;
use crate::{
    as_state::{float_to_bytes_le, int_to_bytes_le, AsState},
    lex::Token,
};
use arch_ops::holeybytes;

#[derive(Default, Clone, Hash, PartialEq, Eq)]
struct Data {}

pub struct HbTargetMachine;
impl TargetMachine for HbTargetMachine {
    #[inline]
    fn group_chars(&self) -> &[char] {
        &['(', '[']
    }

    #[inline]
    fn comment_chars(&self) -> &[char] {
        &[';']
    }

    #[inline]
    fn extra_sym_chars(&self) -> &[char] {
        &['_', '$', '.']
    }

    #[inline]
    fn extra_sym_part_chars(&self) -> &[char] {
        &['_', '$', '.']
    }

    #[inline]
    fn extra_sigil_chars(&self) -> &[char] {
        &[]
    }

    #[inline]
    fn create_data(&self) -> Box<dyn std::any::Any> {
        Box::<Data>::default()
    }

    #[inline]
    fn int_to_bytes<'a>(&self, val: u128, buf: &'a mut [u8]) -> &'a mut [u8] {
        int_to_bytes_le(val, buf)
    }

    #[inline]
    fn float_to_bytes<'a>(&self, val: f64, buf: &'a mut [u8]) -> &'a mut [u8] {
        float_to_bytes_le(val, buf)
    }

    #[inline]
    fn long_width(&self) -> usize {
        core::mem::size_of::<u64>()
    }

    #[inline]
    fn assemble_insn(&self, opc: &str, state: &mut AsState) -> std::io::Result<()> {
        let opcode = Opcode::from_str(opc)
            .map_err(|_| std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid opcode"))?;

        let ops = extract_ops(opcode.ops_type(), state.iter()).ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid operands")
        })?;

        holeybytes::codec::HbEncoder::new(state.output())
            .write_instruction(Instruction::new_unchecked(opcode, ops))
    }

    #[inline]
    fn directive_names(&self) -> &[&str] {
        &[]
    }

    #[inline]
    fn handle_directive(&self, _dir: &str, _state: &mut AsState) -> std::io::Result<()> {
        unreachable!("There ain't no directives yet.")
    }

    #[inline]
    fn def_section_alignment(&self) -> u64 {
        8 // ?
    }

    #[inline]
    fn newline_sensitive(&self) -> bool {
        false
    }
}

#[inline]
pub fn get_target_def() -> &'static HbTargetMachine {
    &HbTargetMachine
}

pub fn extract_ops(opsty: OpsType, iter: &mut impl Iterator<Item = Token>) -> Option<Operands> {
    macro_rules! generate {
        ($opsty:expr, $iter:expr, { $($name:ident ($($subst:pat),* $(,)?)),* $(,)? }) => {{
            let opsty = $opsty;
            let iter  = $iter;

            Some(match opsty {
                $(OpsType::$name => {
                    Operands::$name(
                        holeybytes::$name(
                            $({
                                #[allow(clippy::let_unit_value)]
                                let $subst = ();
                                
                                FromToken::from_token(iter.next()?)?
                            }),*
                        )
                    )
                }),*
            })
        }};
    }

    generate!(opsty, iter, {
        OpsRR   (_, _),
        OpsRRR  (_, _, _),
        OpsRRRR (_, _, _, _),
        OpsRRB  (_, _, _),
        OpsRRH  (_, _, _),
        OpsRRW  (_, _, _),
        OpsRB   (_, _),
        OpsRH   (_, _),
        OpsRW   (_, _),
        OpsRD   (_, _),
        OpsRRD  (_, _, _),
        OpsRRA  (_, _, _),
        OpsRRAH (_, _, _, _),
        OpsRROH (_, _, _, _),
        OpsRRPH (_, _, _, _),
        OpsRRO  (_, _, _),
        OpsRRP  (_, _, _),
        OpsO    (_),
        OpsP    (_),
        OpsN    ( ),
    })
}

trait FromToken: Sized {
    fn from_token(token: Token) -> Option<Self>;
}

impl FromToken for Register {
    fn from_token(token: Token) -> Option<Self> {
        if let Token::Identifier(lit) = token {
            Some(Self(lit.strip_prefix('r')?.parse::<u8>().ok()?))
        } else {
            None
        }
    }
}

impl FromToken for Address {
    fn from_token(token: Token) -> Option<Self> {
        todo!()
    }
}

impl FromToken for Relative16 {
    fn from_token(token: Token) -> Option<Self> {
        todo!()
    }
}

impl FromToken for Relative32 {
    fn from_token(token: Token) -> Option<Self> {
        todo!()
    }
}

macro_rules! from_token_imms {
    ($($ty:ident),* $(,)?) => {
        $(impl FromToken for $ty {
            fn from_token(token: Token) -> Option<Self> {
                use std::convert::TryFrom;
                if let Token::IntegerLiteral(lit) = token {
                    Some($ty::try_from(lit).ok()?)
                } else {
                    None
                }
            }
        })*
    };
}

from_token_imms!(u8, u16, u32, u64);
