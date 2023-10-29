use std::str::FromStr;

use arch_ops::holeybytes::{Instruction, Opcode, Register};

use super::TargetMachine;
use crate::{
    as_state::{float_to_bytes_le, int_to_bytes_le, AsState},
    lex::Token,
};

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
        parse_insn(opc, state);
        Ok(())
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

fn parse_insn(opc: &str, state: &mut AsState) -> Option<Instruction> {
    let mut iter = state.iter();

    let opcode = Opcode::from_str(opc).ok()?;
    None
}

trait FromToken {
    fn from_token(token: Token) -> Option<Self>
    where
        Self: Sized;
}

impl FromToken for Register {
    fn from_token(token: Token) -> Option<Self>
    where
        Self: Sized,
    {
        let Token::Identifier(lit) = token else {
            return None;
        };

        Some(Self(lit.strip_prefix('r')?.parse::<u8>().ok()?))
    }
}
