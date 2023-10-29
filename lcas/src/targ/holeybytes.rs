use super::{clever::CleverTargetMachine, TargetMachine};
use crate::as_state::{float_to_bytes_le, int_to_bytes_le};

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
    fn assemble_insn(
        &self,
        opc: &str,
        state: &mut crate::as_state::AsState,
    ) -> std::io::Result<()> {
        todo!()
    }

    #[inline]
    fn directive_names(&self) -> &[&str] {
        &[]
    }

    #[inline]
    fn handle_directive(
        &self,
        _dir: &str,
        _state: &mut crate::as_state::AsState,
    ) -> std::io::Result<()> {
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
pub fn get_target_def() -> &'static CleverTargetMachine {
    &CleverTargetMachine
}
