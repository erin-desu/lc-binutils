use super::TargetMachine;
use crate::{
    as_state::{float_to_bytes_le, int_to_bytes_le, AsState},
    expr,
    lex::Token,
};
use arch_ops::holeybytes;
use arch_ops::holeybytes::{
    Address, Instruction, Opcode, Operands, OpsType, Register, Relative16, Relative32,
};
use std::{convert::TryFrom, fmt::Display, str::FromStr};

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

        let ops = extract_ops(opcode.ops_type(), state.iter())
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

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

pub fn extract_ops(opsty: OpsType, iter: &mut impl Iterator<Item = Token>) -> Result<Operands> {
    macro_rules! ignore_const_one {
        ($_:tt) => {
            1
        };
    }

    macro_rules! generate {
        (
            $opsty:expr, $iter:expr, {
                $($name:ident (
                    $($subst:pat),* $(,)?
                )),* $(,)?
            }
        ) => {{
            let opsty = $opsty;
            let iter  = $iter;

            Ok(match opsty {
                $(OpsType::$name => {
                    #[allow(unused)]
                    const OPSN: isize = 0 $( + ignore_const_one!($subst))*;
                    #[allow(unused)]
                    let mut counter = 0;

                    Operands::$name(
                        holeybytes::$name(
                            $({
                                #[allow(clippy::let_unit_value)]
                                let $subst = ();
                                let item = FromToken::from_token(iter.next().ok_or(Error::NotEnoughTokens)?)?;

                                counter += 1;
                                if counter < OPSN
                                    && !matches!(iter.next().ok_or(Error::NotEnoughTokens)?, Token::Sigil(s) if s == ",")
                                    { return Err(Error::TooManyOps); }

                                item
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
    fn from_token(token: Token) -> Result<Self>;
}

impl FromToken for Register {
    fn from_token(token: Token) -> Result<Self> {
        if let Token::Identifier(lit) = token {
            Ok(Self(
                lit.strip_prefix('r')
                    .ok_or(Error::ExpectedRegister)?
                    .parse::<u8>()
                    .map_err(|_| Error::ExpectedRegister)?,
            ))
        } else {
            Err(Error::UnexpectedToken)
        }
    }
}

impl FromToken for Address {
    fn from_token(token: Token) -> Result<Self> {
        match token {
            Token::Identifier(name) => Ok(Address::Symbol { name, disp: 0 }),
            Token::IntegerLiteral(addr) => Ok(Address::Abs(addr)),
            _ => Err(Error::UnexpectedToken),
        }
    }
}

fn from_token_rela<T>(token: Token) -> Result<Address>
where
    T: TryFrom<i128> + Into<i64>,
{
    match token {
        Token::Identifier(name) => Ok(Address::Symbol { name, disp: 0 }),
        Token::IntegerLiteral(disp) => Ok(Address::Disp(
            T::try_from(disp as i128)
                .map_err(|_| Error::IntTooBig)?
                .into(),
        )),
        _ => Err(Error::UnexpectedToken),
    }
}

impl FromToken for Relative16 {
    #[inline]
    fn from_token(token: Token) -> Result<Self> {
        from_token_rela::<i16>(token).map(Self)
    }
}

impl FromToken for Relative32 {
    #[inline]
    fn from_token(token: Token) -> Result<Self> {
        from_token_rela::<i32>(token).map(Self)
    }
}

macro_rules! from_token_imms {
    ($($ty:ident),* $(,)?) => {
        $(impl FromToken for $ty {
            fn from_token(token: Token) -> Result<Self> {
                use std::convert::TryFrom;
                if let Token::IntegerLiteral(lit) = token {
                    Ok($ty::try_from(lit).map_err(|_| Error::IntTooBig)?)
                } else {
                    Err(Error::UnexpectedToken)
                }
            }
        })*
    };
}

from_token_imms!(u8, u16, u32, u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Error {
    IntTooBig,
    UnexpectedToken,
    ExpectedRegister,
    TooManyOps,
    NotEnoughTokens,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::IntTooBig => "Integer is too big",
            Self::UnexpectedToken => "Unexpected token",
            Self::ExpectedRegister => "Expected register",
            Self::TooManyOps => "Too many operands",
            Self::NotEnoughTokens => "Not enough tokens",
        })
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
