use std::{
    convert::TryFrom,
    io::{ErrorKind, Read, Write},
    ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
};

use crate::traits::{Address, InsnRead, InsnWrite};

#[derive(Debug)]
pub struct CleverExtensionFromStrError;

impl core::fmt::Display for CleverExtensionFromStrError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("Unknown clever")
    }
}

impl std::error::Error for CleverExtensionFromStrError {}

macro_rules! define_clever_features{
    {
        $(($enum:ident, $feature:literal)),* $(,)?
    } => {
        #[derive(Copy,Clone,Debug,Hash,PartialEq,Eq)]
        #[non_exhaustive]
        #[repr(i32)]
        pub enum CleverExtension{
            $($enum,)*
        }

        impl CleverExtension{
            pub fn extension_name(&self) -> &'static str{
                match self{
                    $(#[allow(unreachable_patterns)] Self::$enum => $feature,)*
                }
            }
        }

        impl core::str::FromStr for CleverExtension{
            type Err = CleverExtensionFromStrError;
            fn from_str(x: &str) -> Result<Self,Self::Err>{
                match x{

                    $(#[allow(unreachable_patterns)] $feature => Ok(Self::$enum),)*
                    _ => Err(CleverExtensionFromStrError)
                }
            }
        }

        impl core::fmt::Display for CleverExtension{
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
                match self{
                    $(Self::$enum => f.write_str($feature),)*
                }
            }
        }
    }
}

define_clever_features! {
    (Main, "main"),
    (Float, "float"),
    (FloatExt, "float-ext"),
    (Vec, "vec"),
    (Rand, "rand"),
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct CleverRegister(pub u8);

pub struct RegisterFromStrError;

macro_rules! clever_registers{
    {
        $($name:ident $(| $altnames:ident)* => $val:expr),* $(,)?
    } => {
        #[allow(non_upper_case_globals)]
        impl CleverRegister{
            $(pub const $name: Self = Self($val); $(pub const $altnames: Self = Self($val);)*)*
        }
        impl ::core::str::FromStr for CleverRegister{
            type Err = RegisterFromStrError;
            fn from_str(st: &str) -> Result<Self,Self::Err>{
                match st{
                    $(
                        ::core::stringify!($name) $(| ::core::stringify!($altnames))* => Ok(Self($val)),
                    )*
                    _ => Err(RegisterFromStrError)
                }
            }
        }
        impl ::core::fmt::Display for CleverRegister{
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
                match self{
                    $(CleverRegister($val) => f.write_str(::core::stringify!($name)),)*
                    CleverRegister(val) => f.write_fmt(::core::format_args!("r{}",val))
                }
            }
        }

        impl ::core::fmt::Debug for CleverRegister{
            fn fmt(&self, f: &mut core::fmt::Formatter) -> ::core::fmt::Result{
                struct DontEscape(&'static str);
                impl ::core::fmt::Debug for DontEscape{
                    fn fmt(&self, f: &mut core::fmt::Formatter) -> ::core::fmt::Result{
                        f.write_str(self.0)
                    }
                }

                match self{
                    $(CleverRegister($val) => {
                        f.debug_tuple("CleverRegister")
                            .field(&DontEscape(::core::stringify!($name))).finish()
                    })*
                    CleverRegister(val) => f.debug_tuple("CleverRegister").field(&val).finish()
                }
            }
        }
    }
}

clever_registers! {
    r0 | racc => 0,
    r1 | rsrc => 1,
    r2 | rdst => 2,
    r3 | rcnt => 3,
    r4 => 4,
    r5 => 5,
    r6 | fbase => 6,
    r7 | sptr => 7,
    r8 => 8,
    r9 => 9,
    r10 => 10,
    r11 => 11,
    r12 => 12,
    r13 => 13,
    r14 => 14,
    r15 | link => 15,
    ip => 16,
    flags => 17,
    mode => 18,
    fpcw => 19,
    f0 => 24,
    f1 => 25,
    f2 => 26,
    f3 => 27,
    f4 => 28,
    f5 => 29,
    f6 => 30,
    f7 => 31,
    v0l => 64,
    v0h => 65,
    v1l => 66,
    v1h => 67,
    v2l => 68,
    v2h => 69,
    v3l => 70,
    v3h => 71,
    v4l => 72,
    v4h => 73,
    v5l => 74,
    v5h => 75,
    v6l => 76,
    v6h => 77,
    v7l => 78,
    v7h => 79,
    v8l => 80,
    v8h => 81,
    v9l => 82,
    v9h => 83,
    v10l => 84,
    v10h => 85,
    v11l => 86,
    v11h => 87,
    v12l => 88,
    v12h => 89,
    v13l => 90,
    v13h => 91,
    v14l => 92,
    v14h => 93,
    v15l => 94,
    v15h => 95,
    v16l => 96,
    v16h => 97,
    v17l => 98,
    v17h => 99,
    v18l => 100,
    v18h => 101,
    v19l => 102,
    v19h => 103,
    v20l => 104,
    v20h => 105,
    v21l => 106,
    v21h => 107,
    v22l => 108,
    v22h => 109,
    v23l => 110,
    v23h => 111,
    v24l => 112,
    v24h => 113,
    v25l => 114,
    v25h => 115,
    v26l => 116,
    v26h => 117,
    v27l => 118,
    v27h => 119,
    v28l => 120,
    v28h => 121,
    v29l => 122,
    v29h => 123,
    v30l => 124,
    v30h => 125,
    v31l => 126,
    v31h => 127,
    cr0 => 128,
    page | cr1 => 129,
    flprotected | cr2 => 130,
    scdp | cr3 => 131,
    scsp | cr4 => 132,
    sccr | cr5 => 133,
    itabp | cr6 => 134,
    ciread | cr7 => 135,
    cpuidlo => 136,
    cpuidhi => 137,
    cpuex2 => 138,
    cpuex3 => 139,
    cpuex4 => 140,
    cpuex5 => 141,
    cpuex6 => 142,
    mscpuex => 143,
    msr0 => 148,
    msr1 => 149,
    msr2 => 150,
    msr3 => 151,
    msr4 => 152,
    msr5 => 153,
    msr6 => 154,
    rdinfo => 156
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum ConditionCode {
    Parity,
    Carry,
    Overflow,
    Zero,
    LessThan,
    LessEq,
    BelowEq,
    Minus,
    Plus,
    Above,
    Greater,
    GreaterEq,
    NotZero,
    NoOverflow,
    NoCarry,
    NoParity,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CleverIndex {
    Register(CleverRegister),
    Abs(i16),
}

impl core::fmt::Display for CleverIndex {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Register(r) => r.fmt(f),
            Self::Abs(n) => n.fmt(f),
        }
    }
}

trait HBits {
    fn from_bits(bits: u16) -> Self;
    fn to_hbits(self) -> u16;
}

impl HBits for bool {
    fn from_bits(bits: u16) -> Self {
        bits != 0
    }

    fn to_hbits(self) -> u16 {
        self as u16
    }
}

impl HBits for u8 {
    fn from_bits(bits: u16) -> Self {
        bits as u8
    }
    fn to_hbits(self) -> u16 {
        self as u16
    }
}

impl HBits for i8 {
    fn from_bits(bits: u16) -> Self {
        (bits as i8) | ((bits & 0x8).wrapping_neg() as i8)
    }
    fn to_hbits(self) -> u16 {
        (self & 0xf) as u16
    }
}

// Because ss is typically represented as `u16` for reasons
impl HBits for u16 {
    fn from_bits(bits: u16) -> Self {
        bits
    }
    fn to_hbits(self) -> u16 {
        self
    }
}

impl HBits for CleverRegister {
    fn from_bits(bits: u16) -> Self {
        Self(bits as u8)
    }

    fn to_hbits(self) -> u16 {
        self.0 as u16
    }
}

impl HBits for ConditionCode {
    fn from_bits(bits: u16) -> Self {
        match bits {
            0 => ConditionCode::Parity,
            1 => ConditionCode::Carry,
            2 => ConditionCode::Overflow,
            3 => ConditionCode::Zero,
            4 => ConditionCode::LessThan,
            5 => ConditionCode::LessEq,
            6 => ConditionCode::BelowEq,
            7 => ConditionCode::Minus,
            8 => ConditionCode::Plus,
            9 => ConditionCode::Above,
            10 => ConditionCode::Greater,
            11 => ConditionCode::GreaterEq,
            12 => ConditionCode::NotZero,
            13 => ConditionCode::NoOverflow,
            14 => ConditionCode::NoCarry,
            15 => ConditionCode::NoParity,
            _ => unreachable!(),
        }
    }

    fn to_hbits(self) -> u16 {
        self as u8 as u16
    }
}

trait HBitRange {
    fn shift(&self) -> u32;
    fn mask(&self) -> u16;
}

impl HBitRange for u32 {
    fn shift(&self) -> u32 {
        *self
    }

    fn mask(&self) -> u16 {
        1u16 << (*self)
    }
}

impl HBitRange for Range<u32> {
    fn shift(&self) -> u32 {
        self.start
    }

    fn mask(&self) -> u16 {
        ((1u16 << ((self.end - 1) - self.start)) - 1) << self.start
    }
}

impl HBitRange for RangeInclusive<u32> {
    fn shift(&self) -> u32 {
        *self.start()
    }

    fn mask(&self) -> u16 {
        ((1u16 << (self.end() - self.start())) - 1) << self.start()
    }
}

impl HBitRange for RangeFrom<u32> {
    fn shift(&self) -> u32 {
        self.start
    }

    fn mask(&self) -> u16 {
        ((1u16 << (4 - self.start)) - 1) << self.start
    }
}

impl HBitRange for RangeTo<u32> {
    fn shift(&self) -> u32 {
        0
    }

    fn mask(&self) -> u16 {
        (1u16 << (self.end - 1)) - 1
    }
}

impl HBitRange for RangeToInclusive<u32> {
    fn shift(&self) -> u32 {
        0
    }

    fn mask(&self) -> u16 {
        (1u16 << self.end) - 1
    }
}

impl HBitRange for RangeFull {
    fn shift(&self) -> u32 {
        0
    }

    fn mask(&self) -> u16 {
        0xf
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CleverOperandKind {
    Normal(u32),
    AbsAddr,
    RelAddr,
    // prefix
    Insn,
}

macro_rules! clever_instructions{
    {
        $([$enum:ident, $insn:literal, $opcode:literal, $operands:expr $(, { $($hfield:ident @ $range:expr => $ty:ty ),* $(,)?})? ]),* $(,)?
    } => {

        #[derive(Copy,Clone,Debug,Hash,PartialEq, Eq)]
        pub enum CleverOpcode{
            $($enum $({$($hfield: $ty),*})?),*
        }

        impl CleverOpcode{

            pub fn from_opcode(opc: u16) -> Option<CleverOpcode>{
                match opc>>4{
                    $(#[allow(unreachable_patterns)] $opcode => {
                        Some(Self:: $enum $({$($hfield: HBits::from_bits(
                            (opc>>HBitRange::shift(&$range))&HBitRange::mask(&$range)
                        )),*})?)
                    },)*
                    _ => None
                }
            }

            pub fn name(&self) -> &'static str{
                match self{
                    $(Self:: $enum $({ $($hfield: _),*})? => $insn),*
                }
            }

            pub fn opcode(&self) -> u16{
                match self{
                    $(Self:: $enum $({$($hfield),*})? => {
                        let base: u16 = $opcode;
                        #[allow(unused_mut)] // mut may be unused if the instruction doesn't have any h bits
                        let mut opc = base<<4;
                        $($({
                            let range = $range;

                            let bits = (HBits::to_hbits(*$hfield)<< HBitRange::shift(&range))&HBitRange::mask(&range);
                            opc |= bits;
                        })*)?
                        opc
                    })*
                }
            }

            pub fn operands(&self) -> CleverOperandKind{
                match self{
                    $(Self:: $enum {..} => $operands),*
                }
            }
        }
    }
}

clever_instructions! {
    // Undefined Instruction 0
    [Und0, "und", 0x000, CleverOperandKind::Normal(0)],

    // Arithmetic Instructions
    [Add, "add", 0x001, CleverOperandKind::Normal(2), {lock @ 3 => bool, flags @ 0 => bool}],
    [Sub, "sub", 0x002, CleverOperandKind::Normal(2), {lock @ 3 => bool, flags @ 0 => bool}],
    [And, "and", 0x003, CleverOperandKind::Normal(2), {lock @ 3 => bool, flags @ 0 => bool}],
    [Or , "or" , 0x004, CleverOperandKind::Normal(2), {lock @ 3 => bool, flags @ 0 => bool}],
    [Xor, "xor", 0x005, CleverOperandKind::Normal(2), {lock @ 3 => bool, flags @ 0 => bool}],

    // Division and multiplication Instructions
    [Mul, "mul", 0x006, CleverOperandKind::Normal(0), {ss @ 2..=4 => u16, flags @ 0 => bool}],
    [Div, "div", 0x007, CleverOperandKind::Normal(0), {ss @ 2..=4 => u16, wide @ 1 => bool, flags @ 0 => bool}],

    // Register Manipulation Instructions
    [Mov, "mov", 0x008, CleverOperandKind::Normal(2)],
    [Lea, "lea", 0x009, CleverOperandKind::Normal(2)],
    [MovRD, "mov", 0x00A, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [MovRS, "mov", 0x00B, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [LeaRD, "lea", 0x00C, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],

    // Nops
    [Nop10, "nop", 0x010, CleverOperandKind::Normal(0)],
    [Nop11, "nop", 0x011, CleverOperandKind::Normal(1)],
    [Nop12, "nop", 0x012, CleverOperandKind::Normal(2)],

    // Stack Manipulation
    [Push, "push", 0x014, CleverOperandKind::Normal(1)],
    [Pop , "pop" , 0x015, CleverOperandKind::Normal(1)],
    [PushR, "push", 0x016, CleverOperandKind::Normal(0), {r @ .. => CleverRegister}],
    [PopR , "pop" , 0x017, CleverOperandKind::Normal(0), {r @ .. => CleverRegister}],

    // Mass Register Storage
    [Stogpr , "stogpr" , 0x018, CleverOperandKind::Normal(1)],
    [Stoar  , "stoar"  , 0x019, CleverOperandKind::Normal(1)],
    [Rstogpr, "rstogpr", 0x01A, CleverOperandKind::Normal(1)],
    [Rstoar , "rstoar" , 0x01B, CleverOperandKind::Normal(1)],
    [Pushgpr, "pushgpr", 0x01C, CleverOperandKind::Normal(0)],
    [Pushar , "pushar" , 0x01D, CleverOperandKind::Normal(0)],
    [Popgpr , "popgpr" , 0x01E, CleverOperandKind::Normal(0)],
    [Popar  , "popar"  , 0x01F, CleverOperandKind::Normal(0)],

    // Converting Moves
    [Movsx, "movsx", 0x020, CleverOperandKind::Normal(2)],
    [Bswap, "bswap", 0x021, CleverOperandKind::Normal(2)],
    [Movsif, "movsif", 0x022, CleverOperandKind::Normal(2), {flags @ 0 => bool}],
    [Movxf, "movxf", 0x023, CleverOperandKind::Normal(2), {flags @0 => bool}],
    [Movfsi, "movfsi", 0x024, CleverOperandKind::Normal(2), {flags @ 0 => bool}],
    [Movfx, "movfx", 0x025, CleverOperandKind::Normal(2), {flags @ 0 => bool}],
    [Cvtf, "cvtf", 0x026, CleverOperandKind::Normal(2), {flags @ 0 => bool}],


    // Block Instructions
    [Repbi, "repbi", 0x028, CleverOperandKind::Insn, {cc @ .. => ConditionCode}],
    [Repbc, "repbc", 0x029, CleverOperandKind::Insn],
    [Bcpy, "bcpy", 0x02a, CleverOperandKind::Normal(0), {ss @ 0..=2 => u16}],
    [Bsto, "bsto", 0x02b, CleverOperandKind::Normal(0), {ss @ 0..=2 => u16}],
    [Bsca, "bsca", 0x02c, CleverOperandKind::Normal(0), {ss @ 0..=2 => u16}],
    [Bcmp, "bcmp", 0x02d, CleverOperandKind::Normal(0), {ss @ 0..=2 => u16}],
    [Btst, "btst", 0x02e, CleverOperandKind::Normal(0), {ss @ 0..=2 => u16}],

    // Integer Shifts
    [Lsh, "lsh", 0x030, CleverOperandKind::Normal(2), {l @ 3 => bool, f @ 0 => bool}],
    [Rsh, "rsh", 0x031, CleverOperandKind::Normal(2), {l @ 3 => bool, f @ 0 => bool}],
    [Arsh, "arsh", 0x032, CleverOperandKind::Normal(2), {l @ 3 => bool, f @ 0 => bool}],
    [Lshc, "lshc", 0x033, CleverOperandKind::Normal(2), {l @ 3 => bool, f @ 0 => bool}],
    [Rshc, "rshc", 0x034, CleverOperandKind::Normal(2), {l @ 3 => bool, f @ 0 => bool}],
    [Lrot, "lrot", 0x035, CleverOperandKind::Normal(2), {l @ 3 => bool, f @ 0 => bool}],
    [Rrot, "rrot", 0x036, CleverOperandKind::Normal(2), {l @ 3 => bool, f @ 0 => bool}],
    [LshR, "lsh", 0x038, CleverOperandKind::Normal(2), {r @ .. => CleverRegister}],
    [RshR, "rsh", 0x039, CleverOperandKind::Normal(2), {r @ .. => CleverRegister}],
    [ArshR, "arsh", 0x03A, CleverOperandKind::Normal(2), {r @ .. => CleverRegister}],
    [LshcR, "lshc", 0x03B, CleverOperandKind::Normal(2), {r @ .. => CleverRegister}],
    [RshcR, "rshc", 0x03C, CleverOperandKind::Normal(2), {r @ .. => CleverRegister}],
    [LrotR, "lrot", 0x03D, CleverOperandKind::Normal(2), {r @ .. => CleverRegister}],
    [RrotR, "rrot", 0x03E, CleverOperandKind::Normal(2), {r @ .. => CleverRegister}],

    // Arithmetic/Logic GPR Specifications
    // Unary Operations
    // Signed Multiplication/Division
    [Imul, "imul", 0x040, CleverOperandKind::Normal(0), {ss @ 2.. => u16, flags @ 0 => bool}],
    [AddRD, "add", 0x041, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [SubRD, "sub", 0x042, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [AndRD, "and", 0x043, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [OrRD, "or", 0x044, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [XorRD, "xor", 0x045, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [BNot, "bnot", 0x046, CleverOperandKind::Normal(1), {l @ 3 => bool, f @ 0 => bool}],
    [Neg, "neg", 0x047, CleverOperandKind::Normal(1), {l @ 3 => bool, f @ 0 => bool}],
    [Idiv, "idiv", 0x048, CleverOperandKind::Normal(1), {ss @ 2.. => u16, wide @ 1 => bool, flags @ 0 => bool}],
    [AddRS, "add", 0x049, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [SubRS, "sub", 0x04A, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [AndRS, "and", 0x04B, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [OrRS, "or", 0x04C, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [XorRS, "xor", 0x04D, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [BNotR, "bnot", 0x046, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [NegR, "neg", 0x047, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],

    // Comparison operations
    [Cmp, "cmp", 0x06C, CleverOperandKind::Normal(2)],
    [Test, "test", 0x06D, CleverOperandKind::Normal(2)],
    [CmpR, "cmp", 0x06C, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],
    [TestR, "test", 0x06D, CleverOperandKind::Normal(1), {r @ .. => CleverRegister}],

    // Floating-Point Operations
    [Round, "round", 0x100, CleverOperandKind::Normal(1), {f @ 0 => bool}],
    [Ceil, "ceil", 0x101, CleverOperandKind::Normal(1), {f @ 0 => bool}],
    [Floor, "floor", 0x102, CleverOperandKind::Normal(1), {f @ 0 => bool}],
    [FAbs, "fabs", 0x103, CleverOperandKind::Normal(1), {f @ 0 => bool}],
    [FNeg, "fneg", 0x104, CleverOperandKind::Normal(1), {f @ 0 => bool}],
    [FInv, "finv",0x105, CleverOperandKind::Normal(1), {f @ 0 => bool}],
    [FAdd, "fadd", 0x106, CleverOperandKind::Normal(2), {f @ 0 => bool}],
    [FSub, "fsub", 0x107, CleverOperandKind::Normal(2), {f @ 0 => bool}],
    [FMul, "fmul", 0x108, CleverOperandKind::Normal(2), {f @ 0 => bool}],
    [FDiv, "fdiv", 0x109, CleverOperandKind::Normal(2), {f @ 0 => bool}],
    [FRem, "frem", 0x10A, CleverOperandKind::Normal(2), {f @ 0 => bool}],
    [FFma, "ffma", 0x10B, CleverOperandKind::Normal(3), {f @ 0 => bool}],

    // Floating-point comparions
    [FCmpz, "fcmpz", 0x118, CleverOperandKind::Normal(1)],
    [FCmp, "fcmp", 0x119, CleverOperandKind::Normal(2)],

    // Floating-point extra instructions
    [Exp, "exp", 0x120, CleverOperandKind::Normal(1)],
    [Ln, "ln", 0x121, CleverOperandKind::Normal(1)],
    [Lg, "lg", 0x122, CleverOperandKind::Normal(1)],
    [Sin, "sin", 0x123, CleverOperandKind::Normal(1)],
    [Cos, "cos", 0x124, CleverOperandKind::Normal(1)],
    [Tan, "tan", 0x125, CleverOperandKind::Normal(1)],
    [Asin, "asin", 0x126, CleverOperandKind::Normal(1)],
    [Acos, "acos", 0x127, CleverOperandKind::Normal(1)],
    [Atan, "atan", 0x128, CleverOperandKind::Normal(1)],
    [Exp2,"exp2", 0x129, CleverOperandKind::Normal(1),{}],
    [Log10, "log10", 0x12A, CleverOperandKind::Normal(1)],
    [Lnp1, "lnp1", 0x12B, CleverOperandKind::Normal(1)],
    [Expm1, "expm1", 0x12C, CleverOperandKind::Normal(1)],
    [Sqrt, "sqrt", 0x12D, CleverOperandKind::Normal(1)],

    // Floating-point exception control
    [FRaiseExcept, "fraiseexcept", 0x130, CleverOperandKind::Normal(0)],
    [FTriggerExcept, "ftriggerexcept", 0x130, CleverOperandKind::Normal(0)],

    // Atomic Operations
    [Xchg, "xchg", 0x200, CleverOperandKind::Normal(2)],
    [Cmpxchg, "cmpxchg", 0x201, CleverOperandKind::Normal(3)],
    [Wcmpxchg, "wcmpxchg", 0x202, CleverOperandKind::Normal(3)],
    [Fence, "fence", 0x203, CleverOperandKind::Normal(0)],

    // Random Device Polling
    [RPoll, "rpoll", 0x230, CleverOperandKind::Normal(0), {r @ .. => CleverRegister}],

    // Vector Instructions
    [Vec, "vec", 0x400, CleverOperandKind::Insn],
    [Vmov, "vmov",0x401, CleverOperandKind::Normal(2)],

    // conditional Branches
    [CBP0A , "jp" , 0x700, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBC0A , "jc" , 0x701, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBV0A , "jo" , 0x702, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBZ0A , "jz" , 0x703, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBL0A , "jlt", 0x704, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBLE0A, "jle", 0x705, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBBE0A, "jbe", 0x706, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBM0A , "jmi", 0x707, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBPS0A, "jps", 0x708, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBA0A , "ja" , 0x709, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBG0A , "jgt", 0x70A, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBGE0A, "jge", 0x70B, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNZ0A, "jnz", 0x70C, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNV0A, "jno", 0x70D, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNC0A, "jnc", 0x70E, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNP0A, "jnp", 0x70F, CleverOperandKind::AbsAddr, {w @ .. => i8}],

    [CBP0R , "jp" , 0x710, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBC0R , "jc" , 0x711, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBV0R , "jo" , 0x712, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBZ0R , "jz" , 0x713, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBL0R , "jlt", 0x714, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBLE0R, "jle", 0x715, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBBE0R, "jbe", 0x716, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBM0R , "jmi", 0x717, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBPS0R, "jps", 0x718, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBA0R , "ja" , 0x719, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBG0R , "jgt", 0x71A, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBGE0R, "jge", 0x71B, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNZ0R, "jnz", 0x71C, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNV0R, "jno", 0x71D, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNC0R, "jnc", 0x71E, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNP0R, "jnp", 0x71F, CleverOperandKind::RelAddr, {w @ .. => i8}],

    [CBP1A , "jp" , 0x740, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBC1A , "jc" , 0x741, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBV1A , "jo" , 0x742, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBZ1A , "jz" , 0x743, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBL1A , "jlt", 0x744, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBLE1A, "jle", 0x745, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBBE1A, "jbe", 0x746, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBM1A , "jmi", 0x747, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBPS1A, "jps", 0x748, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBA1A , "ja" , 0x749, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBG1A , "jgt", 0x74A, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBGE1A, "jge", 0x74B, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNZ1A, "jnz", 0x74C, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNV1A, "jno", 0x74D, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNC1A, "jnc", 0x74E, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNP1A, "jnp", 0x74F, CleverOperandKind::AbsAddr, {w @ .. => i8}],

    [CBP1R , "jp" , 0x750, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBC1R , "jc" , 0x751, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBV1R , "jo" , 0x752, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBZ1R , "jz" , 0x753, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBL1R , "jlt", 0x754, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBLE1R, "jle", 0x755, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBBE1R, "jbe", 0x756, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBM1R , "jmi", 0x757, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBPS1R, "jps", 0x758, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBA1R , "ja" , 0x759, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBG1R , "jgt", 0x75A, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBGE1R, "jge", 0x75B, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNZ1R, "jnz", 0x75C, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNV1R, "jno", 0x75D, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNC1R, "jnc", 0x75E, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNP1R, "jnp", 0x75F, CleverOperandKind::RelAddr, {w @ .. => i8}],

    [CBP2A , "jp" , 0x780, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBC2A , "jc" , 0x781, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBV2A , "jo" , 0x782, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBZ2A , "jz" , 0x783, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBL2A , "jlt", 0x784, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBLE2A, "jle", 0x785, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBBE2A, "jbe", 0x786, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBM2A , "jmi", 0x787, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBPS2A, "jps", 0x788, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBA2A , "ja" , 0x789, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBG2A , "jgt", 0x78A, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBGE2A, "jge", 0x78B, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNZ2A, "jnz", 0x78C, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNV2A, "jno", 0x78D, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNC2A, "jnc", 0x78E, CleverOperandKind::AbsAddr, {w @ .. => i8}],
    [CBNP2A, "jnp", 0x78F, CleverOperandKind::AbsAddr, {w @ .. => i8}],

    [CBP2R , "jp" , 0x790, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBC2R , "jc" , 0x791, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBV2R , "jo" , 0x792, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBZ2R , "jz" , 0x793, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBL2R , "jlt", 0x794, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBLE2R, "jle", 0x795, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBBE2R, "jbe", 0x796, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBM2R , "jmi", 0x797, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBPS2R, "jps", 0x798, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBA2R , "ja" , 0x799, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBG2R , "jgt", 0x79A, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBGE2R, "jge", 0x79B, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNZ2R, "jnz", 0x79C, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNV2R, "jno", 0x79D, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNC2R, "jnc", 0x79E, CleverOperandKind::RelAddr, {w @ .. => i8}],
    [CBNP2R, "jnp", 0x79F, CleverOperandKind::RelAddr, {w @ .. => i8}],

    // Unconditional Branches/Calls
    [JmpA, "jmp", 0x7C0, CleverOperandKind::AbsAddr, {ss @ 0..=2 => u16}],
    [CallA, "call", 0x7C1, CleverOperandKind::AbsAddr, {ss @ 0..=2 => u16}],
    [FcallA, "fcall", 0x7C2, CleverOperandKind::AbsAddr, {ss @ 0..=2 => u16}],
    [Ret, "ret", 0x7C3, CleverOperandKind::Normal(0)],
    [Scall, "scall", 0x7C4, CleverOperandKind::Normal(0)],
    [Int, "int", 0x7C5, CleverOperandKind::Normal(0), {i @ .. => u16}],
    [IjmpA, "ijmp", 0x7C8, CleverOperandKind::Normal(0), {r @ .. => CleverRegister}],
    [IcallA, "icall", 0x7C9, CleverOperandKind::Normal(0), {r @ .. => CleverRegister}],
    [IfcallA, "ifcall", 0x7CA, CleverOperandKind::Normal(0)],
    [JmpSM, "jsm", 0x7CB, CleverOperandKind::AbsAddr, {ss @ 0..=2 => u16}],
    [CallSM, "callsm", 0x7CC, CleverOperandKind::AbsAddr, {v @ 3 => bool, ss @ 0..=2 => u16}],
    [RetRSM, "retrsm", 0x7CD, CleverOperandKind::Normal(0)],
    [JmpR, "jmp", 0x7D0, CleverOperandKind::RelAddr, {ss @ 0..=2 => u16}],
    [CallR, "call", 0x7D1, CleverOperandKind::RelAddr, {ss @ 0..=2 => u16}],
    [FcallR, "fcall", 0x7D2, CleverOperandKind::RelAddr, {ss @ 0..=2 => u16}],
    [IjmpR, "ijmp", 0x7D8, CleverOperandKind::Normal(0), {r @ .. => CleverRegister}],
    [IcallR, "icall", 0x7D9, CleverOperandKind::Normal(0), {r @ .. => CleverRegister}],
    [IfcallR, "ifcall", 0x7DA, CleverOperandKind::Normal(0)],
    [JmpSMR, "jsm", 0x7DB, CleverOperandKind::RelAddr, {ss @ 0..=2 => u16}],
    [CallSMR, "callsm", 0x7DC, CleverOperandKind::RelAddr, {ss @ 0..=2 => u16, v @ 3 => bool}],

    // Halt
    [Halt, "halt", 0x801, CleverOperandKind::Normal(0)],

    // Cache Control
    [Pcfl, "pcfl", 0x802, CleverOperandKind::Normal(0)],
    [FlAll, "flall", 0x803, CleverOperandKind::Normal(0)],
    [Dflush, "dflush", 0x804, CleverOperandKind::Normal(1)],
    [Iflush, "iflush", 0x805, CleverOperandKind::Normal(1)],

    // I/O Transfers
    [In, "in", 0x806, CleverOperandKind::Normal(0), {ss @ 0..=2 => u16}],
    [Out, "out", 0x807, CleverOperandKind::Normal(0), {ss @ 0..=2 => u16}],

    // Mass Register Storage
    [StoRegF, "storegf", 0x808, CleverOperandKind::Normal(1)],
    [RstRegF, "rstregf", 0x809, CleverOperandKind::Normal(1)],

    // Supervisor Branches
    [Scret, "scret", 0xFC6, CleverOperandKind::Normal(0)],
    [Iret, "iret", 0xFC6, CleverOperandKind::Normal(0)],
    [Hcall, "hcall", 0xFCB, CleverOperandKind::Normal(0)],
    [Hret, "hret", 0xFD6, CleverOperandKind::Normal(0)],
    [Hresume, "hresume", 0xFD7, CleverOperandKind::Normal(0)],

    // VM Creation/Disposal
    [VMCreate, "vmcreate", 0xFDA, CleverOperandKind::Normal(1)],
    [VMDestroy,"vmdestroy",0xFDB, CleverOperandKind::Normal(0)]
}

impl CleverOpcode {
    pub fn is_branch(&self) -> bool {
        (self.opcode() < 0xFD80) && (self.opcode() & 0x7200 == 0x7000)
    }

    pub fn is_cbranch(&self) -> bool {
        matches!(self.opcode() & 0xFE00, 0x7000 | 0x7400 | 0x7800)
    }

    pub fn branch_pcrel(&self) -> Option<bool> {
        if self.is_branch() {
            Some((self.opcode() & 0x100) != 0)
        } else {
            None
        }
    }

    pub fn branch_condition(&self) -> Option<ConditionCode> {
        match self.opcode() & 0xFE00 {
            0x7000 | 0x7400 | 0x7800 => Some(ConditionCode::from_bits((self.opcode() & 0xf0) >> 4)),
            _ => None,
        }
    }

    pub fn branch_weight(&self) -> Option<i8> {
        match self.opcode() & 0xFE00 {
            0x7000 | 0x7400 | 0x7800 => Some(i8::from_bits(self.opcode() & 0xf)),
            _ => None,
        }
    }

    pub fn branch_width(&self) -> Option<u16> {
        if self.opcode() & 0xFFF0 == 0x7C40 {
            return None;
        }
        match self.opcode() & 0xFE00 {
            0x7000 | 0x7400 | 0x7800 => Some(((self.opcode() & 0xC00) >> 10) + 1),
            0x7C00 => Some((self.opcode() & 0x2) + 1),
            _ => None,
        }
    }

    pub fn cbranch(cc: ConditionCode, width: u16, pcrel: bool, weight: i8) -> Self {
        assert!(width - 1 < 3);
        let (width, pcrel, cc, weight) = (
            (width - 1) << 10,
            (pcrel as u16) << 8,
            cc.to_hbits() << 4,
            (weight & 0xf) as u16,
        );

        Self::from_opcode(0x7000 | width | pcrel | cc | weight).unwrap()
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CleverOperand {
    Register {
        size: u16,
        reg: CleverRegister,
    },
    Indirect {
        size: u16,
        base: CleverRegister,
        scale: u8,
        index: CleverIndex,
    },
    VecPair {
        size: u16,
        lo: CleverRegister,
    },
    Immediate(CleverImmediate),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CleverImmediate {
    Short(u16),
    ShortRel(i16),
    ShortAddr(Address),
    ShortAddrRel(Address),
    Long(u16, u64),
    LongRel(u16, i64),
    LongAddr(u16, Address),
    LongAddrRel(u16, Address),
    LongMem(u16, Address, u16),
    LongMemRel(u16, Address, u16),
    Vec(u128),
}

impl CleverImmediate {
    pub fn addr(&self) -> Option<(u64, &Address, bool)> {
        match self {
            CleverImmediate::ShortAddr(addr) => Some((12, addr, false)),
            CleverImmediate::ShortAddrRel(addr) => Some((12, addr, true)),
            CleverImmediate::LongAddr(addrsize, addr) => Some((u64::from(*addrsize), addr, false)),
            CleverImmediate::LongAddrRel(addrsize, addr) => {
                Some((u64::from(*addrsize), addr, false))
            }
            CleverImmediate::LongMem(addrsize, addr, _) => {
                Some((u64::from(*addrsize), addr, false))
            }
            CleverImmediate::LongMemRel(addrsize, addr, _) => {
                Some((u64::from(*addrsize), addr, false))
            }
            _ => None,
        }
    }

    pub fn is_short(&self) -> bool {
        matches!(
            self,
            Self::Short(_) | Self::ShortRel(_) | Self::ShortAddr(_) | Self::ShortAddrRel(_)
        )
    }
}

fn write_immediate_size(size: u16, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    match size {
        8 => f.write_str("byte"),
        16 => f.write_str("half"),
        32 => f.write_str("single"),
        64 => f.write_str("double"),
        128 => f.write_str("quad"),
        n => f.write_fmt(format_args!("<invalid-size {}>", n)),
    }
}

impl core::fmt::Display for CleverImmediate {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            CleverImmediate::Short(s) => f.write_fmt(format_args!("short {}", s)),
            CleverImmediate::ShortRel(s) => f.write_fmt(format_args!("short {}+ip", s)),
            CleverImmediate::ShortAddr(addr) => f.write_fmt(format_args!("short {}", addr)),
            CleverImmediate::ShortAddrRel(addr) => f.write_fmt(format_args!("short {}+ip", addr)),
            CleverImmediate::Long(size, val) => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!(" {}", val))
            }
            CleverImmediate::LongRel(size, val) => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!(" {}+ip", val))
            }
            CleverImmediate::LongAddr(size, val) => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!(" {}", val))
            }
            CleverImmediate::LongAddrRel(size, val) => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!(" {}+ip", val))
            }
            CleverImmediate::LongMem(asize, val, refsize) => {
                write_immediate_size(*refsize, f)?;
                f.write_str(" [")?;
                write_immediate_size(*asize, f)?;
                f.write_fmt(format_args!(" {}]", val))
            }
            CleverImmediate::LongMemRel(asize, val, refsize) => {
                write_immediate_size(*refsize, f)?;
                f.write_str(" [")?;
                write_immediate_size(*asize, f)?;
                f.write_fmt(format_args!(" {}+ip]", val))
            }
            CleverImmediate::Vec(v) => f.write_fmt(format_args!("quad {}", v)),
        }
    }
}

impl CleverOperand {
    pub fn as_control_structure(&self) -> u16 {
        match self {
            CleverOperand::Register { size, reg } => {
                let ss = match size {
                    8 => 0,
                    16 => 1,
                    32 => 2,
                    64 => 3,
                    size => panic!("Invalid register size {:?}", size),
                };

                let r = reg.0 as u16;
                (ss << 8) | r
            }
            CleverOperand::Indirect {
                size,
                base,
                scale,
                index,
            } => {
                let ss = match size {
                    8 => 0,
                    16 => 1,
                    32 => 2,
                    64 => 3,
                    size => panic!("Invalid register size {:?}", size),
                };

                let ll = match scale {
                    1 => 0,
                    2 => 1,
                    4 => 2,
                    8 => 3,
                    16 => 4,
                    32 => 5,
                    64 => 6,
                    128 => 7,
                    scale => panic!("Invalid scale {:?}", scale),
                };

                let (o, k) = match index {
                    CleverIndex::Abs(val) => ((*val as u16) & 0xf, 1),
                    CleverIndex::Register(r) => ((r.0 as u16) & 0xf, 0),
                };

                0x4000 | (o << 10) | (ll << 7) | (k << 6) | (ss << 4) | ((base.0 as u16) & 0xf)
            }
            CleverOperand::Immediate(imm) => match imm {
                CleverImmediate::Short(val) => 0x8000 | (*val),
                CleverImmediate::ShortRel(val) => 0x9000 | (*val as u16),
                CleverImmediate::ShortAddr(_) => 0x8000,
                CleverImmediate::ShortAddrRel(_) => 0x9000,
                CleverImmediate::Long(size, _) => {
                    let ss = match size {
                        16 => 0,
                        32 => 1,
                        64 => 2,
                        size => panic!("Invalid Immediate size {:?}", size),
                    };

                    0x6000 | (ss << 8)
                }
                CleverImmediate::LongRel(size, _) => {
                    let ss = match size {
                        16 => 0,
                        32 => 1,
                        64 => 2,
                        size => panic!("Invalid Immediate size {:?}", size),
                    };

                    0x6400 | (ss << 8)
                }
                CleverImmediate::LongAddr(size, _) => {
                    let ss = match size {
                        16 => 0,
                        32 => 1,
                        64 => 2,
                        size => panic!("Invalid Immediate size {:?}", size),
                    };

                    0x6000 | (ss << 8)
                }
                CleverImmediate::LongAddrRel(size, _) => {
                    let ss = match size {
                        16 => 0,
                        32 => 1,
                        64 => 2,
                        size => panic!("Invalid Immediate size {:?}", size),
                    };

                    0x6400 | (ss << 8)
                }
                CleverImmediate::LongMem(size, _, refsize) => {
                    let ss = match size {
                        16 => 0,
                        32 => 1,
                        64 => 2,
                        size => panic!("Invalid Immediate size {:?}", size),
                    };

                    let zz = match refsize {
                        8 => 0,
                        16 => 1,
                        32 => 2,
                        64 => 3,
                        128 => 4,
                        size => panic!("Invalid reference size {:?}", size),
                    };

                    0x7000 | (ss << 8) | (zz << 4)
                }
                CleverImmediate::LongMemRel(size, _, refsize) => {
                    let ss = match size {
                        16 => 0,
                        32 => 1,
                        64 => 2,
                        size => panic!("Invalid Immediate size {:?}", size),
                    };

                    let zz = match refsize {
                        8 => 0,
                        16 => 1,
                        32 => 2,
                        64 => 3,
                        128 => 4,
                        size => panic!("Invalid reference size {:?}", size),
                    };

                    0x7400 | (ss << 8) | (zz << 4)
                }
                CleverImmediate::Vec(_) => 0x6300,
            },
            CleverOperand::VecPair { size, lo } => {
                let ss = match size {
                    8 => 0,
                    16 => 1,
                    32 => 2,
                    64 => 3,
                    128 => 4,
                    size => panic!("Invalid register size {:?}", size),
                };

                let r = lo.0 as u16;
                0x2000 | (ss << 8) | r
            }
        }
    }

    pub fn immediate_value(&self) -> Option<&CleverImmediate> {
        match self {
            Self::Immediate(imm) => Some(imm),
            _ => None,
        }
    }
}

impl core::fmt::Display for CleverOperand {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            CleverOperand::Register { size, reg } => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!(" {}", reg))
            }
            CleverOperand::Indirect {
                size,
                base,
                scale: _,
                index: CleverIndex::Abs(0),
            } => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!("[{}]", base))
            }
            CleverOperand::Indirect {
                size,
                base,
                scale: 1,
                index,
            } => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!("[{}+{}]", index, base))
            }
            CleverOperand::Indirect {
                size,
                base,
                scale,
                index,
            } => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!("[{}*{}+{}]", scale, index, base))
            }
            CleverOperand::VecPair { size, lo } => {
                write_immediate_size(*size, f)?;
                f.write_fmt(format_args!(" {}", lo))
            }
            CleverOperand::Immediate(imm) => imm.fmt(f),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CleverInstruction {
    prefix: Option<CleverOpcode>,
    opcode: CleverOpcode,
    operands: Vec<CleverOperand>,
}

impl CleverInstruction {
    pub const fn new(opcode: CleverOpcode, operands: Vec<CleverOperand>) -> Self {
        Self {
            prefix: None,
            opcode,
            operands,
        }
    }
    pub const fn new_prefixed(
        prefix: CleverOpcode,
        opcode: CleverOpcode,
        operands: Vec<CleverOperand>,
    ) -> Self {
        Self {
            prefix: Some(prefix),
            opcode,
            operands,
        }
    }

    pub fn prefix(&self) -> Option<CleverOpcode> {
        self.prefix
    }

    pub fn opcode(&self) -> CleverOpcode {
        self.opcode
    }

    pub fn operands(&self) -> &[CleverOperand] {
        &self.operands
    }
}

pub struct CleverEncoder<W> {
    inner: W,
}

impl<W> CleverEncoder<W> {
    pub const fn new(inner: W) -> Self {
        Self { inner }
    }

    pub fn into_inner(self) -> W {
        self.inner
    }

    pub fn inner_mut(&mut self) -> &mut W {
        &mut self.inner
    }

    pub fn inner(&self) -> &W {
        &self.inner
    }
}

impl<W: Write> Write for CleverEncoder<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

impl<W: InsnWrite> InsnWrite for CleverEncoder<W> {
    fn write_addr(&mut self, size: usize, addr: Address, rel: bool) -> std::io::Result<()> {
        self.inner.write_addr(size, addr, rel)
    }

    fn write_reloc(&mut self, reloc: crate::traits::Reloc) -> std::io::Result<()> {
        self.inner.write_reloc(reloc)
    }

    fn offset(&self) -> usize {
        self.inner.offset()
    }
}

impl<W: InsnWrite> CleverEncoder<W> {
    pub fn write_instruction(&mut self, insn: CleverInstruction) -> std::io::Result<()> {
        if let Some(prefix) = insn.prefix() {
            self.write_all(&prefix.opcode().to_be_bytes())?;
        }
        self.write_all(&insn.opcode().opcode().to_be_bytes())?;
        match insn.opcode().operands(){
            CleverOperandKind::Normal(n) => {
                assert_eq!(insn.operands().len(), n as usize);

                for op in insn.operands(){
                    let operand = op.as_control_structure();

                    let imm = op.immediate_value();

                    if let Some(imm) = imm{
                        match imm{
                            CleverImmediate::ShortAddr(Address::Abs(addr)) => self.write_all(&(operand | u16::try_from(*addr).unwrap()).to_be_bytes())?,
                            CleverImmediate::ShortAddrRel(Address::Disp(addr)) => self.write_all(&(operand | u16::try_from(*addr).unwrap()).to_be_bytes())?,
                            CleverImmediate::ShortAddr(_) => todo!("short imm with relocation"),
                            CleverImmediate::ShortAddrRel(_) => todo!("short relative imm with relocation"),
                            CleverImmediate::Long(size, val) => {
                                let val_bytes = &(*val).to_le_bytes()[..((*size/8) as usize)];
                                self.write_all(&operand.to_be_bytes())?;
                                self.write_all(val_bytes)?;
                            }
                            CleverImmediate::LongRel(size, val) => {
                                let val_bytes = &(*val).to_le_bytes()[..((*size/8) as usize)];
                                self.write_all(&operand.to_be_bytes())?;
                                self.write_all(val_bytes)?;
                            }
                            CleverImmediate::LongAddr(size, addr) | CleverImmediate::LongMem(size,addr,_) => {
                                self.write_all(&operand.to_be_bytes())?;
                                self.write_addr(*size as usize, addr.clone(), false)?;
                            }
                            CleverImmediate::LongAddrRel(size, addr) | CleverImmediate::LongMemRel(size,addr,_) => {
                                self.write_all(&operand.to_be_bytes())?;
                                self.write_addr(*size as usize, addr.clone(), true)?;
                            }
                            CleverImmediate::Vec(vec) => {
                                self.write_all(&operand.to_be_bytes())?;
                                self.write_all(&vec.to_le_bytes())?;
                            }
                            _ => self.write_all(&operand.to_be_bytes())?
                        }
                    }else{
                        self.write_all(&operand.to_be_bytes())?;
                    }
                }
            },
            CleverOperandKind::AbsAddr => {
                assert_eq!(insn.operands().len(),1);

                let (_,addr,_) = insn.operands()[0].immediate_value()
                    .and_then(|imm|imm.addr()).unwrap();

                let width = insn.opcode().branch_width().unwrap();

                self.write_addr(8<<(width as u32),addr.clone(),false)?;
            },
            CleverOperandKind::RelAddr => {
                assert_eq!(insn.operands().len(),1);

                let (_,addr,_) = insn.operands()[0].immediate_value()
                    .and_then(|imm|imm.addr()).unwrap();

                let width = insn.opcode().branch_width().unwrap();

                self.write_addr(8<<(width as u32),addr.clone(),true)?;
            },
            CleverOperandKind::Insn => panic!("Cannot write a prefix as a primary instruction, use `CleverInstruction::new_with_prefix` instead"),
        }
        Ok(())
    }
}

pub struct CleverDecoder<R> {
    inner: R,
}

impl<R> CleverDecoder<R> {
    pub const fn new(inner: R) -> Self {
        Self { inner }
    }

    pub fn into_inner(self) -> R {
        self.inner
    }

    pub fn inner_mut(&mut self) -> &mut R {
        &mut self.inner
    }

    pub fn inner(&self) -> &R {
        &self.inner
    }
}

impl<R: Read> Read for CleverDecoder<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.inner.read(buf)
    }
}

impl<R: InsnRead> InsnRead for CleverDecoder<R> {
    fn read_addr(&mut self, size: usize, rel: bool) -> std::io::Result<Address> {
        self.inner.read_addr(size, rel)
    }

    fn read_reloc(
        &mut self,
        size: usize,
        rel: bool,
        offset: Option<isize>,
    ) -> std::io::Result<Option<Address>> {
        self.inner.read_reloc(size, rel, offset)
    }
}

impl<R: InsnRead> CleverDecoder<R> {
    pub fn read_insn(&mut self) -> std::io::Result<CleverInstruction> {
        let mut opcode = [0u8; 2];
        self.read_exact(&mut opcode)?;
        let opcode = u16::from_be_bytes(opcode);
        let op = CleverOpcode::from_opcode(opcode).ok_or_else(|| {
            std::io::Error::new(
                ErrorKind::InvalidData,
                format!("Invalid opcode {:#x}", opcode),
            )
        })?;
        match op.operands() {
            CleverOperandKind::Insn => {
                let mut inner = self.read_insn()?;
                if let Some(prefix) = inner.prefix {
                    return Err(std::io::Error::new(
                        ErrorKind::InvalidData,
                        format!("Cannot combine multiple prefixes {:?} and {:?}", op, prefix),
                    ));
                } else {
                    inner.prefix = Some(op);
                }
                Ok(inner)
            }
            kind @ (CleverOperandKind::AbsAddr | CleverOperandKind::RelAddr) => {
                let bytes = 8 << (op.branch_width().unwrap() as u32);
                let rel = kind == CleverOperandKind::RelAddr;
                let addr = self.read_addr(bytes, rel)?;

                Ok(CleverInstruction::new(
                    op,
                    vec![CleverOperand::Immediate(CleverImmediate::LongAddr(
                        bytes as u16,
                        addr,
                    ))],
                ))
            }
            CleverOperandKind::Normal(n) => {
                let mut ops = Vec::with_capacity(n as usize);

                for _ in 0..n {
                    let mut ctrl = [0u8; 2];
                    self.read_exact(&mut ctrl)?;
                    let ctrl = u16::from_be_bytes(ctrl);

                    let opr = match ctrl >> 14 {
                        0b00 => {
                            let v = (ctrl & 0x2000) != 0;

                            let reg = CleverRegister(ctrl as u8);

                            let ss = 8 << ((ctrl >> 8) & 0x7);

                            if v {
                                CleverOperand::VecPair { size: ss, lo: reg }
                            } else {
                                CleverOperand::Register { size: ss, reg }
                            }
                        }
                        0b01 => {
                            let offset = (ctrl >> 10) & 0xf;

                            let scale = 1 << ((ctrl >> 7) & 0x7);

                            let ss = 8 << ((ctrl >> 4) & 0x3) as u32;

                            let k = (ctrl & 0x40) != 0;

                            let base = CleverRegister((ctrl & 0xf) as u8);

                            let index = if k {
                                CleverIndex::Abs(offset as i16)
                            } else {
                                CleverIndex::Register(CleverRegister(offset as u8))
                            };

                            CleverOperand::Indirect {
                                size: ss,
                                base,
                                scale,
                                index,
                            }
                        }
                        0b10 => {
                            let short = ctrl & 0xfff;

                            let rel = (ctrl & 0x1000) != 0;

                            let addr = self.read_reloc(12, rel, Some(-2))?;

                            let imm = match (addr, rel) {
                                (Some(addr), true) => CleverImmediate::ShortAddrRel(addr),
                                (Some(addr), false) => CleverImmediate::ShortAddr(addr),
                                (None, true) => CleverImmediate::ShortRel(short as i16),
                                (None, false) => CleverImmediate::Short(short),
                            };

                            CleverOperand::Immediate(imm)
                        }
                        0b11 => {
                            let mem = (ctrl & 0x2000) != 0;
                            let rel = (ctrl & 0x400) != 0;
                            let size = 8 << (1 + ((ctrl >> 8) & 0x3));

                            let zize = 8 << ((ctrl >> 4) & 0xf);

                            let val = self.read_addr(size as usize, rel)?;

                            let imm = match (val, rel, mem) {
                                (Address::Abs(val), false, false) if size == 128 => {
                                    CleverImmediate::Vec(val)
                                }
                                _ if size == 128 => {
                                    return Err(std::io::Error::new(
                                        ErrorKind::InvalidData,
                                        "invalid immediate with size 128",
                                    ))
                                }
                                (Address::Disp(val), true, false) => {
                                    CleverImmediate::LongRel(size, val)
                                }
                                (Address::Abs(val), false, false) => {
                                    CleverImmediate::Long(size, val as u64)
                                }
                                (addr, true, false) => CleverImmediate::LongAddrRel(size, addr),
                                (addr, false, false) => CleverImmediate::LongAddr(size, addr),
                                (addr, true, true) => CleverImmediate::LongMemRel(size, addr, zize),
                                (addr, false, true) => CleverImmediate::LongMem(size, addr, zize),
                            };

                            CleverOperand::Immediate(imm)
                        }
                        _ => unsafe { core::hint::unreachable_unchecked() },
                    };

                    ops.push(opr);
                }

                Ok(CleverInstruction::new(op, ops))
            }
        }
    }
}

pub struct CleverPrinter<W> {
    inner: W,
}

impl<W> CleverPrinter<W> {
    pub const fn new(inner: W) -> Self {
        Self { inner }
    }

    pub fn into_inner(self) -> W {
        self.inner
    }

    pub fn inner(&self) -> &W {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut W {
        &mut self.inner
    }
}

#[cfg(test)]
mod test {
    use crate::test::TestWriter;

    use super::*;
    #[test]
    pub fn test_encode_nop() {
        let mut encoder = CleverEncoder::new(TestWriter { inner: Vec::new() });
        encoder
            .write_instruction(CleverInstruction::new(CleverOpcode::Nop10, vec![]))
            .unwrap();

        assert_eq!(&*encoder.inner_mut().inner, &[0x01, 0x00]);
    }

    #[test]
    pub fn test_encode_nop1() {
        let mut encoder = CleverEncoder::new(TestWriter { inner: Vec::new() });
        encoder
            .write_instruction(CleverInstruction::new(
                CleverOpcode::Nop11,
                vec![CleverOperand::Register {
                    size: 64,
                    reg: CleverRegister::r0,
                }],
            ))
            .unwrap();

        assert_eq!(&*encoder.inner_mut().inner, &[0x01, 0x10, 0x03, 0x00]);
    }

    #[test]
    pub fn test_encode_nop1_simm() {
        let mut encoder = CleverEncoder::new(TestWriter { inner: Vec::new() });
        encoder
            .write_instruction(CleverInstruction::new(
                CleverOpcode::Nop11,
                vec![CleverOperand::Immediate(CleverImmediate::Short(1337))],
            ))
            .unwrap();

        assert_eq!(&*encoder.inner_mut().inner, &[0x01, 0x10, 0x85, 0x39]);
    }

    #[test]
    pub fn test_encode_nop2() {
        let mut encoder = CleverEncoder::new(TestWriter { inner: Vec::new() });
        encoder
            .write_instruction(CleverInstruction::new(
                CleverOpcode::Nop12,
                vec![
                    CleverOperand::Register {
                        size: 64,
                        reg: CleverRegister::flags,
                    },
                    CleverOperand::Immediate(CleverImmediate::Long(16, 1337)),
                ],
            ))
            .unwrap();

        assert_eq!(
            &*encoder.inner_mut().inner,
            &[0x01, 0x20, 0x03, 0x11, 0x60, 0x00, 0x39, 0x05]
        );
    }

    #[test]
    pub fn test_encode_branch() {
        let mut encoder = CleverEncoder::new(TestWriter { inner: Vec::new() });
        encoder
            .write_instruction(CleverInstruction::new(
                CleverOpcode::cbranch(ConditionCode::Zero, 1, true, 0),
                vec![CleverOperand::Immediate(CleverImmediate::LongAddrRel(
                    16,
                    Address::Disp(-32),
                ))],
            ))
            .unwrap();

        assert_eq!(
            &*encoder.inner().inner,
            &[0x71, 0x30, 0xe0, 0xff],
            "{:x?} != {:x?}",
            encoder.inner().inner,
            [0x71, 0x30, 0xe0, 0xff]
        );
    }
}
