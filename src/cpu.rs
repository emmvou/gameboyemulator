use crate::memory::Memory;
use paste::paste;

#[derive(Default, Clone, Copy)]
struct Flags {
    z: bool,
    // zero
    n: bool,
    // subtract
    h: bool,
    // half carry
    c: bool, // carry
}

impl From<Flags> for u8 {
    fn from(flags: Flags) -> u8 {
        let mut res = 0;
        if flags.z {
            res |= 1 << 7;
        }
        if flags.n {
            res |= 1 << 6;
        }
        if flags.h {
            res |= 1 << 5;
        }
        if flags.c {
            res |= 1 << 4;
        }
        res
    }
}

impl From<u8> for Flags {
    fn from(val: u8) -> Flags {
        Flags {
            z: val & (1 << 7) != 0,
            n: val & (1 << 6) != 0,
            h: val & (1 << 5) != 0,
            c: val & (1 << 4) != 0,
        }
    }
}

#[derive(Default)]
pub struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: Flags,
    h: u8,
    l: u8,
    sp: u16,
    pub(crate) pc: u16,
}

macro_rules! reg16 {
    ($hi: ident, $lo: ident) => {
        paste! {
            pub fn [<$hi $lo>](&self) -> u16 {
            (self.$hi as u16) << 8 | u8::from(self.$lo) as u16
        }
        pub fn [<set_$hi $lo>](&mut self, val: u16) {
            self.$hi = (val >> 8) as u8;
            self.$lo = (val as u8).into();
        }
        }
    };
}

impl Registers {
    pub fn set_reg16(&mut self, reg: Reg16, val: u16) {
        use Reg16::*;
        match reg {
            AF => self.set_af(val),
            BC => self.set_bc(val),
            DE => self.set_de(val),
            HL => self.set_hl(val),
            SP => self.sp = val,
            PC => self.pc = val,
        }
    }
}

impl Registers {
    reg16!(h, l);
    reg16!(b, c);
    reg16!(d, e);
    reg16!(a, f);
}

pub(crate) struct CPU {
    pub(crate) registers: Registers,
    pub(crate) mmu: Memory,
    halted: bool,
    ime: bool,
    ime_request: Option<bool>, //because IME is only set one cycle after ei or di are executed
}

impl CPU {
    pub(crate) fn new() -> CPU {
        CPU {
            registers: Registers::default(),
            mmu: Memory::new(),
            halted: false,
            ime: false,
            ime_request: None,
        }
    }

    fn execute(&mut self, instr: Instruction) -> u32 {
        use Instruction::*;
        match instr {
            Nop => 1,
            Stop => todo!(),
            Halt => {
                self.halted = true;
                1
            }
            Di => {
                self.ime_request = Some(false);
                1
            }
            Ei => {
                self.ime_request = Some(true);
                1
            }
            Jr(Some(cond), s8) => {
                if self.check_jump_cond(cond) {
                    self.execute(Jr(None, s8))
                } else {
                    2
                }
            }
            Jr(None, s8) => {
                self.registers.pc = self.registers.pc.wrapping_add_signed(s8 as i16);
                3
            }
            JpImm16(Some(cond), a16) => {
                if self.check_jump_cond(cond) {
                    self.execute(JpImm16(None, a16))
                } else {
                    3
                }
            }
            JpImm16(None, a16) => {
                self.registers.pc = a16;
                4
            }
            JpHL => {
                self.registers.pc = self.registers.hl();
                1
            }
            Call(Some(cond), a16) => {
                if self.check_jump_cond(cond) {
                    self.execute(Call(None, a16))
                } else {
                    3
                }
            }
            Call(None, a16) => {
                self.registers.sp -= 2;
                let pc = self.registers.pc;
                self.mmu.write_u16(self.registers.sp as usize, pc);
                self.registers.pc = a16;
                6
            }
            RetCond(op_cond) => {
                if self.check_jump_cond(op_cond) {
                    self.execute(Ret);
                    5
                } else {
                    2
                }
            }
            Ret => {
                let pc = self.mmu.read_u16(self.registers.sp as usize);
                self.registers.pc = pc;
                self.registers.sp += 2;
                4
            }
            Reti => {
                self.execute(Ei); // todo check when IME will be updated
                self.execute(Ret)
            }
            Push(op) => {
                self.registers.sp -= 2;
                let d16 = self.extract_reg16_af(op);
                self.mmu.write_u16(self.registers.sp as usize, d16);
                4
            }
            Pop(op) => {
                let d16 = self.mmu.read_u16(self.registers.sp as usize);
                self.registers.set_reg16(op, d16);
                self.registers.sp += 2;
                3
            }
            Rst(op) => {
                self.execute(Call(None, (op * 8) as u16));
                4
            }
            Rlca => {
                let a = self.registers.a;
                let res = a.rotate_left(1);
                self.registers.a = res;
                self.registers.f.z = false;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = a & 0x80 != 0;
                1
            }
            Rla => {
                let a = self.registers.a;
                let res = (a << 1) | (self.registers.f.c as u8);
                self.registers.a = res;
                self.registers.f.z = false;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = a & 0x80 != 0;
                1
            }
            Rrca => {
                let a = self.registers.a;
                let res = a.rotate_right(1);
                self.registers.a = res;
                self.registers.f.z = false;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = a & 0x01 != 0;
                1
            }
            Rra => {
                let a = self.registers.a;
                let res = (a >> 1) | ((self.registers.f.c as u8) << 7);
                self.registers.a = res;
                self.registers.f.z = false;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = a & 0x01 != 0;
                1
            }
            Daa => {
                let mut a = self.registers.a;
                let mut adjust = 0;
                if self.registers.f.h || (!self.registers.f.n && (a & 0xF) > 9) {
                    adjust |= 0x06;
                }
                if self.registers.f.c || (!self.registers.f.n && a > 0x99) {
                    adjust |= 0x60;
                    self.registers.f.c = true;
                }
                a = if self.registers.f.n {
                    a.wrapping_sub(adjust)
                } else {
                    a.wrapping_add(adjust)
                };
                self.registers.a = a;
                self.registers.f.z = a == 0;
                self.registers.f.h = false;
                1
            }
            Scf => {
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = true;
                1
            }
            Cpl => {
                self.registers.a = !self.registers.a;
                self.registers.f.n = true;
                self.registers.f.h = true;
                1
            }
            Ccf => {
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = !self.registers.f.c;
                1
            }
            AddAImm8(d8) => {
                let a = self.registers.a;
                let res = a.wrapping_add(d8);
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = (a & 0xF) + (d8 & 0xF) > 0xF;
                self.registers.f.c = (a as u16) + (d8 as u16) > 0xFF;
                2
            }
            AddA8(op) => {
                let a = self.registers.a;
                let d8 = self.read_reg8orhl(op);
                let res = a.wrapping_add(d8);
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = (a & 0xF) + (d8 & 0xF) > 0xF;
                self.registers.f.c = (a as u16) + (d8 as u16) > 0xFF;
                if op == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            AdcAImm8(d8) => {
                let a = self.registers.a;
                let c = self.registers.f.c as u8;
                let res = a.wrapping_add(d8).wrapping_add(c);
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = (a & 0xF) + (d8 & 0xF) + c > 0xF;
                self.registers.f.c = (a as u16) + (d8 as u16) + (c as u16) > 0xFF;
                2
            }
            AdcA8(op) => {
                let a = self.registers.a;
                let d8 = self.read_reg8orhl(op);
                let c = self.registers.f.c as u8;
                let res = a.wrapping_add(d8).wrapping_add(c);
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = (a & 0xF) + (d8 & 0xF) + c > 0xF;
                self.registers.f.c = (a as u16) + (d8 as u16) + (c as u16) > 0xFF;
                if op == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            SubAImm8(d8) => {
                let a = self.registers.a;
                let res = a.wrapping_sub(d8);
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = true;
                self.registers.f.h = (a & 0xF) < (d8 & 0xF);
                self.registers.f.c = (a as u16) < (d8 as u16);
                2
            }
            SubA8(op) => {
                let a = self.registers.a;
                let d8 = self.read_reg8orhl(op);
                let res = a.wrapping_sub(d8);
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = true;
                self.registers.f.h = (a & 0xF) < (d8 & 0xF);
                self.registers.f.c = (a as u16) < (d8 as u16);
                if op == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            SbcAImm8(d8) => {
                let a = self.registers.a;
                let c = self.registers.f.c as u8;
                let res = a.wrapping_sub(d8).wrapping_sub(c);
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = true;
                self.registers.f.h = (a & 0xF) < (d8 & 0xF) + c;
                self.registers.f.c = (a as u16) < (d8 as u16) + (c as u16);
                2
            }
            SbcA8(op) => {
                let a = self.registers.a;
                let d8 = self.read_reg8orhl(op);
                let c = self.registers.f.c as u8;
                let res = a.wrapping_sub(d8).wrapping_sub(c);
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = true;
                self.registers.f.h = (a & 0xF) < (d8 & 0xF) + c;
                self.registers.f.c = (a as u16) < (d8 as u16) + (c as u16);
                if op == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            AndAImm8(d8) => {
                let a = self.registers.a;
                let res = a & d8;
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = true;
                self.registers.f.c = false;
                2
            }
            AndA8(op) => {
                let a = self.registers.a;
                let d8 = self.read_reg8orhl(op);
                let res = a & d8;
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = true;
                self.registers.f.c = false;
                if op == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            XorAImm8(d8) => {
                let a = self.registers.a;
                let res = a ^ d8;
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = false;
                2
            }
            XorA8(op) => {
                let a = self.registers.a;
                let d8 = self.read_reg8orhl(op);
                let res = a ^ d8;
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = false;
                if op == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            OrAImm8(d8) => {
                let a = self.registers.a;
                let res = a | d8;
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = false;
                2
            }
            OrA8(op) => {
                let a = self.registers.a;
                let d8 = self.read_reg8orhl(op);
                let res = a | d8;
                self.registers.a = res;
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = false;
                if op == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            CpAImm8(d8) => {
                let a = self.registers.a;
                let res = a.wrapping_sub(d8);
                self.registers.f.z = res == 0;
                self.registers.f.n = true;
                self.registers.f.h = (a & 0xF) < (d8 & 0xF);
                self.registers.f.c = (a as u16) < (d8 as u16);
                2
            }
            CpA8(op) => {
                let a = self.registers.a;
                let d8 = self.read_reg8orhl(op);
                let res = a.wrapping_sub(d8);
                self.registers.f.z = res == 0;
                self.registers.f.n = true;
                self.registers.f.h = (a & 0xF) < (d8 & 0xF);
                self.registers.f.c = (a as u16) < (d8 as u16);
                if op == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            AddSp(s8) => {
                let sp = self.registers.sp;
                let res = sp.wrapping_add(s8 as u16);
                self.registers.sp = res;
                self.registers.f.z = false;
                self.registers.f.n = false;
                self.registers.f.h = (sp & 0xF) + (s8 as u16 & 0xF) > 0xF;
                self.registers.f.c = (sp as u32) + (s8 as u32) > 0xFF;
                4
            }
            AddHl(op) => {
                let hl = self.registers.hl();
                let d16 = self.extract_reg16(op);
                let res = hl.wrapping_add(d16);
                self.registers.set_hl(res);
                self.registers.f.n = false;
                self.registers.f.h = (hl & 0xFFF) + (d16 & 0xFFF) > 0xFFF;
                self.registers.f.c = (hl as u32) + (d16 as u32) > 0xFFFF;
                2
            }
            Inc16(op) => {
                let d16 = self.extract_reg16(op);
                let res = d16.wrapping_add(1);
                self.registers.set_reg16(op, res);
                2
            }
            Inc8(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8.wrapping_add(1);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = (d8 & 0xF) == 0xF;
                self.write_reg8orhl(op, res);
                if op == Reg8OrHl::Hl {
                    3
                } else {
                    1
                }
            }
            Dec16(op) => {
                let d16 = self.extract_reg16(op);
                let res = d16.wrapping_sub(1);
                self.registers.set_reg16(op, res);
                2
            }
            Dec8(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8.wrapping_sub(1);
                self.registers.f.z = res == 0;
                self.registers.f.n = true;
                self.registers.f.h = (d8 & 0xF) == 0;
                self.write_reg8orhl(op, res);
                if op == Reg8OrHl::Hl {
                    3
                } else {
                    1
                }
            }
            LdImm16(op, d16) => {
                self.registers.set_reg16(op, d16);
                3
            }
            Ld8to8(op1, op2) => {
                let d8 = self.read_reg8orhl(op2);
                self.write_reg8orhl(op1, d8);
                if op1 == Reg8OrHl::Hl || op2 == Reg8OrHl::Hl {
                    2
                } else {
                    1
                }
            }
            LdImm8to8(op, d8) => {
                self.write_reg8orhl(op, d8);
                if op == Reg8OrHl::Hl {
                    3
                } else {
                    2
                }
            }
            LdAIndReg16(op, kind) => {
                let addr = self.extract_reg16_or_incdec(op) as usize;
                match kind {
                    LoadStore::Load => self.registers.a = self.mmu.read_u8(addr),
                    LoadStore::Store => self.mmu.write_u8(addr, self.registers.a),
                }
                2
            }
            LdAIndImm8(d8, kind) => {
                match kind {
                    LoadStore::Load => self.registers.a = self.mmu.read_u8(d8 as usize + 0xFF00),
                    LoadStore::Store => self.mmu.write_u8(d8 as usize + 0xFF00, self.registers.a),
                }
                3
            }
            LdAIndC(kind) => {
                let c = self.registers.c;
                match kind {
                    LoadStore::Load => self.registers.a = self.mmu.read_u8(c as usize + 0xFF00),
                    LoadStore::Store => self.mmu.write_u8(c as usize + 0xFF00, self.registers.a),
                }
                2
            }
            LdAIndImm16(d16, kind) => {
                match kind {
                    LoadStore::Load => self.registers.a = self.mmu.read_u8(d16 as usize),
                    LoadStore::Store => self.mmu.write_u8(d16 as usize, self.registers.a),
                }
                4
            }
            LdSPtoIndImm16(d16) => {
                let sp = self.registers.sp;
                self.mmu.write_u16(d16 as usize, sp);
                5
            }
            LdHLtoSP => {
                let hl = self.registers.hl();
                self.registers.sp = hl;
                2
            }
            LdSPs8toHL(s8) => {
                let sp = self.registers.sp;
                let res = sp.wrapping_add_signed(s8 as i16);
                self.registers.set_hl(res);
                self.registers.f.z = false;
                self.registers.f.n = false;
                self.registers.f.h = (sp & 0xF) + (s8 as u16 & 0xF) > 0xF;
                self.registers.f.c = (sp as u32) + (s8 as u32) > 0xFF;
                3
            }
            //CB instructions
            Rlc(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8.rotate_left(1);
                self.write_reg8orhl(op, res);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = d8 & 0x80 != 0;
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Rrc(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8.rotate_right(1);
                self.write_reg8orhl(op, res);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = d8 & 0x01 != 0;
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Rl(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = (d8 << 1) | (self.registers.f.c as u8);
                self.write_reg8orhl(op, res);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = d8 & 0x80 != 0;
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Rr(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8 >> 1 | ((self.registers.f.c as u8) << 7);
                self.write_reg8orhl(op, res);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = d8 & 0x01 != 0;
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Sla(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8 << 1; // bit 0 is set to 0
                self.write_reg8orhl(op, res);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = d8 & 0x80 != 0;
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Sra(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = (d8 >> 1) | (d8 & 0x80);
                self.write_reg8orhl(op, res);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = d8 & 0x01 != 0;
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Swap(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = (d8 >> 4) | (d8 << 4);
                self.write_reg8orhl(op, res);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = false;
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Srl(op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8 >> 1; // bit 7 is set to 0
                self.write_reg8orhl(op, res);
                self.registers.f.z = res == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = d8 & 0x01 != 0;
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Bit(bit, op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8 & (1 << bit) != 0;
                self.registers.f.z = !res;
                if op == Reg8OrHl::Hl {
                    3
                } else {
                    2
                }
            }
            Res(bit, op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8 & !(1 << bit);
                self.write_reg8orhl(op, res);
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
            Set(bit, op) => {
                let d8 = self.read_reg8orhl(op);
                let res = d8 | (1 << bit);
                self.write_reg8orhl(op, res);
                if op == Reg8OrHl::Hl {
                    4
                } else {
                    2
                }
            }
        };
        panic!("invalid instruction")
    }

    fn extract_reg16(&mut self, op: Reg16) -> u16 {
        match op {
            Reg16::BC => self.registers.bc(),
            Reg16::DE => self.registers.de(),
            Reg16::HL => self.registers.hl(),
            Reg16::SP => self.registers.sp,
            _ => panic!(),
        }
    }
    fn extract_reg16_af(&mut self, op: Reg16) -> u16 {
        match op {
            Reg16::BC => self.registers.bc(),
            Reg16::DE => self.registers.de(),
            Reg16::HL => self.registers.hl(),
            Reg16::AF => self.registers.af(),
            _ => panic!(),
        }
    }
    fn extract_reg16_or_incdec(&mut self, op: LdIndReg16) -> u16 {
        // add second case for inc/dec??
        match op {
            LdIndReg16::BC => self.registers.bc(),
            LdIndReg16::DE => self.registers.de(),
            LdIndReg16::HLInc => {
                let hl = self.registers.hl();
                self.registers.set_hl(hl.wrapping_add(1));
                hl
            }
            LdIndReg16::HLDec => {
                let hl = self.registers.hl();
                self.registers.set_hl(hl.wrapping_sub(1));
                hl
            }
        }
    }

    fn read_reg8orhl(&mut self, op: Reg8OrHl) -> u8 {
        match op {
            Reg8OrHl::Reg8(Reg8::A) => self.registers.a,
            Reg8OrHl::Reg8(Reg8::B) => self.registers.b,
            Reg8OrHl::Reg8(Reg8::C) => self.registers.c,
            Reg8OrHl::Reg8(Reg8::D) => self.registers.d,
            Reg8OrHl::Reg8(Reg8::E) => self.registers.e,
            Reg8OrHl::Reg8(Reg8::H) => self.registers.h,
            Reg8OrHl::Reg8(Reg8::L) => self.registers.l,
            Reg8OrHl::Hl => self.mmu.read_u8(self.registers.hl() as usize),
        }
    }

    fn write_reg8orhl(&mut self, op: Reg8OrHl, value: u8) {
        match op {
            Reg8OrHl::Reg8(Reg8::A) => self.registers.a = value,
            Reg8OrHl::Reg8(Reg8::B) => self.registers.b = value,
            Reg8OrHl::Reg8(Reg8::C) => self.registers.c = value,
            Reg8OrHl::Reg8(Reg8::D) => self.registers.d = value,
            Reg8OrHl::Reg8(Reg8::E) => self.registers.e = value,
            Reg8OrHl::Reg8(Reg8::H) => self.registers.h = value,
            Reg8OrHl::Reg8(Reg8::L) => self.registers.l = value,
            Reg8OrHl::Hl => self.mmu.write_u8(self.registers.hl() as usize, value),
        }
    }

    pub(crate) fn fetch_instruction(&mut self) -> Instruction {
        let opcode = self.fetch_u8_pc();
        if opcode == 0xCB {
            let opcode = self.fetch_u8_pc();
            self.fetch_parse(opcode, true)
        } else {
            self.fetch_parse(opcode, false)
        }
    }

    fn fetch_parse(&mut self, opcode: u8, is_extended: bool) -> Instruction {
        use Instruction::*;
        use LoadStore::*;
        let reg16 = Reg16::parse;

        let (hi, lo) = (opcode >> 4, opcode & 0xF);

        // for instructions such as INC or RST which take an operand from 0 to 7
        // following the instruction table's symmetry
        // hi  |lo  0xxx  ... 1xxx
        // xx00      0         1
        // xx01      2         3
        // xx10      4         5
        // xx11      6         7
        let op_index_8 = ((hi & 0b0011) * 2) + if lo >= 8 { 1 } else { 0 };

        let op_reg8orhl = Reg8OrHl::parse(op_index_8);
        let op_cond = JumpCond::parse(op_index_8 % 4);

        macro_rules! op_imm8 {
            () => {
                self.fetch_u8_pc()
            };
        }

        macro_rules! op_imm16 {
            () => {
                self.fetch_u16_pc()
            };
        }

        if is_extended {
            match (hi, lo) {
                (0x0, 0x0..=0x7) => Rlc(Reg8OrHl::parse(lo)),
                (0x0, 0x8..=0xF) => Rrc(Reg8OrHl::parse(lo)),
                (0x1, 0x0..=0x7) => Rl(Reg8OrHl::parse(lo)),
                (0x1, 0x8..=0xF) => Rr(Reg8OrHl::parse(lo)),
                (0x2, 0x0..=0x7) => Sla(Reg8OrHl::parse(lo)),
                (0x2, 0x8..=0xF) => Sra(Reg8OrHl::parse(lo)),
                (0x3, 0x0..=0x7) => Swap(Reg8OrHl::parse(lo)),
                (0x3, 0x8..=0xF) => Srl(Reg8OrHl::parse(lo)),
                (0x4..=0x7, 0x0..=0xF) => Bit(op_index_8, Reg8OrHl::parse(lo & 0b0111)),
                (0x8..=0xB, 0x0..=0xF) => Res(op_index_8, Reg8OrHl::parse(lo & 0b0111)),
                (0xC..=0xF, 0x0..=0xF) => Set(op_index_8, Reg8OrHl::parse(lo & 0b0111)),
                (_, 16..) => unreachable!("Impossible"),
                (16.., _) => unreachable!("Impossible"),
                _ => unreachable!("Impossible"),
            }
        } else {
            match (hi, lo) {
                (0x0, 0x0) => Nop,
                (0x1, 0x0) => Stop,
                (0x7, 0x6) => Halt,
                (0x2 | 0x3, 0x0 | 0x8) => Jr(Some(op_cond), op_imm8!() as i8),
                (0x1, 0x8) => Jr(None, op_imm8!() as i8), //always
                (0x0, 0x7) => Rlca,
                (0x1, 0x7) => Rla,
                (0x2, 0x7) => Daa,
                (0x3, 0x7) => Scf,
                (0x0, 0xF) => Rrca,
                (0x1, 0xF) => Rra,
                (0x2, 0xF) => Cpl,
                (0x3, 0xF) => Ccf,
                (0xF, 0x3) => Di,
                (0xF, 0xB) => Ei,
                (0xC, 0x6) => AddAImm8(op_imm8!()),
                (0xC, 0xE) => AdcAImm8(op_imm8!()),
                (0xD, 0x6) => SubAImm8(op_imm8!()),
                (0xD, 0xE) => SbcAImm8(op_imm8!()),
                (0xE, 0x6) => AndAImm8(op_imm8!()),
                (0xE, 0xE) => XorAImm8(op_imm8!()),
                (0xF, 0x6) => OrAImm8(op_imm8!()),
                (0xF, 0xE) => CpAImm8(op_imm8!()),
                (0xE, 0x8) => AddSp(op_imm8!() as i8),
                (0xC | 0xD, 0x0 | 0x8) => RetCond(op_cond),
                (0xC, 0x9) => Ret,
                (0xD, 0x9) => Reti,
                (0xC | 0xD, 0x2 | 0xA) => JpImm16(Some(op_cond), op_imm16!()),
                (0xC, 0x3) => JpImm16(None, op_imm16!()),
                (0xC | 0xD, 0x4 | 0xC) => Call(Some(op_cond), op_imm16!()),
                (0xC, 0xD) => Call(None, op_imm16!()),
                (0xE, 0x9) => JpHL,

                (0x8, 0x0..=0x7) => AddA8(Reg8OrHl::parse(lo)),
                (0x8, 0x8..=0xF) => AdcA8(Reg8OrHl::parse(lo & 0b0111)),
                (0x9, 0x0..=0x7) => SubA8(Reg8OrHl::parse(lo)),
                (0x9, 0x8..=0xF) => SbcA8(Reg8OrHl::parse(lo & 0b0111)),
                (0xA, 0x0..=0x7) => AndA8(Reg8OrHl::parse(lo)),
                (0xA, 0x8..=0xF) => XorA8(Reg8OrHl::parse(lo & 0b0111)),
                (0xB, 0x0..=0x7) => OrA8(Reg8OrHl::parse(lo)),
                (0xB, 0x8..=0xF) => CpA8(Reg8OrHl::parse(lo & 0b0111)),
                (0x0..=0x3, 0x9) => AddHl(reg16(hi)),
                (0x0..=0x3, 0x1) => LdImm16(reg16(hi), op_imm16!()),
                (0x0..=0x3, 0x3) => Inc16(reg16(hi)),
                (0xc..=0xF, 0x1) => Pop(reg16(hi)),
                (0xc..=0xF, 0x5) => Push(reg16(hi)),
                (0x0..=0x3, 0x4 | 0xC) => Inc8(op_reg8orhl),
                (0x0..=0x3, 0x5 | 0xD) => Dec8(op_reg8orhl),
                (0x0..=0x3, 0xB) => Dec16(reg16(hi)),
                (0xC..=0xF, 0x7 | 0xF) => Rst(op_index_8),

                (0x4..=0x7, 0x0..=0xF) => Ld8to8(op_reg8orhl, Reg8OrHl::parse(lo & 0b0111)),
                (0x0..=0x3, 0x6 | 0xE) => LdImm8to8(Reg8OrHl::parse(lo & 0b0111), op_imm8!()),
                (0x0..=0x3, 0x2) => LdAIndReg16(LdIndReg16::parse(hi), Store),
                (0x0..=0x3, 0xA) => LdAIndReg16(LdIndReg16::parse(hi), Load),
                (0xE, 0x0) => LdAIndImm8(op_imm8!(), Store),
                (0xF, 0x0) => LdAIndImm8(op_imm8!(), Load),
                (0xE, 0x2) => LdAIndC(Store),
                (0xF, 0x2) => LdAIndC(Load),
                (0xE, 0xA) => LdAIndImm16(op_imm16!(), Store),
                (0xF, 0xA) => LdAIndImm16(op_imm16!(), Load),
                (0x0, 0x8) => LdSPtoIndImm16(op_imm16!()),
                (0xF, 0x9) => LdHLtoSP,
                (0xF, 0x8) => LdSPs8toHL(op_imm8!() as i8),

                (_, 16..) => unreachable!("Impossible"),
                (16.., _) => unreachable!("Impossible"),
                _ => todo!(),
            }
        }
    }

    fn fetch_u8_pc(&mut self) -> u8 {
        let pc = self.registers.pc as usize;
        let res = self.mmu.read_u8(pc);
        self.registers.pc += 1;
        res
    }

    fn fetch_u16_pc(&mut self) -> u16 {
        let pc = self.registers.pc as usize;
        let res = self.mmu.read_u16(pc);
        self.registers.pc += 2;
        res
    }

    fn check_jump_cond(&self, cond: JumpCond) -> bool {
        use JumpCond::*;
        match cond {
            NotZero => !self.registers.f.z,
            Zero => self.registers.f.z,
            NotCarry => !self.registers.f.c,
            Carry => self.registers.f.c,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl Reg8 {
    pub fn parse(n: u8, hi: bool) -> Reg8 {
        match (n, hi) {
            (0, false) => Reg8::B,
            (1, false) => Reg8::D,
            (2, false) => Reg8::H,

            (0, true) => Reg8::C,
            (1, true) => Reg8::E,
            (2, true) => Reg8::L,
            (3, true) => Reg8::A,

            _ => panic!("Invalid register"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

impl Reg16 {
    pub fn parse(n: u8) -> Reg16 {
        match n {
            0x0 => Reg16::BC,
            0x1 => Reg16::DE,
            0x2 => Reg16::HL,
            0x3 => Reg16::SP,

            0xc => Reg16::BC,
            0xd => Reg16::DE,
            0xe => Reg16::HL,
            0xf => Reg16::AF,
            _ => panic!("Invalid register pair"),
        }
    }
}

#[derive(Debug)]
pub enum LdIndReg16 {
    BC,
    DE,
    HLInc,
    HLDec,
}

impl LdIndReg16 {
    pub fn parse(n: u8) -> LdIndReg16 {
        match n {
            0x0 => LdIndReg16::BC,
            0x1 => LdIndReg16::DE,
            0x2 => LdIndReg16::HLInc,
            0x3 => LdIndReg16::HLDec,
            _ => panic!("Invalid register pair"),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Reg8OrHl {
    /// B, C, D, E, H, L, A
    Reg8(Reg8),
    /// (HL)
    Hl,
}

#[derive(Clone, Copy, Debug)]
pub enum JumpCond {
    /// NZ
    NotZero,
    /// Z
    Zero,
    /// NC
    NotCarry,
    /// C
    Carry,
}

impl JumpCond {
    pub fn parse(n: u8) -> JumpCond {
        use JumpCond::*;
        match n {
            0x0 => NotZero,
            0x1 => Zero,
            0x2 => NotCarry,
            0x3 => Carry,
            _ => panic!("Invalid jump condition"),
        }
    }
}

impl Reg8OrHl {
    pub fn parse(n: u8) -> Reg8OrHl {
        use self::Reg8::*;
        use Reg8OrHl::*;
        match n {
            0 => Reg8(B),
            1 => Reg8(C),
            2 => Reg8(D),
            3 => Reg8(E),
            4 => Reg8(H),
            5 => Reg8(L),
            6 => Hl,
            7 => Reg8(A),
            _ => panic!("Invalid register"),
        }
    }
}

#[derive(Debug)]
pub enum LoadStore {
    Load,
    Store,
}

#[derive(Debug)]
pub enum Instruction {
    Nop,
    Stop,
    Halt,
    Jr(Option<JumpCond>, i8),
    LdImm16(Reg16, u16),
    Inc16(Reg16),
    Pop(Reg16),
    Push(Reg16),
    Inc8(Reg8OrHl),
    Dec8(Reg8OrHl),
    Dec16(Reg16),
    Rlca,
    Rla,
    Rra,
    Rrca,
    Rlc(Reg8OrHl),
    Rrc(Reg8OrHl),
    AddHl(Reg16),
    AddA8(Reg8OrHl),
    AddAImm8(u8),
    AddSp(i8),
    AdcA8(Reg8OrHl),
    AdcAImm8(u8),
    SubA8(Reg8OrHl),
    SubAImm8(u8),
    SbcA8(Reg8OrHl),
    SbcAImm8(u8),
    AndA8(Reg8OrHl),
    AndAImm8(u8),
    XorA8(Reg8OrHl),
    XorAImm8(u8),
    OrA8(Reg8OrHl),
    OrAImm8(u8),
    CpA8(Reg8OrHl),
    CpAImm8(u8),
    Daa,
    Scf,
    Cpl,
    Ccf,
    Di,
    Ei,
    Rst(u8), // variant only varies between 0 and 7
    Ld8to8(Reg8OrHl, Reg8OrHl),
    LdImm8to8(Reg8OrHl, u8),
    LdAIndReg16(LdIndReg16, LoadStore),
    LdAIndImm8(u8, LoadStore),
    LdAIndImm16(u16, LoadStore),
    LdAIndC(LoadStore),
    LdSPtoIndImm16(u16),
    LdHLtoSP,
    LdSPs8toHL(i8),
    RetCond(JumpCond),
    Ret,
    Reti,
    JpImm16(Option<JumpCond>, u16),
    JpHL,
    Call(Option<JumpCond>, u16),
    Rl(Reg8OrHl),
    Rr(Reg8OrHl),
    Sla(Reg8OrHl),
    Sra(Reg8OrHl),
    Swap(Reg8OrHl),
    Srl(Reg8OrHl),
    Bit(u8, Reg8OrHl),
    Res(u8, Reg8OrHl),
    Set(u8, Reg8OrHl),
}
