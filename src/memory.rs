const BOOTSTRAM_ROM: &'static [u8; 256] = include_bytes!("DMG_ROM.bin");

pub struct Memory {
    pub(crate) memory: [u8; 0xFFFF], // TODO
}

impl Memory {
    pub fn new() -> Self {
        Self {
            memory: [0u8; 0xFFFF],
        }
    }

    pub fn read_u16(&self, addr: usize) -> u16 {
        let lo = self.read_u8(addr) as u16;
        let hi = self.read_u8(addr + 1) as u16;
        (hi << 8) | lo
    }

    pub fn read_u8(&self, addr: usize) -> u8 {
        match addr {
            0x0000..=0x00FF => BOOTSTRAM_ROM[addr],
            _ => self.memory[addr],
        }
    }

    pub fn write_u8(&mut self, addr: usize, val: u8) {
        match addr {
            0x0000..=0x00FF => panic!("Cannot write to bootstram ROM"),
            _ => self.memory[addr] = val,
        }
    }

    pub fn write_u16(&mut self, addr: usize, val: u16) {
        let lo = val as u8;
        let hi = (val >> 8) as u8;
        self.write_u8(addr, lo);
        self.write_u8(addr + 1, hi);
    }
}
