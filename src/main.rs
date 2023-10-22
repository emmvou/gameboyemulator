mod cpu;
mod memory;

fn main() {
    println!("Hello, world!");
    let mut cpu = cpu::CPU::new();
    while cpu.registers.pc < 0x00a8 {
        println!("{:?}", cpu.fetch_instruction());
    }
}
