extern crate rand;
extern crate stdweb;

use std::cell::RefCell;
use std::rc::Rc;

use stdweb::traits::*;
use stdweb::web::{self, CanvasRenderingContext2d};
use stdweb::web::event::KeyDownEvent;
use stdweb::web::html_element::CanvasElement;

macro_rules! regx {
    ($x:expr) => (((($x) & 0x0F00) >> 8) as usize);
}

macro_rules! regy {
    ($x:expr) => (((($x) & 0x00F0) >> 4) as usize);
}

macro_rules! imm4 {
    ($x:expr) => ((($x) & 0x000F) as u8);
}

macro_rules! imm8 {
    ($x:expr) => ((($x) & 0x00FF) as u8);
}

macro_rules! imm12 {
    ($x:expr) => (((($x) & 0x0FFF) >> 8) as u16);
}

macro_rules! panic_unknown {
    ($x:expr) => (panic!(format!("Unknown instruction: {:x}", ($x))));
}

const INSTR_SIZE: u16 = 2;
const NUM_GEN_REGS: usize = 16;
const STACK_SIZE: u8 = 17;
const RAM_SIZE: usize = 4096;
const DISP_WIDTH: usize = 64;
const DISP_HEIGHT: usize = 32;
const VRAM_SIZE: usize = DISP_WIDTH as usize * DISP_HEIGHT as usize / 8;
const NUM_KEYS: usize = 16;

enum State {
    Halted,
    Running,
    BlockTillKeyPressed(usize),
}

pub struct Emulator {
    canvas: CanvasElement,
    ctx2d: CanvasRenderingContext2d,
    rom_loaded: bool,
    paused: bool,
    regs: [u8; NUM_GEN_REGS],
    reg_i: u16,
    reg_delay: u8,
    reg_sound: u8,
    pc: u16,
    sp: u8,
    stack: [u16; STACK_SIZE as usize],
    ram: [u8; RAM_SIZE],
    vram: [u8; VRAM_SIZE],
    rom: Vec<u8>,
    keys: [bool; NUM_KEYS],
    state: State,
}

impl Emulator {
    pub fn new(canvas: CanvasElement) -> Rc<RefCell<Emulator>> {
        let ctx2d: CanvasRenderingContext2d = canvas.get_context().unwrap();

        let emulator = Rc::new(RefCell::new(Emulator {
            canvas,
            ctx2d,
            rom_loaded: false,
            paused: false,
            regs: [0; NUM_GEN_REGS],
            reg_i: 0,
            reg_delay: 0,
            reg_sound: 0,
            pc: 0,
            sp: 0,
            stack: [0; STACK_SIZE as usize],
            ram: [0; RAM_SIZE],
            vram: [0; VRAM_SIZE],
            rom: Vec::new(),
            keys: [false; NUM_KEYS],
            state: State::Halted,
        }));

        Emulator::setup_input(&emulator);

        emulator
    }

    pub fn start(emulator: &Rc<RefCell<Emulator>>) {
        if emulator.borrow().rom_loaded {
            emulator.borrow_mut().state = State::Running;
            Emulator::emulation_loop(Rc::clone(emulator));
        }
    }

    fn setup_input(emulator: &Rc<RefCell<Emulator>>) {
        web::window().add_event_listener({
            let emulator = Rc::clone(emulator);

            move |event: KeyDownEvent| {
                if emulator.borrow_mut().key_down(&event.key()) {
                    event.prevent_default();
                }
            }
        });
    }

    fn key_down(&mut self, key: &str) -> bool {
        false
    }

    fn emulation_loop(emulator: Rc<RefCell<Emulator>>) {
        emulator.borrow_mut().step();

        web::window().request_animation_frame({
            let emulator = Rc::clone(&emulator);

            move |_| {
                Emulator::emulation_loop(emulator);
            }
        });
    }

    fn step(&mut self) {
        match self.state {
            State::Running => self.fetch_decode_execute(),
            State::BlockTillKeyPressed(reg_idx) => self.block_till_key_pressed(reg_idx),
            _ => return,
        }
    }

    fn fetch_decode_execute(&mut self) {
        let instruction =
            ((self.rom[self.pc as usize] as u16) << 8) | self.rom[(self.pc + 1) as usize] as u16;

        self.pc += INSTR_SIZE;

        match instruction & 0xF000 {
            0x0000 => match instruction {
                0x00E0 => self.clear_vram(),
                0x00EE => self.ret(),
                _ => panic!("Instruction 0x0NNN has not been implemented."),
            },
            0x1000 => self.jump(imm12!(instruction)),
            0x2000 => self.call(imm12!(instruction)),
            0x3000 => self.skip_reg_eq_imm(regx!(instruction), imm8!(instruction)),
            0x4000 => self.skip_reg_neq_imm(regx!(instruction), imm8!(instruction)),
            0x5000 => self.skip_reg_eq_reg(regx!(instruction), regy!(instruction)),
            0x6000 => self.load_reg_from_imm(regx!(instruction), imm8!(instruction)),
            0x7000 => self.add_reg_and_imm(regx!(instruction), imm8!(instruction)),
            0x8000 => match instruction & 0x000F {
                0x0000 => self.load_reg_from_reg(regx!(instruction), regy!(instruction)),
                0x0001 => self.set_reg_or_reg(regx!(instruction), regy!(instruction)),
                0x0002 => self.set_reg_and_reg(regx!(instruction), regy!(instruction)),
                0x0003 => self.set_reg_xor_reg(regx!(instruction), regy!(instruction)),
                0x0004 => self.add_reg_and_reg(regx!(instruction), regy!(instruction)),
                0x0005 => self.sub_reg_from_reg(regx!(instruction), regy!(instruction)),
                0x0006 => self.shr_reg(regx!(instruction)),
                0x0007 => self.subn_reg_from_reg(regx!(instruction), regy!(instruction)),
                0x000E => self.shl_reg(regx!(instruction)),
                _ => panic_unknown!(instruction),
            },
            0x9000 => self.skip_reg_neq_reg(regx!(instruction), regy!(instruction)),
            0xA000 => self.load_regi_from_imm(imm12!(instruction)),
            0xB000 => self.jump_reg0_plus_imm(imm12!(instruction)),
            0xC000 => self.set_reg_rand_and_imm(regx!(instruction), imm8!(instruction)),
            0xD000 => self.display_sprite(
                regx!(instruction), regy!(instruction), imm4!(instruction),
            ),
            0xE000 => match instruction & 0x00FF {
                0x009E => self.skip_key_pressed(regx!(instruction)),
                0x00A1 => self.skip_key_released(regx!(instruction)),
                _ => panic_unknown!(instruction),
            },
            0xF000 => match instruction & 0x00FF {
                0x0007 => self.set_reg_to_delay(regx!(instruction)),
                0x000A => self.set_reg_to_key_pressed(regx!(instruction)),
                _ => panic_unknown!(instruction),
            },
            _ => panic_unknown!(instruction),
        }
    }

    fn block_till_key_pressed(&mut self, reg_idx: usize) {
        let key = self.keys
            .iter()
            .enumerate()
            .find(|(_, pressed)| **pressed);

        if let Some((key, _)) = key {
            self.regs[reg_idx] = key as u8;
            self.state = State::Running;
        }
    }

    fn clear_vram(&mut self) {
        for i in self.vram.iter_mut() {
            *i = 0;
        }
    }

    fn ret(&mut self) {
        self.pc = self.stack[self.sp as usize];
        self.sp -= 1;
    }

    fn jump(&mut self, address: u16) {
        self.pc = address;
    }

    fn call(&mut self, address: u16) {
        self.sp += 1;

        if self.sp >= STACK_SIZE {
            panic!("Stack overflow.");
        }

        self.stack[self.sp as usize] = self.pc;
        self.pc = address;
    }

    fn skip_reg_eq_imm(&mut self, reg_idx: usize, immediate: u8) {
        if self.regs[reg_idx] == immediate {
            self.pc += INSTR_SIZE;
        }
    }

    fn skip_reg_neq_imm(&mut self, reg_idx: usize, immediate: u8) {
        if self.regs[reg_idx] != immediate {
            self.pc += INSTR_SIZE;
        }
    }

    fn skip_reg_eq_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        if self.regs[reg_x_idx] == self.regs[reg_y_idx] {
            self.pc += INSTR_SIZE;
        }
    }

    fn load_reg_from_imm(&mut self, reg_idx: usize, immediate: u8) {
        self.regs[reg_idx] = immediate;
    }

    fn add_reg_and_imm(&mut self, reg_idx: usize, immediate: u8) {
        self.regs[reg_idx] += immediate;
    }

    fn load_reg_from_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        self.regs[reg_x_idx] = self.regs[reg_y_idx];
    }

    fn set_reg_or_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        self.regs[reg_x_idx] |= self.regs[reg_y_idx];
    }

    fn set_reg_and_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        self.regs[reg_x_idx] &= self.regs[reg_y_idx];
    }

    fn set_reg_xor_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        self.regs[reg_x_idx] ^= self.regs[reg_y_idx];
    }

    fn add_reg_and_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        let (sum, overflow) = self.regs[reg_x_idx].overflowing_add(self.regs[reg_y_idx]);
        self.regs[reg_x_idx] = sum;
        self.regs[0xF] = if overflow { 1 } else { 0 };
    }

    fn sub_reg_from_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        let (diff, overflow) = self.regs[reg_x_idx].overflowing_sub(self.regs[reg_y_idx]);
        self.regs[reg_x_idx] = diff;
        self.regs[0xF] = if overflow { 0 } else { 1 };
    }

    fn shr_reg(&mut self, reg_idx: usize) {
        self.regs[0xF] = self.regs[reg_idx] & 0b00000001;
        self.regs[reg_idx] >>= 1;
    }

    fn subn_reg_from_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        let (diff, overflow) = self.regs[reg_y_idx].overflowing_sub(self.regs[reg_x_idx]);
        self.regs[reg_x_idx] = diff;
        self.regs[0xF] = if overflow { 0 } else { 1 };
    }

    fn shl_reg(&mut self, reg_idx: usize) {
        self.regs[0xF] = self.regs[reg_idx] & 0b00000001;
        self.regs[reg_idx] <<= 1;
    }

    fn skip_reg_neq_reg(&mut self, reg_x_idx: usize, reg_y_idx: usize) {
        if self.regs[reg_x_idx] != self.regs[reg_y_idx] {
            self.pc += INSTR_SIZE;
        }
    }

    fn load_regi_from_imm(&mut self, immediate: u16) {
        self.reg_i = immediate;
    }

    fn jump_reg0_plus_imm(&mut self, immediate: u16) {
        self.pc = self.regs[0] as u16 + immediate;
    }

    fn set_reg_rand_and_imm(&mut self, reg_idx: usize, immediate: u8) {
        self.regs[reg_idx] = rand::random::<u8>() & immediate;
    }

    fn display_sprite(&mut self, reg_x_idx: usize, reg_y_idx: usize, num_bytes: u8) {
        let sprite = &self.ram[self.reg_i as usize..self.reg_i as usize + num_bytes as usize];
        let x = self.regs[reg_x_idx] / 8;
        let y = self.regs[reg_y_idx];
        let bit_offset = self.regs[reg_x_idx] % 8;
        let mut bit_erased = false;

        for (i, byte) in sprite.iter().enumerate() {
            let left = (byte & (0xFF << bit_offset)) >> bit_offset;
            let right = (byte & (0xFF >> (8 - bit_offset))) << (8 - bit_offset);

            let mut addr =
                (x as usize % DISP_WIDTH) + ((y as usize + i) % DISP_HEIGHT) * DISP_HEIGHT;

            bit_erased |= self.vram[addr] & left > 0;
            self.vram[addr] ^= left;

            addr = ((x as usize + 1) % DISP_WIDTH) + ((y as usize + i) % DISP_HEIGHT) * DISP_HEIGHT;
            bit_erased |= self.vram[addr] & right > 0;
            self.vram[addr] ^= right;
        }

        self.regs[0xF] = if bit_erased { 1 } else { 0 };
    }

    fn skip_key_pressed(&mut self, reg_idx: usize) {
        if self.keys[self.regs[reg_idx] as usize] {
            self.pc += INSTR_SIZE;
        }
    }

    fn skip_key_released(&mut self, reg_idx: usize) {
        if !self.keys[self.regs[reg_idx] as usize] {
            self.pc += INSTR_SIZE;
        }
    }

    fn set_reg_to_delay(&mut self, reg_idx: usize) {
        self.regs[reg_idx] = self.reg_delay;
    }

    fn set_reg_to_key_pressed(&mut self, reg_idx: usize) {
        self.state = State::BlockTillKeyPressed(reg_idx);
        self.block_till_key_pressed(reg_idx);
    }
}
