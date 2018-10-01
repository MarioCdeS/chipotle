#[macro_use]
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

macro_rules! imm8 {
    ($x:expr) => ((($x) & 0x00FF) as u8);
}

macro_rules! imm12 {
    ($x:expr) => (((($x) & 0x0FFF) >> 8) as u16);
}

const INSTR_SIZE: u16 = 2;
const NUM_GEN_REGS: usize = 16;
const STACK_SIZE: u8 = 17;
const RAM_SIZE: usize = 4096;
const DISP_WIDTH: usize = 64;
const DISP_HEIGHT: usize = 32;

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
    vram: [bool; DISP_WIDTH * DISP_HEIGHT],
    rom: Vec<u8>,
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
            vram: [false; DISP_WIDTH * DISP_HEIGHT],
            rom: Vec::new(),
        }));

        Emulator::setup_input(&emulator);

        emulator
    }

    pub fn start(emulator: &Rc<RefCell<Emulator>>) {
        if emulator.borrow().rom_loaded {
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
                _ => panic!(format!("Unknown instruction: {:x}", instruction)),
            },
            _ => panic!(format!("Unknown instruction: {:x}", instruction)),
        };
    }

    fn key_down(&mut self, key: &str) -> bool {
        false
    }

    fn clear_vram(&mut self) {
        for i in self.vram.iter_mut() {
            *i = false;
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
}
