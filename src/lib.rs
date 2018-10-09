extern crate rand;
extern crate stdweb;

use std::cell::RefCell;
use std::rc::Rc;

use stdweb::traits::*;
use stdweb::web::{self, CanvasRenderingContext2d};
use stdweb::web::event::{KeyDownEvent, KeyUpEvent};
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
const SCREEN_WIDTH: usize = 64;
const SCREEN_HEIGHT: usize = 32;
const VRAM_SIZE: usize = SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize / 8;
const NUM_KEYS: usize = 16;
const DIGIT_SPRITE_SIZE: u16 = 5;

enum State {
    Halted,
    Paused,
    Running,
    BlockTillKeyPressed(usize),
}

enum KeyState {
    Pressed,
    Released,
}

pub struct Emulator {
    canvas: CanvasElement,
    ctx2d: CanvasRenderingContext2d,
    pixel_rect: (f64, f64),
    regs: [u8; NUM_GEN_REGS],
    reg_i: u16,
    reg_delay: u8,
    reg_sound: u8,
    pc: u16,
    sp: u8,
    stack: [u16; STACK_SIZE as usize],
    ram: [u8; RAM_SIZE],
    vram: [u8; VRAM_SIZE],
    rom: Option<Vec<u8>>,
    keys: [bool; NUM_KEYS],
    state: State,
}

impl Emulator {
    pub fn new(canvas: CanvasElement) -> Rc<RefCell<Emulator>> {
        let ctx2d: CanvasRenderingContext2d = canvas.get_context().unwrap();
        let pixel_rect = (
            canvas.width() as f64 / SCREEN_WIDTH as f64,
            canvas.height() as f64 / SCREEN_HEIGHT as f64
        );

        let emulator = Rc::new(RefCell::new(Emulator {
            canvas,
            ctx2d,
            pixel_rect,
            regs: [0; NUM_GEN_REGS],
            reg_i: 0,
            reg_delay: 0,
            reg_sound: 0,
            pc: 0,
            sp: 0,
            stack: [0; STACK_SIZE as usize],
            ram: [0; RAM_SIZE],
            vram: [0; VRAM_SIZE],
            rom: None,
            keys: [false; NUM_KEYS],
            state: State::Halted,
        }));

        Emulator::setup_input(&emulator);
        emulator.borrow_mut().load_digit_sprites();

        emulator
    }

    pub fn load_rom(emulator: &Rc<RefCell<Emulator>>, rom: Vec<u8>) {
        emulator.borrow_mut().rom = Some(rom);
    }

    pub fn start(emulator: &Rc<RefCell<Emulator>>) {
        emulator.borrow_mut().state = State::Running;
        Emulator::emulation_loop(Rc::clone(emulator));
    }

    pub fn pause(emulator: &Rc<RefCell<Emulator>>) {
        if let Emulator { state: ref mut state @ State::Running, .. } = *emulator.borrow_mut() {
            *state = State::Paused;
        }
    }

    pub fn resume(emulator: &Rc<RefCell<Emulator>>) {
        if let Emulator { state: ref mut state @ State::Paused, .. } = *emulator.borrow_mut() {
            *state = State::Running;
        }
    }

    fn setup_input(emulator: &Rc<RefCell<Emulator>>) {
        web::window().add_event_listener({
            let emulator = Rc::clone(emulator);

            move |event: KeyDownEvent| {
                if emulator.borrow_mut().update_key_state(&event.key(), KeyState::Pressed) {
                    event.prevent_default();
                }
            }
        });

        web::window().add_event_listener({
            let emulator = Rc::clone(emulator);

            move |event: KeyUpEvent| {
                if emulator.borrow_mut().update_key_state(&event.key(), KeyState::Released) {
                    event.prevent_default();
                }
            }
        });
    }

    fn load_digit_sprites(&mut self) {
        let sprites = [
            0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
            0x20, 0x60, 0x20, 0x20, 0x70, // 1
            0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
            0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
            0x90, 0x90, 0xF0, 0x10, 0x10, // 4
            0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
            0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
            0xF0, 0x10, 0x20, 0x40, 0x40, // 7
            0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
            0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
            0xF0, 0x90, 0xF0, 0x90, 0x90, // A
            0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
            0xF0, 0x80, 0x80, 0x80, 0xF0, // C
            0xE0, 0x90, 0x90, 0x90, 0xE0, // D
            0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
            0xF0, 0x80, 0xF0, 0x80, 0x80, // F
        ];

        self.ram[..sprites.len()].clone_from_slice(&sprites);
    }

    fn update_key_state(&mut self, key: &str, state: KeyState) -> bool {
        let key_idx = match key {
            "7" => Some(1),
            "8" => Some(2),
            "9" => Some(3),
            "0" => Some(12),
            "u" => Some(4),
            "i" => Some(5),
            "o" => Some(6),
            "p" => Some(13),
            "j" => Some(7),
            "k" => Some(8),
            "l" => Some(9),
            ";" => Some(14),
            "m" => Some(10),
            "," => Some(0),
            "." => Some(11),
            "/" => Some(15),
            _ => None,
        };

        if let Some(key_idx) = key_idx {
            self.keys[key_idx] = match state {
                KeyState::Pressed => true,
                KeyState::Released => false,
            };

            true
        } else {
            false
        }
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
        if self.reg_sound > 0 {
            self.reg_sound -= 1;
        }

        if self.reg_delay > 0 {
            self.reg_delay -= 1;
            return;
        }

        match self.state {
            State::Running => {
                self.fetch_decode_execute();
                self.draw_screen();
            }
            State::BlockTillKeyPressed(reg_idx) => self.block_till_key_pressed(reg_idx),
            _ => return,
        }
    }

    fn fetch_decode_execute(&mut self) {
        let instruction = match &self.rom {
            Some(rom) =>
                ((rom[self.pc as usize] as u16) << 8) | rom[(self.pc + 1) as usize] as u16,
            None => return,
        };

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
                0x0015 => self.set_delay_to_reg(regx!(instruction)),
                0x0018 => self.set_sound_to_reg(regx!(instruction)),
                0x001E => self.add_regi_and_reg(regx!(instruction)),
                0x0029 => self.set_regi_to_digit_sprite(regx!(instruction)),
                0x0033 => self.load_mem_from_reg_bcd(regx!(instruction)),
                0x0055 => self.load_mem_from_regs(regx!(instruction)),
                0x0065 => self.load_regs_from_mem(regx!(instruction)),
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

    fn draw_screen(&self) {
        self.ctx2d.clear_rect(0.0, 0.0, self.canvas.width() as f64, self.canvas.height() as f64);
        self.ctx2d.set_fill_style_color("white");

        for (i, byte) in self.vram.iter().enumerate() {
            for j in 0..8 {
                if *byte & (1 << (7 - j)) == 0 {
                    continue;
                }

                let x = ((i * 8 + j) % SCREEN_WIDTH) as f64 * self.pixel_rect.0;
                let y = ((i * 8 + j) / SCREEN_WIDTH) as f64 * self.pixel_rect.1;

                self.ctx2d.fill_rect(x, y, self.pixel_rect.0, self.pixel_rect.1);
            }
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
                (x as usize % SCREEN_WIDTH) + ((y as usize + i) % SCREEN_HEIGHT) * SCREEN_HEIGHT;

            bit_erased |= self.vram[addr] & left > 0;
            self.vram[addr] ^= left;

            addr =
                ((x as usize + 1) % SCREEN_WIDTH) + ((y as usize + i) % SCREEN_HEIGHT) *
                    SCREEN_HEIGHT;

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

    fn set_delay_to_reg(&mut self, reg_idx: usize) {
        self.reg_delay = self.regs[reg_idx];
    }

    fn set_sound_to_reg(&mut self, reg_idx: usize) {
        self.reg_sound = self.regs[reg_idx];
    }

    fn add_regi_and_reg(&mut self, reg_idx: usize) {
        self.reg_i += self.regs[reg_idx] as u16;
    }

    fn set_regi_to_digit_sprite(&mut self, reg_idx: usize) {
        self.reg_i = self.regs[reg_idx] as u16 * DIGIT_SPRITE_SIZE;
    }

    fn load_mem_from_reg_bcd(&mut self, reg_idx: usize) {
        let mut units = self.regs[reg_idx];

        let hundreds = units / 100;
        units %= 100;

        let tens = units / 10;
        units %= 10;

        self.ram[self.reg_i as usize] = hundreds;
        self.ram[self.reg_i as usize + 1] = tens;
        self.ram[self.reg_i as usize + 2] = units;
    }

    fn load_mem_from_regs(&mut self, reg_idx: usize) {
        self.ram[self.reg_i as usize..self.reg_i as usize + reg_idx]
            .clone_from_slice(&self.regs[..reg_idx]);
    }

    fn load_regs_from_mem(&mut self, reg_idx: usize) {
        self.regs[..reg_idx].clone_from_slice(
            &self.ram[self.reg_i as usize..self.reg_i as usize + reg_idx]
        );
    }
}
