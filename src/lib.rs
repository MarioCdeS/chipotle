#[macro_use]
extern crate stdweb;

use std::cell::RefCell;
use std::rc::Rc;

use stdweb::traits::*;
use stdweb::web::{self, CanvasRenderingContext2d};
use stdweb::web::event::KeyDownEvent;
use stdweb::web::html_element::CanvasElement;

pub struct Emulator {
    canvas: CanvasElement,
    ctx2d: CanvasRenderingContext2d,
    rom_loaded: bool,
    paused: bool,
    test: &'static str,
}

impl Emulator {
    pub fn new(canvas: CanvasElement) -> Rc<RefCell<Emulator>> {
        let ctx2d: CanvasRenderingContext2d = canvas.get_context().unwrap();

        let emulator = Rc::new(RefCell::new(Emulator {
            canvas,
            ctx2d,
            rom_loaded: false,
            paused: false,
            test: "red",
        }));

        setup_input(&emulator);

        emulator
    }

    fn key_down(&mut self, key: &str) -> bool {
        if key == " " {
            self.test = match self.test {
                "red" => "green",
                "green" => "blue",
                "blue" => "red",
                _ => "white",
            };

            true
        } else {
            false
        }
    }
}

pub fn start(emulator: &Rc<RefCell<Emulator>>) {
    emulator_step(Rc::clone(emulator));
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

fn emulator_step(emulator: Rc<RefCell<Emulator>>) {
    web::window().request_animation_frame({
        let emulator = Rc::clone(&emulator);

        move |_| {
            emulator_step(emulator);
        }
    });

    let emulator = emulator.borrow();

    emulator.ctx2d.set_fill_style_color(emulator.test);
    emulator.ctx2d.fill_rect(
        0.0,
        0.0,
        emulator.canvas.width() as f64,
        emulator.canvas.height() as f64,
    );
}
