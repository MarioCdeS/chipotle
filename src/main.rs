#[macro_use]
extern crate stdweb;
extern crate chipotle;

use std::rc::Rc;
use std::cell::RefCell;

use stdweb::traits::*;
use stdweb::unstable::TryInto;
use stdweb::web::{self, FileList, FileReader, FileReaderResult};
use stdweb::web::event::{ChangeEvent, ProgressLoadEvent};
use stdweb::web::html_element::{CanvasElement, InputElement};

use chipotle::Emulator;

fn main() {
    stdweb::initialize();

    let canvas: CanvasElement = web::document()
        .get_element_by_id("chipotle-canvas")
        .unwrap()
        .try_into()
        .unwrap();

    let emulator = Emulator::new(canvas);

    let load_button = web::document()
        .get_element_by_id("load-rom-button")
        .unwrap();

    load_button.add_event_listener({
        let emulator = Rc::clone(&emulator);

        move |event: ChangeEvent| {
            load_button_event_listener(&emulator, event);
        }
    });

    Emulator::start(&emulator);

    stdweb::event_loop();
}

fn load_button_event_listener(emulator: &Rc<RefCell<Emulator>>, event: ChangeEvent) {
    Emulator::pause(emulator);
    
    let input: InputElement = event.target()
        .unwrap()
        .try_into()
        .unwrap();

    let files: FileList = js! {
        return @{input}.files;
    }.try_into().unwrap();

    let file = match files.iter().next() {
        Some(file) => file,
        None => return,
    };

    let reader = FileReader::new();

    reader.add_event_listener({
        let emulator = Rc::clone(emulator);

        move |event: ProgressLoadEvent| {
            let reader: FileReader = event.target()
                .unwrap()
                .try_into()
                .unwrap();

            let rom = match reader.result() {
                Some(FileReaderResult::ArrayBuffer(bytes)) => Some(bytes),
                _ => None,
            };

            if let Some(rom) = rom {
                Emulator::load_rom(&emulator, Vec::from(rom));
                Emulator::resume(&emulator);
            }
        }
    });

    reader.read_as_array_buffer(&file).unwrap();
}
