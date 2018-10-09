#[macro_use]
extern crate stdweb;
extern crate chipotle;

use stdweb::traits::*;
use stdweb::unstable::TryInto;
use stdweb::web::{self, FileList};
use stdweb::web::event::ChangeEvent;
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

    load_button.add_event_listener(move |event: ChangeEvent| {
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

        js! {
            console.log(@{file});
        }
    });
    
    Emulator::start(&emulator);

    stdweb::event_loop();
}
