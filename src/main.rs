#[macro_use]
extern crate stdweb;
extern crate chipotle;

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

    load_button.add_event_listener(load_button_event_listener);

    Emulator::start(&emulator);

    stdweb::event_loop();
}

fn load_button_event_listener(event: ChangeEvent) {
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
        let reader = reader.clone();
        move |_: ProgressLoadEvent| {
            let text = match reader.result() {
                Some(FileReaderResult::String(text)) => text,
                _ => String::from(""),
            };

            js! {
                console.log(@{text});
            }
        }
    });
    reader.read_as_text(&file).unwrap();
}
