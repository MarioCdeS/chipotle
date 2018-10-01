extern crate stdweb;
extern crate chipotle;

use stdweb::traits::*;
use stdweb::unstable::TryInto;
use stdweb::web;
use stdweb::web::html_element::CanvasElement;

use chipotle::Emulator;

fn main() {
    stdweb::initialize();

    let canvas: CanvasElement = web::document()
        .query_selector("#chipotle-canvas")
        .unwrap()
        .unwrap()
        .try_into()
        .unwrap();

    let emulator = Emulator::new(canvas);
    Emulator::start(&emulator);

    stdweb::event_loop();
}
