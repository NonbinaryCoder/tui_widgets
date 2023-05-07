use std::io;

mod stdout;
mod test;

pub use stdout::*;
pub use test::*;

use crate::{
    math::{Rect2, Size2},
    widget::{RenderResult, Widget},
};

/// Something that writes to the terminal or a fake terminal.
///
/// # Implementation notes
///
/// All widgets assume their top left corner is at (0, 0), so writers must
/// remember the offset the current widget is at and add it to the inputs of any
/// function that takes a position
pub trait TerminalWriter: Sized {
    fn term_size(&self) -> io::Result<Size2<u16>>;

    fn enter_alternate_screen(&mut self) -> io::Result<&mut Self>;
    fn leave_alternate_screen(&mut self) -> io::Result<&mut Self>;

    fn add_widget<W: Widget<Self>>(
        &mut self,
        widget: &mut W,
        rect: Rect2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult>;

    /// Writes any output this has buffered.
    fn flush(&mut self) -> io::Result<()>;
}
