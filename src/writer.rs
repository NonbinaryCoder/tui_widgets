use std::{fmt::Display, io};

mod stdout;
mod test;

pub use stdout::*;
pub use test::*;

use crate::{
    math::{Pos2, Rect2, Size2},
    widget::{
        style::{Color, Colors},
        RenderResult, Widget,
    },
};

/// Something that writes to the terminal or a fake terminal.
///
/// # Implementation notes
///
/// All widgets assume their top left corner is at (0, 0), so writers must
/// remember the offset the current widget is at and add it to the inputs of any
/// function that takes a position
pub trait TerminalWriter: Sized {
    /* Size */

    fn term_size(&self) -> io::Result<Size2<u16>>;

    /* Screen */

    fn enter_alternate_screen(&mut self) -> io::Result<&mut Self>;
    fn leave_alternate_screen(&mut self) -> io::Result<&mut Self>;

    /* Cursor position */

    /// Moves the terminal cursor to the given position.
    ///
    /// Top left cell is (0, 0).
    fn cursor_to(&mut self, pos: impl Into<Pos2<u16>>) -> io::Result<&mut Self>;
    /// Moves the terminal cursor to the given column on the current row.
    ///
    /// 0 is the leftmost column.
    fn cursor_to_column(&mut self, column: u16) -> io::Result<&mut Self>;
    /// Moves the terminal cursor to the given row on the current column.
    ///
    /// 0 is the top row.
    fn cursor_to_row(&mut self, row: u16) -> io::Result<&mut Self>;

    /* Cursor move */

    fn cursor_left(&mut self, distance: u16) -> io::Result<&mut Self>;
    fn cursor_right(&mut self, distance: u16) -> io::Result<&mut Self>;
    fn cursor_up(&mut self, distance: u16) -> io::Result<&mut Self>;
    fn cursor_down(&mut self, distance: u16) -> io::Result<&mut Self>;

    /* Print */

    fn write_char(&mut self, ch: char) -> io::Result<&mut Self>;
    fn print(&mut self, arg: impl Display) -> io::Result<&mut Self>;
    fn repeat_char(&mut self, ch: char, times: impl Into<usize>) -> io::Result<&mut Self> {
        for _ in 0..times.into() {
            self.write_char(ch)?;
        }
        Ok(self)
    }
    fn fill_char(&mut self, ch: char, area: Rect2<u16>) -> io::Result<&mut Self> {
        self.cursor_to(area.pos())?.repeat_char(ch, area.width)?;
        for _ in 1..area.height {
            self.cursor_to_column(area.x)?
                .cursor_down(1)?
                .repeat_char(ch, area.width)?;
        }
        Ok(self)
    }

    /* Color */

    fn set_foreground_color(&mut self, color: Color) -> io::Result<&mut Self>;
    fn set_background_color(&mut self, color: Color) -> io::Result<&mut Self>;
    fn set_colors(&mut self, colors: Colors) -> io::Result<&mut Self> {
        self.set_foreground_color(colors.fg)?
            .set_background_color(colors.bg)
    }

    fn add_widget<W: Widget<Self>>(
        &mut self,
        widget: &mut W,
        rect: Rect2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult>;

    /// Writes any output this has buffered.
    fn flush(&mut self) -> io::Result<()>;
}
