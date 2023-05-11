use std::{
    io::{self, BufWriter, StdoutLock, Write},
    sync::atomic::{AtomicBool, Ordering},
};

use crossterm::{cursor, queue, style, terminal};

use crate::{
    math::{Pos2, Rect2, Size2, Vec2},
    widget::{style::Color, RenderResult, Widget},
};

use super::TerminalWriter;

/// Set to true while a `StdoutWriter` exists; set to false when it is dropped
static WRITER_RUNNING: AtomicBool = AtomicBool::new(false);

/// Terminal writer outputting to stdout.  Only one may exist at a time
#[derive(Debug)]
pub struct StdoutWriter {
    out: BufWriter<StdoutLock<'static>>,

    offset: Vec2<u16>,
}

impl StdoutWriter {
    /// Creates a new [`StdoutWriter`].
    ///
    /// # Panics
    ///
    /// Panics if a [`StdoutWriter`] already exists.
    pub fn new() -> Self {
        // Probably doesn't need to be `SeqCst`, but I don't understand atomics
        // and don't want anything to go wrong.
        let was_running = WRITER_RUNNING.swap(true, Ordering::SeqCst);
        if was_running {
            panic!("Only one `StdoutWriter` can exist at a time!")
        }
        Self {
            out: BufWriter::new(io::stdout().lock()),
            offset: Vec2::splat(0),
        }
    }
}

impl Drop for StdoutWriter {
    fn drop(&mut self) {
        // Probably doesn't need to be `SeqCst`, but I don't understand atomics
        // and don't want anything to go wrong.
        WRITER_RUNNING.store(false, Ordering::SeqCst)
    }
}

impl TerminalWriter for StdoutWriter {
    /* Size */

    fn term_size(&self) -> io::Result<Size2<u16>> {
        terminal::size().map(Into::into)
    }

    /* Screen */

    fn enter_alternate_screen(&mut self) -> io::Result<&mut Self> {
        queue!(self.out, terminal::EnterAlternateScreen).map(|()| self)
    }

    fn leave_alternate_screen(&mut self) -> io::Result<&mut Self> {
        queue!(self.out, terminal::LeaveAlternateScreen).map(|()| self)
    }

    /* Cursor position */

    fn cursor_to(&mut self, pos: impl Into<Pos2<u16>>) -> io::Result<&mut Self> {
        let pos = pos.into() + self.offset;
        queue!(self.out, cursor::MoveTo(pos.x, pos.y)).map(|()| self)
    }

    fn cursor_to_column(&mut self, column: u16) -> io::Result<&mut Self> {
        queue!(self.out, cursor::MoveToColumn(column + self.offset.x)).map(|()| self)
    }

    fn cursor_to_row(&mut self, row: u16) -> io::Result<&mut Self> {
        queue!(self.out, cursor::MoveToRow(row + self.offset.y)).map(|()| self)
    }

    /* Cursor move */

    fn cursor_left(&mut self, distance: u16) -> io::Result<&mut Self> {
        queue!(self.out, cursor::MoveLeft(distance)).map(|()| self)
    }

    fn cursor_right(&mut self, distance: u16) -> io::Result<&mut Self> {
        queue!(self.out, cursor::MoveRight(distance)).map(|()| self)
    }

    fn cursor_up(&mut self, distance: u16) -> io::Result<&mut Self> {
        queue!(self.out, cursor::MoveUp(distance)).map(|()| self)
    }

    fn cursor_down(&mut self, distance: u16) -> io::Result<&mut Self> {
        queue!(self.out, cursor::MoveDown(distance)).map(|()| self)
    }

    /* Print */

    fn write_char(&mut self, ch: char) -> io::Result<&mut Self> {
        self.out.write_all(ch.encode_utf8(&mut [0; 4]).as_bytes())?;
        Ok(self)
    }

    fn print(&mut self, arg: impl std::fmt::Display) -> io::Result<&mut Self> {
        queue!(self.out, style::Print(arg)).map(|()| self)
    }

    /* Color */

    fn set_foreground_color(&mut self, color: Color) -> io::Result<&mut Self> {
        queue!(self.out, style::SetForegroundColor(color)).map(|()| self)
    }

    fn set_background_color(&mut self, color: Color) -> io::Result<&mut Self> {
        queue!(self.out, style::SetBackgroundColor(color)).map(|()| self)
    }

    fn add_widget<W: Widget<Self>>(
        &mut self,
        widget: &mut W,
        rect: Rect2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult> {
        let old_offset = self.offset;
        self.offset = rect.pos().to_vec();
        let overdrawn = overdrawn.map(|overdrawn| overdrawn.translate_sub(self.offset));
        let result = widget.render(self, rect.size(), overdrawn);
        self.offset = old_offset;
        result
    }

    fn flush(&mut self) -> io::Result<()> {
        self.out.flush()
    }
}
