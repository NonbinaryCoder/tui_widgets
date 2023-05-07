use std::{
    io::{self, StdoutLock, Write},
    sync::atomic::{AtomicBool, Ordering},
};

use crossterm::{queue, terminal};

use crate::{
    math::{Rect2, Size2, Vec2},
    widget::{RenderResult, Widget},
};

use super::TerminalWriter;

/// Set to true while a `StdoutWriter` exists; set to false when it is dropped
static WRITER_RUNNING: AtomicBool = AtomicBool::new(false);

/// Terminal writer outputting to stdout.  Only one may exist at a time
#[derive(Debug)]
pub struct StdoutWriter {
    out: StdoutLock<'static>,

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
            out: io::stdout().lock(),
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
    fn term_size(&self) -> io::Result<Size2<u16>> {
        terminal::size().map(Into::into)
    }

    fn enter_alternate_screen(&mut self) -> io::Result<&mut Self> {
        queue!(self.out, terminal::EnterAlternateScreen).map(|()| self)
    }

    fn leave_alternate_screen(&mut self) -> io::Result<&mut Self> {
        queue!(self.out, terminal::LeaveAlternateScreen).map(|()| self)
    }

    fn add_widget<W: Widget<Self>>(
        &mut self,
        widget: &mut W,
        rect: Rect2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult> {
        let old_offset = self.offset;
        self.offset = rect.pos().to_vec();
        let overdrawn = overdrawn.map(|overdrawn| overdrawn.translate(self.offset));
        let result = widget.render(self, rect.size(), overdrawn);
        self.offset = old_offset;
        result
    }

    fn flush(&mut self) -> io::Result<()> {
        self.out.flush()
    }
}
