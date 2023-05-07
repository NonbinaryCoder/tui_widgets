use std::io;

use math::{Rect2, Size2};
use widget::Widget;
use writer::{StdoutWriter, TerminalWriter};

pub mod math;
pub mod widget;
pub mod writer;

/// A tui app.  Will try to restore terminal settings to defaults when dropped,
/// but it is recommended to call [`exit()`](Self::exit()) to catch any errors.
///
/// Only one app at a time may use the default writer ([`StdoutWriter`]).
#[derive(Debug)]
#[must_use = "this `App` should be exited with the `exit` function to catch errors"]
pub struct App<W: TerminalWriter = StdoutWriter, R: Widget<W> = Box<dyn Widget<W>>> {
    /// This field will always be `Some` except for when this is getting
    /// dropped.
    writer: Option<W>,
    root: R,
    term_size: Size2<u16>,
    /// Set to true if the entire terminal has been overdrawn (for example as a
    /// result of it being resized).
    overdrawn: bool,
    alternate_screen: bool,
}

impl<R: Widget<StdoutWriter>> App<StdoutWriter, R> {
    /// Creates a new [`App`] using the default [`TerminalWriter`]
    /// ([`StdoutWriter`]).
    ///
    /// # Panics
    ///
    /// Panics if an app is already running with [`StdoutWriter`]
    pub fn new(root: R) -> io::Result<Self> {
        Self::with_writer(StdoutWriter::new(), root)
    }
}

macro_rules! writer {
    ($app:ident) => {
        $app.writer.as_mut().unwrap()
    };
}

impl<W: TerminalWriter, R: Widget<W>> App<W, R> {
    /// Starts the app, using the provided [`TerminalWriter`].
    pub fn with_writer(writer: W, root: R) -> io::Result<Self> {
        Ok(Self {
            term_size: writer.term_size()?,
            writer: Some(writer),
            root,
            overdrawn: true,
            alternate_screen: false,
        })
    }

    /// Renders this into its [`TerminalWriter`]
    pub fn render(&mut self) -> io::Result<()> {
        let writer = writer!(self);
        if !self.alternate_screen {
            writer.enter_alternate_screen()?;
            self.alternate_screen = true;
        }
        writer.add_widget(
            &mut self.root,
            Rect2::new([0, 0], self.term_size),
            self.overdrawn.then_some(Rect2::new([0, 0], self.term_size)),
        )?;
        writer.flush()
    }

    /// Exits the app.
    ///
    /// # Errors
    ///
    /// This function will return an error if writing to its writer fails.
    pub fn exit(mut self) -> io::Result<W> {
        self.exit_internal()?;
        Ok(self.writer.take().unwrap())
    }

    fn exit_internal(&mut self) -> io::Result<()> {
        let writer = writer!(self);
        if self.alternate_screen {
            writer.leave_alternate_screen()?;
            self.alternate_screen = false;
        }
        writer.flush()
    }
}

impl<W: TerminalWriter, R: Widget<W>> Drop for App<W, R> {
    fn drop(&mut self) {
        if self.writer.is_some() {
            let _ = self.exit_internal();
        }
    }
}

#[cfg(test)]
mod tests {
    use serial_test::serial;

    use crate::writer::TestWriter;

    use super::*;

    #[test]
    #[serial]
    #[should_panic]
    fn cannot_double_start() {
        let _app = App::new(());
        let _ = App::new(());
    }

    #[test]
    #[serial]
    fn can_restart() -> io::Result<()> {
        let app = App::new(())?;
        app.exit()?;
        let _ = App::new(())?;
        Ok(())
    }

    #[test]
    #[serial]
    fn can_restart_after_drop() -> io::Result<()> {
        let app = App::new(())?;
        drop(app);
        let _ = App::new(())?;
        Ok(())
    }

    #[test]
    fn restores_terminal_settings() -> io::Result<()> {
        let mut app = App::with_writer(TestWriter::new([4, 4]), ())?;
        app.render()?;
        app.render()?;
        let writer = app.exit()?;
        assert!(writer.is_terminal_reset());
        Ok(())
    }
}
