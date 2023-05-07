use std::io;

use crate::{
    math::{Rect2, Size2, Vec2},
    widget::{RenderResult, Widget},
};

use super::TerminalWriter;

/// A [`TerminalWriter`] that can be used to test that a widget works as
/// intended.  Not intended to be used outside of tests.
#[derive(Debug)]
pub struct TestWriter {
    size: Size2<u16>,
    assertions: bool,
    alternate_screen: bool,

    offset: Vec2<u16>,
}

impl TestWriter {
    pub fn new(size: impl Into<Size2<u16>>) -> Self {
        let size = size.into();
        Self {
            size,
            assertions: true,
            alternate_screen: false,
            offset: Vec2::splat(0),
        }
    }

    pub fn is_terminal_reset(&self) -> bool {
        !self.alternate_screen
    }
}

impl TerminalWriter for TestWriter {
    fn term_size(&self) -> io::Result<crate::math::Size2<u16>> {
        Ok(self.size)
    }

    fn enter_alternate_screen(&mut self) -> io::Result<&mut Self> {
        if self.assertions && self.alternate_screen {
            panic!("Attempt to enter alternate screen when already in alternate screen");
        }
        self.alternate_screen = true;
        Ok(self)
    }

    fn leave_alternate_screen(&mut self) -> io::Result<&mut Self> {
        if self.assertions && !self.alternate_screen {
            panic!("Attempt to leave alternate screen when not in alternate screen");
        }
        self.alternate_screen = false;
        Ok(self)
    }

    fn add_widget<W: Widget<Self>>(
        &mut self,
        widget: &mut W,
        rect: Rect2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult> {
        if self.assertions {
            let warea = Rect2::new(self.offset, self.size);
            assert!(
                warea.contains_rect(rect),
                "Avalible terminal space ({warea:?}) should contain widget rect ({rect:?})"
            );
            if let Some(overdrawn) = overdrawn {
                assert!(
                    overdrawn.contains_rect(rect),
                    "Widget rect ({rect:?}) should contain overdrawn area ({overdrawn:?})"
                );
            }
        }

        let old_offset = self.offset;
        self.offset = rect.pos().to_vec();
        let overdrawn = overdrawn.map(|overdrawn| overdrawn.translate(self.offset));
        let result = widget.render(self, rect.size(), overdrawn);
        self.offset = old_offset;
        result
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::App;

    use super::*;

    #[test]
    fn offset_set() -> io::Result<()> {
        let mut app = App::with_writer(
            TestWriter::new([16, 16]),
            |writer: &mut TestWriter, _, _| {
                let mut widget = |writer: &mut TestWriter, _, _| {
                    assert_eq!(writer.offset, Vec2::new(2, 3));
                    Ok(RenderResult::NOTHING_DRAWN)
                };
                writer.add_widget(&mut widget, Rect2::new([2, 3], [1, 4]), None)
            },
        )?;
        app.render()?;
        let writer = app.exit()?;
        assert_eq!(writer.offset, Vec2::new(0, 0));
        Ok(())
    }
}
