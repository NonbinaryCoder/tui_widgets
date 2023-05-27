use crate::{math::Size2, terminal::TerminalWindow};

pub mod decoration;
pub mod style;

/// A widget in a tui app, for example text.
pub trait Widget {
    /// Renders this widget into the provided [`TerminalWriter`].
    ///
    /// `overdrawn` should be contained by the terminal window.  If it doesn't,
    /// widgets may not render correctly.
    ///
    /// # Implementation Notes
    ///
    /// The window keeps track of the offset of the widget, so all widgets can
    /// assume they are at (0, 0).
    ///
    /// Nothing should be written outside of `terminal.size()`.
    ///
    /// The terminal keeps track of what area of it has been overwritten
    fn render(&mut self, terminal: &mut TerminalWindow);

    /// What size this widget wants to be.
    ///
    /// Note that widgets may be rendered with a size that is smaller or larger
    /// than requested.
    fn desired_size(&self) -> Size2<Range<u16>> {
        Size2::splat(Range::Any)
    }
}

impl Widget for Box<dyn Widget> {
    fn render(&mut self, terminal: &mut TerminalWindow) {
        (**self).render(terminal)
    }

    fn desired_size(&self) -> Size2<Range<u16>> {
        (**self).desired_size()
    }
}

impl Widget for () {
    fn render(&mut self, _terminal: &mut TerminalWindow) {}
}

impl<F> Widget for F
where
    F: FnMut(&mut TerminalWindow),
{
    fn render(&mut self, terminal: &mut TerminalWindow) {
        self(terminal)
    }
}

/// A size range.  Values are inclusive.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Range<T> {
    Exactly(T),
    /// A size of `self.0` or higher.
    AtLeast(T),
    /// A size of `self.0` or lower.
    AtMost(T),
    /// A size of `min`, `max`, or somewhere in between
    Range {
        min: T,
        max: T,
    },
    Any,
}
