use crate::{
    math::{Rect2, Size2},
    terminal::TerminalWindow,
};

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
    /// If `overdrawn` is set, the area it covers has been changed and needs to
    /// be rewritten.  The widget can assume anything outside of the overdrawn
    /// area hasn't changed.  If `overdrawn` is not set, nothing changed.
    fn render(&mut self, terminal: TerminalWindow, overdrawn: Option<Rect2<u16>>) -> RenderResult;

    /// What size this widget wants to be.
    ///
    /// Note that widgets can be rendered with a size that is smaller or larger
    /// than requested.
    fn desired_size(&self) -> Size2<Range<u16>> {
        Size2::splat(Range::Any)
    }
}

impl Widget for Box<dyn Widget> {
    fn render(&mut self, terminal: TerminalWindow, overdrawn: Option<Rect2<u16>>) -> RenderResult {
        (**self).render(terminal, overdrawn)
    }

    fn desired_size(&self) -> Size2<Range<u16>> {
        (**self).desired_size()
    }
}

impl Widget for () {
    fn render(
        &mut self,
        _terminal: TerminalWindow,
        _overdrawn: Option<Rect2<u16>>,
    ) -> RenderResult {
        RenderResult::NOTHING_DRAWN
    }
}

impl<F> Widget for F
where
    F: FnMut(TerminalWindow, Option<Rect2<u16>>) -> RenderResult,
{
    fn render(&mut self, terminal: TerminalWindow, overdrawn: Option<Rect2<u16>>) -> RenderResult {
        self(terminal, overdrawn)
    }
}

pub struct RenderResult {
    overdrawn: Option<Rect2<u16>>,
}

impl RenderResult {
    pub const NOTHING_DRAWN: Self = Self { overdrawn: None };
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
