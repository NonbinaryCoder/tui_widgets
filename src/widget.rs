use std::io;

use crate::{
    math::{Rect2, Size2},
    writer::TerminalWriter,
};

pub mod decoration;
pub mod style;

/// A widget in a tui app, such as text.
pub trait Widget<W: TerminalWriter> {
    /// Renders this widget into the provided [`TerminalWriter`].
    ///
    /// `size` should contain `overdrawn`.  If it doesn't, widgets may not
    /// render correctly.
    ///
    /// # Implementation Notes
    ///
    /// The writer keeps track of the offset of the widget, so all widgets can
    /// assume they are at (0, 0).
    ///
    /// Nothing should be written outside of `size`.
    ///
    /// If `overdrawn` is set, the area it covers has been changed and needs to
    /// be rewritten.  The widget can assume anything outside of the overdrawn
    /// area hasn't changed.  If `overdrawn` is not set, nothing changed.
    ///
    /// # Errors
    ///
    /// This function will return an error if writing fails.
    fn render(
        &mut self,
        writer: &mut W,
        size: Size2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult>;

    /// What size this widget wants to be.
    ///
    /// Note that widgets can be rendered with a size that is smaller or larger
    /// than requested.
    fn desired_size(&self) -> Size2<Range<u16>> {
        Size2::splat(Range::Any)
    }
}

impl<W: TerminalWriter> Widget<W> for Box<dyn Widget<W>> {
    fn render(
        &mut self,
        writer: &mut W,
        size: Size2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult> {
        (**self).render(writer, size, overdrawn)
    }

    fn desired_size(&self) -> Size2<Range<u16>> {
        (**self).desired_size()
    }
}

impl<W: TerminalWriter> Widget<W> for () {
    fn render(
        &mut self,
        _writer: &mut W,
        _size: Size2<u16>,
        _overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult> {
        Ok(RenderResult::NOTHING_DRAWN)
    }
}

impl<F, W> Widget<W> for F
where
    F: FnMut(&mut W, Size2<u16>, Option<Rect2<u16>>) -> io::Result<RenderResult>,
    W: TerminalWriter,
{
    fn render(
        &mut self,
        writer: &mut W,
        size: Size2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult> {
        self(writer, size, overdrawn)
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
