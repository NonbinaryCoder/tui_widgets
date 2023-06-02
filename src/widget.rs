use std::ops::Add;

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
    fn desired_size(&self) -> Size2<Size<u16>> {
        Size2::splat(Size::Any)
    }
}

impl Widget for Box<dyn Widget> {
    fn render(&mut self, terminal: &mut TerminalWindow) {
        (**self).render(terminal)
    }

    fn desired_size(&self) -> Size2<Size<u16>> {
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
pub enum Size<T> {
    Exactly(T),
    AtLeast(T),
    Any,
}

impl<T> Size<T> {
    pub fn intersection(self, other: Self) -> Option<Self>
    where
        T: Ord,
    {
        match (self, other) {
            (Size::Any, size) | (size, Size::Any) => Some(size),
            (Size::Exactly(a), Size::Exactly(b)) => (a == b).then_some(Size::Exactly(a)),
            (Size::AtLeast(a), Size::AtLeast(b)) => Some(Size::AtLeast(a.max(b))),
            (Size::Exactly(size), Size::AtLeast(min))
            | (Size::AtLeast(min), Size::Exactly(size)) => {
                (size >= min).then_some(Size::Exactly(size))
            }
        }
    }
}

impl<T: Add<Output = T>> Add for Size<T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Size::Exactly(a), Size::Exactly(b)) => Size::Exactly(a + b),
            (Size::Exactly(a), Size::AtLeast(b)) | (Size::AtLeast(b), Size::Exactly(a)) => {
                Size::AtLeast(a + b)
            }
            (Size::Exactly(size) | Size::AtLeast(size), Size::Any)
            | (Size::Any, Size::Exactly(size) | Size::AtLeast(size)) => Size::AtLeast(size),
            (Size::AtLeast(a), Size::AtLeast(b)) => Size::AtLeast(a + b),
            (Size::Any, Size::Any) => Size::Any,
        }
    }
}
