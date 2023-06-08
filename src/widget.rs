use crate::terminal::Window;

pub mod collection;
pub mod decoration;
pub mod layout;
pub mod style;

/// A widget in a tui app that fills the entire area it is given.
pub trait AreaFillingWidget {
    /// Renders this widget into the provided [`TerminalWriter`].
    ///
    /// # Implementation Notes
    ///
    /// The window keeps track of the offset of the widget, so all widgets can
    /// assume they are at (0, 0).
    ///
    /// Nothing should be written outside of `terminal.size()`.
    ///
    /// The terminal keeps track of what area of it has been overwritten
    fn render(&mut self, terminal: Window);
}

impl AreaFillingWidget for Box<dyn AreaFillingWidget> {
    fn render(&mut self, terminal: Window) {
        (**self).render(terminal)
    }
}

impl AreaFillingWidget for () {
    fn render(&mut self, _terminal: Window) {}
}

impl<F> AreaFillingWidget for F
where
    F: FnMut(Window),
{
    fn render(&mut self, terminal: Window) {
        self(terminal)
    }
}
