use crate::terminal::Window;

pub mod collection;
pub mod decoration;
pub mod layout;
pub mod style;

/// A widget in a tui app that fills an entire [`Window`].
pub trait AreaFillingWidget {
    /// Renders this widget into the provided [`Window`].
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

impl<F: FnMut(Window)> AreaFillingWidget for F {
    fn render(&mut self, terminal: Window) {
        self(terminal)
    }
}
