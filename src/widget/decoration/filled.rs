use crate::{
    terminal::{Cell, TerminalWindow},
    widget::{style::Color, Widget},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Filled(Cell);

impl Filled {
    pub fn with_char(ch: char) -> Self {
        Self(ch.into())
    }

    pub fn with_color(color: Color) -> Self {
        Self(Cell::new(' ').bg(color))
    }
}

impl Widget for Filled {
    fn render(&mut self, mut terminal: TerminalWindow) {
        if let Some(overdrawn) = terminal.overdrawn() {
            terminal.fill_area(overdrawn, self.0)
        }
    }
}
