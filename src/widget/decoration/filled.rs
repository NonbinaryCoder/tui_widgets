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
    fn render(&mut self, terminal: &mut TerminalWindow) {
        if let Some(overdrawn) = terminal.overdrawn() {
            terminal.fill_area(overdrawn, self.0);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::terminal::{Overdrawn, Terminal};

    use super::*;

    #[test]
    fn renders_sw() {
        Terminal::test_widget([8, 6], Overdrawn::All, |term| {
            term.fill_widget(&mut Filled::with_char('c'));

            term.assert_chars_equal([
                "cccccccc", "cccccccc", "cccccccc", "cccccccc", "cccccccc", "cccccccc",
            ]);
        })
    }

    #[test]
    fn renders_dw() {
        Terminal::test_widget([8, 6], Overdrawn::All, |term| {
            term.fill_widget(&mut Filled::with_char('✨'));

            term.assert_chars_equal([
                "✨✨✨✨",
                "✨✨✨✨",
                "✨✨✨✨",
                "✨✨✨✨",
                "✨✨✨✨",
                "✨✨✨✨",
            ]);
        });
    }

    #[test]
    fn renders_dw_odd_width() {
        Terminal::test_widget([7, 6], Overdrawn::All, |term| {
            term.fill_widget(&mut Filled::with_char('✨'));

            term.assert_chars_equal([
                "✨✨✨ ",
                "✨✨✨ ",
                "✨✨✨ ",
                "✨✨✨ ",
                "✨✨✨ ",
                "✨✨✨ ",
            ]);
        });
    }
}
