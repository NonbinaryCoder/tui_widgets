use crate::{
    terminal::{Cell, Window},
    widget::{style::Color, AreaFillingWidget},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FillArea(Cell);

impl FillArea {
    pub fn with_char(ch: char) -> Self {
        Self(ch.into())
    }

    pub fn with_color(color: Color) -> Self {
        Self(Cell::new(' ').bg(color))
    }
}

impl AreaFillingWidget for FillArea {
    fn render(&mut self, mut term: Window) {
        if let Some(overdrawn) = term.overdrawn() {
            term.fill_area(overdrawn, self.0);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::terminal::{Overdrawn, Terminal};

    use super::*;

    #[test]
    fn renders_sw() {
        Terminal::test_widget([8, 6], Overdrawn::All, FillArea::with_char('c'), |term| {
            term.assert_chars_equal([
                "cccccccc", "cccccccc", "cccccccc", "cccccccc", "cccccccc", "cccccccc",
            ]);
        })
    }

    #[test]
    fn renders_dw() {
        Terminal::test_widget([8, 6], Overdrawn::All, FillArea::with_char('✨'), |term| {
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
        Terminal::test_widget([7, 6], Overdrawn::All, FillArea::with_char('✨'), |term| {
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

    #[test]
    fn stress_sw() {
        Terminal::stress_area_filling_widget(FillArea::with_char('c'));
    }

    #[test]
    fn stress_dw() {
        Terminal::stress_area_filling_widget(FillArea::with_char('✨'));
    }
}
