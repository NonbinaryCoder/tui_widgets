use crate::terminal::Window;

use super::AreaFillingWidget;

/// A vertical split, sharing space evenly between the left and right widgets.
#[non_exhaustive]
pub struct VSplit<L, R> {
    left: L,
    right: R,
}

impl<L, R> VSplit<L, R> {
    pub fn new(left: L, right: R) -> Self {
        Self { left, right }
    }
}

impl<L: AreaFillingWidget, R: AreaFillingWidget> AreaFillingWidget for VSplit<L, R> {
    fn render(&mut self, term: Window) {
        let width = term.size().width;
        if width >= 2 {
            term.vsplit(width / 2)
                .add_widgets(&mut self.left, &mut self.right);
        }
    }
}

/// A horizontal split, sharing space evenly between the top and bottom widgets.
#[non_exhaustive]
pub struct HSplit<T, B> {
    top: T,
    bottom: B,
}

impl<T, B> HSplit<T, B> {
    pub fn new(top: T, bottom: B) -> Self {
        Self { top, bottom }
    }
}

impl<T: AreaFillingWidget, B: AreaFillingWidget> AreaFillingWidget for HSplit<T, B> {
    fn render(&mut self, term: Window) {
        let height = term.size().height;
        if height >= 2 {
            term.hsplit(height / 2)
                .add_widgets(&mut self.top, &mut self.bottom);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        terminal::{Overdrawn, Terminal},
        widget::decoration::FillArea,
    };

    use super::*;

    #[test]
    fn vsplit_renders() {
        Terminal::test_widget(
            [8, 6],
            Overdrawn::All,
            VSplit::new(FillArea::with_char('l'), FillArea::with_char('r')),
            |term| {
                term.assert_chars_equal([
                    "llllrrrr", "llllrrrr", "llllrrrr", "llllrrrr", "llllrrrr", "llllrrrr",
                ])
            },
        );
    }

    #[test]
    fn stress_vsplit() {
        Terminal::stress_area_filling_widget(VSplit::new(
            FillArea::with_char('l'),
            FillArea::with_char('r'),
        ));
    }

    #[test]
    fn hsplit_renders() {
        Terminal::test_widget(
            [6, 8],
            Overdrawn::All,
            HSplit::new(FillArea::with_char('t'), FillArea::with_char('b')),
            |term| {
                term.assert_chars_equal([
                    "tttttt", "tttttt", "tttttt", "tttttt", "bbbbbb", "bbbbbb", "bbbbbb", "bbbbbb",
                ])
            },
        )
    }

    #[test]
    fn stress_hsplit() {
        Terminal::stress_area_filling_widget(HSplit::new(
            FillArea::with_char('t'),
            FillArea::with_char('b'),
        ))
    }
}
