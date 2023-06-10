use crate::{
    math::{Box2, Size2},
    terminal::Window,
};

use super::{collection::AreaFillingWidgetCollection, AreaFillingWidget};

/// A vertical split, sharing horizontal space between the left and right widgets.
#[non_exhaustive]
pub struct VSplit<L, R> {
    pub left: L,
    pub right: R,
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

/// A horizontal split, sharing vertical space between the top and bottom widgets.
#[non_exhaustive]
pub struct HSplit<T, B> {
    pub top: T,
    pub bottom: B,
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

/// Vertical splits, sharing horizontal space evenly between widgets.
pub struct VSplitN<C: AreaFillingWidgetCollection>(C);

impl<C: AreaFillingWidgetCollection> AreaFillingWidget for VSplitN<C> {
    fn render(&mut self, mut term: Window) {
        let Size2 { width, height } = term.size();
        let widget_count = self.0.len();
        if widget_count > 0 && width as usize >= widget_count {
            let widget_width = width / widget_count as u16;
            let num_larger = (width - widget_width * widget_count as u16) as usize;
            let mut left_edge = 0;
            for index in 0..widget_count {
                let widget_width = widget_width + (num_larger > index) as u16;
                let right_edge = left_edge + widget_width - 1;
                self.0.render(
                    index,
                    term.subwindow(Box2::new([left_edge, 0], [right_edge, height - 1])),
                );
                left_edge = right_edge + 1;
            }
        }
    }
}

/// Horizontal splits, sharing vertical space evenly between widgets.
pub struct HSplitN<C: AreaFillingWidgetCollection>(C);

impl<C: AreaFillingWidgetCollection> AreaFillingWidget for HSplitN<C> {
    fn render(&mut self, mut term: Window) {
        let Size2 { width, height } = term.size();
        let widget_count = self.0.len();
        if widget_count > 0 && height as usize >= widget_count {
            let widget_height = height / widget_count as u16;
            let num_larger = (height - widget_height * widget_count as u16) as usize;
            let mut top_edge = 0;
            for index in 0..widget_count {
                let widget_height = widget_height + (num_larger > index) as u16;
                let bottom_edge = top_edge + widget_height - 1;
                self.0.render(
                    index,
                    term.subwindow(Box2::new([0, top_edge], [width - 1, bottom_edge])),
                );
                top_edge = bottom_edge + 1;
            }
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

    #[test]
    fn vsplit_n_renders_array_8() {
        Terminal::test_widget(
            [8, 1],
            Overdrawn::All,
            VSplitN([
                FillArea::with_char('a'),
                FillArea::with_char('b'),
                FillArea::with_char('c'),
                FillArea::with_char('d'),
            ]),
            |term| term.assert_chars_equal(["aabbccdd"]),
        );
    }

    #[test]
    fn vsplit_n_renders_array_mut_8() {
        Terminal::test_widget(
            [8, 1],
            Overdrawn::All,
            VSplitN(&mut [
                FillArea::with_char('a'),
                FillArea::with_char('b'),
                FillArea::with_char('c'),
                FillArea::with_char('d'),
            ]),
            |term| term.assert_chars_equal(["aabbccdd"]),
        );
    }

    #[test]
    fn vsplit_n_renders_tuple_8() {
        Terminal::test_widget(
            [8, 1],
            Overdrawn::All,
            VSplitN((FillArea::with_char('a'), (), FillArea::with_char('c'), ())),
            |term| term.assert_chars_equal(["aa  cc  "]),
        );
    }

    #[test]
    fn vsplit_n_renders_array_6() {
        Terminal::test_widget(
            [6, 1],
            Overdrawn::All,
            VSplitN([
                FillArea::with_char('a'),
                FillArea::with_char('b'),
                FillArea::with_char('c'),
                FillArea::with_char('d'),
            ]),
            |term| term.assert_chars_equal(["aabbcd"]),
        );
    }

    #[test]
    fn stress_vsplit_n() {
        Terminal::stress_area_filling_widget(VSplitN([
            FillArea::with_char('a'),
            FillArea::with_char('b'),
            FillArea::with_char('c'),
            FillArea::with_char('d'),
        ]));
    }

    #[test]
    fn stress_vsplit_n_equals_0() {
        Terminal::stress_area_filling_widget(VSplitN::<[(); 0]>([]));
    }

    #[test]
    fn hsplit_n_renders_array_8() {
        Terminal::test_widget(
            [1, 8],
            Overdrawn::All,
            HSplitN([
                FillArea::with_char('a'),
                FillArea::with_char('b'),
                FillArea::with_char('c'),
                FillArea::with_char('d'),
            ]),
            |term| term.assert_chars_equal(["a", "a", "b", "b", "c", "c", "d", "d"]),
        )
    }

    #[test]
    fn hsplit_n_renders_array_6() {
        Terminal::test_widget(
            [1, 6],
            Overdrawn::All,
            HSplitN([
                FillArea::with_char('a'),
                FillArea::with_char('b'),
                FillArea::with_char('c'),
                FillArea::with_char('d'),
            ]),
            |term| term.assert_chars_equal(["a", "a", "b", "b", "c", "d"]),
        )
    }

    #[test]
    fn stress_hsplit_n() {
        Terminal::stress_area_filling_widget(HSplitN([
            FillArea::with_char('a'),
            FillArea::with_char('b'),
            FillArea::with_char('c'),
            FillArea::with_char('d'),
        ]));
    }

    #[test]
    fn stress_hsplit_n_equals_0() {
        Terminal::stress_area_filling_widget(HSplitN::<[(); 0]>([]));
    }
}
