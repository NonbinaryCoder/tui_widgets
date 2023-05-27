use std::{
    fmt::Display,
    iter::{self, FusedIterator},
};

use unicode_width::UnicodeWidthChar;

use crate::{
    math::{Box2, Pos2, Size2, Vec2},
    widget::{
        style::{Color, Colors, Formatting},
        Widget,
    },
};

#[derive(Debug)]
pub struct Terminal {
    size: Size2<u16>,
    cells: Box<[Cell]>,
}

impl Terminal {
    pub fn new(size: impl Into<Size2<u16>>) -> Self {
        let size = size.into();
        Self {
            size,
            cells: vec![Cell::default(); size.map_into::<usize>().area()].into_boxed_slice(),
        }
    }

    pub fn edit(&mut self) -> TerminalWindow<'_> {
        TerminalWindow {
            area: Box2::new([0, 0], self.size.to_vec().map(|v| v.saturating_sub(1))),
            overdrawn: None,
            term: self,
        }
    }

    fn cell_index(&self, pos: Pos2<u16>) -> usize {
        let pos = pos.map_into::<usize>();
        pos.x + pos.y * self.size.width as usize
    }

    pub fn rows(&self) -> Rows<'_> {
        Rows {
            width: self.size.width as usize,
            cells: &self.cells,
        }
    }

    fn assert_equal_inner<R, E, I>(
        &self,
        template: &[R],
        mut row_elements: impl FnMut(&R) -> I,
        mut cell_to_template: impl FnMut(Cell) -> E,
    ) where
        I: Iterator<Item = E>,
        E: PartialEq + Display,
    {
        let height = self.size.height;
        let template_height = template.len();
        assert_eq!(
            height as usize, template_height,
            "Height of terminal ({height}) should match height of template ({template_height})"
        );
        let mut mismatch = None;
        for (y, (row, template_row)) in iter::zip(self.rows(), template).enumerate() {
            let mut templates_iter = row_elements(template_row);
            for (x, (cell, template)) in iter::zip(row.clone(), templates_iter.by_ref()).enumerate()
            {
                if mismatch.is_none() && !(cell_to_template(cell) == template) {
                    mismatch = Some(Pos2::new(x, y));
                }
            }
        }
        if let Some(Pos2 { x, y }) = mismatch {
            eprintln!("Template:");
            for row in template {
                for template in row_elements(row) {
                    eprint!("{}", template);
                }
                eprintln!();
            }
            eprintln!();
            eprintln!("Actual:");
            for row in self.rows() {
                for cell in row {
                    eprint!("{}", cell_to_template(cell));
                }
                eprintln!();
            }
            eprintln!();
            panic!("Mismatch at column {x}, row {y}");
        }
    }

    /// Asserts that all the chars in the cells this are the same as the chars
    /// in the template
    pub fn assert_chars_equal<'a>(&self, template: impl AsRef<[&'a str]>) {
        self.assert_equal_inner(template.as_ref(), |row| row.chars(), |cell| cell.ch);
    }
}

/// An iterator over the rows of a terminal.  Rows are returned as iterators
/// over their elements.
#[derive(Debug, Clone)]
pub struct Rows<'a> {
    width: usize,
    cells: &'a [Cell],
}

impl<'a> Iterator for Rows<'a> {
    type Item = Row<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        (self.cells.len() >= self.width).then(|| {
            let cells;
            (cells, self.cells) = self.cells.split_at(self.width);
            Row { cells }
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.len()
    }

    fn last(mut self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        self.next_back()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let discard_width = self.width * n;
        if self.cells.len() >= discard_width {
            (_, self.cells) = self.cells.split_at(discard_width);
            self.next()
        } else {
            self.cells = &[];
            None
        }
    }
}

impl<'a> ExactSizeIterator for Rows<'a> {
    fn len(&self) -> usize {
        self.cells.len() / self.width
    }
}

impl<'a> DoubleEndedIterator for Rows<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        (self.cells.len() >= self.width).then(|| {
            let cells;
            (self.cells, cells) = self.cells.split_at(self.cells.len() - self.width);
            Row { cells }
        })
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        let discard_width = self.width * n;
        if self.cells.len() >= discard_width {
            (self.cells, _) = self.cells.split_at(self.cells.len() - discard_width);
            self.next_back()
        } else {
            self.cells = &[];
            None
        }
    }
}

impl<'a> FusedIterator for Rows<'a> {}

/// An iterator over the characters in a row.  Double width characters are only
/// returned once.
#[derive(Debug, Clone)]
pub struct Row<'a> {
    cells: &'a [Cell],
}

impl<'a> Iterator for Row<'a> {
    type Item = Cell;

    fn next(&mut self) -> Option<Self::Item> {
        if let [cell, rest @ ..] = self.cells {
            match UnicodeWidthChar::width(cell.ch) {
                Some(1) => {
                    self.cells = rest;
                    Some(*cell)
                }
                Some(2) => {
                    if let [_, rest @ ..] = rest {
                        self.cells = rest;
                        Some(*cell)
                    } else {
                        self.cells = &[];
                        Some(*cell)
                    }
                }
                width @ _ => unreachable!(
                    "Chars in cells should have a width of 1 or 2; char '{}' has a width of \
                    {width:?}",
                    cell.ch
                ),
            }
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.cells.len();
        (len / 2 + len % 2, Some(len))
    }

    fn last(mut self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for Row<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.cells {
            [rest @ .., maybe_dw, cell] => match UnicodeWidthChar::width(maybe_dw.ch) {
                Some(1) => {
                    let [cells @ .., _] = self.cells else { unreachable!() };
                    self.cells = cells;
                    Some(*cell)
                }
                Some(2) => {
                    self.cells = rest;
                    Some(*maybe_dw)
                }
                width @ _ => unreachable!(
                    "Chars in cells should have a width of 1 or 2; char '{}' has a width of \
                    {width:?}",
                    cell.ch
                ),
            },
            [rest @ .., cell] => {
                self.cells = rest;
                Some(*cell)
            }
            _ => None,
        }
    }
}

impl<'a> FusedIterator for Row<'a> {}

#[derive(Debug)]
pub struct TerminalWindow<'a> {
    area: Box2<u16>,
    overdrawn: Option<Box2<u16>>,
    term: &'a mut Terminal,
}

impl<'a> TerminalWindow<'a> {
    fn offset_pos(&self, pos: impl Into<Pos2<u16>>) -> Pos2<u16> {
        let pos = pos.into();
        let size = self.size();
        debug_assert!(
            size.contains_pos(pos),
            "Size of terminal window ({size}) should contain pos ({pos})",
        );
        pos + self.area.min
    }

    fn offset_area(&self, area: Box2<u16>) -> Box2<u16> {
        let size = self.size();
        debug_assert!(
            size.contains_box(area),
            "Size of terminal window ({size}) should contain area ({area:?})",
        );
        area.translate(self.area.min)
    }

    fn cell_index(&self, pos: Pos2<u16>) -> usize {
        self.term.cell_index(pos)
    }

    pub fn size(&self) -> Size2<u16> {
        (self.area.max.to_vec() + Vec2::splat(1) - self.area.min.to_vec()).to_size()
    }

    pub fn overdrawn(&self) -> Option<Box2<u16>> {
        self.overdrawn
    }

    fn mark_cell_overdrawn(&mut self, cell: Pos2<u16>) {
        debug_assert!(self.area.contains_pos(cell));
        self.overdrawn = Some(self.overdrawn.map_or(cell.into(), |o| o.contain_pos(cell)));
    }

    fn mark_double_width_cell_overdrawn(&mut self, cell: Pos2<u16>) {
        self.mark_area_overdrawn(Box2::new(cell, cell + Pos2::new(1, 0)));
    }

    fn mark_area_overdrawn(&mut self, area: Box2<u16>) {
        debug_assert!(self.area.contains_box(area));
        self.overdrawn = Some(self.overdrawn.map_or(area, |o| o.contain_box(area)));
    }

    pub fn set_cell(&mut self, pos: impl Into<Pos2<u16>>, cell: impl Into<Cell>) -> &mut Self {
        let pos = self.offset_pos(pos);
        let cell = cell.into();
        let cell_index = self.cell_index(pos);
        match CharType::classify(cell.ch) {
            CharType::SingleWidth(_) => {
                self.mark_cell_overdrawn(pos);
                self.term.cells[cell_index] = cell;
            }
            CharType::DoubleWidth(_) => {
                self.mark_double_width_cell_overdrawn(pos);
                self.term.cells[cell_index] = cell;
                self.term.cells[cell_index + 1] = cell.ch(' ');
            }
            CharType::Other(_) => {}
        }
        self
    }

    /// Repeats a cell to fill a strip `width` cells long horizontally.
    pub fn fill_horizontal(
        &mut self,
        pos: impl Into<Pos2<u16>>,
        width: u16,
        cell: impl Into<Cell>,
    ) -> &mut Self {
        let pos = self.offset_pos(pos);
        let cell = cell.into();
        let cell_index = self.cell_index(pos);
        match CharType::classify(cell.ch) {
            CharType::SingleWidth(_) => {
                self.mark_area_overdrawn(Box2::new(
                    pos,
                    pos + Pos2::new(width.saturating_sub(1), 0),
                ));
                for cell_index in cell_index..(cell_index + width as usize) {
                    self.term.cells[cell_index] = cell;
                }
            }
            CharType::DoubleWidth(_) => {
                self.mark_area_overdrawn(Box2::new(
                    pos,
                    pos + Pos2::new((width - width % 2).saturating_sub(1), 0),
                ));
                for cell_index in ((cell_index + 1)..(cell_index + width as usize)).step_by(2) {
                    self.term.cells[cell_index - 1] = cell;
                    self.term.cells[cell_index] = cell;
                }
            }
            CharType::Other(_) => {}
        }
        self
    }

    /// Repeats a cell to fill a strip `height` cells tall vertically.
    pub fn fill_vertical(
        &mut self,
        pos: impl Into<Pos2<u16>>,
        height: u16,
        cell: impl Into<Cell>,
    ) -> &mut Self {
        let pos = self.offset_pos(pos);
        let cell = cell.into();
        let cell_index = self.cell_index(pos);
        let jump = self.term.size.width as usize;
        self.mark_area_overdrawn(Box2::new(pos, pos + Pos2::new(0, height.saturating_sub(1))));
        let iter = (cell_index..(cell_index + jump * height as usize)).step_by(jump);
        match CharType::classify(cell.ch) {
            CharType::SingleWidth(_) => {
                for cell_index in iter {
                    self.term.cells[cell_index] = cell;
                }
            }
            CharType::DoubleWidth(_) => {
                for cell_index in iter {
                    self.term.cells[cell_index] = cell;
                    self.term.cells[cell_index + 1] = cell.ch(' ');
                }
            }
            CharType::Other(_) => {}
        }
        self
    }

    /// Repeats a cell to fill the specified area.
    pub fn fill_area(&mut self, area: Box2<u16>, cell: impl Into<Cell>) -> &mut Self {
        let area = self.offset_area(area);
        let cell = cell.into();
        let cell_index = self.cell_index(area.min);
        let jump = self.term.size.width as usize;
        let size = (area.max.to_vec() - area.min.to_vec() + Vec2::splat(1)).to_size();
        let iter = (cell_index..(cell_index + jump * size.height as usize)).step_by(jump);
        match CharType::classify(cell.ch) {
            CharType::SingleWidth(_) => {
                self.mark_area_overdrawn(area);
                for cell_index in iter {
                    for cell_index in cell_index..(cell_index + size.width as usize) {
                        self.term.cells[cell_index] = cell;
                    }
                }
            }
            CharType::DoubleWidth(_) => {
                self.mark_area_overdrawn(Box2::new(
                    area.min,
                    [area.max.x - size.width % 2, area.max.y],
                ));
                for cell_index in iter {
                    for cell_index in
                        ((cell_index + 1)..(cell_index + size.width as usize)).step_by(2)
                    {
                        self.term.cells[cell_index - 1] = cell;
                        self.term.cells[cell_index] = cell;
                    }
                }
            }
            CharType::Other(_) => {}
        }
        self
    }

    pub fn subwindow(&mut self, area: Box2<u16>, f: impl FnOnce(&mut TerminalWindow)) {
        let area = self.offset_area(area);
        let mut subwindow = TerminalWindow {
            area,
            overdrawn: self.overdrawn.and_then(|o| o.intersection(area)),
            term: self.term,
        };
        f(&mut subwindow);
        if let Some(overdrawn) = subwindow.overdrawn {
            self.mark_area_overdrawn(overdrawn);
        }
    }

    pub fn add_widget<W: Widget>(&mut self, area: Box2<u16>, widget: &mut W) {
        self.subwindow(area, |w| widget.render(w))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Cell {
    pub ch: char,
    pub fmt: Formatting,
}

impl Default for Cell {
    fn default() -> Self {
        Self {
            ch: ' ',
            fmt: Formatting::default(),
        }
    }
}

impl From<char> for Cell {
    fn from(ch: char) -> Self {
        Self {
            ch,
            ..Self::default()
        }
    }
}

impl Cell {
    pub fn new(ch: char) -> Self {
        Self {
            ch,
            fmt: Formatting::default(),
        }
    }

    pub fn ch(self, ch: char) -> Self {
        Self { ch, ..self }
    }

    pub fn fmt(self, fmt: impl Into<Formatting>) -> Self {
        let fmt = fmt.into();
        Self { fmt, ..self }
    }

    pub fn colors(self, colors: Colors) -> Self {
        self.fmt(self.fmt.colors(colors))
    }

    pub fn fg(self, fg: Color) -> Self {
        self.colors(self.fmt.colors.fg(fg))
    }

    pub fn bg(self, bg: Color) -> Self {
        self.colors(self.fmt.colors.bg(bg))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CharType {
    SingleWidth(SingleWidthChar),
    DoubleWidth(DoubleWidthChar),
    Other(char),
}

impl AsRef<char> for CharType {
    fn as_ref(&self) -> &char {
        match self {
            CharType::SingleWidth(SingleWidthChar(ch))
            | CharType::DoubleWidth(DoubleWidthChar(ch))
            | CharType::Other(ch) => ch,
        }
    }
}

impl From<CharType> for char {
    fn from(value: CharType) -> Self {
        *value.as_ref()
    }
}

impl From<char> for CharType {
    fn from(value: char) -> Self {
        CharType::classify(value)
    }
}

impl CharType {
    pub fn classify(ch: char) -> Self {
        match UnicodeWidthChar::width(ch) {
            Some(1) => CharType::SingleWidth(SingleWidthChar(ch)),
            Some(2) => CharType::DoubleWidth(DoubleWidthChar(ch)),
            _ => CharType::Other(ch),
        }
    }

    pub fn char(self) -> char {
        *self.as_ref()
    }
}

macro_rules! char_type {
    ($name:ident, $label:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(char);

        impl AsRef<char> for $name {
            fn as_ref(&self) -> &char {
                &self.0
            }
        }

        impl From<$name> for char {
            fn from(value: $name) -> Self {
                value.0
            }
        }

        impl From<$name> for CharType {
            fn from(value: $name) -> Self {
                CharType::$label(value)
            }
        }
    };
}

char_type!(SingleWidthChar, SingleWidth);
char_type!(DoubleWidthChar, DoubleWidth);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rows() {
        let cells = ['a', 'b', 'c', 'd', 'e', 'f'].map(Into::into);
        let mut rows = Rows {
            width: 2,
            cells: &cells,
        };
        assert_eq!(rows.size_hint(), (3, Some(3)));

        assert_eq!(rows.next().unwrap().cells, &['a', 'b'].map(Into::into));
        assert_eq!(rows.size_hint(), (2, Some(2)));

        assert_eq!(rows.next().unwrap().cells, &['c', 'd'].map(Into::into));
        assert_eq!(rows.size_hint(), (1, Some(1)));

        assert_eq!(rows.next().unwrap().cells, &['e', 'f'].map(Into::into));
        assert_eq!(rows.size_hint(), (0, Some(0)));

        assert!(rows.next().is_none());
    }

    #[test]
    fn rows_rev() {
        let cells = ['a', 'b', 'c', 'd', 'e', 'f'].map(Into::into);
        let mut rows = Rows {
            width: 2,
            cells: &cells,
        }
        .rev();

        assert_eq!(rows.next().unwrap().cells, &['e', 'f'].map(Into::into));
        assert_eq!(rows.next().unwrap().cells, &['c', 'd'].map(Into::into));
        assert_eq!(rows.next().unwrap().cells, &['a', 'b'].map(Into::into));
        assert!(rows.next().is_none());
    }

    #[test]
    fn rows_nth() {
        let cells = ['a', 'b', 'c', 'd', 'e', 'f'].map(Into::into);
        let rows = Rows {
            width: 2,
            cells: &cells,
        };

        assert_eq!(
            rows.clone().nth(0).unwrap().cells,
            &['a', 'b'].map(Into::into)
        );
        assert_eq!(
            rows.clone().nth(1).unwrap().cells,
            &['c', 'd'].map(Into::into)
        );
        assert_eq!(
            rows.clone().nth(2).unwrap().cells,
            &['e', 'f'].map(Into::into)
        );
        assert!(rows.clone().nth(3).is_none());
        assert!(rows.clone().nth(256).is_none());
    }

    #[test]
    fn rows_nth_back() {
        let cells = ['a', 'b', 'c', 'd', 'e', 'f'].map(Into::into);
        let rows = Rows {
            width: 2,
            cells: &cells,
        }
        .rev();

        assert_eq!(
            rows.clone().nth(0).unwrap().cells,
            &['e', 'f'].map(Into::into)
        );
        assert_eq!(
            rows.clone().nth(1).unwrap().cells,
            &['c', 'd'].map(Into::into)
        );
        assert_eq!(
            rows.clone().nth(2).unwrap().cells,
            &['a', 'b'].map(Into::into)
        );
        assert!(rows.clone().nth(3).is_none());
        assert!(rows.clone().nth(256).is_none());
    }

    #[test]
    fn row() {
        let cells = ['a', 'b', '✨', ' ', 'c', 'd'].map(Into::into);
        let mut row = Row { cells: &cells };
        assert_eq!(row.size_hint(), (3, Some(6)));

        assert_eq!(row.next().unwrap().ch, 'a');
        assert_eq!(row.size_hint(), (3, Some(5)));

        assert_eq!(row.next().unwrap().ch, 'b');
        assert_eq!(row.size_hint(), (2, Some(4)));

        assert_eq!(row.next().unwrap().ch, '✨');
        assert_eq!(row.size_hint(), (1, Some(2)));

        assert_eq!(row.next().unwrap().ch, 'c');
        assert_eq!(row.size_hint(), (1, Some(1)));

        assert_eq!(row.next().unwrap().ch, 'd');
        assert_eq!(row.size_hint(), (0, Some(0)));

        assert!(row.next().is_none());
    }

    #[test]
    fn row_rev() {
        let cells = ['a', 'b', '✨', ' ', 'c', 'd'].map(Into::into);
        let mut row = Row { cells: &cells }.rev();

        assert_eq!(row.next().unwrap().ch, 'd');
        assert_eq!(row.next().unwrap().ch, 'c');
        assert_eq!(row.next().unwrap().ch, '✨');
        assert_eq!(row.next().unwrap().ch, 'b');
        assert_eq!(row.next().unwrap().ch, 'a');
        assert!(row.next().is_none());
    }

    #[test]
    fn set_cell() {
        let mut term = Terminal::new([4, 2]);

        let mut window = term.edit();
        window.set_cell([1, 0], 'a');
        assert_eq!(window.overdrawn(), Some(Box2::from(Pos2::new(1, 0))));

        let mut window = term.edit();
        window.set_cell([1, 1], '✨');
        assert_eq!(window.overdrawn(), Some(Box2::new([1, 1], [2, 1])));

        term.assert_chars_equal([" a  ", " ✨ "]);
    }

    #[test]
    fn fill_horizontal() {
        let mut term = Terminal::new([12, 3]);

        let mut window = term.edit();
        window.fill_horizontal([1, 0], 10, 'c');
        assert_eq!(window.overdrawn(), Some(Box2::new([1, 0], [10, 0])));

        let mut window = term.edit();
        window.fill_horizontal([1, 1], 10, '✨');
        assert_eq!(window.overdrawn(), Some(Box2::new([1, 1], [10, 1])));

        let mut window = term.edit();
        window.fill_horizontal([1, 2], 9, '✨');
        assert_eq!(window.overdrawn(), Some(Box2::new([1, 2], [8, 2])));

        term.assert_chars_equal([" cccccccccc ", " ✨✨✨✨✨ ", " ✨✨✨✨   "]);
    }

    #[test]
    fn fill_vertical() {
        let mut term = Terminal::new([3, 12]);

        let mut window = term.edit();
        window.fill_vertical([1, 1], 10, '✨');
        assert_eq!(window.overdrawn(), Some(Box2::new([1, 1], [1, 10])));

        let mut window = term.edit();
        window.fill_vertical([0, 1], 10, 'c');
        assert_eq!(window.overdrawn(), Some(Box2::new([0, 1], [0, 10])));

        term.assert_chars_equal([
            "   ", "c✨", "c✨", "c✨", "c✨", "c✨", "c✨", "c✨", "c✨", "c✨", "c✨", "   ",
        ]);
    }

    #[test]
    fn fill_area() {
        let mut term = Terminal::new([16, 5]);

        let mut window = term.edit();
        window.fill_area(Box2::new([1, 1], [4, 3]), 'c');
        assert_eq!(window.overdrawn(), Some(Box2::new([1, 1], [4, 3])));

        let mut window = term.edit();
        window.fill_area(Box2::new([6, 1], [9, 3]), '✨');
        assert_eq!(window.overdrawn(), Some(Box2::new([6, 1], [9, 3])));

        let mut window = term.edit();
        window.fill_area(Box2::new([11, 1], [13, 3]), '✨');
        assert_eq!(window.overdrawn(), Some(Box2::new([11, 1], [12, 3])));

        term.assert_chars_equal([
            "                ",
            " cccc ✨✨ ✨   ",
            " cccc ✨✨ ✨   ",
            " cccc ✨✨ ✨   ",
            "                ",
        ]);
    }

    #[test]
    fn subwindow() {
        let mut term = Terminal::new([5, 5]);
        let mut window = term.edit();

        window.subwindow(Box2::new([1, 2], [4, 4]), |w| {
            w.set_cell([1, 1], 'a');
        });

        assert_eq!(window.overdrawn(), Some(Box2::new([2, 3], [2, 3])));
        term.assert_chars_equal(["     ", "     ", "     ", "  a  ", "     "]);
    }
}
