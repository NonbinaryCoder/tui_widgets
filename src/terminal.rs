use std::{
    fmt::Display,
    iter::{self, FusedIterator},
};

use unicode_width::UnicodeWidthChar;

use crate::{
    math::{Pos2, Rect2, Size2},
    widget::style::{Color, Colors, Formatting},
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

    fn cell_index(&self, pos: Pos2<u16>) -> usize {
        let pos = pos.map_into::<usize>();
        pos.x + pos.y * self.size.width as usize
    }

    pub fn set_cell(
        &mut self,
        pos: impl Into<Pos2<u16>>,
        ch: impl Into<CharType>,
        fmt: impl Into<Formatting>,
    ) {
        let ch = ch.into();
        let cell = Cell {
            ch: ch.char(),
            fmt: fmt.into(),
        };
        let pos = pos.into();
        let cell_index = self.cell_index(pos);
        match ch {
            CharType::SingleWidth(_) => {
                self.cells[cell_index] = cell;
            }
            CharType::DoubleWidth(_) => {
                if pos.x < self.size.width {
                    self.cells[cell_index] = cell;
                    self.cells[cell_index + 1].ch = ' ';
                }
            }
            CharType::Other(_) => {}
        }
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
            let mut template_count = 0;
            for (x, (cell, template)) in iter::zip(row.clone(), templates_iter.by_ref()).enumerate()
            {
                template_count += 1;
                if mismatch.is_none() && !(cell_to_template(cell) == template) {
                    mismatch = Some(Pos2::new(x, y));
                }
            }
            template_count += templates_iter.count();
            let cell_count = row.count();
            assert_eq!(
                cell_count, template_count,
                "Number of cells ({cell_count}) should match number of templates \
                ({template_count})"
            );
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
    inner: &'a mut Terminal,
    area: Rect2<u16>,
}

impl<'a> TerminalWindow<'a> {
    fn offset_pos(&self, pos: Pos2<u16>) -> Pos2<u16> {
        debug_assert!(
            self.area.size().contains_pos(pos),
            "Size of terminal window ({}) should contain pos ({pos})",
            self.area.size()
        );
        pos + self.area.pos()
    }

    pub fn size(&self) -> Size2<u16> {
        self.area.size()
    }

    pub fn set_cell(
        &mut self,
        pos: impl Into<Pos2<u16>>,
        ch: impl Into<CharType>,
        fmt: impl Into<Formatting>,
    ) {
        self.inner
            .set_cell(self.offset_pos(pos.into()), ch.into(), fmt.into());
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
    fn terminal_rows() {
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
    fn terminal_rows_rev() {
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
    fn terminal_rows_nth() {
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
    fn terminal_rows_nth_back() {
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
    fn terminal_row() {
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
    fn terminal_row_rev() {
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
    fn terminal_set_cell() {
        let mut term = Terminal::new([4, 2]);
        term.set_cell([1, 0], 'a', ());
        term.set_cell([1, 1], '✨', ());
        term.assert_chars_equal([" a  ", " ✨ "]);
    }
}
