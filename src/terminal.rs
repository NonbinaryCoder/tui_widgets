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

    pub fn edit(&mut self, overdrawn: Overdrawn) -> TerminalWindow<'_> {
        let area = Box2::new([0, 0], self.size.to_vec().map(|v| v.saturating_sub(1)));
        TerminalWindow {
            area,
            overdrawn: match overdrawn {
                Overdrawn::None => None,
                Overdrawn::Area(overdrawn) => area.intersection(overdrawn),
                Overdrawn::All => Some(area),
            },
            term: self,
        }
    }

    fn cell_index(&self, pos: Pos2<u16>) -> usize {
        let pos = pos.map_into::<usize>();
        pos.x + pos.y * self.size.width as usize
    }

    /// Creates a terminal window that can be used to test whether or not
    /// widgets work as intended.  Not intended to be used outside of tests.
    pub fn test_widget(
        size: impl Into<Size2<u16>>,
        overdrawn: Overdrawn,
        mut f: impl FnMut(&mut TerminalWindow),
    ) {
        let size = size.into();

        let mut term = Self::new(size);
        eprintln!("Small terminal");
        term.edit(overdrawn)
            .subwindow(Box2::new([0, 0], size.to_vec() - Vec2::splat(1)), &mut f);

        let inner_area = Box2::new([1, 2], size.to_vec() + Vec2::new(1, 2) - Vec2::splat(1));
        let full_size = size + Vec2::new(1, 2) + Vec2::new(3, 4);

        let mut term = Self::new(full_size);
        let mut window = term.edit(overdrawn);
        eprintln!("Large terminal");
        window.subwindow(inner_area, &mut f);
    }

    /// Renders a widget in a wide variety of conditions to see if it panics.
    /// Not intended to be used outside of tests.
    ///
    /// # Panics
    ///
    /// Panics if rendering the widget panics.
    pub fn stress_widget<W: Widget>(mut widget: W) {
        let sizes = [1, 2, 3, 4, 5, 8, 16, 17, 32, 33, 64, 65, 128, 129, 256, 257];

        let mut term = Terminal::new(Size2::splat(*sizes.last().unwrap()));
        let mut term = term.edit(Overdrawn::All);

        for width in sizes {
            for height in sizes {
                eprintln!("{width}x{height}");
                term.add_widget(Box2::new([0, 0], [width - 1, height - 1]), &mut widget);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Overdrawn {
    None,
    Area(Box2<u16>),
    All,
}

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
        self.term.cell_index(self.offset_pos(pos))
    }

    pub fn size(&self) -> Size2<u16> {
        (self.area.max.to_vec() + Vec2::splat(1) - self.area.min.to_vec()).to_size()
    }

    pub fn overdrawn(&self) -> Option<Box2<u16>> {
        self.overdrawn
    }

    pub fn rows(&self) -> Rows<'_> {
        Rows {
            width: self.size().width as usize,
            term_width: self.term.size.width as usize,
            cells: &self.term.cells
                [self.term.cell_index(self.area.min)..=self.term.cell_index(self.area.max)],
        }
    }

    fn mark_cell_overdrawn(&mut self, cell: Pos2<u16>) {
        debug_assert!(self.size().contains_pos(cell));
        self.overdrawn = Some(self.overdrawn.map_or(cell.into(), |o| o.contain_pos(cell)));
    }

    fn mark_double_width_cell_overdrawn(&mut self, cell: Pos2<u16>) {
        self.mark_area_overdrawn(Box2::new(cell, cell + Pos2::new(1, 0)));
    }

    fn mark_area_overdrawn(&mut self, area: Box2<u16>) {
        debug_assert!(self.size().contains_box(area));
        self.overdrawn = Some(self.overdrawn.map_or(area, |o| o.contain_box(area)));
    }

    pub fn set_cell(&mut self, pos: impl Into<Pos2<u16>>, cell: impl Into<Cell>) -> &mut Self {
        let pos = pos.into();
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
            _ => {}
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
        let pos = pos.into();
        let cell = cell.into();
        let cell_index = self.cell_index(pos);
        match CharType::classify(cell.ch) {
            CharType::SingleWidth(_) if width > 0 => {
                self.mark_area_overdrawn(Box2::new(pos, pos + Pos2::new(width - 1, 0)));
                for cell_index in cell_index..(cell_index + width as usize) {
                    self.term.cells[cell_index] = cell;
                }
            }
            CharType::DoubleWidth(_) if width > 1 => {
                let offset = width % 2;
                self.mark_area_overdrawn(Box2::new(pos, pos + Pos2::new(width - 1 - offset, 0)));
                for cell_index in (cell_index..(cell_index + (width - offset) as usize)).step_by(2)
                {
                    self.term.cells[cell_index] = cell;
                    self.term.cells[cell_index + 1] = cell.ch(' ');
                }
            }
            _ => {}
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
        if height < 1 {
            return self;
        }

        let pos = pos.into();
        let cell = cell.into();
        let cell_index = self.cell_index(pos);
        let iter = (cell_index..)
            .step_by(self.term.size.width as usize)
            .take(height as usize);
        match CharType::classify(cell.ch) {
            CharType::SingleWidth(_) => {
                self.mark_area_overdrawn(Box2::new(pos, pos + Pos2::new(0, height - 1)));
                for cell_index in iter {
                    self.term.cells[cell_index] = cell;
                }
            }
            CharType::DoubleWidth(_) => {
                self.mark_area_overdrawn(Box2::new(pos, pos + Pos2::new(1, height - 1)));
                for cell_index in iter {
                    self.term.cells[cell_index] = cell;
                    self.term.cells[cell_index + 1] = cell.ch(' ');
                }
            }
            _ => {}
        }
        self
    }

    /// Repeats a cell to fill the specified area.
    pub fn fill_area(&mut self, area: Box2<u16>, cell: impl Into<Cell>) -> &mut Self {
        let size = (area.max.to_vec() + Vec2::splat(1) - area.min.to_vec()).to_size();

        let cell = cell.into();
        let cell_index = self.cell_index(area.min);
        let iter = (cell_index..)
            .step_by(self.term.size.width as usize)
            .take(size.height as usize);
        match CharType::classify(cell.ch) {
            CharType::SingleWidth(_) if size.width >= 1 && size.height >= 1 => {
                self.mark_area_overdrawn(area);
                for cell_index in iter {
                    for cell_index in cell_index..(cell_index + size.width as usize) {
                        self.term.cells[cell_index] = cell;
                    }
                }
            }
            CharType::DoubleWidth(_) if size.width >= 2 && size.height >= 1 => {
                let offset = size.width % 2;
                let mut area = area;
                area.max.x -= offset;
                self.mark_area_overdrawn(area);
                for cell_index in iter {
                    for cell_index in
                        (cell_index..(cell_index + (size.width - offset) as usize)).step_by(2)
                    {
                        self.term.cells[cell_index] = cell;
                        self.term.cells[cell_index + 1] = cell;
                    }
                }
            }
            _ => {}
        }
        self
    }

    pub fn subwindow<T>(&mut self, area: Box2<u16>, f: impl FnOnce(&mut TerminalWindow) -> T) -> T {
        let offset_area = self.offset_area(area);
        let mut subwindow = TerminalWindow {
            area: offset_area,
            overdrawn: self
                .overdrawn
                .and_then(|o| o.intersection(area))
                .map(|o| o.translate_sub(area.min)),
            term: self.term,
        };
        let ret = f(&mut subwindow);
        if let Some(overdrawn) = subwindow.overdrawn {
            self.mark_area_overdrawn(overdrawn);
        }
        ret
    }

    pub fn add_widget<W: Widget>(&mut self, area: Box2<u16>, widget: &mut W) -> &mut Self {
        self.subwindow(area, |w| widget.render(w));
        self
    }

    /// Fills the entirety of this window with the provided widget.
    pub fn fill_widget<W: Widget>(&mut self, widget: &mut W) -> &mut Self {
        widget.render(self);
        self
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
        let height = self.size().height;
        let template_height = template.len();
        assert_eq!(
            height as usize, template_height,
            "Height of terminal ({height}) should match height of template ({template_height})"
        );
        let mut mismatch = None;
        let mut mismatch_count = 0;
        for (y, (row, template_row)) in iter::zip(self.rows(), template).enumerate() {
            let mut templates_iter = row_elements(template_row);
            for (x, (cell, template)) in iter::zip(row.clone(), templates_iter.by_ref()).enumerate()
            {
                if !(cell_to_template(cell) == template) {
                    mismatch_count += 1;
                    if mismatch.is_none() {
                        mismatch = Some(Pos2::new(x, y));
                    }
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
            panic!("{mismatch_count} mismatches; First mismatch at column {x}, row {y}");
        }
    }

    /// Asserts that all the chars in the cells of this are the same as the
    /// chars in the template
    pub fn assert_chars_equal<'b>(&self, template: impl AsRef<[&'b str]>) {
        self.assert_equal_inner(template.as_ref(), |row| row.chars(), |cell| cell.ch);
    }
}

/// An iterator over the rows of a terminal window.  Rows are returned as
/// iterators over their elements.
#[derive(Debug, Clone)]
pub struct Rows<'a> {
    width: usize,
    term_width: usize,
    cells: &'a [Cell],
}

impl<'a> Iterator for Rows<'a> {
    type Item = Row<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        (self.cells.len() >= self.width).then(|| {
            let row = Row {
                cells: &self.cells[..self.width],
            };
            self.cells = if self.cells.len() >= self.term_width {
                &self.cells[self.term_width..]
            } else {
                &[]
            };
            row
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
        let discard_width = self.term_width * n;
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
        (self.cells.len() + self.term_width - self.width) / self.term_width
    }
}

impl<'a> DoubleEndedIterator for Rows<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        (self.cells.len() >= self.width).then(|| {
            if self.cells.len() > self.term_width {
                let tail;
                (self.cells, tail) = self.cells.split_at(self.cells.len() - self.term_width);
                Row {
                    cells: &tail[(self.term_width - self.width)..],
                }
            } else {
                let row = Row { cells: self.cells };
                self.cells = &[];
                row
            }
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

/// An iterator over the cells in a row of a terminal window.  Double width
/// cells are only returned once
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

    fn term(mut f: impl FnMut(&mut TerminalWindow)) {
        #[rustfmt::skip]
        let mut term = Terminal {
            size: Size2::new(4, 3),
            cells: Box::new([
                'a', 'b', 'c', 'd',
                'e', 'f', 'g', 'h',
                'i', 'j', 'k', 'l',
            ].map(Into::into)),
        };
        eprintln!("Small terminal");
        term.edit(Overdrawn::None)
            .subwindow(Box2::new([0, 0], [3, 2]), &mut f);

        #[rustfmt::skip]
        let mut term = Terminal {
            size: Size2::new(7, 8),
            cells: Box::new([
                '_', '_', '_', '_', '_', '_', '_',
                '_', '_', '_', '_', '_', '_', '_',
                '_', 'a', 'b', 'c', 'd', '_', '_',
                '_', 'e', 'f', 'g', 'h', '_', '_',
                '_', 'i', 'j', 'k', 'l', '_', '_',
                '_', '_', '_', '_', '_', '_', '_',
                '_', '_', '_', '_', '_', '_', '_',
                '_', '_', '_', '_', '_', '_', '_',
            ].map(Into::into)),
        };
        eprintln!("Large terminal");
        term.edit(Overdrawn::None)
            .subwindow(Box2::new([1, 2], [4, 4]), &mut f);
    }

    #[test]
    fn rows() {
        term(|term| {
            let mut rows = term.rows();

            assert_eq!(rows.size_hint(), (3, Some(3)));

            assert_eq!(
                rows.next().unwrap().cells,
                &['a', 'b', 'c', 'd'].map(Into::into),
            );
            assert_eq!(rows.size_hint(), (2, Some(2)));

            assert_eq!(
                rows.next().unwrap().cells,
                &['e', 'f', 'g', 'h'].map(Into::into),
            );
            assert_eq!(rows.size_hint(), (1, Some(1)));

            assert_eq!(
                rows.next().unwrap().cells,
                &['i', 'j', 'k', 'l'].map(Into::into),
            );
            assert_eq!(rows.size_hint(), (0, Some(0)));

            assert!(rows.next().is_none());
        });
    }

    #[test]
    fn rows_rev() {
        term(|term| {
            let mut rows = term.rows().rev();

            assert_eq!(rows.size_hint(), (3, Some(3)));

            assert_eq!(
                rows.next().unwrap().cells,
                &['i', 'j', 'k', 'l'].map(Into::into),
            );
            assert_eq!(rows.size_hint(), (2, Some(2)));

            assert_eq!(
                rows.next().unwrap().cells,
                &['e', 'f', 'g', 'h'].map(Into::into),
            );
            assert_eq!(rows.size_hint(), (1, Some(1)));

            assert_eq!(
                rows.next().unwrap().cells,
                &['a', 'b', 'c', 'd'].map(Into::into),
            );
            assert_eq!(rows.size_hint(), (0, Some(0)));

            assert!(rows.next().is_none());
        });
    }

    #[test]
    fn rows_nth() {
        term(|term| {
            let rows = term.rows();

            assert_eq!(
                rows.clone().nth(0).unwrap().cells,
                &['a', 'b', 'c', 'd'].map(Into::into),
            );
            assert_eq!(
                rows.clone().nth(1).unwrap().cells,
                &['e', 'f', 'g', 'h'].map(Into::into),
            );
            assert_eq!(
                rows.clone().nth(2).unwrap().cells,
                &['i', 'j', 'k', 'l'].map(Into::into),
            );
            assert!(rows.clone().nth(3).is_none());
            assert!(rows.clone().nth(256).is_none());
        });
    }

    #[test]
    fn rows_nth_back() {
        term(|term| {
            let rows = term.rows();

            assert_eq!(
                rows.clone().nth(0).unwrap().cells,
                &['a', 'b', 'c', 'd'].map(Into::into),
            );
            assert_eq!(
                rows.clone().nth(1).unwrap().cells,
                &['e', 'f', 'g', 'h'].map(Into::into),
            );
            assert_eq!(
                rows.clone().nth(2).unwrap().cells,
                &['i', 'j', 'k', 'l'].map(Into::into),
            );
            assert!(rows.clone().nth(3).is_none());
            assert!(rows.clone().nth(256).is_none());
        });
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
    fn set_cell_sw_center() {
        Terminal::test_widget([3, 3], Overdrawn::None, |term| {
            term.set_cell([1, 1], 'c');
            term.assert_chars_equal(["   ", " c ", "   "]);
            assert_eq!(term.overdrawn(), Some(Box2::from_pos([1, 1])));
        });
    }

    #[test]
    fn set_cell_dw_center() {
        Terminal::test_widget([4, 3], Overdrawn::None, |term| {
            term.set_cell([1, 1], '✨');
            term.assert_chars_equal(["    ", " ✨ ", "    "]);
            assert_eq!(term.overdrawn(), Some(Box2::new([1, 1], [2, 1])));
        });
    }

    #[test]
    fn set_cell_sw_corners() {
        Terminal::test_widget([4, 4], Overdrawn::None, |term| {
            term.set_cell([0, 0], 'a')
                .set_cell([3, 0], 'b')
                .set_cell([0, 3], 'c')
                .set_cell([3, 3], 'd');

            term.assert_chars_equal(["a  b", "    ", "    ", "c  d"]);
            assert_eq!(term.overdrawn(), Some(Box2::new([0, 0], [3, 3])));
        });
    }

    #[test]
    fn set_cell_dw_corners() {
        Terminal::test_widget([8, 4], Overdrawn::None, |term| {
            term.set_cell([0, 0], '✨')
                .set_cell([6, 0], '🌈')
                .set_cell([0, 3], '全')
                .set_cell([6, 3], '角');

            term.assert_chars_equal(["✨    🌈", "        ", "        ", "全    角"]);
            assert_eq!(term.overdrawn(), Some(Box2::new([0, 0], [7, 3])));
        });
    }

    #[test]
    fn fill_horizontal_sw_center() {
        Terminal::test_widget([12, 3], Overdrawn::None, |term| {
            term.fill_horizontal([1, 1], 10, 'c');

            term.assert_chars_equal(["            ", " cccccccccc ", "            "]);
            assert_eq!(term.overdrawn(), Some(Box2::new([1, 1], [10, 1])));
        });
    }

    #[test]
    fn fill_horizontal_dw_center() {
        Terminal::test_widget([12, 3], Overdrawn::None, |term| {
            term.fill_horizontal([1, 1], 10, '✨');

            term.assert_chars_equal(["            ", " ✨✨✨✨✨ ", "            "]);
            assert_eq!(term.overdrawn(), Some(Box2::new([1, 1], [10, 1])));
        });
    }

    #[test]
    fn fill_horizontal_dw_center_odd_width() {
        Terminal::test_widget([11, 3], Overdrawn::None, |term| {
            term.fill_horizontal([1, 1], 9, '✨');
            term.assert_chars_equal(["           ", " ✨✨✨✨  ", "           "]);
            assert_eq!(term.overdrawn(), Some(Box2::new([1, 1], [8, 1])));
        });
    }

    #[test]
    fn fill_horizontal_sw_edges() {
        Terminal::test_widget([12, 3], Overdrawn::None, |term| {
            term.fill_horizontal([0, 0], 12, 'a')
                .fill_horizontal([0, 2], 12, 'b');

            term.assert_chars_equal(["aaaaaaaaaaaa", "            ", "bbbbbbbbbbbb"]);
            assert_eq!(term.overdrawn(), Some(Box2::new([0, 0], [11, 2])));
        });
    }

    #[test]
    fn fill_horizontal_dw_edges() {
        Terminal::test_widget([12, 3], Overdrawn::None, |term| {
            term.fill_horizontal([0, 0], 12, '✨')
                .fill_horizontal([0, 2], 12, '全');

            term.assert_chars_equal(["✨✨✨✨✨✨", "            ", "全全全全全全"]);
            assert_eq!(term.overdrawn(), Some(Box2::new([0, 0], [11, 2])));
        });
    }

    #[test]
    fn fill_horizontal_dw_edges_odd_width() {
        Terminal::test_widget([11, 3], Overdrawn::None, |term| {
            term.fill_horizontal([0, 0], 11, '✨')
                .fill_horizontal([0, 2], 11, '全');

            term.assert_chars_equal(["✨✨✨✨✨ ", "            ", "全全全全全 "]);
            assert_eq!(term.overdrawn(), Some(Box2::new([0, 0], [9, 2])));
        });
    }

    #[test]
    fn fill_vertical_sw_center() {
        Terminal::test_widget([3, 12], Overdrawn::None, |term| {
            term.fill_vertical([1, 1], 10, 'c');

            term.assert_chars_equal([
                "   ", " c ", " c ", " c ", " c ", " c ", " c ", " c ", " c ", " c ", " c ", "   ",
            ]);
            assert_eq!(term.overdrawn(), Some(Box2::new([1, 1], [1, 10])));
        });
    }

    #[test]
    fn fill_vertical_dw_center() {
        Terminal::test_widget([4, 12], Overdrawn::None, |term| {
            term.fill_vertical([1, 1], 10, '✨');

            term.assert_chars_equal([
                "    ", " ✨ ", " ✨ ", " ✨ ", " ✨ ", " ✨ ", " ✨ ", " ✨ ", " ✨ ", " ✨ ",
                " ✨ ", "    ",
            ]);
            assert_eq!(term.overdrawn(), Some(Box2::new([1, 1], [2, 10])));
        });
    }

    #[test]
    fn fill_vertical_sw_edges() {
        Terminal::test_widget([3, 12], Overdrawn::None, |term| {
            term.fill_vertical([0, 0], 12, 'a')
                .fill_vertical([2, 0], 12, 'b');

            term.assert_chars_equal([
                "a b", "a b", "a b", "a b", "a b", "a b", "a b", "a b", "a b", "a b", "a b", "a b",
            ]);
            assert_eq!(term.overdrawn(), Some(Box2::new([0, 0], [2, 11])));
        });
    }

    #[test]
    fn fill_vertical_dw_edges() {
        Terminal::test_widget([5, 12], Overdrawn::None, |term| {
            term.fill_vertical([0, 0], 12, '✨')
                .fill_vertical([3, 0], 12, '全');

            term.assert_chars_equal([
                "✨ 全", "✨ 全", "✨ 全", "✨ 全", "✨ 全", "✨ 全", "✨ 全", "✨ 全", "✨ 全",
                "✨ 全", "✨ 全", "✨ 全",
            ]);
            assert_eq!(term.overdrawn(), Some(Box2::new([0, 0], [4, 11])));
        });
    }

    #[test]
    fn fill_area_sw_center() {
        Terminal::test_widget([8, 5], Overdrawn::None, |term| {
            let area = Box2::new([1, 1], [6, 3]);
            term.fill_area(area, 'c');

            term.assert_chars_equal(["        ", " cccccc ", " cccccc ", " cccccc ", "        "]);
            assert_eq!(term.overdrawn(), Some(area));
        });
    }

    #[test]
    fn fill_area_dw_center() {
        Terminal::test_widget([8, 5], Overdrawn::None, |term| {
            let area = Box2::new([1, 1], [6, 3]);
            term.fill_area(area, '✨');

            term.assert_chars_equal(["        ", " ✨✨✨ ", " ✨✨✨ ", " ✨✨✨ ", "        "]);
            assert_eq!(term.overdrawn(), Some(area));
        });
    }

    #[test]
    fn fill_area_dw_center_odd_width() {
        Terminal::test_widget([7, 5], Overdrawn::None, |term| {
            term.fill_area(Box2::new([1, 1], [5, 3]), '✨');

            term.assert_chars_equal(["       ", " ✨✨  ", " ✨✨  ", " ✨✨  ", "       "]);
            assert_eq!(term.overdrawn(), Some(Box2::new([1, 1], [4, 3])));
        });
    }

    #[test]
    fn fill_area_sw_edges() {
        Terminal::test_widget([8, 5], Overdrawn::None, |term| {
            let area = Box2::new([0, 0], [7, 4]);
            term.fill_area(area, 'c');

            term.assert_chars_equal(["cccccccc", "cccccccc", "cccccccc", "cccccccc", "cccccccc"]);
            assert_eq!(term.overdrawn(), Some(area));
        });
    }

    #[test]
    fn fill_area_dw_edges() {
        Terminal::test_widget([8, 5], Overdrawn::None, |term| {
            let area = Box2::new([0, 0], [7, 4]);
            term.fill_area(area, '✨');

            term.assert_chars_equal(["✨✨✨✨", "✨✨✨✨", "✨✨✨✨", "✨✨✨✨", "✨✨✨✨"]);
            assert_eq!(term.overdrawn(), Some(area));
        });
    }

    #[test]
    fn fill_area_dw_edges_odd_width() {
        Terminal::test_widget([7, 5], Overdrawn::None, |term| {
            term.fill_area(Box2::new([0, 0], [6, 4]), '✨');

            term.assert_chars_equal(["✨✨✨ ", "✨✨✨ ", "✨✨✨ ", "✨✨✨ ", "✨✨✨ "]);
            assert_eq!(term.overdrawn(), Some(Box2::new([0, 0], [5, 4])))
        })
    }
}
