use std::{fmt::Display, io, iter};

use unicode_width::UnicodeWidthChar;

use crate::{
    math::{Pos2, Rect2, Size2, Vec2},
    widget::{
        style::{Color, Colors},
        RenderResult, Widget,
    },
};

use super::TerminalWriter;

/// A [`TerminalWriter`] that can be used to test that a widget works as
/// intended.  Not intended to be used outside of tests.
#[derive(Debug)]
pub struct TestWriter {
    size: Size2<u16>,
    cursor_pos: Pos2<u16>,
    cursor_colors: Colors,
    cells: Box<[Cell]>,

    term_settings_assertions: bool,
    area_assertions: bool,
    alternate_screen: bool,

    offset: Vec2<u16>,
    widget_size: Size2<u16>,
}

impl TestWriter {
    pub fn new(size: impl Into<Size2<u16>>) -> Self {
        let size = size.into();
        Self {
            size,
            cursor_pos: Pos2::splat(0),
            cursor_colors: Colors::splat(Color::Reset),
            cells: vec![Cell::default(); size.map_into::<usize>().area()].into_boxed_slice(),
            term_settings_assertions: true,
            area_assertions: true,
            alternate_screen: false,
            offset: Vec2::splat(0),
            widget_size: size,
        }
    }

    pub fn is_terminal_reset(&self) -> bool {
        !self.alternate_screen
    }

    pub fn current_widget_rect(&self) -> Rect2<u16> {
        Rect2::new(self.offset, self.size)
    }

    pub fn char_at(&self, pos: impl Into<Pos2<u16>>) -> Cell {
        let pos = pos.into().map_into::<usize>();
        self.cells[pos.y * self.size.width as usize + pos.x]
    }

    fn set_char_at(&mut self, pos: impl Into<Pos2<u16>>, cell: Cell) {
        let pos = pos.into().map_into::<usize>();
        self.cells[pos.y * self.size.width as usize + pos.x] = cell;
    }

    fn assert_cell_matches_by(
        &self,
        template: &[&str],
        mut matches: impl FnMut(Cell, char) -> bool,
        mut print_row: impl FnMut(&[Cell]),
    ) {
        let height = self.size.width;
        let template_height = template.len();
        assert_eq!(
            height as usize, template_height,
            "Height ({height}) and template height ({template_height}) are different"
        );

        let width = self.size.width;
        for (y, template_row) in template.iter().enumerate() {
            let template_width = template_row.chars().count();
            assert_eq!(
                width as usize, template_width,
                "Width ({width}) and width of row {y} ({template_width}) should be the same"
            );
        }

        for (y, (row, template_row)) in
            iter::zip(self.cells.windows(width as usize), template.iter()).enumerate()
        {
            for (x, (&cell, ch)) in iter::zip(row, template_row.chars()).enumerate() {
                if !matches(cell, ch) {
                    println!("Non match at [{x}, {y}]");
                }
                println!();
                println!("Template:");
                for line in template {
                    println!("{}", line);
                }
                println!();
                println!("Cells:");
                for line in self.cells.windows(width as usize) {
                    print_row(line);
                }
            }
        }
    }

    pub fn assert_cells_match(&self, template: &[&str], cells: impl Into<Vec<(char, Cell)>>) {
        let cells: Vec<_> = cells.into();
        self.assert_cell_matches_by(
            template,
            |cell, ch| {
                let Some(template_cell) = cells
                    .iter()
                    .find_map(|&(key, cell)| (key == ch).then_some(cell))
                else { panic!("No cell mapped to char '{ch}'") };
                cell == template_cell
            },
            |row| {
                for &cell in row {
                    let ch = cells
                        .iter()
                        .find_map(|&(ch, template_cell)| (cell == template_cell).then_some(ch))
                        .unwrap_or('?');
                    print!("{}", ch);
                }
                println!();
            },
        );
    }

    pub fn assert_bg_colors_match(&self, template: &[&str], colors: impl Into<Vec<(char, Color)>>) {
        let colors: Vec<_> = colors.into();
        self.assert_cell_matches_by(
            template,
            |cell, ch| {
                let Some(color) = colors
                    .iter()
                    .find_map(|&(key, color)| (key == ch).then_some(color))
                else { panic!("No color mapped to char '{ch}'") };
                cell.colors.bg == color
            },
            |row| {
                for cell in row {
                    let ch = colors
                        .iter()
                        .find_map(|&(ch, color)| (cell.colors.fg == color).then_some(ch))
                        .unwrap_or('?');
                    print!("{}", ch);
                }
                println!();
            },
        );
    }
}

impl TerminalWriter for TestWriter {
    /* Size */

    fn term_size(&self) -> io::Result<crate::math::Size2<u16>> {
        Ok(self.size)
    }

    /* Screen */

    fn enter_alternate_screen(&mut self) -> io::Result<&mut Self> {
        if self.term_settings_assertions && self.alternate_screen {
            panic!("Attempt to enter alternate screen when already in alternate screen");
        }
        self.alternate_screen = true;
        Ok(self)
    }

    fn leave_alternate_screen(&mut self) -> io::Result<&mut Self> {
        if self.term_settings_assertions && !self.alternate_screen {
            panic!("Attempt to leave alternate screen when not in alternate screen");
        }
        self.alternate_screen = false;
        Ok(self)
    }

    /* Cursor position */

    fn cursor_to(&mut self, pos: impl Into<Pos2<u16>>) -> io::Result<&mut Self> {
        let pos = pos.into();
        if self.area_assertions {
            let widget_size = self.widget_size;
            assert!(
                widget_size.contains_pos(pos),
                "Widget size ({widget_size}) should contain pos ({pos})"
            );
        }
        self.cursor_pos = pos + self.offset;
        Ok(self)
    }

    fn cursor_to_column(&mut self, column: u16) -> io::Result<&mut Self> {
        if self.area_assertions {
            let widget_width = self.widget_size.width;
            assert!(
                column <= widget_width,
                "Widget width ({widget_width}) should contain column ({column})"
            );
        }
        self.cursor_pos.x = column + self.offset.x;
        Ok(self)
    }

    fn cursor_to_row(&mut self, row: u16) -> io::Result<&mut Self> {
        if self.area_assertions {
            let widget_height = self.widget_size.height;
            assert!(
                row <= widget_height,
                "Widget height ({widget_height}) should contain row ({row})"
            );
        }
        self.cursor_pos.y = row + self.offset.y;
        Ok(self)
    }

    /* Cursor move */

    fn cursor_left(&mut self, distance: u16) -> io::Result<&mut Self> {
        if self.area_assertions {
            let widget_rect = self.current_widget_rect();
            let cursor_pos = self.cursor_pos;
            assert!(
                self.offset.x <= distance,
                "Cursor moved outside widget rect 
(distance: {distance}, rect: {widget_rect:?}, original cursor pos: {cursor_pos})"
            );
        }
        self.cursor_pos.x -= distance;
        Ok(self)
    }

    fn cursor_right(&mut self, distance: u16) -> io::Result<&mut Self> {
        if self.area_assertions {
            let widget_rect = self.current_widget_rect();
            let cursor_pos = self.cursor_pos;
            assert!(
                self.offset.x + distance <= self.widget_size.width,
                "Cursor moved outside widget rect 
(distance: {distance}, rect: {widget_rect:?}, original cursor pos: {cursor_pos})"
            );
        }
        self.cursor_pos.x += distance;
        Ok(self)
    }

    fn cursor_up(&mut self, distance: u16) -> io::Result<&mut Self> {
        if self.area_assertions {
            let widget_rect = self.current_widget_rect();
            let cursor_pos = self.cursor_pos;
            assert!(
                self.offset.y <= distance,
                "Cursor moved outside widget rect 
(distance: {distance}, rect: {widget_rect:?}, original cursor pos: {cursor_pos})"
            );
        }

        self.cursor_pos.y -= distance;
        Ok(self)
    }
    fn cursor_down(&mut self, distance: u16) -> io::Result<&mut Self> {
        if self.area_assertions {
            let widget_rect = self.current_widget_rect();
            let cursor_pos = self.cursor_pos;
            assert!(
                self.offset.x + distance <= self.widget_size.width,
                "Cursor moved outside widget rect 
(distance: {distance}, rect: {widget_rect:?}, original cursor pos: {cursor_pos})"
            );
        }
        self.cursor_pos.y += distance;
        Ok(self)
    }

    /* Print */

    fn write_char(&mut self, ch: char) -> io::Result<&mut Self> {
        dbg!(self.cursor_pos);
        match UnicodeWidthChar::width(ch) {
            Some(0) | None => {}
            Some(1) => {
                if self.area_assertions {
                    assert!(
                        self.cursor_pos.x + 1 <= self.offset.x + self.widget_size.width,
                        "Attempt to write character '{ch}' out of widget bounds"
                    );
                }
                self.set_char_at(
                    self.cursor_pos,
                    Cell {
                        ch,
                        colors: self.cursor_colors,
                    },
                );
                self.cursor_pos.x += 1;
            }
            Some(2) => {
                if self.area_assertions {
                    assert!(
                        self.cursor_pos.x + 2 <= self.offset.x + self.widget_size.width,
                        "Attempt to write character '{ch}' out of widget bounds"
                    );
                }
                let cell = Cell {
                    ch,
                    colors: self.cursor_colors,
                };
                self.set_char_at(self.cursor_pos, cell);
                self.set_char_at(self.cursor_pos + Vec2::new(1, 0), cell);
                self.cursor_pos.x += 2;
            }
            Some(_) => unreachable!(),
        }
        Ok(self)
    }

    fn print(&mut self, arg: impl Display) -> io::Result<&mut Self> {
        let arg = arg.to_string();
        if self.area_assertions {
            assert!(
                self.cursor_pos.x as usize + arg.chars().count()
                    <= (self.offset.x + self.widget_size.width) as usize,
                "Attempt to print object \"{arg}\" outside of widget bounds"
            );
        }
        for ch in arg.chars() {
            self.write_char(ch)?;
        }
        Ok(self)
    }

    /* Color */

    fn set_foreground_color(&mut self, color: Color) -> io::Result<&mut Self> {
        self.cursor_colors.fg = color;
        Ok(self)
    }

    fn set_background_color(&mut self, color: Color) -> io::Result<&mut Self> {
        self.cursor_colors.bg = color;
        Ok(self)
    }

    fn add_widget<W: Widget<Self>>(
        &mut self,
        widget: &mut W,
        rect: Rect2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult> {
        if self.area_assertions {
            let warea = Rect2::new(self.offset, self.widget_size);
            assert!(
                warea.contains_rect(rect),
                "Avalible terminal space ({warea:?}) should contain widget rect ({rect:?})"
            );
            if let Some(overdrawn) = overdrawn {
                assert!(
                    overdrawn.contains_rect(rect),
                    "Widget rect ({rect:?}) should contain overdrawn area ({overdrawn:?})"
                );
            }
        }

        let old_offset = self.offset;
        let old_widget_size = self.widget_size;
        self.offset = rect.pos().to_vec();
        self.widget_size = rect.size();
        let overdrawn = overdrawn.map(|overdrawn| overdrawn.translate_sub(self.offset));
        let result = widget.render(self, rect.size(), overdrawn);
        self.offset = old_offset;
        self.widget_size = old_widget_size;
        result
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cell {
    pub ch: char,
    pub colors: Colors,
}

impl Default for Cell {
    fn default() -> Self {
        Cell {
            ch: ' ',
            colors: Colors::splat(Color::Reset),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::App;

    use super::*;

    #[test]
    fn offset_set() -> io::Result<()> {
        let mut app = App::with_writer(
            TestWriter::new([16, 16]),
            |writer: &mut TestWriter, _, _| {
                let mut widget = |writer: &mut TestWriter, _, _| {
                    assert_eq!(writer.offset, Vec2::new(2, 3));
                    Ok(RenderResult::NOTHING_DRAWN)
                };
                writer.add_widget(&mut widget, Rect2::new([2, 3], [1, 4]), None)
            },
        )?;
        app.render()?;
        let writer = app.exit()?;
        assert_eq!(writer.offset, Vec2::new(0, 0));
        Ok(())
    }
}
