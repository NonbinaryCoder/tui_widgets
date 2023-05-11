use std::io;

use crate::{
    math::{Rect2, Size2},
    widget::{style::Colors, RenderResult, Widget},
    writer::TerminalWriter,
};

pub struct Filled(char, Colors);

impl<W: TerminalWriter> Widget<W> for Filled {
    fn render(
        &mut self,
        writer: &mut W,
        _size: Size2<u16>,
        overdrawn: Option<Rect2<u16>>,
    ) -> io::Result<RenderResult> {
        if let Some(overdrawn) = overdrawn {
            let Filled(ch, colors) = *self;
            dbg!(overdrawn);
            writer
                .set_colors(colors)?
                .cursor_to(overdrawn.pos())?
                .repeat_char(' ', overdrawn.width)?;
            dbg!("here");
            for _y in (overdrawn.y + 1)..=(overdrawn.y + overdrawn.height) {
                writer
                    .cursor_to_column(overdrawn.x)?
                    .cursor_down(1)?
                    .repeat_char(ch, overdrawn.width)?;
            }
        }
        Ok(RenderResult { overdrawn })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        widget::style::{Color, Colors},
        writer::{Cell, TestWriter},
    };

    use super::*;

    #[test]
    fn color_box_renders() -> io::Result<()> {
        let mut writer = TestWriter::new([8, 8]);
        let mut widget = Filled(
            'r',
            Colors {
                fg: Color::Reset,
                bg: Color::Red,
            },
        );
        let rect = Rect2::new([1, 2], [3, 4]);
        writer.add_widget(&mut widget, rect, Some(rect))?;
        writer.assert_cells_match(
            &[
                &"        ",
                &"        ",
                &" rrr    ",
                &" rrr    ",
                &" rrr    ",
                &" rrr    ",
                &"        ",
                &"        ",
            ],
            [
                (
                    ' ',
                    Cell {
                        ch: ' ',
                        colors: Colors {
                            fg: Color::Reset,
                            bg: Color::Reset,
                        },
                    },
                ),
                (
                    'r',
                    Cell {
                        ch: 'r',
                        colors: Colors {
                            fg: Color::Reset,
                            bg: Color::Red,
                        },
                    },
                ),
            ],
        );

        Ok(())
    }
}
