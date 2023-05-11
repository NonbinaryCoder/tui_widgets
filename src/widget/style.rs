pub use crossterm::style::Color;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Colors {
    pub fg: Color,
    pub bg: Color,
}

impl Colors {
    pub fn splat(color: Color) -> Colors {
        Self {
            fg: color,
            bg: color,
        }
    }
}
