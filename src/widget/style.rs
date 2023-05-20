pub use crossterm::style::Color;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Colors {
    pub fg: Color,
    pub bg: Color,
}

impl Default for Colors {
    fn default() -> Self {
        Self {
            fg: Color::Reset,
            bg: Color::Reset,
        }
    }
}

impl Colors {
    pub fn splat(color: Color) -> Colors {
        Self {
            fg: color,
            bg: color,
        }
    }

    pub fn fg(self, fg: Color) -> Self {
        Self { fg, bg: self.bg }
    }

    pub fn bg(self, bg: Color) -> Self {
        Self { fg: self.fg, bg }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct Formatting {
    pub colors: Colors,
}

impl From<()> for Formatting {
    fn from((): ()) -> Self {
        Self::default()
    }
}

impl From<Colors> for Formatting {
    fn from(colors: Colors) -> Self {
        Self {
            colors,
            ..Self::default()
        }
    }
}

impl Formatting {
    pub fn colors(self, colors: Colors) -> Self {
        Self { colors, ..self }
    }

    pub fn fg(self, fg: Color) -> Self {
        self.colors(self.colors.fg(fg))
    }

    pub fn bg(self, bg: Color) -> Self {
        self.colors(self.colors.bg(bg))
    }
}
