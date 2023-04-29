/// A widget in a tui app, such as text.
pub trait Widget {}

impl Widget for Box<dyn Widget> {}

impl Widget for () {}
