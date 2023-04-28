pub trait Widget {}

impl Widget for Box<dyn Widget> {}

impl Widget for () {}
