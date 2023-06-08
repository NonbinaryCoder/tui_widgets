use std::collections::VecDeque;

use crate::terminal::Window;

use super::AreaFillingWidget;

/// A collection of area filling widgets.
///
/// Implemented for `[W; N]`, `[W]`, [`Vec<W>`], and [`VecDeque<W>`] where W implements
/// [`AreaFillingWidget`]; and for tuples of up to 12 elements where all elements
/// implement [`AreaFillingWidget`].
pub trait AreaFillingWidgetCollection {
    fn len(&self) -> usize;

    fn render(&mut self, index: usize, terminal: Window);
}

impl<W, const N: usize> AreaFillingWidgetCollection for [W; N]
where
    W: AreaFillingWidget,
{
    fn len(&self) -> usize {
        N
    }

    fn render(&mut self, index: usize, term: Window) {
        self[index].render(term)
    }
}

impl<W> AreaFillingWidgetCollection for [W]
where
    W: AreaFillingWidget,
{
    fn len(&self) -> usize {
        self.len()
    }

    fn render(&mut self, index: usize, term: Window) {
        self[index].render(term)
    }
}

impl<W> AreaFillingWidgetCollection for Vec<W>
where
    W: AreaFillingWidget,
{
    fn len(&self) -> usize {
        self.len()
    }

    fn render(&mut self, index: usize, term: Window) {
        self[index].render(term)
    }
}

impl<W> AreaFillingWidgetCollection for VecDeque<W>
where
    W: AreaFillingWidget,
{
    fn len(&self) -> usize {
        self.len()
    }

    fn render(&mut self, index: usize, term: Window) {
        self[index].render(term)
    }
}

macro_rules! replace_expr {
    ($_:tt, $sub:expr) => {
        $sub
    };
}

macro_rules! impl_tuple {
    ($( $t:ident ),+) => {
        impl<$( $t ),+> AreaFillingWidgetCollection for ($( $t ),+,)
        where
            $(
                $t: AreaFillingWidget,
            )+
        {
            fn len(&self) -> usize {
                [$( replace_expr!($t, ()) ),+].len()
            }

            fn render(&mut self, index: usize, term: Window) {
                #[allow(non_snake_case)]
                let ($( $t ),+,) = self;
                let n = 0;
                $(
                    if index == replace_expr!($t, n) {
                        $t.render(term);
                        return
                    }
                    let n = n + 1;
                )+
                _ = n;
                panic!("Attempt to render element {index} of {} element tuple", self.len());
            }
        }
    }
}

impl_tuple!(A);
impl_tuple!(A, B);
impl_tuple!(A, B, C);
impl_tuple!(A, B, C, D);
impl_tuple!(A, B, C, D, E);
impl_tuple!(A, B, C, D, E, F);
impl_tuple!(A, B, C, D, E, F, G);
impl_tuple!(A, B, C, D, E, F, G, H);
impl_tuple!(A, B, C, D, E, F, G, H, I);
impl_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
