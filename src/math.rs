use std::ops::{Add, Sub};

/// A two-dimensional vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vec2<T> {
    x: T,
    y: T,
}

/// A position in 2d space.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos2<T> {
    x: T,
    y: T,
}

/// A two-dimensional size.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Size2<T> {
    width: T,
    height: T,
}

impl<T> Vec2<T> {
    pub fn new(x: T, y: T) -> Self {
        Self { x, y }
    }

    /// \[x, y\].
    pub fn decompose(self) -> [T; 2] {
        [self.x, self.y]
    }

    pub fn to_pos(self) -> Pos2<T> {
        self.into()
    }

    /// x -> width,
    /// y -> height.
    pub fn to_size(self) -> Size2<T> {
        self.into()
    }
}

impl<T> Pos2<T> {
    pub fn new(x: T, y: T) -> Self {
        Self { x, y }
    }

    /// \[x, y\].
    pub fn decompose(self) -> [T; 2] {
        [self.x, self.y]
    }

    pub fn to_vec(self) -> Vec2<T> {
        self.decompose().into()
    }
}

impl<T> Size2<T> {
    pub fn new(width: T, height: T) -> Self {
        Self { width, height }
    }

    /// \[width, height\].
    pub fn decompose(self) -> [T; 2] {
        [self.width, self.height]
    }

    /// width -> x,
    /// height -> y.
    pub fn to_vec(self) -> Vec2<T> {
        self.into()
    }
}

macro_rules! vec_ops {
    ($([$lhs:ident op $rhs:ident]),+) => {
        $(
            vec_ops_inner!($lhs, $rhs, [Add, add], [Sub, sub]);
        )+
    }
}
macro_rules! vec_ops_inner {
    ($lhs:ident, $rhs:ident, $([$trait:ident, $fn:ident]),+) => {
        $(
            impl<T: $trait<U>, U> $trait<$rhs<U>> for $lhs<T> {
                type Output = $lhs<T::Output>;

                fn $fn(self, rhs: $rhs<U>) -> Self::Output {
                    let [x1, y1] = self.decompose();
                    let [x2, y2] = rhs.decompose();
                    $lhs::new(x1.$fn(x2), y1.$fn(y2))
                }
            }
        )+
    };
}
vec_ops!([Vec2 op Vec2], [Pos2 op Vec2], [Size2 op Vec2], [Pos2 op Size2]);

macro_rules! conversions {
    ($ty:ident into $($from:ident),+) => {
        impl<T> From<(T, T)> for $ty<T> {
            fn from((x, y): (T, T)) -> Self {
                Self::new(x, y)
            }
        }

        impl<T> From<[T; 2]> for $ty<T> {
            fn from([x, y]: [T; 2]) -> Self {
                Self::new(x, y)
            }
        }

        $(
            impl<T> From<$from<T>> for $ty<T> {
                fn from(value: $from<T>) -> Self {
                    value.decompose().into()
                }
            }
        )+
    }
}
conversions!(Vec2 into Pos2, Size2);
conversions!(Pos2 into Vec2);
conversions!(Size2 into Vec2);

/// A rectangle defined by it's top left point and it's extents
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Rect2<T> {
    x: T,
    y: T,
    width: T,
    height: T,
}

impl<T> Rect2<T> {
    pub fn new(pos: Pos2<T>, size: Size2<T>) -> Self {
        Self {
            x: pos.x,
            y: pos.y,
            width: size.width,
            height: size.height,
        }
    }

    pub fn pos(self) -> Pos2<T> {
        Pos2::new(self.x, self.y)
    }

    pub fn size(self) -> Size2<T> {
        Size2::new(self.width, self.height)
    }

    pub fn decompose(self) -> (Pos2<T>, Size2<T>) {
        (
            Pos2::new(self.x, self.y),
            Size2::new(self.width, self.height),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vec2_ops() {
        assert_eq!(Vec2::new(4, 2) + Vec2::new(3, 1), Vec2::new(7, 3));
        assert_eq!(Pos2::new(4, 2) + Vec2::new(3, 1), Pos2::new(7, 3));
        assert_eq!(Size2::new(4, 2) + Vec2::new(3, 1), Size2::new(7, 3));
        assert_eq!(Pos2::new(4, 2) + Size2::new(3, 1), Pos2::new(7, 3));
    }

    #[test]
    fn vec2_conversions() {
        assert_eq!(Vec2::new(4, 2), [4, 2].into());
        assert_eq!(Vec2::new(4, 2), (4, 2).into());
        assert_eq!(Vec2::new(4, 2), Pos2::new(4, 2).into());
        assert_eq!(Vec2::new(4, 2), Size2::new(4, 2).into());

        assert_eq!(Pos2::new(4, 2), [4, 2].into());
        assert_eq!(Pos2::new(4, 2), (4, 2).into());
        assert_eq!(Pos2::new(4, 2), Vec2::new(4, 2).into());

        assert_eq!(Size2::new(4, 2), [4, 2].into());
        assert_eq!(Size2::new(4, 2), (4, 2).into());
        assert_eq!(Size2::new(4, 2), Vec2::new(4, 2).into());
    }
}
