use std::{
    fmt::{self, Debug, Display},
    ops::{Add, Mul, Sub},
};

/// A two-dimensional vector.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vec2<T> {
    x: T,
    y: T,
}

/// A position in 2d space.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos2<T> {
    x: T,
    y: T,
}

/// A two-dimensional size.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Size2<T> {
    width: T,
    height: T,
}

macro_rules! common_methods {
    ($ty:ident, $x:ident, $y:ident) => {
        pub const fn new($x: T, $y: T) -> Self {
            Self { $x, $y }
        }

        pub fn splat(v: T) -> Self
        where
            T: Clone,
        {
            Self {
                $y: v.clone(),
                $x: v,
            }
        }

        pub const fn as_ref(&self) -> $ty<&T> {
            $ty {
                $x: &self.$x,
                $y: &self.$y,
            }
        }

        pub fn decompose(self) -> [T; 2] {
            [self.$x, self.$y]
        }

        pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> $ty<U> {
            $ty {
                $x: f(self.$x),
                $y: f(self.$y),
            }
        }

        /// Converts each field in `self` to `U` with [`Into::into()`].
        pub fn map_into<U: From<T>>(self) -> $ty<U> {
            $ty {
                $x: self.$x.into(),
                $y: self.$y.into(),
            }
        }
    };
}

impl<T> Vec2<T> {
    common_methods!(Vec2, x, y);

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
    common_methods!(Pos2, x, y);

    pub fn to_vec(self) -> Vec2<T> {
        self.decompose().into()
    }
}

impl<T> Size2<T> {
    common_methods!(Size2, width, height);

    /// width -> x,
    /// height -> y.
    pub fn to_vec(self) -> Vec2<T> {
        self.into()
    }

    pub fn area(self) -> T::Output
    where
        T: Mul,
    {
        self.width * self.height
    }

    /// Returns `true` if `self` contains `rect`.  `rect` is considered
    /// contained even if it touches the edge of `self`.
    pub fn contains_rect(self, rect: Rect2<T>) -> bool
    where
        T: PartialOrd + Add<Output = T>,
    {
        rect.x <= self.width
            && rect.y < self.height
            && (rect.x + rect.width) <= self.width
            && (rect.y + rect.height) <= self.height
    }

    /// Returns `true` if `self` contains `pos`.  `pos` is considered contained
    /// even if it touches the edge of `self`.
    pub fn contains_pos(self, pos: impl Into<Pos2<T>>) -> bool
    where
        T: PartialOrd,
    {
        let pos = pos.into();
        pos.x <= self.width && pos.y <= self.height
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

macro_rules! formatting {
    ($($ty:ident: $a:expr, $b:expr, $c:expr),+) => {
        $(
            impl<T: Debug> Debug for $ty<T> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    let [x, y] = self.as_ref().decompose();
                    write!(f, concat!($a, "{:?}", $b, "{:?}", $c), x, y)
                }
            }

            impl<T: Display> Display for $ty<T> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    let [x, y] = self.as_ref().decompose();
                    write!(f, concat!($a, "{}", $b, "{}", $c), x, y)
                }
            }
        )+
    };
}
formatting!(
    Vec2: "(", ", ", ")",
    Pos2: "(", ", ", ")",
    Size2: "", "x", ""
);

/// A rectangle defined by it's top left point and it's extents
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Rect2<T> {
    x: T,
    y: T,
    width: T,
    height: T,
}

impl<T> Rect2<T> {
    pub fn new(pos: impl Into<Pos2<T>>, size: impl Into<Size2<T>>) -> Self {
        let (pos, size) = (pos.into(), size.into());
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

    /// (top left, bottom right).
    pub fn corners(self) -> (Pos2<T>, Pos2<T>)
    where
        T: Add<Output = T> + Clone,
    {
        let (pos, size) = self.decompose();
        (pos.clone(), pos + size)
    }

    pub const fn as_ref(&self) -> Rect2<&T> {
        Rect2 {
            x: &self.x,
            y: &self.y,
            width: &self.width,
            height: &self.height,
        }
    }

    pub fn translate(self, vec: impl Into<Vec2<T>>) -> Self
    where
        T: Add<Output = T>,
    {
        let (pos, size) = self.decompose();
        Self::new(pos + vec.into(), size)
    }

    /// Returns `true` if `self` contains `rect`.  `rect` is considered
    /// contained even if it touches the edge of `self`.
    pub fn contains_rect(self, rect: Rect2<T>) -> bool
    where
        T: PartialOrd + Add<Output = T> + Clone,
    {
        let (tl, br) = rect.corners();
        self.clone().contains_pos(tl) && self.contains_pos(br)
    }

    /// Returns `true` if `self` contains `pos`.  `pos` is considered contained
    /// even if it touches the edge of `self`.
    pub fn contains_pos(self, pos: impl Into<Pos2<T>>) -> bool
    where
        T: PartialOrd + Add<Output = T>,
    {
        let pos = pos.into();
        pos.x >= self.x
            && pos.y >= self.y
            && pos.x <= (self.x + self.width)
            && pos.y <= (self.y + self.height)
    }
}

impl<T: Debug> Debug for Rect2<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (pos, size) = self.as_ref().decompose();
        f.debug_struct(stringify!(Rect2))
            .field("pos", &pos)
            .field("size", &size)
            .finish()
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

    #[test]
    fn size_contains_pos() {
        assert!(Size2::new(4u16, 2).contains_pos([0, 0]));
        assert!(Size2::new(4u16, 2).contains_pos([4, 0]));
        assert!(Size2::new(4u16, 2).contains_pos([0, 2]));
        assert!(Size2::new(4u16, 2).contains_pos([4, 2]));

        assert!(Size2::new(4u16, 2).contains_pos([1, 1]));

        assert!(!Size2::new(4u16, 2).contains_pos([5, 3]));
        assert!(!Size2::new(4u16, 2).contains_pos([5, 0]));
        assert!(!Size2::new(4u16, 2).contains_pos([0, 3]));
    }

    #[test]
    fn size_contains_rect() {
        assert!(Size2::new(6u16, 4).contains_rect(Rect2::new([0, 0], [6, 4])));
        assert!(Size2::new(6u16, 4).contains_rect(Rect2::new([3, 0], [3, 4])));
        assert!(Size2::new(6u16, 4).contains_rect(Rect2::new([0, 2], [4, 2])));
        assert!(Size2::new(6u16, 4).contains_rect(Rect2::new([3, 2], [3, 2])));

        assert!(Size2::new(6u16, 4).contains_rect(Rect2::new([1, 1], [1, 1])));

        assert!(!Size2::new(6u16, 4).contains_rect(Rect2::new([1, 1], [6, 4])));
        assert!(!Size2::new(6u16, 4).contains_rect(Rect2::new([1, 1], [6, 1])));
        assert!(!Size2::new(6u16, 4).contains_rect(Rect2::new([1, 1], [1, 4])));

        assert!(!Size2::new(6u16, 4).contains_rect(Rect2::new([7, 5], [1, 1])));
        assert!(!Size2::new(6u16, 4).contains_rect(Rect2::new([1, 5], [1, 1])));
        assert!(!Size2::new(6u16, 4).contains_rect(Rect2::new([7, 1], [1, 1])));
    }

    #[test]
    fn rect_contains_pos() {
        assert!(Rect2::new([4u16, 2], [6, 4]).contains_pos([4, 2]));
        assert!(Rect2::new([4u16, 2], [6, 4]).contains_pos([10, 2]));
        assert!(Rect2::new([4u16, 2], [6, 4]).contains_pos([4, 6]));
        assert!(Rect2::new([4u16, 2], [6, 4]).contains_pos([10, 6]));

        assert!(Rect2::new([4u16, 2], [6, 4]).contains_pos([8, 4]));

        assert!(!Rect2::new([4u16, 2], [6, 4]).contains_pos([3, 1]));
        assert!(!Rect2::new([4u16, 2], [6, 4]).contains_pos([11, 1]));
        assert!(!Rect2::new([4u16, 2], [6, 4]).contains_pos([3, 7]));
        assert!(!Rect2::new([4u16, 2], [6, 4]).contains_pos([11, 7]));

        assert!(!Rect2::new([4u16, 2], [6, 4]).contains_pos([3, 4]));
        assert!(!Rect2::new([4u16, 2], [6, 4]).contains_pos([11, 4]));
        assert!(!Rect2::new([4u16, 2], [6, 4]).contains_pos([8, 1]));
        assert!(!Rect2::new([4u16, 2], [6, 4]).contains_pos([8, 7]));
    }
}
