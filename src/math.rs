use std::{
    fmt::{self, Debug, Display},
    ops::{Add, Mul, Sub},
};

/// A two-dimensional vector.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vec2<T> {
    pub x: T,
    pub y: T,
}

/// A position in 2d space.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos2<T> {
    pub x: T,
    pub y: T,
}

/// A two-dimensional size.
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Size2<T> {
    pub width: T,
    pub height: T,
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

    pub fn contains_pos(self, pos: impl Into<Pos2<T>>) -> bool
    where
        T: PartialOrd,
    {
        let pos = pos.into();
        pos.x < self.width && pos.y < self.height
    }

    pub fn contains_box(self, b: Box2<T>) -> bool
    where
        T: PartialOrd + Copy,
    {
        let Box2 { min, max } = b;
        self.contains_pos(min) && self.contains_pos(max)
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
vec_ops!([Vec2 op Vec2], [Pos2 op Pos2], [Pos2 op Vec2], [Size2 op Vec2], [Pos2 op Size2]);

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

/// A rectangle defined by it's top left and bottom right points
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Box2<T> {
    pub min: Pos2<T>,
    pub max: Pos2<T>,
}

impl<T> Box2<T> {
    pub fn new(min: impl Into<Pos2<T>>, max: impl Into<Pos2<T>>) -> Box2<T> {
        Self {
            min: min.into(),
            max: max.into(),
        }
    }

    pub fn translate(self, offset: impl Into<Vec2<T>>) -> Box2<T::Output>
    where
        T: Add<T> + Copy,
    {
        let offset = offset.into();
        Box2 {
            min: self.min + offset,
            max: self.max + offset,
        }
    }

    pub fn contain_pos(self, pos: impl Into<Pos2<T>>) -> Self
    where
        T: Ord + Clone,
    {
        let pos = pos.into();
        Self::new(
            [self.min.x.min(pos.x.clone()), self.min.y.min(pos.y.clone())],
            [self.max.x.max(pos.x), self.max.y.max(pos.y)],
        )
    }

    pub fn contain_box(self, b: Box2<T>) -> Self
    where
        T: Ord,
    {
        Self::new(
            [self.min.x.min(b.min.x), self.min.y.min(b.min.y)],
            [self.max.x.max(b.max.x), self.max.y.max(b.max.y)],
        )
    }

    pub fn contains_pos(self, pos: Pos2<T>) -> bool
    where
        T: PartialOrd,
    {
        self.min.x <= pos.x && self.min.y <= pos.y && self.max.x >= pos.x && self.max.y >= pos.y
    }

    pub fn contains_box(self, other: Box2<T>) -> bool
    where
        T: PartialOrd + Clone,
    {
        self.clone().contains_pos(other.min) && self.contains_pos(other.max)
    }

    pub fn intersection(self, other: Self) -> Option<Self>
    where
        T: Ord + Copy,
    {
        fn intersection<T: Ord + Copy>(a_min: T, a_max: T, b_min: T, b_max: T) -> Option<[T; 2]> {
            if a_max < b_min || b_max < a_min {
                None
            } else {
                Some([a_min.max(b_min), a_max.min(b_max)])
            }
        }
        let [min_x, max_x] = intersection(self.min.x, self.max.x, other.min.x, other.max.x)?;
        let [min_y, max_y] = intersection(self.min.y, self.max.y, other.min.y, other.max.y)?;
        Some(Self::new([min_x, min_y], [max_x, max_y]))
    }
}

impl<T: Clone> From<Pos2<T>> for Box2<T> {
    fn from(value: Pos2<T>) -> Self {
        Self {
            min: value.clone(),
            max: value,
        }
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
        let size = Size2::new(5, 3);
        let contained = ["yyyyynn", "yyyyynn", "yyyyynn", "nnnnnnn", "nnnnnnn"];
        for (y, row) in contained.into_iter().enumerate() {
            for (x, ch) in row.chars().enumerate() {
                match ch {
                    'y' => assert!(size.contains_pos([x, y])),
                    'n' => assert!(!size.contains_pos([x, y])),
                    _ => unreachable!(),
                }
            }
        }
    }

    #[test]
    fn box_contain_pos() {
        let b = Box2::from(Pos2::new(4u16, 8));
        assert_eq!(b.contain_pos([2, 1]), Box2::new([2, 1], [4, 8]));
        assert_eq!(b.contain_pos([200, 40]), Box2::new([4, 8], [200, 40]));
    }

    #[test]
    fn box_intersection() {
        let b = Box2::new([4u16, 5], [8, 12]);
        assert_eq!(
            b.intersection(Box2::new([0, 0], [6, 6])),
            Some(Box2::new([4, 5], [6, 6])),
        );
        assert_eq!(
            b.intersection(Box2::new([6, 0], [12, 6])),
            Some(Box2::new([6, 5], [8, 6])),
        );
        assert_eq!(
            b.intersection(Box2::new([0, 7], [6, 13])),
            Some(Box2::new([4, 7], [6, 12])),
        );
        assert_eq!(
            b.intersection(Box2::new([6, 7], [12, 13])),
            Some(Box2::new([6, 7], [8, 12])),
        );
        assert_eq!(
            b.intersection(Box2::new([5, 6], [7, 11])),
            Some(Box2::new([5, 6], [7, 11])),
        );
    }
}
