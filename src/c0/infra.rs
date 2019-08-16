use std::str::{Chars, FromStr};
use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
    fmt::Display,
    fmt::Formatter,
    hash::Hash,
    rc::{Rc, Weak},
    string::String,
};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Pos {
    pub ln: usize,
    pub pos: usize,
    pub index: usize,
}

impl Pos {
    pub fn new(ln: usize, pos: usize, index: usize) -> Pos {
        Pos { ln, pos, index }
    }

    pub fn zero() -> Pos {
        Pos {
            ln: 0,
            pos: 0,
            index: 0,
        }
    }

    #[must_use]
    #[inline]
    pub fn bump(mut self) -> Pos {
        self.index += 1;
        self
    }

    #[must_use]
    #[inline]
    pub fn inc(mut self) -> Pos {
        self.pos += 1;
        self.index += 1;
        self
    }

    #[must_use]
    #[inline]
    pub fn lf(mut self) -> Pos {
        self.pos = 0;
        self.ln += 1;
        self.index += 1;
        self
    }

    #[inline]
    pub fn inc_self(&mut self) {
        self.pos += 1;
        self.index += 1;
    }

    #[inline]
    pub fn lf_self(&mut self) {
        self.pos = 0;
        self.ln += 1;
        self.index += 1;
    }

    #[inline]
    pub fn bump_self(&mut self) {
        self.index += 1;
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "line {}, col {} ({} from start)",
            self.ln, self.pos, self.index
        )
    }
}

pub struct StringPosIter<'a> {
    chars: std::iter::Chain<Chars<'a>, std::iter::Once<char>>,
    pos: Pos,
    is_last_cr: bool,
}

impl<'a> StringPosIter<'a> {
    pub fn new(string: &'a str) -> StringPosIter<'a> {
        let chars = string.chars().chain(std::iter::once('\0'));
        StringPosIter {
            chars,
            pos: Pos::zero(),
            is_last_cr: false,
        }
    }
}

impl<'a> Iterator for StringPosIter<'a> {
    type Item = (Pos, char);
    fn next(&mut self) -> Option<Self::Item> {
        let next_char = self.chars.next();
        match next_char {
            None => None,
            Some(ch) => {
                match ch {
                    '\n' => {
                        if !self.is_last_cr {
                            self.pos.lf_self();
                        } else {
                            self.pos.bump_self();
                        }
                        self.is_last_cr = false;
                    }
                    '\r' => {
                        self.pos.lf_self();
                        self.is_last_cr = true;
                    }
                    _ => {
                        self.is_last_cr = false;
                        self.pos.inc_self();
                    }
                };
                Some((self.pos, ch))
            }
        }
    }
}

#[derive(Eq, PartialEq)]
pub struct Ptr<T>(Rc<RefCell<T>>);

impl<T> Ptr<T> {
    pub fn new(val: T) -> Ptr<T> {
        Ptr(Rc::new(RefCell::new(val)))
    }

    pub fn borrow(&self) -> Ref<T> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<T> {
        self.0.borrow_mut()
    }

    pub fn downgrade(self) -> Weak<RefCell<T>> {
        std::rc::Rc::downgrade(&self.0)
    }
}

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Ptr(self.0.clone())
    }
}

pub trait IntoPtr<T> {
    fn into_ptr(self) -> Ptr<T>;
}

impl<T> IntoPtr<T> for Rc<RefCell<T>> {
    fn into_ptr(self) -> Ptr<T> {
        Ptr(self)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Span {
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn from(start: Pos, end: Pos) -> Span {
        Span { start, end }
    }
}

pub trait AstNode {
    fn get_span(&self) -> Span;
}


pub fn variant_eq<T>(a: &T, b: &T) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

pub enum LoopCtrl<T> {
    Stop(T),
    Continue,
}

use LoopCtrl::*;


impl<T> LoopCtrl<T> {
    pub fn unwrap(self) -> T {
        match self {
            LoopCtrl::Stop(x) => x,
            _ => panic!("Cannot unwrap a LoopCtrl with Continue statement"),
        }
    }

    pub fn is_continue(&self) -> bool {
        match self {
            LoopCtrl::Continue => true,
            _ => false,
        }
    }
}

pub fn loop_while<F, T>(mut f: F) -> T
where
    F: FnMut() -> LoopCtrl<T>,
{
    let mut x: LoopCtrl<T> = Continue;
    while x.is_continue() {
        x = f();
    }
    // the following unwrap CANNOT panic because x is garanteed to be Some.
    x.unwrap()
}

#[macro_export]
macro_rules! set {
    ( $( $x:expr ),* ) => {  // Match zero or more comma delimited items
        {
            let mut temp_set = HashSet::new();  // Create a mutable HashSet
            $(
                temp_set.insert($x); // Insert each item matched into the HashSet
            )*
            temp_set // Return the populated HashSet
        }
    };
}


