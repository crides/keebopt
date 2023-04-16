#![allow(dead_code)]
use std::ops::{Index, IndexMut};

#[derive(Clone, Debug)]
pub struct CharMap<T>(Vec<Option<T>>);

#[derive(Clone, Debug)]
pub struct RawCharMap<T>(Vec<T>);

#[inline]
fn clamp_ind(index: usize) -> usize {
    assert!(index > CharMap::<()>::OFFSET as usize, "index: {}", index);
    let offed = index - CharMap::<()>::OFFSET as usize;
    assert!(offed < CharMap::<()>::CAP, "orig: {}, offed: {}", index, offed);
    offed
}

#[inline]
fn char2ind(index: char) -> usize {
    clamp_ind(index as usize)
}

#[inline]
fn ind2char(ind: usize) -> char {
    (ind as u8 + CharMap::<()>::OFFSET as u8) as char
}

impl<T: Sized + Clone> CharMap<T> {
    pub const CAP: usize = 95;
    pub const OFFSET: char = ' ';
    pub fn new() -> Self {
        Self(vec![None; CharMap::<()>::CAP])
    }

    fn index(&self, index: usize) -> &T {
        self.0[clamp_ind(index)].as_ref().expect("no element")
    }

    fn index_mut(&mut self, index: usize) -> &mut T {
        self.0[clamp_ind(index)].as_mut().expect("no element")
    }

    pub fn get(&self, index: char) -> Option<&T> {
        self.0[char2ind(index)].as_ref()
    }

    pub fn set(&mut self, index: char, val: T) {
        self.0[char2ind(index)] = Some(val);
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.0.iter().filter_map(|v| v.as_ref())
    }

    pub fn keys(&self) -> impl Iterator<Item = char> {
        let interm: Vec<_> = self.0.iter().enumerate().filter_map(|(i, v)| v.as_ref().and_then(|_| Some(ind2char(i)))).collect();
        interm.into_iter()
    }

    pub fn iter(&self) -> impl Iterator<Item = (char, &T)> {
        self.0.iter().enumerate().filter_map(|(i, v)| v.as_ref().map(|v| (ind2char(i), v)))
    }
}

impl<T: Sized + Clone + Default> RawCharMap<T> {
    pub fn new() -> Self {
        Self(vec![Default::default(); CharMap::<()>::CAP])
    }
}

impl<T: Sized + Clone> RawCharMap<T> {
    fn index(&self, index: usize) -> &T {
        &self.0[clamp_ind(index)]
    }

    fn index_mut(&mut self, index: usize) -> &mut T {
        &mut self.0[clamp_ind(index)]
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }

    pub fn keys(&self) -> impl Iterator<Item = char> {
        (0..self.0.len()).map(ind2char)
    }

    pub fn iter(&self) -> impl Iterator<Item = (char, &T)> {
        self.0.iter().enumerate().map(|(i, v)| (ind2char(i), v))
    }
}

macro_rules! impl_index {
    ($ty:ident) => {
        impl<T: Sized + Clone> Index<$ty> for CharMap<T> {
            type Output = T;
            fn index(&self, index: $ty) -> &Self::Output {
                self.index(index as usize)
            }
        }

        impl<T: Sized + Clone> IndexMut<$ty> for CharMap<T> {
            fn index_mut(&mut self, index: $ty) -> &mut Self::Output {
                self.index_mut(index as usize)
            }
        }

        impl<T: Sized + Clone> Index<$ty> for RawCharMap<T> {
            type Output = T;
            fn index(&self, index: $ty) -> &Self::Output {
                self.index(index as usize)
            }
        }

        impl<T: Sized + Clone> IndexMut<$ty> for RawCharMap<T> {
            fn index_mut(&mut self, index: $ty) -> &mut Self::Output {
                self.index_mut(index as usize)
            }
        }
    };
}

impl_index!(char);
impl_index!(usize);
