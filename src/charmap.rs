#![allow(dead_code)]
use std::ops::{Index, IndexMut};

#[derive(Clone, Debug)]
pub struct FlatCharMap<T>(Vec<Option<T>>);

#[inline]
fn clamp_ind(index: usize) -> usize {
    assert!(index > FlatCharMap::<()>::OFFSET as usize, "index: {}", index);
    let offed = index - FlatCharMap::<()>::OFFSET as usize;
    assert!(offed < FlatCharMap::<()>::CAP, "orig: {}, offed: {}", index, offed);
    offed
}

#[inline]
fn char2ind(index: char) -> usize {
    clamp_ind(index as usize)
}

#[inline]
fn ind2char(ind: usize) -> char {
    (ind as u8 + FlatCharMap::<()>::OFFSET as u8) as char
}

impl<T: Sized + Clone> FlatCharMap<T> {
    pub const CAP: usize = 95;
    pub const OFFSET: char = ' ';
    pub fn new() -> Self {
        Self(vec![None; FlatCharMap::<()>::CAP])
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

macro_rules! impl_index {
    ($ty:ident) => {
impl<T: Sized + Clone> Index<$ty> for FlatCharMap<T> {
    type Output = T;
    fn index(&self, index: $ty) -> &Self::Output {
        self.index(index as usize)
    }
}

impl<T: Sized + Clone> IndexMut<$ty> for FlatCharMap<T> {
    fn index_mut(&mut self, index: $ty) -> &mut Self::Output {
        self.index_mut(index as usize)
    }
}
    };
}

impl_index!(char);
impl_index!(usize);
impl_index!(u8);
