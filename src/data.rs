#![allow(dead_code)]
use std::collections::BTreeMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, digit1, none_of, one_of, u32},
    combinator::{eof, map, opt, value},
    multi::many0,
    sequence::{delimited, tuple},
    Finish,
};

type ParseResult<'a, O> = nom::IResult<&'a str, O>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ModName {
    Shift,
    Ctrl,
    Alt,
    Super,
    AltGr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModEvent {
    name: ModName,
    left: bool,
    press: bool,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Key {
    Char(char),
    Key(u32),
    Special(String),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum RawEvent {
    Key(Key),
    Mod(ModEvent),
}

macro_rules! parsers {
    ($($name:ident: $ret:ty = $body:expr)*) => {
        $(fn $name(s: &str) -> ParseResult<$ret> { $body(s) })*
    };
}

parsers! {
    mod_name: ModName = alt((
        value(ModName::Alt, tag("Alt")),
        value(ModName::Shift, tag("Shft")),
        value(ModName::Super, tag("Meta")),
        value(ModName::Ctrl, tag("Ctrl")),
    ))

    mod_ev: ModEvent = map(delimited(char('<'), tuple((opt(char('/')), one_of("LR"), mod_name)), char('>')), |(r, lr, name)| {
        ModEvent { press: r.is_none(), left: lr == 'L', name }
    })

    key: Key = alt((
        map(delimited(char('<'), alpha1, char('>')), |s: &str| Key::Special(s.into())),
    ))

    repeat: usize = map(delimited(tag("<#+"), u32, char('>')), |i| i as usize)
    any: Key = map(none_of("\n\t"), |c| Key::Char(c))
    keys: Vec<RawEvent> = many0(alt((
        map(key, RawEvent::Key),
        map(mod_ev, RawEvent::Mod),
        map(any, RawEvent::Key),
    )))

    line: Vec<RawEvent> = delimited(tuple((digit1, char('-'), digit1, char('-'), digit1, char(' '), digit1, char(':'), digit1, char(':'), digit1, one_of("+-"), digit1, tag(" > "))), keys, eof)
}

#[derive(Clone, Debug)]
struct Mod {
    name: ModName,
    left: bool,
}

#[derive(Clone, Debug)]
enum SingleEvent {
    Key(Key),
    Moded(Vec<Mod>, Key),
}

#[derive(Clone, Debug)]
enum Event {
    Single(SingleEvent),
    Repeated(SingleEvent, usize),
}

pub fn parse_logkeys(file: impl AsRef<Path>) -> Vec<RawEvent> {
    BufReader::new(File::open(file.as_ref()).expect("input file"))
        .lines()
        .filter_map(|l| {
            let l = l.unwrap();
            l.starts_with("20").then(|| line(&l).finish().unwrap().1)
        })
        .flatten()
        .collect()
}

pub fn gram_chars(evs: impl Iterator<Item = RawEvent>, len: usize) -> BTreeMap<String, usize> {
    let mut grams = BTreeMap::new();
    let keys: Vec<_> = evs
        .filter_map(|e| {
            if let RawEvent::Key(Key::Char(c)) = e {
                Some(c)
            } else {
                None
            }
        })
        .collect();
    dbg!(keys.len());
    for win in keys.windows(len) {
        grams
            .entry(win.iter().copied().collect())
            .and_modify(|c| *c += 1)
            .or_insert(1);
    }
    dbg!(grams.len());
    grams
}

#[test]
fn simple() {
    assert_eq!(
        line("2023-02-27 22:27:37-0600 > e /var/lo<Tab><Tab><Tab><Tab>/<Tab>lo<Tab>")
            .finish()
            .unwrap()
            .1
            .len(),
        18
    );
}
