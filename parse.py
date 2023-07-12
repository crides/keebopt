from collections import defaultdict
from parsec import *
from dataclasses import dataclass
import json

@dataclass(unsafe_hash=True)
class Key:
    key: str

@dataclass(unsafe_hash=True)
class Mod:
    release: bool
    mod: str

@dataclass(unsafe_hash=True)
class Char:
    char: str

@dataclass(unsafe_hash=True)
class Unknown:
    code: int

@dataclass(unsafe_hash=True)
class Repeat:
    num: int

Event = Key | Mod | Char | Repeat | Unknown

def parse(f: str) -> list[Event]:
    mod_name = string("Alt") | string("Shft") | string("Ctrl") | string("Meta")
    mod = (string("<") >> optional(string("/")) + (one_of("LR") >> mod_name << string(">"))).parsecmap(lambda a: Mod(bool(a[0]), a[1]))
    key = (string("<") >> parsecmap(many1(letter()), "".join) << string(">")).parsecmap(Key)
    repeat = (string("<#+") >> parsecmap(many1(digit()), lambda l: int("".join(l))) << string(">")).parsecmap(Repeat)
    unknown = (string("<E-") >> parsecmap(many1(digit() | one_of("abcdef")), lambda l: int("".join(l), 16)) << string(">")).parsecmap(Unknown)
    char = none_of("\n\t").parsecmap(lambda c: Char(c.lower()))
    keys = many(mod ^ key ^ repeat ^ unknown ^ char)
    line = regex(r"\d+-\d+-\d+ \d+:\d+:\d+-\d+") >> string(" > ") >> keys << optional(string("\n"))

    return [u for s in open(f).readlines() if s.startswith("20") for u in line.parse(s)]

def gram_len(k: Event) -> int:
    match k:
        case Char(c):
            return 4 if c.isalpha() else 2
        case Key(_):
            return 2
        case _:
            raise ValueError()

def gram(evs: list[Event], length: int = 3) -> list[tuple[tuple[Event, ...], int]]:
    keys = [i for i in evs if isinstance(i, (Char)) and i.char != " "]
    # json.dump([c.char for c in keys], open("keys.json", "w"))
    hists = [[] for _ in range(length - 2 + 1)]
    grams: defaultdict[tuple[Event, ...], int] = defaultdict(int)
    for key in keys:
        unaffected = gram_len(key) - 2 + 1
        for i in range(unaffected, length - 2 + 1):
            hists[i] = []
        for i, hist in enumerate(hists[:unaffected]):
            hist.append(key)
            if len(hist) > i + 2:
                hist.pop(0)
        for i, hist in enumerate(hists):
            if len(hist) == i + 2:
                grams[tuple(hist)] += 1
    return list(grams.items())

def dump(f: str, grams: list[tuple[tuple[Event, ...], int]]):
    def ser_ev(e: Event) -> str:
        match e:
            case Char(c):
                return c
            # case Key(k):
            #     return "<" + k + ">"
            case _:
                raise ValueError()
    ser = [["".join(ser_ev(e) for e in evs), v] for evs, v in grams]
    with open(f, "w") as out:
        json.dump(ser, out)

if __name__ == "__main__":
    # import sys
    # # print([i for i in parse(sys.argv[1]) if isinstance(i, (Key, Char))])
    # p = parse(sys.argv[1])
    # letters = defaultdict(int)
    # for c in (k.char.lower() for k in parse(sys.argv[1]) if isinstance(k, Char)):
    #     letters[c] += 1
    # import pprint
    # pprint.pp(dict(letters))
    # dump("grams.json", gram(p, 2))

    grams = json.load(open("grams.json"))
    letters = defaultdict(int)
    for s, c in grams:
        for c in s:
            letters[c] += 1
    print(dict(letters))
