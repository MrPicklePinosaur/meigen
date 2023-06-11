
# 迷言語 design spec

## Outline

- Complied functional programming language
- llvm?
- type checked
- pure?
- doesn't have to be super useful (isoteric PL is fine too)

## Goals

- learn about functional programming language compiler design
    - parsing
    - optimization
- get good at haskell

## Language features

Uses primarily kanji characters

literals, can be rust/zig as reference
- integers of different sizes: u8, u16, i8, i16, usize, isize etc
- floats
- boolean
- strings, chars

standard library
- collections: vec, map etc
- io

first class functions

stretch goals
- LSP
- debugger/stepper

interesting ideas
- rate the quality of code after (gramatical-ness, consistency (ie same politeness))

## Syntax

maybe inspired by haku?
- TODO investigate APL

functions:
```
[id]は
...
のことです。
```

strongly typed (how to do type annotations?):
```
[type]として
```

match statements


let bindings


(arithmetic) expressions


comparison operations
```
[A]より[B]  (a < b)
[A] [B]
[A] [B]
[A] [B]
[A] [B]
```

function application


### literals

integers, can use either kanji or literal roman numbers
```
1207
千二百七
```

```
12.56
十二点五割六ぶ
```

fractions?
```
2/3
三分の二
```

Strings
```
"hello world"
「hello world」
```

