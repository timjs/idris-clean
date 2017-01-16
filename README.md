# Idris to Clean backend

A priliminary backend for [Idris](http://www.idris-lang.org/) that compiles to [Clean](http://clean.cs.ru.nl/).

## Example

```
$ cat pythagoras.idr
module Main

pythagoras : Int -> List (Int, Int, Int)
pythagoras max = [
    (x, y, z)
    | z <- [1..max]
    , y <- [1..z]
    , x <- [1..y]
    , x * x + y *y == z * z
  ]

main : IO ()
main = do
  [_, n] <- getArgs
  printLn $ pythagoras (cast n)
$ idris --codegen clean pythagoras.idr -o pythagoras.icl
$ clm -b -I ../Libraries/ pythagoras
Compiling pythagoras
Generating code for pythagoras
Linking pythagoras
$ ./a.out 300
[(3, (4, 5)), (6, (8, 10)), (5, (12, 13)), (9, (12, 15)), (8, (15, 17)), (12, (16, 20)), (15, (20, 25)), (7, (24, 25)), (10, (24, 26)), (20, (21, 29)), (18, (24, 30)), (16, (30, 34)), (21, (28, 35)), (12, (35, 37)), (15, (36, 39)), (24, (32, 40)), (9, (40, 41)), (27, (36, 45)), (30, (40, 50)), (14, (48, 50)), (24, (45, 51)), (20, (48, 52)), (28, (45, 53)), (33, (44, 55)), (40, (42, 58)), (36, (48, 60)), (11, (60, 61)), (39, (52, 65)), (33, (56, 65)), (25, (60, 65)), (16, (63, 65)), (32, (60, 68)), (42, (56, 70)), (48, (55, 73)), (24, (70, 74)), (45, (60, 75)), (21, (72, 75)), (30, (72, 78)), (48, (64, 80)), (18, (80, 82)), (51, (68, 85)), (40, (75, 85)), (36, (77, 85)), (13, (84, 85)), (60, (63, 87)), (39, (80, 89)), (54, (72, 90)), (35, (84, 91)), (57, (76, 95)), (65, (72, 97)), (60, (80, 100)), (28, (96, 100)), (20, (99, 101)), (48, (90, 102)), (40, (96, 104)), (63, (84, 105)), (56, (90, 106)), (60, (91, 109)), (66, (88, 110)), (36, (105, 111)), (15, (112, 113)), (69, (92, 115)), (80, (84, 116)), (45, (108, 117)), (56, (105, 119)), (72, (96, 120)), (22, (120, 122)), (27, (120, 123)), (75, (100, 125)), (44, (117, 125)), (35, (120, 125)), (78, (104, 130)), (66, (112, 130)), (50, (120, 130)), (32, (126, 130)), (81, (108, 135)), (64, (120, 136)), (88, (105, 137)), (84, (112, 140)), (55, (132, 143)), (100, (105, 145)), (87, (116, 145)), (24, (143, 145)), (17, (144, 145)), (96, (110, 146)), (48, (140, 148)), (51, (140, 149)), (90, (120, 150)), (42, (144, 150)), (72, (135, 153)), (93, (124, 155)), (60, (144, 156)), (85, (132, 157)), (84, (135, 159)), (96, (128, 160)), (36, (160, 164)), (99, (132, 165)), (119, (120, 169)), (65, (156, 169)), (102, (136, 170)), (80, (150, 170)), (72, (154, 170)), (26, (168, 170)), (52, (165, 173)), (120, (126, 174)), (105, (140, 175)), (49, (168, 175)), (78, (160, 178)), (108, (144, 180)), (19, (180, 181)), (70, (168, 182)), (33, (180, 183)), (111, (148, 185)), (104, (153, 185)), (60, (175, 185)), (57, (176, 185)), (88, (165, 187)), (114, (152, 190)), (95, (168, 193)), (130, (144, 194)), (117, (156, 195)), (99, (168, 195)), (75, (180, 195)), (48, (189, 195)), (28, (195, 197)), (120, (160, 200)), (56, (192, 200)), (40, (198, 202)), (140, (147, 203)), (96, (180, 204)), (133, (156, 205)), (123, (164, 205)), (84, (187, 205)), (45, (200, 205)), (80, (192, 208)), (126, (168, 210)), (112, (180, 212)), (129, (172, 215)), (120, (182, 218)), (144, (165, 219)), (132, (176, 220)), (140, (171, 221)), (104, (195, 221)), (85, (204, 221)), (21, (220, 221)), (72, (210, 222)), (135, (180, 225)), (63, (216, 225)), (30, (224, 226)), (60, (221, 229)), (138, (184, 230)), (160, (168, 232)), (105, (208, 233)), (90, (216, 234)), (141, (188, 235)), (112, (210, 238)), (144, (192, 240)), (120, (209, 241)), (44, (240, 244)), (147, (196, 245)), (54, (240, 246)), (95, (228, 247)), (150, (200, 250)), (88, (234, 250)), (70, (240, 250)), (153, (204, 255)), (120, (225, 255)), (108, (231, 255)), (39, (252, 255)), (32, (255, 257)), (84, (245, 259)), (156, (208, 260)), (132, (224, 260)), (100, (240, 260)), (64, (252, 260)), (180, (189, 261)), (159, (212, 265)), (140, (225, 265)), (96, (247, 265)), (23, (264, 265)), (117, (240, 267)), (69, (260, 269)), (162, (216, 270)), (128, (240, 272)), (105, (252, 273)), (176, (210, 274)), (165, (220, 275)), (77, (264, 275)), (115, (252, 277)), (168, (224, 280)), (160, (231, 281)), (171, (228, 285)), (110, (264, 286)), (63, (280, 287)), (161, (240, 289)), (136, (255, 289)), (200, (210, 290)), (174, (232, 290)), (48, (286, 290)), (34, (288, 290)), (195, (216, 291)), (192, (220, 292)), (68, (285, 293)), (177, (236, 295)), (96, (280, 296)), (102, (280, 298)), (115, (276, 299)), (180, (240, 300)), (84, (288, 300))]
Execution: 0.45  Garbage collection: 0.07  Total: 0.52
```

## Purpose

The purpose of this backend is to see if the [Clean programming language](https://en.wikipedia.org/wiki/Clean_(programming_language)), but especially the [ABC machine](https://en.wikipedia.org/wiki/Clean_(programming_language)#The_ABC-Machine) are a good fit for Idris.

Clean is a general purpose, pure, lazy functional programming language, similar to [Haskell](https://www.haskell.org/), with lots of high level features.
It's the fastest lazy language out in the wild that I know of :wink:.
The ABC machine is an abstract machine to close the gap between high level functional languages and low level machine code.
The four main parts of the machine are a _graph store_, containing _nodes_ (or _closures_) to be rewritten to normal form, and three stacks:

- The **A**ddress or **A**rgument stack: holding references to nodes in the graph store.
- The **B**asic value stack: holding basic values like `Int`s, `Char`s, `Real`s and `Bool`s.
- The **C**ontrol stack: holding return addresses for control flow.

The code generator for the ABC machine developed at the [Software Science group](http://www.sws.cs.ru.nl/) of the [Radboud University](http://www.ru.nl/icis) generates fast code for Intel (64-bit and 32-bit) and ARM (32-bit only) on Windows, Linux and macOS.

First tests show that this backend is __3 to 4 times faster__ than Idris' current C backend (on my 2012 MacBook Air).
This is _without_ any optimisations like unboxed `Bool`s, `Int`s and `Real`s that Clean and ABC use heavily.
Clean's native version of the [Pythagoras benchmark](Benchmarks/pythagoras.idr) runs one order of magnitude faster, so there is a lot to win here!

## Usage

### Installing

To test the code generator, clone this repository and run `cabal`.

```shell
$ git clone https://github.com/timjs/idris-clean
$ cd idris-clean
$ cabal install
```

This should download all dependencies (including Idris itself).

In the mean time (compiling Haskell takes a long time...) install Clean from ftp://ftp.cs.ru.nl/pub/Clean/builds/.
Download a `clean-classic-<your-platform>-<todays date>.tgz` tarball (nothing with `itasks` in it), unpack and run `make`.
Don't forget to append `clean/bin/` and `clean/lib/exe/` to your `$PATH`.

```shell
$ tar -xzf clean-classic-<your plafrom>-<todays date>.tgz
$ cd clean
$ make
$ export PATH <path to clean>/bin/:<path to clean>/lib/exe/:$PATH
```

### Running

To compile an Idris file with the Clean backend run Idris with the `--codegen clean` argument and run `clm` (Clean's make utility) on the generated clean file.
Do not forget to add the `Libraries/` directory to Clean's search path!

```shell
$ idris --codegen clean <filename.idr> -o <filename.icl>
$ clm -b -I <path to idris-clean>/Libraries/ <filename WITHOUT extension> -o <executable>
```

Alternatively you can run the `idris-codegen-clean` utility on any already generated `.ibc` file and run `clm` afterwards.

```shell
$ idris-codegen-clean <filename.idr> -o <excecutable>
$ clm -b -I <path to idris-clean>/Libraries/ <filename WITHOUT extension> -o <executable>
```

### Testing

To run the tests or benchmarks build the files inside `Tests\` or `Benchmarks\` using [Ninja](https://ninja-build.org/).

```shell
$ cd Tests
$ ninja
```

## Enjoy!

This is experimental software and contains BUGS!
Not all features of Idris are already implemented.
Some known bugs/limitations:

- `BigInt`s are implemented as native ABC `Int`s and are **not checked on overflow**.
- No FFI
- No unboxing


