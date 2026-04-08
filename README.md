# minicheck

**minicheck** is a CTL (Computation Tree Logic) model checker written in Haskell.

## Building

Make sure you have [GHC](https://www.haskell.org/ghcup/) and [cabal](https://www.haskell.org/cabal/) installed.

Clone the project and run:

```bash
cabal build
```

To get the path to the built executable:

```bash
cabal list-bin minicheck
```

Optionally, define a shell alias for convenience:

```bash
alias minicheck=$(cabal list-bin minicheck)
```

Now you can run it like this:

```bash
minicheck data/soda.txt formulas/f1.ctl
```

## Running

Basic usage:

```bash
minicheck [OPTIONS] MODEL FORMULA [FORMULA ...]
```

### Arguments

* `MODEL`: Path to the model file (either a `.mini` program or a raw transition system)
* `FORMULA [FORMULA ...]`: One or more files containing CTL formulas

### Common Options

* `--only-syntax`: Validate only the syntax of the model and formulas, then exit
* `--debug`: Print the parsed transition system and formulas before verification
* `--dot`: Print the transition system in DOT graph format instead of plain text
* `--extensions`: Show supported language extensions and exit
* `--help`: Show help text

### Example

```bash
minicheck examples/soda.txt formulas/f1.ctl
```

You can also verify a MINI source program:

```bash
minicheck examples/voting.mini formulas/termination.ctl
```

With debug output:

```bash
minicheck --debug --dot examples/soda.txt formulas/f1.ctl
```

You may also pass a CTL formula directly as a string instead of a file:

```bash
minicheck examples/soda.txt "EF (p && q)"
```

---

## TS Format (for minicheck)

The TS format is a plain-text representation of a transition system. It consists of up to six (mandatory) sections in the following order: `states`, `actions`, `transitions`, `init`, `props`, `labels`. Section headers are optional and have aliases (e.g., `s`, `a`, `t`, etc.).

Identifiers may only consist of lowercase letters, digits, `_`, `-` and `.` (e.g., `s1`, `a-2`, `foo.bar.baz`).

Each section contains a list in brackets:

```
states:      [s1, s2, s3]                     -- aliases: state, s
actions:     [a1, a2]                         -- aliases: action, a
transitions: [(s1, a1, s2), (s2, a2, s3)]     -- aliases: trans, t
init:        [s1]                             -- aliases: initial, i
props:       [p1, p2]                         -- aliases: propositions, p
labels:      [(s1, p1), (s2, p2)]             -- aliases: lables, lable, l
```

* Transitions are triples: (source_state, action, target_state)

* Labels are pairs: (state, proposition) indicating propositions that hold at specific states

### Notes and Validation

* States and actions must be **declared** before being referenced.
* Every state in transitions, initial states, or labels must be declared.
* Every action in transitions must be declared.
* At least one initial state is required.
* Extra or duplicate elements are **deduplicated automatically**.
* States without outgoing transitions are automatically given a **self-loop** using a special action `_`.

### Example

This example models a vending machine with two products and a single initial state (pay).

```
states:
    [pay, soda, select, beer]
actions:
    [get_soda, get_beer, insert_coin, _]
transitions: [
    (pay, insert_coin, select),
    (select, _, beer),
    (select, _, soda),
    (soda, get_soda, pay),
    (beer, get_beer, pay)
]
initial:
    [pay]
propositions: []
labels: []
```

## CTL Format

There are four temporal modalities. Each modality must be prepended by either `E` (Exists) or `A` (For all) quantifiers.
Whitespace between quantifier and modalities is optional.

* Next `X`
* Until `U`
* Eventually `F`
* Globally `G`

There are six supported logical operators, five binary operators and the negation.
There are no precedence rules.
The same binary operator, except for the non-associative implication, can be repeated multiple times without parentheses.
Different operators always require parentheses to avoid defining their precedence.

```
E (green && red && (yellow || white) && open) U !blue

AF ((yellow && red) || (black => yellow) || (red == white) || (green != yellow) || !blue)

A G (green => (white => blue))
AG ((yellow => black) => red))

EX True
AX (False || red)

EF !blue

A !red U !white
```
