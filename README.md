# minicheck

**minicheck** is a CTL (Computation Tree Logic) model checker written in Haskell.

## 🔧 Building

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
minicheck --ts=data/soda.txt
```

## 🏃 Running

Basic usage:
```bash
minicheck --ts=<file> [--help]
```

* `--ts=<file>`: Path to the transition system file
* `--help`: Show usage information and exit

---

## 🗂 TS Format Syntax

The **TS format** (Transition System) is a plain-text format for describing transition systems, used as input for `minicheck`.

### ✅ Sections

A TS file may contain the following (optionally named) **sections**, in the specified order. Section headers are optional and some aliases are supported (e.g., `states`, `s`, etc.). The expected sections are (in order):

* **States**: `states`, `state`, `s`
* **Actions**: `actions`, `action`, `a`
* **Transitions**: `transitions`, `transition`, `trans`, `t`
* **Initial states**: `initial`, `init`, `i`
* **Propositions**: `propositions`, `props`, `p`
* **Labels**: `labels`, `lables`, `lable`, `l`

Each section contains a list enclosed in brackets (`[...]`) and uses the following formats:

---

### 🔤 Identifiers

Identifiers can contain letters, digits, and underscores (`_`). Example: `s1`, `my_state_3`.

---

### 🔣 Section Formats

#### States

```txt
states: [s1, s2, s3]
```

#### Actions

```txt
actions: [a1, a2]
```

#### Transitions

```txt
transitions: [(s1, a1, s2), (s2, a2, s3)]
```

Each transition is a **triple**: `(source_state, action, target_state)`

#### Initial States

```txt
init: [s1]
```

Must contain at least one state from the `states` list.

#### Propositions

```txt
props: [p1, p2]
```

Used to label states.

#### Labels

```txt
labels: [(s1, p1), (s2, p2)]
```

Each label is a **pair**: `(state, proposition)`, meaning that the proposition holds at that state.

---

### 🧼 Notes and Validation

* States and actions must be **declared** before being referenced.
* Every state in transitions, initial states, or labels must be declared.
* Every action in transitions must be declared.
* At least one initial state is required.
* Extra or duplicate elements are **deduplicated automatically**.
* States without outgoing transitions are automatically given a **self-loop** using a special action `_`.

---

📄 Example

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

This example models a vending machine with two products and a single initial state (pay).

---