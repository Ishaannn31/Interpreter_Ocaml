# Interpreter_Ocaml

# Mini-OCaml Interpreter

## Overview
This repository contains a **Mini-OCaml interpreter** written in OCaml, developed as part of my university's *Programming 1* course on functional programming. Mini-OCaml is a lightweight interpreter for a subset of OCaml, supporting lexing, parsing, type-checking, and evaluation of expressions. Through this course, I’ve learned how interpreters work from the ground up and even built parsers and evaluators for small languages during exams—this project reflects that experience!

The code here is based on course material, but I’ve internalized the concepts and can write similar interpreters independently. Explore, test it, or contribute—I’d love to hear your feedback!

---

## Features
Mini-OCaml supports:
- **Lexing:** Converts input strings into tokens (e.g., `if`, `let`, integers).
- **Parsing:** Builds an Abstract Syntax Tree (AST) with recursive descent and operator precedence.
- **Type Checking:** Ensures type safety (e.g., `int`, `bool`, function arrows).
- **Evaluation:** Computes results with lazy operators, recursion, and closures.

### Supported Constructs
- Basic types: `int`, `bool`
- Constants: Integers (e.g., `42`), Booleans (`true`, `false`)
- Variables and let-bindings: `let x = 5 in x + 3`
- Conditionals: `if e1 then e2 else e3`
- Binary operators: `+`, `*`, `-`, `<=`, `<`, `=`, `<>` (lazy `&&`, `||`)
- Unary operators: `not`, `-` (negation)
- Lambda functions: `fun (x: int) -> x + 1`
- Function application: `f 42`
- Recursive functions: `let rec f (n: int) : bool = if n = 0 then true else not f (n-1)`

---

## How It Works
1. **Lexer (`lex`)**: Turns input into tokens (e.g., `"if true then 1 else 0"` → `[IF; CON(BCON true); THEN; CON(ICON 1); ELSE; CON(ICON 0)]`).
2. **Parser (`parse_exp`)**: Creates an AST, handling operator precedence and nested expressions.
3. **Type Checker (`check_ty`)**: Verifies type correctness (e.g., `if` conditions must be `bool`).
4. **Evaluator (`eval`)**: Executes the AST, supporting lazy `&&`/`||`, closures, and recursion.

Run it with `interpret`:
```ocaml
interpret "let rec f (n: int) : bool = if n = 0 then true else not f (n-1) in f 42"
```
---

## Example

Here’s a Mini-OCaml program that defines a recursive function and calls it:
ocaml
```
let rec f (n: int) : bool = if n = 0 then true else not f (n-1) in f 42
```

    Output: Bval false (evaluates f 42 by recursively flipping true/false until n = 0).

---

## How to Run

Local Installation

    Install OCaml (e.g., via opam).
    Compile the code:
    bash

ocamlc -o miniocaml miniocaml.ml
Run it:
bash
./miniocaml
Or use the OCaml REPL:
bash

    ocaml miniocaml.ml
    Then call interpret "your code here".

---

## Online Interpreter

If installing OCaml locally feels cumbersome, test the code online! Copy miniocaml.ml into an OCaml interpreter like:

    SOOCaml (Saarland University’s interpreter)
    Try OCaml
    Repl.it

Paste the code, add an interpret call (e.g., above example), and run it!
Project Structure

    miniocaml.ml: Contains all components:
        Helper functions: rev, map, lookup, update, etc.
        Core logic: lex, parse_exp, check_ty, eval, interpret.
        Types: ty (types), exp (AST), token (lexer output), va (values).

---

## Learning Outcomes
Through this project and course:
- Mastered functional programming in OCaml (recursion, pattern matching, higher-order functions).
- Learned to build interpreters: lexing, parsing, type checking, and evaluation.
- Practiced writing parsers/evaluators for toy languages in exams.
- Gained skills applicable to compilers, programming languages, and tech projects.

---

## Contributing

Contributions are welcome! If you have ideas or fixes, please:

    Open an issue to discuss.
    Submit a pull request with your changes.

Ways to Contribute
- **Add Features:** Extend the language (e.g., lists, pattern matching).
- **Improve Docs:** Enhance this README or add code comments.
- **Write Tests:** Add test cases for edge scenarios.

---

## Acknowledgments

    Thanks to the Programming 1 instructors and TAs at Saarland University for their guidance.
    Inspired by the OCaml community and online resources.

---

## License

This project is licensed under the MIT License. See the LICENSE file for details.


### Happy coding! 🚀
