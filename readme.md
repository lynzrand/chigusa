# Chigusa

![logo](res/img/chigusa_the_c0_compiler.png)

Chigusa is an unfinished C0 compiler written in Rust. This compiler is a homework of the _Compilation Principle_ class in BUAA.

## Progress

- [x] Tokenizer
- [x] Parser
- [x] Code generator
- [x] CLI

## About C0

C0 is a subset of C programming language. A description of the grammar used in this project can be found [here][c0_grammar_info].

## Usage

```sh
# Parse and compile a c0 source file to `out` as c0 binary
$ chigusa <file> -o <output_file>

# Compile the file as c0 assembly
$ chigusa <file> -s -o <output_file>
# or
$ chigusa <file> --emit s0 -o <output_file>
```

## Chigusa's implementation

Chigusa uses a handwritten recursive-descending parser to parse C0 programs.
## License

Chigusa is licensed under MIT license.

(c) 2018 Rynco Maekawa

## Documentation

For more documentation and Chinese readme, see [docs](./docs).

## Dependencies

![](docs/deps.png)

## Naming

Chigusa (Fukazawa Chigusa) is a character from the anime _Iroduku: The World In Colors_. ~~Finally an anime boy gets on the list!~~

[c0_grammar_info]: docs/c0_grammar_1.txt
