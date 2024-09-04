# What is Hecate

Hecate is a C compiler that is based off Nora Sandler's book [Writing a C Compiler](https://nostarch.com/writing-c-compiler).


# Usage

Currently, only x86 Linux platforms are supported and you must have gcc installed

```
Usage: hecate [OPTIONS] <PATH>

Arguments:
  <PATH>
          Path to C source file

Options:
      --lex
          Stop after lexer

      --parse
          Stop after parser

      --codegen
          Stop after assembly generation

      --tacky
          Stop after TACKY generation

      --validate
          Stop after semantic analysis

  -S
          Emit assembly file, but do not assemble or link it

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version

```


To run the test suite, see [here](./writing-a-c-compiler-tests/README.md)


# Why the name Hecate

I chose the name Hecate becuase she is the Greek goddess of magic. Friends have referred to compilers as being like black magic and I thought that would be an appropriate name. 
And so many projects have names that are seeemingly unrelated to the project and I thought it would be funn
