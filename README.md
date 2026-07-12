# Scheme-to-x86 Compiler - Our Contributions

Components of a compiler translating Scheme to x86-64 assembly, implemented as part of the Compiler Construction course at Ben-Gurion University of the Negev. The compiler pipeline (reader → tag parser → semantic analysis → code generation, in OCaml, emitting x86 assembly against a runtime library) was provided as a course framework; **this repository contains only the parts we implemented** - the framework itself is course material and is not included, so the repo does not build standalone. Within the full framework, the compiler worked end-to-end: real Scheme programs compiled to executable x86 assembly.

By Galit Oren and [Amir Avital](https://github.com/Amir777-Z).

## What we implemented

### Macro expansion (OCaml)

- **Quasiquote expansion** - recursive expansion of quasiquoted templates (including `unquote` and `unquote-splicing`) into equivalent `cons`/`append` construction code.

### Semantic analysis (OCaml)

- **Lexical-address annotation** for `or`, sequences, applications, and `lambda` - both fixed-arity and optional-argument forms - resolving each variable reference to bound (major/minor env indices), parameter, or free.
- **Tail-call annotation** - identifying applications in tail position across the expression forms above, marking them for tail-call optimized code generation.

### Code generation & runtime (x86 assembly)

- **Application compilation** - code generation for procedure application, in both the ordinary and tail-call forms; tail calls overwrite the caller's stack frame rather than growing the stack.
- **Closure environment construction** (`extend_lexical_environment`) - runtime allocation of the extended lexical environment on closure creation: copying the enclosing environment's rib pointers and building the new parameter rib.
- **`apply` primitive** (`L_code_ptr_bin_apply`) - implements Scheme's `apply` by unpacking the argument list and overwriting the current stack frame, using the same frame-replacement technique as tail calls.

## Pipeline context

```
Scheme source
  → Reader (S-expression parsing)
  → Tag parser (AST)
  → Semantic analysis   ← our passes: lexical addressing, TC annotation
  → Code generation     ← our work: applications, closures, apply (x86)
  → x86-64 assembly → assembled & linked → executable
```

Stages not listed under "What we implemented" were provided by the course framework.

## Context

Course project for Compiler Construction 202-1-3021 at Ben-Gurion University of the Negev, based on the course framework by the course staff leaded by Prof. Mayer Goldberg.
