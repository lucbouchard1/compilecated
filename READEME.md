# Compilecated

Github Repo Link: https://github.com/lucbouchard1/compilecated

Compilecated is a C compiler written in Haskell.

## Build and Run

To build the compiler, run:

```
stack build
```

And to open the repl, run:

```
stack exec compilecated
```


## Milestone 1

At this point, I'm still learning how to use LLVM. Parsec, and the Haskell features I need for this project, so much of my time has been spent reading up on those things. Most of the code in this repository is from the Stephen Diehl tutorial on Haskell and LLVM (Stephen Diehl), which is the starting point for my project.

My next steps are going to be to transition from the Kaleidoscope syntax to C syntax, then start adding more and more language features. Here are my workshop discussion questions:

- Do I need to significantly change the AST definition to support C style syntax?
- Right now all variables are doubles. How does adding types change the AST and parser setup?