# Compilecated

Github Repo Link: https://github.com/lucbouchard1/compilecated

Compilecated is a C compiler written in Haskell.

## Build and Run

### Installing LLVM

The LLVM haskell binding is a bit behind in it's LLVM compatibility; it **requires llvm 4.0.0** which was released in 2017. If you're running Ubuntu, you can install this dependency by running:

```
apt install llvm-4.0-dev
```

Eric, I know you mentioned you have llvm-10.0.0 installed. I don't think installing 4.0.0 will interfere because the files should be properly versioned, but proceed cautiously. 

To build my compiler, run:

```
stack build
```

Right now, the program just outputs LLVM IR for a C file. To see this in action, run the following:

```
stack exec compilecated test/build_success.c
```

You can also write your own C files and pass them to the compiler, just be aware that I haven't yet implemented MANY C features.


## Milestone 1

At this point, I'm still learning how to use LLVM, Parsec, and the Haskell features I need for this project, so much of my time has been spent reading up on those things. Most of the code in this repository is from the Stephen Diehl tutorial on Haskell and LLVM (Stephen Diehl), which is the starting point for my project.

My next steps are going to be to transition from the Kaleidoscope syntax to C syntax, then start adding more and more language features. Here are my workshop discussion questions:

- Do I need to significantly change the AST definition to support C style syntax?
- Right now all variables are doubles. How does adding types change the AST and parser setup?

## Milestone 2

I've made a fair amount of progress in my understanding of Parsec and LLVM, but I still have quite a bit to implement. I've started modifying the AST/Code Generation to support C syntax, but I'm doing it rather slowly, ensuring that I actually understand the changes I'm making.

Now that I have a solid foundation in Parsec and LLVM IR, my productivity will be much better. I will be spending a lot of time on this project during two weeks remaining.