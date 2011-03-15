1. Phillip Mates u0284736
2.
  Cabal build system (Haskell's build system)
  GHC >= 7.0.1
  Text.Derp package for Haskell

  install cabal on your system
  run the following commands:
    cabal install derp
    cabal install 'base >= 4.3'

3. My parser cannot currently take in prelexed input since it receives the lexed data in a custom Haskell data-structure and I didn't take the time to write an S-Expression reader. I have fixed my lexer from project 1. To test the lexer, type "make test_lexer". My lexer currently doesn't handle carriages (\r style newlines)

4.
  - readme.txt: you are here
  - Makefile: for running cabal commands and running lexer & parser. Adheres to makefile structure defined in assignment
  - pyparser.cabal: cabal build file, like Makefile for the Haskell build system
  - Setup.hs: Basic setup file for cabal build
  - src/Main.hs: Entry point for parser
  - src/LexerMain.hs: Entry point for lexer
  - src/DerRegex.hs: David Darais' Derivative Regular Expression Library
  - src/LexerRegex.hs: Regular Expressions definitions for lexer
  - src/PythonLexer.hs: Improved lexer from assignment 1
  - src/PythonParser.hs: Parser
  - tests/: parser tests
  - tests/test_lexer/: lexer tests
