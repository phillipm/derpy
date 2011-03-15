#!/bin/bash

# Phillip Mates
# Compilers Spring 2011
# Lexer Assignment

# Download test cases from TA

for i in {1..100}
do
   wget "http://www.cs.utah.edu/~yrbahn/test/$i.txt"
   # save *.res.txt to *.txt.res
   wget "http://www.cs.utah.edu/~yrbahn/test/result/$i.res.txt" -O "$i.txt.res"
done
