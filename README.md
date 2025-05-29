# Pinto

Pinto is an esoteric language based off of [Turing Machines](https://en.wikipedia.org/wiki/Turing_machine) with [Set Theory](https://en.wikipedia.org/wiki/Set_theory).

Pinto is heavily inspired from Tsoding's [Tula](https://github.com/tsoding/tula).

## Motivation and Reasoning

A couple features that inspired me to make Pinto was recursive set definitions and Set restrictions.  With recursive sets, one can easily define countably infinite sets on their own such as Z or ℚ.  Another useful feature I wanted to implement are maps, reminiscent of pure functions mapping elements of one set to another (i.e. f :: a -> b), when combined with exhaustive matching could be really powerful.  I also wanted to add in built in states which would be used for general programming actions such as reading from and writing to stdin and stdout, reading from and writing to other files, etc.  A far off goal would be to implement Pinto in itself.

Another big reason for this project is I wanted to create a project using Haskell. 

## Writing Programs

A Program consists of rules and a (theoretically) long tape.  At any given moment, there is a state of the Machine and a pointer to a given location on the tape, where state is an element of the set of all Mathematical Objects.  Every location on the tape stores an element of the set of all Mathematical Objects.  Rules map the element the pointer is currently pointing to to another object within the space of Mathematical Objects based of the current State of the program, before either moving the pointer left or right.

### Rules
The program consists of rules, defined like so:

```
case <CurrentState> <Read> <Write> <Step> <NextState>
case <CurrentState> <Read> <Write> <Step> <NextState>
case <CurrentState> <Read> <Write> <Step> <NextState>
case <CurrentState> <Read> <Write> <Step> <NextState>
```

Each rule starts with the keyword case and 5 expressions:

- <CurrentState> - The current state of the Machine,
- <Read> - What the Machine reads on the Tape,
- <Write> - What the Machine should write on the Tape,
- <Step> - Where the Head of the Machine must step (<- left, -> right),
- <NextState> - What is the next state of the Machine.

Note:
    I'm debating adding the stand step since "vanilla" Turing Machines do not have this instruction and it is trivial to implement an isomorphic set of rules and states that replicate this behavior.  Same goes for Arbitrary Left and Arbitrary Right steps, however they're slightly less trivial to implement.  I'd prefer having a standard library with these implementations or building a macro system which wrap over these implementations.

 #### Examples
```
// When in the state `Inc` and read `0`, replace it with `1` move the head
// to the right and switch to the state `Halt` which halts the program.
case Inc 0 1 -> Halt

// When in the state `Inc` and read `1`, replace it with `0` move the head
// to the right and keep staying in the state `Inc` effectively looping over
// the tape
case Inc 1 0 -> Inc

// Start the program from the state `Inc` with the tape `{ 1 1 0 1 }` 
// (with an infinite number of zeros to the left and to the right)
start tape with Inc = [ 1 1 0 1 ] + default(0)
```

This program by itself doesn't do anything.  In order to see anything, we must use built in States: (PrintHead a), (PrintState a), or (PrintTape start end a).  We can also use a helper State to act as a stand step, or a step where the Head of the Machine does not move.

```
// When in the state `Inc` and read `0`, replace it with `1` move the head
// to the right and switch to the state `Halt` which halts the program.
case Inc 0 1 -> (PrintTape 0 3 Halt)

// Really the following rules would be in the standard library
// and would not have to be implemented
for a in All case (PrintTape _ _ a) _ _ -> (Reset a)
for a in All case (Reset a) _ _ <- a 

// When in the state `Inc` and read `1`, replace it with `0` move the head
// to the right and keep staying in the state `Inc` effectively looping over
// the tape
case Inc 1 0 -> Inc

// Start the program from the state `(PrintTape 0 3 Inc)` 
// with the tape `[ 1 1 0 1 ]`.  Immediately we print the tape 
// (with an infinite number of zeros to the left and to the right)
start tape with (PrintTape 1 4 Inc) = [ 1 1 0 1 ] + default(0)
```

Now this would print out:

```
1 1 0 1
0 0 1 1
```

Or instead, you could run the original program with a -d Debug flag,
which would print out

```
Inc: 1 1 0 1
     ^
Inc: 0 1 0 1
       ^
Inc: 0 0 0 1
         ^
Halt: 0 0 1 1
            ^
```

You can concatenate different lists using set and function operators.  This lets the user eAsILy define where the head starts using the Here keyword as well as create infinite tapes of patterns.  List Comprehensions can aid in repetitive patterns.  For example, this tape has 16 rows of 16 "-"'s each bordered by an "&", where the Head starts on the third row and column.

```
start tape with State = [&] + [- for a in [0..16]] + 
                        [&] + [- for a in [0..16]] +
                        [&] + [- -] + Here [- for a in [2..16]] +
                        [[&] + [- for a in [0..16] for a in [4..16]]] +
                        [&]
```

Or it might be better to write it like this with an initial state of (RightUntil & (RightUntil & (Right 3 State))):

```
start tape with (RightUntil & (RightUntil & (Right 3 State))) = [&] + 
    Here [[- for a in [0..16]] + [&] for a in 0..16]
```

### Tuple and S expressions

Normally we can only work with single symbols, a list of characters surrounded by whitespace: "Symbol", "A" or "Print" for example.  However that can be limiting when trying to programmatically generate rules for a turing machine.  It might be useful to make the next state be based off the Head's current value and/or the current State.  We can group symbols together in a Tuple, a group of symbols delimited by parenthesis.  For example:

```
case Swap (1 2) (2 1) -> Swap
case Swap (2 3) (3 2) -> Swap
case Swap (1 3) (3 1) -> Swap

start tape with Swap = [(1 2) (2 3) (1 3) &]
```

Running this machine with the debug flag, prints out:

```
Swap: (1 2) (2 3) (1 3) &
      ^~~~~
Swap: (2 1) (2 3) (1 3) &
            ^~~~~
Swap: (2 1) (3 2) (1 3) &
                  ^~~~~
Swap: (2 1) (3 2) (3 1) &
                        ^
```

You can also nest these expressions, forming S expressions or S-like expressions

```
case Dec ((((4 2) 0) 6) 9) ((((4 2) 0) 6) 8) -> Dec
```

Theoretically we could rewrite everything with symbols such as 1_2 and 4_2_0_6_9, however Tuples and S expressions can be used in powerful ways to extract and to inject information into a Value or into a State.

### Sets and Universal Quantification

Pinto supports defining Sets (which are collections of Compound Expressions) and using Universal Quantification on those Sets to generate Rules automatically.

```
let Set = { a b c }
for n in Set case S n 0 -> S
```

The above program will expand to

```
case S a 0 -> S
case S b 0 -> S
case S c 0 -> S
```

Note that `for n in Set` quantifier is applied only to a single statement that follows it. To apply the quantifier to a block of statements use curly braces:

```
let Set = { a b c }
for n in Set {
    case S n 0 -> S
    case I n 1 -> I
}
```

#### Examples

```
let Numbers = { 1 2 3 4 5 6 7 8 9 0 }

// Numbers * Numbers is a set including all possible combinations
// between two Numbers.  We generate a rule for every combination
// swapping the two.
for a b in Numbers * Numbers case Swap (a b) (b a) -> Swap

start tape with Swap = [ (9 6) (2 4) (8 0) (0 0) (5 8) & ]
```

The above machine ran in debug mode prints out:

```
Swap: (9 6) (2 4) (8 0) (0 0) (5 8) &
      ^~~~~
Swap: (6 9) (2 4) (8 0) (0 0) (5 8) &
            ^~~~~
Swap: (6 9) (4 2) (8 0) (0 0) (5 8) &
                  ^~~~~
Swap: (6 9) (4 2) (0 8) (0 0) (5 8) &
                        ^~~~~
Swap: (6 9) (4 2) (0 8) (0 0) (5 8) &
                              ^~~~~
Swap: (6 9) (4 2) (0 8) (0 0) (8 5) &
                                    ^
```

### Set Operations

You can combine Sets in a variety of ways: Union (+), Intersection (&), Difference (-), Symmetric Difference (^), and Cartesian Product (*) binary operators and Power Set ($) prefix unary operator.

```
let A = { 1 2 3 } + { 2 3 4 }
let B = { 1 2 3 } & { 2 3 4 }
let C = { 1 2 3 } - { 2 3 4 }
let D = { 2 3 4 } - { 1 2 3 }
let E = { 1 2 3 } ^ { 2 3 4 }
let F = { 1 2 3 } * { 2 3 4 }
let G = ${ 1 2 3 }
```

The above operated sets become:

```
A: { 1 2 3 4 }
B: { 2 3 }
C: { 1 }
D: { 4 }
E: { 1 4 }
F: { (1 2), (1 3), (1 4), (2 2), (2 3), (2 4), (3 2), (3 3), (3 4) }
G: { (), (1), (2), (3), (1 2), (1 3), (2 3), (1 2 3) }
```

### Anonymous Sets

It is not necessary to define the Sets upfront with the let keyword. You can use them directly in Universal Quantifiers:

```
for n in { a b c } case S n 0 -> S
```

This is useful for modifying pre-existing Sets:

```
let Decimal = { 0 1 2 3 4 5 6 7 8 9 }

for n in Decimal - { 0 } case Annhilate n 0 -> Annhilate
```

will only generate rules:

```
case Annhilate 1 0 -> Annhilate
case Annhilate 2 0 -> Annhilate
case Annhilate 3 0 -> Annhilate
case Annhilate 4 0 -> Annhilate
case Annhilate 5 0 -> Annhilate
case Annhilate 6 0 -> Annhilate
case Annhilate 7 0 -> Annhilate
case Annhilate 8 0 -> Annhilate
case Annhilate 9 0 -> Annhilate
```

but not `case Annhilate 0 0 -> Annhilate`

### Maps

Sometimes it is useful to define mappings from one set to another, or even special mappings from one set to itself.

```
let Point3 = { A B C }
let Triangle = (Point3 Point3 Point3)
let Ops = { RotateClockwise RotateCounterClockwise Reflect }

map rotate_clockwise :: Triangle -> Triangle {
    (a b c) => (c a b)
}

map rotate_counterclockwise :: Triangle -> Triangle {
    (a b c) => (b c a)
}

map reflect :: Triangle -> Triangle {
    (a b c) => (c b a)
}

for triangle in Triangle case RotateClockwise triangle rotate_clockwise[triangle] -> RotateClockwise
for triangle in Triangle case RotateCounterClockwise triangle rotate_counterclockwise[triangle] -> RotateCounterClockwise
for triangle in Triangle case Reflect triangle reflect[triangle] -> Reflect

for op in Ops case _ op op -> op

start tape with RotateClockwise = [ (A B C) (C B A) RotateCounterClockwise (A B C) (C B A) Reflect (A B C) (B A C) ]
```

We could've theoretically rewritten the statement `for triangle in Triangle case RotateClockwise triangle rotate_clockwise[triangle] -> RotateClockwise` as `for (a b c) in Triangle case RotateClockwise (a b c) (c a b) -> RotateClockwise`, however now everytime we need to rotate a triangle, we can just use the map that we defined.  Or even better we can maps are mathematical objects (think Functors), therefore we can map one set to a set of maps:

```
let Point3 = { A B C }
let Triangle = (Point3 Point3 Point3)
let TriangleOps = { RotateClockwise RotateCounterClockwise Reflect }

map rotate_clockwise :: Triangle -> Triangle {
    (a b c) => (c a b)
}

map rotate_counterclockwise :: Triangle -> Triangle {
    (a b c) => (b c a)
}

map reflect :: Triangle -> Triangle {
    (a b c) => (c b a)
}

map triangle_op :: TriangleOps -> (Triangle -> Triangle) {
    RotateClockwise -> rotate_clockwise
    RotateCounterClockwise -> rotate_counterclockwise
    Reflect -> reflect
}

for op triangle in TriangleOps * Triangle case op triangle triangle_op[op][triangle] -> op

for op in Ops case _ op op -> op

start tape with RotateClockwise = [ (A B C) (C B A) RotateCounterClockwise (A B C) (C B A) Reflect (A B C) (B A C) ]
```

## TODO

[X] - Lexer \
[X] - Parser \
[ ] - Sets \
[ ] - Universal Quantification \
[ ] - Maps \
[X] - Interpreter \
[X] - Multiple Files \
[ ] - Standard Library \