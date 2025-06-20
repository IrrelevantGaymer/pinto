# Pinto

Pinto is an esoteric language based off of [Turing Machines](https://en.wikipedia.org/wiki/Turing_machine) with [Set Theory](https://en.wikipedia.org/wiki/Set_theory).

Pinto is heavily inspired from Tsoding's [Tula](https://github.com/tsoding/tula).

Pinto is imcomplete and has not fully implemented its feature set.  Also everything is susceptible to change.

## Motivation and Reasoning

A couple features that inspired me to make Pinto was recursive set definitions and Set restrictions.  With recursive sets, one can easily define countably infinite sets on their own such as Z or ℚ.  Another useful feature I wanted to implement are maps, reminiscent of pure functions mapping elements of one set to another (i.e. f :: a -> b), when combined with exhaustive matching could be really powerful.  I also wanted to add in built in states which would be used for general programming actions such as reading from and writing to stdin and stdout, reading from and writing to other files, etc.  A far off goal would be to implement Pinto in itself.

Another big reason for this project is I wanted to create a project using Haskell. 

## Build from Src

These are temporary instructions to building Pinto from Src.  In the future, executables for targets will be provided, as well as a more universal build system such as make.

1. Download Haskell using [GHCup](https://www.haskell.org/ghcup/)
2. Make sure you can run the build.sh file.  If you are on Linux, great!  If not, heres a [stack overflow post](https://stackoverflow.com/questions/26522789/how-to-run-sh-on-windows-command-prompt) that'll help you be able to run .sh scripts in Windows.  If there's any issues, please create an issue in the repository.
3. Run the build.sh file in a terminal to create a pinto executable.

## Using Pinto

These are temporary instructions to using Pinto.  There are 5 commands: help, lex, parse, interpret, and dinterpret.  The help command gives detailed descriptions and instructions to use the other commands (Not yet).  The lex command, when provided with files, will print out the tokens of the pinto program.  The parse command, when provided with files, will print out the Sets, Tapes, and Rules parsed in the pinto program.  The interpret command will run a program.  The dinterpret command (debug interpret) will run a program, printing out the Tape and its State while highlighting the Head at every step.  The lex and parse commands are used for debug purposes.

## Example Programs

At the moment, only the examples in the simple directory are supported.  The other examples use features which are not yet implemented yet.  As Pinto gets developed more and more, the examples will be useful to look at while there's a lack of documentation.

## Writing Programs

A Program consists of rules and a (theoretically infinitely) long tape.  At any given moment, there is a state of the Machine and a pointer to a given location on the tape, where state is an element of the set of all Mathematical Objects.  Every location on the tape stores an element of the set of all Mathematical Objects.  Rules map the element the pointer is currently pointing to to another object within the space of Mathematical Objects based of the current State of the program, before either moving the pointer left or right.

### Rules
The program consists of rules, defined like so:

```
case <CurrentState> <Read> <Write> <Step> <NextState>
case <CurrentState> <Read> <Write> <Step> <NextState>
case <CurrentState> <Read> <Write> <Step> <NextState>
case <CurrentState> <Read> <Write> <Step> <NextState>
```

Each rule starts with the keyword case and 5 expressions:

- \<CurrentState> - The current state of the Machine,
- \<Read> - What the Machine reads on the Tape,
- \<Write> - What the Machine should write on the Tape,
- \<Step> - Where the Head of the Machine must step (<- left, -> right),
- \<NextState> - What is the next state of the Machine.

Note: \
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

Note: \
    At the moment default(0) does not work.  You can only define very simple tapes of just one list.

This program by itself doesn't do anything.  In order to see anything, we must use built in States: (PrintHead a), (PrintState a), or (PrintTape start end a).  We can also use a helper State to act as a stand step, or a step where the Head of the Machine does not move.

```
// When in the state `Inc` and read `0`, replace it with `1` move the head
// to the right and switch to the state `Halt` which halts the program.
case Inc 0 1 -> (PrintTape 0 3 Halt)

// Really the following rules would be in the standard library
// and would not have to be implemented
for a in All case (PrintTape _ _ a) _ _ . a 

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

Note: \
    Built in functions (soecifically IO) do not technically violate the definition of a Turing Machine.  We can interpet all states as including a world object, so instead of Foo, we'd have (Foo world).  So the "true" version of the Print state would be: `for (string a world) in All * All * World case ((Print a) world) string string . (a print_to_world(string, world))`

Or instead, you could run the original program with a -d Debug flag, which would print out:

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

Note: \
    Flags are not supported at the moment, so instead use the dinterpret command over the interpret command.

**Not Implemented Yet**: You can concatenate different lists using set and function operators.  This lets the user eAsILy define where the head starts using the Here keyword as well as create infinite tapes of patterns.  List Comprehensions can aid in repetitive patterns.  For example, this tape has 16 rows of 16 "-"'s each bordered by an "&", where the Head starts on the third row and column.

```
start tape with State = [&] + [- for a in range(0, 16)] + 
                        [&] + [- for a in range(0, 16)] +
                        [&] + [- -] + Here [- for a in range(2, 16)] +
                        [[&] + [- for a in range(0, 16) for a in range(4, 16)]] +
                        [&]
```

Or it might be better to write it like this with an initial state of (RightUntil & (RightUntil & (Right 3 State))):

```
start tape with (RightUntil & (RightUntil & (Right 3 State))) = [&] + 
    Here [[- for a in range(0, 16)] + [&] for a in range(0, 16)]
```

### Built In and Std Rules

There are a few Rules built into the language that allows the Turing Machine to interact with the outside world.  Here's the list of the built in rules.

- (Show a) -> Will print out the pattern of the Head
- (ShowLn a) -> Will print out the pattern of the Head with a new line
- (Print a) -> Will print out the pattern of the Head if it's a StringLiteral
- (PrintLn a) -> Will print out the pattern of the Head if it's a StringLiteral with a new line
- (ReadChar a) -> Will replace the current Head with a char read from stdin
- (ReadString a) -> Will replace the current Head with a string read until whitespace from stdin
- (ReadLine a) -> Will replace the current Head with a string read until a new line from stdin
- (GetHeadAddr a) -> Will replace the current Head with the index of the Head
- (PrintTape start end a) -> Will print the Tape from indices start to end inclusive

Other useful rules can be found in the standard library.

### Tuple and S expressions

Normally we can only work with single symbols, a list of characters surrounded by whitespace: "Symbol", "A" or "Print" for example.  However that can be limiting when trying to programmatically generate rules for a turing machine.  It might be useful to make the next state be based off the Head's current value and/or the current State: We can implement this type of behavior using Tuples/S-expressions combined with Universal Qualification.  

We can group symbols together in a Tuple, a group of symbols delimited by parenthesis.  For example:

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

There are a couple Built In Sets which are useful.  There's the All Set, which contains all possible patterns, and the Int Set which contains all possible integers.

Note: \
    Technically, again, we can shoo away the fact that Int and All are infinite sized sets (any rule we make with them would expand to an infinite number of rules which is not compatible with "vanilla" Turing Machines) by saying realistically Computers can only hold finite number of states, so All and Int are actually subsets of the True All and Int sets wherein each element is physically possible, such that True All - All and True Int - Int would only contain values that are impossible for the Computer to process.

### Set Builder

You can also use set builder notation to concisely define several elements.  Set Builder Notation in math looks like {x\*2|x∈ℤ}.  In Pinto, Set Builder Notation is very similar: "x*2" is some pattern comprised of Tuples, Symbols, and/or Maps; "|" is replaced with "for"; "x" is a Pattern matched to a set; "∈" is replaced with in; and "ℤ" is some Set.

```
let Animal = {Cat Dog Fish Turtle}
let Shrondiger = {(Alive x) for x in Animal} + {(Dead x) for x in Animal}
```

The above would expand to:

```
let Animal = {Cat Dog Fish Turtle}
let Shrondiger = {(Alive Cat) (Alive Dog) (Alive Fish) (Alive Turtle) (Dead Cat) (Dead Dog) (Dead Fish) (Dead Turtle)}
```

in this case however, using set operations, you could more concisely define Shrondiger as such:

```
let Animal = {Cat Dog Fish Turtle}
let AnimalState = {Alive Dead}
let Shrondiger = AnimalState * Animal
```

You can even use Set Builders to build recursive sets (Sets that reference themselves).

```
let IntList = {(Cons i r) for (i r) in Int * IntList} + {Nil}

for a in IntList case PrintList a a . (PrintList Nil (PrintLn' "" (Right PrintList)))

for (i l a) in Int * IntList * All case (PrintList Nil a) (Cons i l) i . (ShowLn (Inject l (PrintList (Cons i Nil) a)))
for (i j l m a) in Int * Int * IntList * IntList * All case (PrintList (Cons i l) a) (Cons j m) j . (ShowLn (Inject m (PrintList (Cons j (Cons i l)) a)))
for (l a) in IntList * All case (PrintList l a) Nil Nil . (Inject l (ReverseList Nil a))

for (a b) in All * All case (ReverseList b a) Nil b . a
for (i l a) in Int * IntList * All case (ReverseList Nil a) (Cons i l) l . (ReverseList (Cons i Nil) a)
for (i j l m a) in Int * Int * IntList * IntList * All case (ReverseList (Cons i l) a) (Cons j m) m . (ReverseList (Cons j (Cons i l)) a)
```

### Record Sets

Sometimes it's useful to group data together.  Our current mechanism for this is applying a Cartesian Product on Sets.  If we wanted to represent a coordinate, it would be in the set `Int * Int`.  However, it's useful to have each element have a name.  Technically, this is still doable via this clunky notation: `({x} * Int) * ({y} * Int)`.  This is annoying.

We have built in syntax to better present this concept.  We can define a record set:

```
let Coord = {Coord in x: Int * y: Int}
```

One can construct or destruct an element of a Record:

```
{Coord where x: val1 y: val2}
```

or if you want the patterns to have the same names as the members of the record:

```
{Coord where x y}
```

#### Examples

```
let Coord = {Coord in x: Int * y: Int * z: Int}

for coord in Coord where
    {Coord in x y z: depth} = coord
    case Offset coord {Coord where x y z: (add for depth 1)}
```

Note: \
    At the moment, where clauses are not supported.  To make this example work, you need to delete line 4, and replace both instance of coord with `{Coord where x y z: depth}`

### **Not Implemented Yet**: Generic Sets

You can also define generic sets.

```
let Animal = {Cat Dog Fish Turtle}
let Maybe(A) = {Nothing} + {(Just a) for a in A}

for v in Maybe(Animal) case Tame v Nothing -> (Pet v)
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
G: { {}, {1}, {2}, {3}, {1 2}, {1 3}, {2 3}, {1 2 3} }
```

There is a quality of life feature with Cartesian Products, where the Tuples will flatten.  For example:

```
let A = {1 2 3}
let B = {4 5 6}
let C = {7 8 9}

A * B * C: {(1 4 7) (1 4 8) (1 4 9) (1 5 7) (1 5 8) (1 5 9) (1 6 7) (1 6 8) (1 6 9) (2 4 7) (2 4 8) (2 4 9) (2 5 7) (2 5 8) (2 5 9) (2 6 7) (2 6 8) (2 6 9) (3 4 7) (3 4 8) (3 4 9) (3 5 7) (3 5 8) (3 5 9) (3 6 7) (3 6 8) (3 6 9)}
```

The shape of each element in A * B * C is (_ _ _) instead of (_ (_ _))

Note: \
    At the moment, only Union, Difference, and Cartesian Product are implemented.  Power Set requires a lot more features to be implemented before it can be introduced, let alone be remotely useful.

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

### **Not Implemented Yet**: Maps

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

We could've theoretically rewritten the statement `for triangle in Triangle case RotateClockwise triangle rotate_clockwise[triangle] -> RotateClockwise` as `for (a b c) in Triangle case RotateClockwise (a b c) (c a b) -> RotateClockwise`, however now everytime we need to rotate a triangle, we can just use the map that we defined.  Or even better, maps are mathematical objects (think Functors), therefore we can map one set to a set of maps:

```
let Point3 = { A B C }
let Triangle = {(a b c) for (a b c) in Point3 * Point3 * Point3}
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
[ ] - Comments \
[X] - Parser \
[ ] - Discard Pattern Support
[ ] - Many Discard Pattern
[X] - Sets \
[X] - Built In Sets \
[ ] - Set Builder Notation \
[ ] - Structs \
[X] - Universal Quantification \
[ ] - Maps \
[ ] - QoL List Syntax/Maps \
[X] - Interpreter \
[ ] - Built In Testing Support \
[X] - Multiple Files \
[ ] - Fix TUI \
[X] - Built In Rules \
[ ] - Import System \
[ ] - Standard Library