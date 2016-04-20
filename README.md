# pokelang
Pokelang is a general purpose, pokemon-based programming language. It was developed as a six-month project for the elective course of Programming Languages at Universidad Simón Bolívar. 

This language supports most of common types, arithmetic and logical operators, control structures (if, while, for, and case), procedures and functions, recursion, block nesting, line and block comments, compound data structures (record) and unions. Likewise, this is an imperative and strongly typed language, it is designed to be compiled. In this lenguage you might find some similarities with C, python, ruby and haskell. Learn it if you want to be the very best, like no one ever was, to compile it’s your real test and to program is your cause.

This document is designed to give the specification for the language syntax and semantics, required to know in order to implement any program using it.

## Lexical considerations
The following are keywords. They are all reserved, which means they cannot be used as identifiers or redefined.

> `pINTachu, BOOLbasaur, squirtrue, squirfalse, CHARmander, VOIDtorb, butterFloat, STRUCTtabuzz, arcticUNION, ENUManyte, GLOBAt, nullikarp, procball, funcball, vamo_a_para, vamo_a_segui, vamos_a_retorna, vamo_a_sali, vamo_a_lee, vamo_a_escribi, vamo_a_imprimi, vamo_a_itera, vamo_mientra, vamo_a_para, vamo_a_segui, vamos_a_retorna, vamo_a_sali, si, y_si, si_no, vamo_a_empeza, vamo_a_calmano`

___

An identifier is a sequence of letters, digits, underscores and the character '?', starting always with a letter. This language is case-sensitive and has no size limit for identifiers. 

In the case of having an identifier that starts with 'poke', we're speaking of a data type identifier. 

___

Whitespace (i.e. spaces, tabs, and newlines) serves to separate tokens, but is otherwise ignored. Keywords and identifiers must be separated by whitespace or a token that is neither a keyword nor an identifier.
`vamo_a_sufri` is a single identifier, not three keywords. `vamo_a sufri` and `(pINTachu)vamo_a` scans both as two tokens.

___

A boolean (BOOLbasaur) constant is either true or false. Like keywords, these words are reserved.
An integer constant can either be specified in decimal (base 10) or hexadecimal (base 16). A decimal integer
is a sequence of decimal digits (0-9). A hexadecimal integer must begin with 0X or 0x (that is a zero, not
the letter oh) and is followed by a sequence of hexadecimal digits. Hexadecimal digits include the decimal
digits and the letters a through f (either upper or lowercase). Examples of valid integers: 8, 012, 0x0,
0X12aE
A double constant is a sequence of digits, a period, followed by any sequence of digits, maybe none. Thus,
.12 is not a valid double but both 0.12 and 12. are valid. A double can also have an optional exponent,
e.g., 12.2E+2 For a double in this sort of scientific notation, the decimal point is required, the sign of
1
the exponent is optional (if not specified, + is assumed), and the E can be lower or upper case. As above,
.12E+2 is invalid, but 12.E+2 is valid. Leading zeroes on the mantissa and exponent are allowed.
A string constant is a sequence of characters enclosed in double quotes. Strings can contain any character
except a newline or double quote. A string must start and end on a single line, it cannot be split over multiple
lines:
‘‘this string is missing its close quote
this is not a part of the string above
Operators and punctuation characters used by the language includes:
+ - * / % < <= > >= = == != && || ! ; , . [ ] ( ) { }
A single-line comment is started by // and extends to the end of the line. Multi-line comments start with
/* and end with the first subsequent */. Any symbol is allowed in a comment except the sequence */
which ends the current comment. Multi-line comments do not nest.


## Grammar examples
Nabil

## Scopes
This language supports nested program blocks and it's statically scoped.

Global variables must be defined outside program blocks using the reserved word GLOBAt. These global variables can be seen everywhere in the program, and are hidden only on program blocks that redeclare them.

Local variables must be declared at the begining of a program block, and will be visible inside that block, hidden only inside nested blocks that redeclares that identifier.

## Types
Pokelang includes the primitives data types.

| Type  | Label       | Size (bytes)  |
|-------|-------------|---------------|
|  Int  | pINTachu    | 4             |
|  bool | BOOLbasaur  | 4             |
|  float| butterFloat | 4             |
|  char | CHARmander  | 4             |
|  void | VOIDtorb    | NO            |
|  char | CHARmander  |               |
|  enum | ENUManyte   | 4             |
|  pointer | *        | 4             |

* Integers are stored in 2's complement.

* Bools are represented with the words 0x1 and 0x0.

* Floating porint numbers are 32 bits precision with the IEEE 754 standar.

* Void type variables cannot be stored.

This lenguage has uses value model like c, pointers can be a reference to any primitive type they are 32 bits long and can refer to any data type. 

Arrays variables are pointers that might refer to any of the primitive or composite type. Pointers are 32bits long.

Pokelang requires explicit casting, no type conversion will be done implicitly.

Dynamic structures as dynamic arrays and structs can be allocated in heap with the built-in procedure *atrapar* and can be freed with *liberar*

## Variables
Nabil

## Arrays
Arrays are a reference that points to the first element of it. 

Arrays can be be stored in the static area if it's declared as a global var and has a fixed size given explicitly, otherwise the content will be stored in the heap and it's memory must be allocated by the user.

This data type is indexed starting at 0, static array declarations must include the number of elements that can hold.

Pokelang array's are homogenous,

## Structs
Structs are a data type, will be implemented as a reference to the first field of it.

Every struct field has a name and type.

Structs can only be allocated in the heap.

## Unions
Nabil

## Enums
Pokelang provides enums and the built-in functions *evolucion* and *preevolucion* to get the successor and predecessor of an element from an enum element. 

The elements that are part from an enum enum are unique (cannot be in another enum, or twice in the same declaration), and must start with a capital letter.

Every element from an enum has an integer asociated starting form 0, this number can be obtained using the function *obtener_numero*

## Functions and Procedures
Nabil

## Types Equivalence
Pokelang uses name equivalence (two types are not equal if their types don't have the same name).

Two variables of the same type are compared by value. 

When two composite types (structures, arrays or unions) are compared, the result will be the comparison of their pointers.

## Assignment
Nabil

## Control Structures

### Conditionals
The conditional control structure keywords *if* *elseif* and *else* are represented by the keywords *si* *y_si* and *si_no*.

The construct "si (expression):" introduces a new non-empty code block that ends with the "y_si", "si_no" or "vamo_a_calmano" keywords.

The construct "y_si (expression):" works like "si (expression):".

The construct "si_no" works like "else" introduces a non-empty code block that will end with the reserved word *vamo_a_calmano*

Pokelang supports case statements and provives some low level optimizations over the if,elseif, else structures.

### Bounded iterations
Bounded iterations work over integers ranges and enumerated types.

The structure of a bounded iteration must include explicitly the begining and end values as constants (Integers or enum types)

i.e

    vamo_a_itera x desde 1 hasta 10:
        instruccion0;
    vamo_a_calmano

or 

    vamo_a_itera dias desde Lunes hasta Viernes:
        instruction0;
    vamo_a_calmano

Instructions between the tokens *:* and *vamo_a_calmano* introduce a new non-empty code block

### Unbounded iterations
Unbounded iterations require the keyword *vamo_mientra* followed by an expression that must evaluate to a boolean (BOOLbasaur). 

Instructions between the tokens *:* and *vamo_a_calmano* introduce a new non-empty code block.

## Expressions
Nabil

### Final notes
Javier & Nabil