# pokelang
A pokemon programming language. Learn it if you want to be the very best, like no one ever was, to compile it is your real test and to program it is your cause.

Pokelang is an imperative and structured programming language, it is designed to be compiled. In this lenguage you might find some similarities with C, python, ruby and haskell. 

## Lexical considerations
Nabil

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