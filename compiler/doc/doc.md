# The Javalette compiler

## testsuite/good/shadowedfunction
We consider failing this test acceptable as we have implemented higher-order
functions. If we were to implement a fix then it would not be possible to
shadow functions, which in our opinion is reasonable.

## Using the compiler
The compiler is used by either providing the path to a source file or text
immediately to stdin. It provides no options, all extensions are enabled by
default at all times.

The output of the compiler consists of two things, a file called `output.ll`
which is the file containing LLVM-Ir code and, an executable which matches the
name of the file compiled which is an executable compiled by clang.

The compiler comes with one optional argument `[-n|--normal]` which, if used,
displays eventual front end errors in a pretty way.

## Javalette specification

The following is a rough BNF spec of our compiler. Do note that it is not valid
BNFC as each separator/terminator is local to the previous non-terminal

```hs
-- Program
PROGRAM ::= [TOPDEF]

-- Top-level definition
TOPDEF ::= TYPEDEF
         | STRUCT
         | FUNCTION
         | USE
TYPEDEF ::= "typedef" "struct" IDENTIFIER "*" IDENTIFIER ";"

STRUCT ::= "struct" "{" [ARG] "}" ";"
terminator ARG = ";"

USE ::= "use" IDENTIFIER ";"

FUNCTION ::= TYPE IDENTIFIER "(" [ARG] ")" "{" [STMT] "}"
separator ARG = ","

-- Statement

STMT ::= ";"
       | "{" [STMT] "}"
       | TYPE [ITEM] ";"
       | TYPE IDENTIFIER ";"
       | EXPR "=" EXPR ";"
       | IDENTIFIER "++" ";"
       | IDENTIFIER "--" ";"
       | "return" EXPR ";"
       | "return" ";"
       | "if" "(" EXPR ")" STMT
       | "if" "(" EXPR ")" STMT "else" STMT
       | "while" "(" EXPR ")" STMT
       | "for" "(" ARG ":" EXPR ")" STMT
       | "for" "(" STMT EXPR STMT ")" STMT
       | "break" ";"
       | EXPR ";"
       | FUNCTION ";"

separator ITEM = ","
nonempty ITEM

ITEM ::= IDENTIFIER
       | IDENTIFIER "=" EXPR

-- Expression

EXPR ::= "null"
       | integer
       | float
       | "false"
       | "true"
       | stringliteral
       | "new" TYPE ["[" EXPR "]"]
       | "(" EXPR ")"
       | IDENTIFIER
       | "\\" "(" [ARG] ")" "->" TYPE ":" EXPR
       | EXPR "||" EXPR
       | EXPR "&&" EXPR
       | EXPR ">" EXPR
       | EXPR "==" EXPR
       | EXPR "!=" EXPR
       | EXPR "<=" EXPR
       | EXPR ">=" EXPR
       | EXPR "<" EXPR
       | EXPR "-" EXPR
       | EXPR "+" EXPR
       | EXPR "%" EXPR
       | EXPR "/" EXPR
       | EXPR "*" EXPR
       | "!" EXPR
       | "-" EXPR
       | EXPR "." IDENTIFIER
       | EXPR "[" EXPR "]"
       | EXPR "->" IDENTIFIER
       | EXPR "(" [EXPR] ")"

separator ARG = ","
separator EXPR ","

-- Type
TYPE ::= "int"
       | "double"
       | "string"
       | "boolean"
       | "void"
       | "(" TYPE ")"
       | "fn" "(" [TYPE] ")" "->" TYPE
       | TYPE "[]"
       | TYPE "*"
       | IDENTIFIER

separator TYPE = ","

-- Argument
ARG ::= TYPE IDENTIFER

IDENTIFIER ::= (letter | '_') (alpha | number | '_')*

keywords = ["while", "if", "else", "for", "main", "return", "true"
           ,"false", "int", "double", "boolean", "void", "string"
           ,"break", "struct", "null", "new", "typedef", "fn"]

keysymbols = ["+", "++", "-", "--", "*", "%", "/", "!", "<", "<="
             ,">", ">=", "==", "!=", "&&", "||", ";", ",", "=", "[]"
             ,".", "->", ":", "::" ]

```

## Shift/reduce
We have none as we use a recursive descent parser. However we still had to deal
with the classical problems such as dangling-else.
We mitigated these problems by backtracking if we failed parsing the longer
version.

* Extensions

- Struct/pointers
- Arrays1
- Arrays2 (non-uniform size for subarrays as well!)
- Higher-order functions
- Simple module system

As far as we know and have tested these should all work correctly. 
Of course if we dove deep into finding bugs some would definitely emerge.

## Module system

For our 5th extension we create a simple module system (Ok'd by Krasimir).
A module can be imported by writing `use <name_of_module>`. A module refers to
a file in the same directory where the name must match.
The functions in the imported module can now be used by explicit namespacing, e.g
```java
use sort;

int main() {
    int[] arr = new int[3];
    arr[0] = 2;
    arr[1] = 1;
    arr[2] = 0;

    sort::bubblesort(arr);
    for (int i : arr) {
        printInt(i);
    }

    return 0;
}
```

## Testing 
The compiler should pass all tests no problem. 
We have also create some tests for the module system, these exist in `testsuite/modules/good` and `testsuite/modules/bad` directory in `src`.
To test it run it on one of

* `testsuite/modules/good/sort_caller.bl`
* `testsuite/modules/good/mutual_recursion_a.bl`
* `testsuite/modules/good/mutual_recursion_b.bl`
* `testsuite/modules/bad/bad_import_1.bl`
* `testsuite/modules/bad/bad_import_2.bl`
* `testsuite/modules/bad/bad_type.bl`
* `testsuite/modules/bad/mod.bl`
