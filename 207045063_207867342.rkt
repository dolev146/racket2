#lang pl 02

#| 
As in previous assignment, you need to use the special form for tests: test.
Tests: For each question, you should have enough test cases for complete
coverage (DrRacket indicates covered expressions with colors for covered and
uncovered source code, unless your code is completely covered). See below on
the way to create tests. Note that your tests should not only cover the code, but
also all end-cases and possible pitfalls.
Important: Your tests should cover your whole code  otherwise the server will
heavily penalize your submission. You should not have any uncovered
expressions after you hit “Run” — it should stay at the same color, indicating
complete coverage. Furthermore, the server will run its own tests over your code,
which means that you will not be able to submit code that does not work.
General note: Code quality will be graded. Write clean and tidy code. Consult the
Style Guide, and if something is unclear, ask questions on the course forum.

The test form can be used to test that an expression is true, that an expression
evaluates to some given value, or that an expressions raises an error with some
expected text message. For example, the three kinds of tests are used in this
example:
#lang pl
(: smallest : (Listof Number) -> Number)
(define (smallest l)
(match l
[(list) (error 'smallest "got an empty list")]
[(list n) n]
[(cons n ns) (min n (smallest ns))]))
(test (smallest '(5 7 6 4 8 9)) => 4)
(test (zero? (smallest '(0 1 2 3 4))))
(test (smallest '()) =error> "got an empty list")
In case of an expected error, the string specifies a pattern to match against the
error message. (Most text stands for itself, “?” matches a single character and “*”
matches any sequence of characters.)
Note that the =error> facility checks only errors that your code throws, not
Racket errors. For example, the following test will not succeed:
(test (/ 4 0) =error> "division by zero")
|#


#| 

<WAE>::=<num> 1
       | {+ <WAE> <WAE> } 2
       | {- <WAE> <WAE> } 3
       | {* <WAE> <WAE> } 4
       | {/ <WAE> <WAE> } 5
       | {with {<id> <WAE>} <WAE>} 6
       | <id> 7
Where <num> is any number in racket, and <id> is any symbol in racket


{with {x {+ 4 2}}
      {* x x}}
<WAE> =6> {with {x <WAE>} <WAE>}
=2>  {with {x {+ <WAE> <WAE>}} <WAE>} =1,1>
     {with {x {+ 4 2}} <WAE>} =4>
     {with {x {+ 4 2}} {* <WAE> <WAE>}} =7,7>
     {with {x {+ 4 2}} {* x x}}


"{* {- 3 {+ 5 {- 8 {/ 8 2}}}} {- 3 {+ 5 {- 8 {/ 8 2}}}}}"

{with {radius {- 3 {+ 5 {- 8 {/ 8 2}}}}}
{* {* radius radius} 3.14}}


  1. avoid code duplication
  2. Effecincy
  3. Expressiveness



;==== AST definition ====
(define-type WAE
            [Num Number]
            [Add WAE WAE]
            [Sub WAE WAE]
            [Mul WAE WAE]
            [Div WAE WAE]
            [With Symbol WAE WAE];name, named-expr, body
            [Id Symbol]
)


    Sexpr : Symbol | Number
           | Listof Sexpr





(: parse-sexpr : Sexpr -> WAE)

(define (parse-sexpr sxp)
        (match sxp
          [(number: n) (Num n)]
          [(symbol: name) (Id name)]
          [(cons 'with more)
          ( match sxp
          [(list 'with (list (symbol: name) named-expr) body)
                                 (With name (parse-sexpr named-expr)
                                             (parse-sexpr body))]
             [else (error 'parse-sexpr "bad with syntax!!")])]
          
          [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
          [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
          [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
          [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
          [else (error 'parse-sexpr "bad syntax in ~s" sxp)]
         ))

(: parse : String -> WAE)
(define (parse code)
       (parse-sexpr (string->sexpr code)))

(test (parse "4") => (Num 4))
(test (parse "{+ 3 5}") => (Add (Num 3) (Num 5)))
(test (parse "{+ 3 {- 8 {+ 2 1}}}") => (Add (Num 3) (Sub (Num 8) (Add (Num 2) (Num 1)))))
(test (parse "{+ 1 2 3}") =error> "bad syntax")

(test (parse "{with {x {+ 4 2}} {* x x}}") => (With 'x (Add (Num 4) (Num 2))
                                              (Mul (Id 'x) (Id 'x))))



 eval(<num>) = <num>
 eval("+ E1 E2") = eval(E1) + eval(E2)
 eval("- E1 E2") = eval(E1) - eval(E2)



(: eval : WAE -> Number)
(define (eval expr)
        (cases expr
          [(Num n) n]
          [(Add l r) (+ (eval l) (eval r))]
          [(Sub l r) (- (eval l) (eval r))]
          [(Mul l r) (* (eval l) (eval r))]
          [(Div l r) (if (= (eval r) 0)
                         (error 'eval "division by 0!!")
                      (/ (eval l) (eval r)))]
         ))
(: run : String -> Number)

(define (run code)
         (eval (parse code)
          ))


In class we have seen the grammar for AE  a simple language for
“Arithmetic Expressions”. Write a BNF for “SE”: a similarly simple language
of “String Expressions”. Valid programs (i.e., words in the SE language)
should go along the lines of pl expressions for Strings, with two exceptions:
1. Only digits 0,...,9 are allowed as valid characters within strings; 2. We
will have two types of expressions that are not available in the pl language
(see below 'stringinsert' and 'number->string' type expressions). The valid

operators that can be used in these expressions are string, string-
length, and string-append, and also string-insert and number-

>string. It is also legal to have expressions of the form "<D>", where
<D> stands for a (finite) sequence of digits. Plain values in the language
are characters (of digits) and natural numbers, thus the following are also
valid expressions: a sequence of digits (such as, 347226) and an
expression of the form #\v, where v is a digit.


Here, we do not care about implementing anything – neither the parser nor
the evaluator. Thus, your code should be commented out. Still, it may be
helpful to consider some future semantics that will help you understand
the requirements. The operations " ", string, string-append,

string-insert, and number->string are considered expressions
that represent a string (they would return a string). The operation
string-length, and digit sequences are considered expressions that
represent a natural number (they would return a natural number).
Expression of the form #\v represent a character.

Note the following requirements for the grammar:
string is allowed with a sequence of any number of characters.
string-append is allowed with a sequence of any number of
expressions that represent strings. string-insert is allowed with an
expression that represents a string, a character, and a natural number.
number->string is allowed with a natural number.
For example, some valid expressions in this language are:

"12344"
12
( string #\1 #\2 #\4 )
( string-append ( string #\1 #\2 #\4 ) "12" )
( string-insert "1357" #\4 66 )
( number->string 156879 )
( number->string ( string-length "0033344" ) )
( string-append "45" ( number->string ( string-length
"0033344" ) ))
( string-append )
( string-append "" ( string-insert "1357" #\4 66 ) "" )
#\3


but the following are invalid expressions:

"a2b"
12 13 4 67
( string 124 )
( string-append ( string-length "44" ) "12" )
( string-insert "1357" 4 66 )
( number->string "156879" )
( string-append 33 44 66)
#\3 #\4
#\32
#\q

NOTE: The use of ellipsis ('...') or '*' is not allowed here (find ways
within the BNF framework to specify zero-or-more occurrences of a
previous piece of syntax). Use λ to specify the empty string.

Important remark: Your solution should only be a BNF and not a code in
Racket (or in any other language). You cannot test your code!!! Indeed,
your answer should appear inside a comment block (write the grammar in
a #|---|# comment).

b. Add to your BNF a derivation process for 3 different SE expressions, such
that every operator (e.g., string-append, string-length, and
number->string) appears in at least one of these expressions. You may
either provide a derivation tree or a series of replacements starting with <SE> and
(i)

ending with your string. Mark each derivation rule by an index (use “ ” to state
=>
that in a certain step, you have used rule number i of your BNF).

#|

<SE> ::= <string> | <string-length> | <string-append> | <string-insert> | <number->string> | <digit-sequence>
<string> ::= " <char>* "
<string-length> ::= ( string-length <SE> )
<string-append> ::= ( string-append <SE>* )
<string-insert> ::= ( string-insert <SE> <char> <natural-number> )
<number->string> ::= ( number->string <SE> )
<digit-sequence> ::= <digit>+
<char> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<natural-number> ::= <digit>+



























