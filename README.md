# Symbolic-Simplification
Clojure Application for symbolic simplification (AND OR NOT)

[Programming Languages (CSC 344) Project 2](https://danielschlegel.org/wp/teaching/csc344-spring-2021/assignment-2/)

The simplify methods takes a list and outputs a simplified list. The ```(evalexp [l m])``` takes a simplified expression and a map of variables to booleans and evaluates the expression by substituting the varibles into the expression and then calling the respective simplify method again. 


### Test Cases
```clojure
(println (or-simplify '(or true)))         ; - true
(println (or-simplify '(or false)))        ; - false
(println (or-simplify '(or x)))            ; - x
(println (and-simplify '(and true)))       ; - true
(println (and-simplify '(and false)))      ; - false
(println (and-simplify '(and x)))          ; - x
(println (not-simplify '(not false)))      ; - true
(println (not-simplify '(not true)))       ; - false
(println (not-simplify '(not (and x y))))  ; - (or (not x) (not y))
(println (not-simplify '(not (or x y))))   ; - (or (not x) (not y))
(println (not-simplify '(not (not x))))    ; - x
(println (or-simplify '(or x false)))      ; - x
(println (or-simplify '(or false x)))      ; - x
(println (or-simplify '(or true x)))       ; - true
(println (or-simplify '(or x true)))       ; - true
(println (and-simplify '(and x false)))    ; - false
(println (and-simplify '(and false x)))    ; - false
(println (and-simplify '(and x true)))     ; - x
(println (and-simplify '(and true x)))     ; - x
(println (or-simplify '(or x y true)))     ; - true
(println (or-simplify '(or x false y)))    ; - (or x y)
(println (and-simplify '(and false x y)))  ; - false
(println (and-simplify '(and x true y)))   ; - (and x y)

(println (evalexp (and-simplify '(and x true y z)) '{x true, y true}))    ; - z
(println (simplify-exp '(not (and (not x) true))))                        ; - x
(println (evalexp '(and x (or x (and y (not z)))) '{x false, z true}))    ; - false
```
