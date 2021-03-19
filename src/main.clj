;; Author: Gregory Maldonado
;; Symbolic Simplification for boolean expressions
;; Created March 5, 2021

(ns main
  (:import (clojure.lang PersistentList LazySeq)))

"The four below functions are utility function that will be used through out the entire program to make life easier"

"Function that takes two expressions and make an AND expression"
(defn andexp [exp1 exp2] (list 'and exp1 exp2))
"Function that takes two expressions and makes an OR expression"
(defn orexp  [e1 e2] (list 'or e1 e2))
"Function that takes an expression a"
(defn notexp [e1] (list 'not e1))

"Function that drops the nth element in a list. The standard library for Clojure does not come with a remove index function.
This function finds the nth term in the list to remove recursively and then returns a new list. Since clojure is a functional
language and all variables are immutable, a work around is to pass the newly generated list each recursive call and then at the
end return the new list {new-coll} at the end.."
(defn drop-nth

  ; n:        index in the list
  ; new-coll: the new list without the nth term                     NOTE ON NEW-COLL: ; when calling drop-nth, the parameter for new-coll, should always be set to and empty list '()
  ; coll:     original list with the nth term                                         ; If the parameter is a non zero count list then the return list will be that non zero count + all elements
  [n new-coll c]                                                                   ; appended to the end of that list. This is a way to get away with variables being immutable.
  (let [coll (into (list) (into (list) c))]
  (cond
    ; If the nth term is found, pop the term off the original list and return the new list without adding the nth term
    (= n 0) (drop-nth (- n 1) new-coll (pop coll))

    ; If the nth term has already been removed
    (< n 0) (cond
              ; If the count of the original list is 0 then return the new list generated without the nth term
              (= (count coll) 0) (apply list new-coll)
              ; If there are still elements in the original list, then recursively remove each element in the original list and place them in the new list
              (> (count coll) 0) (drop-nth n (seq (conj (vec new-coll) (first coll))) (pop coll)))

    ; If the nth term has not been found, recursively call this function but with (n - 1) and remove the first element in the original list and place it in the new list
    (> n 0) (drop-nth (- n 1) (seq (conj (vec new-coll) (first coll))) (pop coll))))
  )


"
Function that performs symbolic and simplification on un-nested expressions

; [] The first element in the parameter list is 'and'
; [] Everything after that are expressions
; [] Thought-process: Look at two elements in the parameterized list at a time, then get the result of the symbolic simplification and apply it
;    To the next element in the list
; [] Continue till there are no more elements in the list and return the finally symbolic simplification

; Try to get the expression down to just 3 inputs -> (and x y) and then return that value to be used on another set of expressions
"
(defn and-simplify
  [exp]

  (cond

    ; If the count of the list is 2 (and {expression} ) then return the {expression}
    (= (count exp) 2) (last exp)

    ; If the count of the list is 3 (and {expression} {expression}) then try to evaluate the expression and if the expression can not be evaluate ie. (and x y) then return the AND expression
    ; This is done by checking the types of the two expressions, if the types are boolean expressions then evaluate the expression, variables then the expression can not be evaluated fully
    (= (count exp) 3) (let [variable-one-type (type (nth exp 1))]
                        (let [variable-two-type (type (nth exp 2))]

                          (cond
                            ; If the first expression is a boolean and the second expression is not
                            (and (= variable-one-type Boolean) (not= variable-two-type Boolean)) (cond
                                                                                                                       ; If the first exp is true then return the variable
                                                                                                                       (= (nth exp 1) true) (nth exp 2)
                                                                                                                       ; If the first exp is false then return false
                                                                                                                       (= (nth exp 1) false) false
                                                                                                                       )

                            ;Conditional that the second variable's type is a boolean but the first variable type isnt
                            (and (= variable-two-type Boolean) (not= variable-one-type Boolean)) (cond
                                                                                                                       ; If the second exp is true then return variable
                                                                                                                       (= (nth exp 2) true) (nth exp 1)
                                                                                                                       ; If the first exp is false then return false
                                                                                                                       (= (nth exp 2) false) false
                                                                                                                       )
                            ; If the both expressions are lists, then concat the expressions together
                            (and (and (= (type (first (pop exp))) PersistentList) (= (type (last exp)) PersistentList)) (= (first (first (pop exp)) ) (first (last exp)) ))  (concat (first (pop (exp))) (drop-nth 0 '() (last exp)))
                            ; If the first expression is a list and not the second, then concat the second element to the list
                            (and (= (type (first (pop exp))) PersistentList) (= (first exp) (first (pop exp)))) (concat (first (pop exp)) (list (last exp)))
                            ; If the second expression is a list and not the first, then concat the first element to the list
                            (and (= (type (last exp)) PersistentList) (= (first exp) (first (last exp)))) (concat (last exp) (list (first (pop exp))))

                            ;Conditional that both variable types are booleans then return the evaluated expression
                            (and (= variable-one-type Boolean) (= variable-two-type Boolean)) (and (nth exp 1) (nth exp 2))

                            ;Conditional that can not be evaluated farther, then return the AND expression
                            (and (not= variable-one-type Boolean) (not= variable-two-type Boolean)) (list "and" (nth exp 1) (nth exp 2)))))
    ; If the count is greater than 3, then recursively simplify the expression
    ; This is done by evaluating the first two expressions and then recursively calling the function on the rest of the expression, minus the first two expresions
    ; and then evaluating the expression in full
    (> (count exp) 3) (and-simplify (list "and" (and-simplify (list "and" (nth exp 1) (nth exp 2))) (and-simplify (drop-nth 1 '() (drop-nth 1 '() exp)))))
    ))

"Function that performs symbolic simplification for the or boolean operator
The function is recursive to break down expressions that have 3 or more booleans
The first element in the exp list is or and everything after that is a boolean"
(defn or-simplify [exp]

  (cond
    ; Checks if the exp contains true -> if true then dont bother going through the exp, just return true
    (.contains exp 'true) true

    ; If the exp just contains one boolean then just return that boolean
    (= (count exp) 2) (last exp)

    ;If the exp has two booleans then grab the first and second types for the booleans
    (= (count exp) 3) (let [variable-one-type (type (first (pop exp)))]
                        (let [variable-two-type (type (last exp))]

                          (cond

                            ; If the both expressions are lists, then concat the expressions together
                            (and (= (type (first (pop exp))) PersistentList) (= (type (last exp)) PersistentList)) (concat (first (pop (exp))) (drop-nth 0 '() (last exp)))
                            ; If the first expression is a list and not the second, then concat the second element to the list
                            (= (type (first (pop exp))) PersistentList) (concat (first (pop exp)) (list (last exp)))
                            ; If the second expression is a list and not the first, then concat the first element to the list
                            (= (type (last exp)) PersistentList) (concat (last exp) (list (first (pop exp))))

                            ; If both  types are booleans then evaluate the booleans and return the final boolean
                            (and (= variable-one-type Boolean) (= variable-two-type Boolean)) (or (first (pop exp)) (last exp))

                            ; If the first type is boolean but not the second
                            (and (= variable-one-type Boolean) (not= variable-two-type Boolean)) (cond
                                                                                                                       ; If the boolean is false then return the variable
                                                                                                                       (= (first (pop exp)) false) (last exp)
                                                                                                                       ; If the boolean is true then return true
                                                                                                                       (= (first (pop exp)) true) true
                                                                                                                       )
                            ; If the second type is boolean but not the first
                            (and (= variable-two-type Boolean) (not= variable-one-type Boolean)) (cond
                                                                                                                       ; If the boolean is false then return the variable
                                                                                                                       (= (last exp) false) (first (pop exp))
                                                                                                                       ; If the boolean is true then return true
                                                                                                                       (= (last exp) true) true
                                                                                                                       )
                            ; If both types are symbols then return the exp
                            (and (not= variable-one-type Boolean) (not= variable-two-type Boolean)) exp

                            )))
    ; If the count of the exp is greater than 3, then recursively simplify the expression. First simplify the first two booleans together and then recursively call the function on the rest of the list
    ; At the end, combine all the evaluations together with another recursive call
    (> (count exp) 3) (or-simplify (list "or" (or-simplify (list "or" (nth exp 1) (nth exp 2))) (or-simplify (drop-nth 1 '() (drop-nth 1 '() exp)))))

    ))

"Recursive Function for de-morgans law
 The function iterates over each boolean variable and negates them and places them in a list with the opposite boolean operator
 new-coll starts with the new boolean operator and each recursive call builds the new-coll with each negated variable
      - If new-coll has the boolean operator 'and' then the new-coll with start with 'or'
      -If new-coll has the boolean operator 'or' then the new-coll with start with 'and'
 The function returns the new-coll when there are no more variables to iterate over
"
(defn demorgans
  ; n - the number of variables in the expression
  ; new-coll - the list that will be returned at the end of all of the negated variables plus the starting boolean operator
  ; coll - the starting expression that removes a variable each recursive call
  [n new-coll coll]

  ;Checks if there are anymore variables
  (cond
    ; If no more variables then return the new-coll
    (= n 0) new-coll
    ; If there are more variables then recursively call this function with one less variable in coll and place it in new-coll but negated
    (> n 0) (demorgans (- n 1) (concat new-coll (list (list 'not (first coll)) ) ) (pop coll) )))

"Function that negates an expression
Takes in an expression that is a list and evaluates it
"
(defn not-simplify
  [exp]
  ; Checks the type of the boolean
  (let [exp-type (type (last exp))]

    (cond
      ; If the type is a boolean
      (= exp-type Boolean) (cond
                                          ; return the opposite boolean
                                         (= (last exp) true) false
                                         (= (last exp) false) true
                                         )

      ; If the type is a list then it is a special case
      (= exp-type PersistentList) (let [boolean-operation (first (last exp))]
                                                 (cond
                                                   ; If the list starts with a not operator, then its a double negation and just return the variable
                                                   (= boolean-operation (symbol "not")) (last (last exp))
                                                   ; If the list starts with and, then it is demorgan and return the negated variables with the boolean operator or
                                                   (= boolean-operation (symbol "and")) (demorgans (count (pop (last exp))) '(or) (pop (last exp)))
                                                   ; If the list starts with or, then it is demorgan and return the negated variables with the boolean operator and
                                                   (= boolean-operation (symbol "or")) (demorgans (count (pop (last exp))) '(and) (pop (last exp)))
                                                   )
                                                 )
      ; If the type is neither a boolean or list then just return the expression
      (and (not= exp-type Boolean) (not= exp-type PersistentList)) exp
      )))

"Recursive function that checks if an arg is a list and if it is a list then check if their args are lists so on so forth until there are no more lists and the expression can be evaluated"
(defn simplify-exp [exp]
  (let [boolean-operator (first exp)]
    ; Checks the boolean-operator

    (cond
      (= boolean-operator (symbol 'and)) (let [arg1 (first (pop exp))]
                                           (let [arg2 (last exp)]
                                             (cond
                                               (and (= (type arg1) PersistentList) (= (type arg2) PersistentList)) (and-simplify (list "and" (simplify-exp arg1) (simplify-exp arg2)))
                                               (= (type arg1) PersistentList) (and-simplify (list "and" (simplify-exp arg1) arg2))
                                               (= (type arg2) PersistentList)  (and-simplify (list "and" arg1 (simplify-exp arg2)))
                                               (and (not= (type arg1) PersistentList) (not= (type arg2) PersistentList)) (and-simplify exp)
                                               )
                                             ))

      (= boolean-operator (symbol 'or)) (let [arg1 (first (pop exp))]
                                          (let [arg2 (last exp)]
                                            (cond
                                              (and (= (type arg1) PersistentList) (= (type arg2) PersistentList)) (or-simplify (list "or" (simplify-exp arg1) (simplify-exp arg2)))
                                              (= (type arg1) PersistentList) (or-simplify (list "or" (simplify-exp arg1) arg2))
                                              (= (type arg2) PersistentList) (or-simplify (list "or" arg1 (simplify-exp arg2)))
                                              (and (not= (type arg1) PersistentList) (not= (type arg2) PersistentList)) (or-simplify exp)
                                              )

                                            ))

      (= boolean-operator (symbol 'not)) (let [arg (last exp)]
                                           (cond
                                             (= (type arg) PersistentList) (not-simplify (list "not" (simplify-exp arg)))
                                             (not= (type arg) PersistentList) (not-simplify exp)
                                             )
                                           )
      )))
(defn turn-to-list [exp]
    (let [boolean-operator (first exp)]
      (cond
        (or (= boolean-operator (symbol 'and)) (= boolean-operator (symbol 'or)))
                                            (let [arg1 (first (drop-nth 0 '() exp)) ]
                                             (let [arg2 (last exp)]
                                               (cond
                                                 (and (= (type arg1) LazySeq) (= (type arg2) LazySeq)) (list boolean-operator (turn-to-list arg1) (turn-to-list arg2))
                                                 (= (type arg1) LazySeq) (list boolean-operator (turn-to-list arg1) arg2)
                                                 (= (type arg2) LazySeq) (list boolean-operator arg1 (turn-to-list arg2))
                                                 (and (not= (type arg1) LazySeq) (not= (type arg2) LazySeq)) (list boolean-operator arg1 arg2)
                                                 )
                                               ))

        (= boolean-operator (symbol 'not)) (let [arg (last exp)]
                                             (cond
                                               (= (type arg) LazySeq) (list boolean-operator (turn-to-list arg))
                                               (not= (type arg) LazySeq) (list boolean-operator arg)
                                               )
                                             )

        )
      )
  )
(defn lookup [i m]
  "This function looks up a value, i, in map m and returns the result if it exists, and otherwise returns i."
  (get m i i))

(defn substitute [l m]
  "This function does a deep substitute on lists"
  (map (fn [i]
         (if (seq? i)
           (substitute i m)
           (lookup i m)))
       l))

(defn evalexp [l m]
  "This function further evalutes the expression based on the boolean operator in the first index of the list and returns the value"
  (let [exp (substitute l m)]
      (simplify-exp (turn-to-list exp))
      ))

;;

;Test Cases:


;(println (simplify-exp '(not (and (not x) true))))
;(println (simplify-exp '(or false x)))
(println (evalexp '(and x (or x (and y (not z)))) '{x false, z true}))
;(println (evalexp '(and x (or x (and y (not z)))) '{z false}))
;(println (evalexp '(not (and y (not x))) '{y true}))


