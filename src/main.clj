;; Author: Gregory Maldonado
;; Symbolic Simplification for boolean expressions
;; Created March 5, 2021

(ns main)

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
  ; new-coll: the new list without the nth term
  ; coll:     original list with the nth term
  [n new-coll coll]

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

(defn AND
  [exp]

  "
  Function that performs symbolic simplification on un-nested expressions

  ; [] The first element in the parameter list is 'and'
  ; [] Everything after that are expressions
  ; [] Thought-process: Look at two elements in the parameterized list at a time, then get the result of the symbolic simplification and apply it
  ;    To the next element in the list
  ; [] Continue till there are no more elements in the list and return the finally symbolic simplification

  ; Try to get the expression down to just 3 inputs -> (and x y) and then return that value to be used on another set of expressions
  "
  (cond

    ; If the count of the list is 2 (and {expression} ) then return the {expression}
    (= (count exp) 2) (last exp)

    ; If the count of the list is 3 (and {expression} {expression}) then try to evaluate the expression and if the expression can not be evaluate ie. (and x y) then return the AND expression
    ; This is done by checking the types of the two expressions, if the types are boolean expressions then evaluate the expression, variables then the expression can not be evaluated fully
    (= (count exp) 3) (let [variable-one-type (type (nth exp 1))]
                        (let [variable-two-type (type (nth exp 2))]

                          (cond

                            ; If the both expressions are lists, then concat the expressions together
                            (and (= (type (first (pop exp))) clojure.lang.PersistentList) (= (type (last exp)) clojure.lang.PersistentList)) (concat (first (pop (exp))) (drop-nth 0 '() (last exp)))
                            ; If the first expression is a list and not the second, then concat the second element to the list
                            (= (type (first (pop exp))) clojure.lang.PersistentList) (concat (first (pop exp)) (list (last exp)))
                            ; If the second expression is a list and not the first, then concat the first element to the list
                            (= (type (last exp)) clojure.lang.PersistentList) (concat (last exp) (list (first (pop exp))))

                            ; If the first expression is a boolean and the second expression is not
                            (and (= variable-one-type java.lang.Boolean) (not= variable-two-type java.lang.Boolean)) (cond
                                                                                                                       ; If the first exp is true then return the variable
                                                                                                                       (= (nth exp 1) true) (nth exp 2)
                                                                                                                       ; If the first exp is false then return false
                                                                                                                       (= (nth exp 1) false) false
                                                                                                                       )

                            ;Conditional that the second variable's type is a boolean but the first variable type isnt
                            (and (= variable-two-type java.lang.Boolean) (not= variable-one-type java.lang.Boolean)) (cond
                                                                                                                       ; If the second exp is true then return variable
                                                                                                                       (= (nth exp 2) true) (nth exp 1)
                                                                                                                       ; If the first exp is false then return false
                                                                                                                       (= (nth exp 2) false) false
                                                                                                                       )

                            ;Conditional that both variable types are booleans then return the evaluated expression
                            (and (= variable-one-type java.lang.Boolean) (= variable-two-type java.lang.Boolean)) (and (nth exp 1) (nth exp 2))

                            ;Conditional that can not be evaluated farther, then return the AND expression
                            (and (not= variable-one-type java.lang.Boolean) (not= variable-two-type java.lang.Boolean)) (list "and" (nth exp 1) (nth exp 2)))))


    ; If the count is greater than 3, then recursively simplify the expression
    ; This is done by evaluating the first two expressions and then recursively calling the function on the rest of the expression, minus the first two expresions
    ; and then evaluating the expression in full
    (> (count exp) 3) (AND (list "and" (AND (list "and" (nth exp 1) (nth exp 2))) (AND (drop-nth 1 '() (drop-nth 1 '() exp)))))))

(defn NOT
  [exp]

  

  )