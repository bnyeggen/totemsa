(ns ^{:author "Bryce Nyeggen, with modifications by David Edgar Liebke"}
  totemsa.core)

(defn ^double pow [^double a ^double b]
  (Math/pow a b))
(defn ^double exp [^double a]
  (Math/exp a))
(defn ^double log [^double a]
  (Math/log a))
(defn ^double sin [^double a]
  (Math/sin a))
(defn ^double cos [^double a]
  (Math/cos a))
(defn ^double tan [^double a]
  (Math/tan a))

;functions of multiple arguments
(def fn-list #{ '+ '- '* '/ 'pow})

;functions of one argument
(def chain-list #{'exp 'log 'sin 'cos 'tan })

;their combination
(def all-fn-list (into fn-list chain-list ))

(defn- same-var?
  "on one arg, checks if variable, ie a symbol not in our list, on two,
  checks vars and equality"
  ([v1] (and (not (contains? all-fn-list v1)) (symbol? v1)))
  ([v1 v2] (and (= v1 v2) (same-var? v1))))

(defn- reduce-expr [e op]
  "return the last (third) item of a list, or a symbol and then everything after that"
  (if (= (count e) 3)
      (nth e 2)
      (conj (nthnext e 2) op)))

(defn- sum? [x] (and (>= (count x) 3) (= (first x) '+)))
(defn- product? [x] (and (>= (count x) 3) (= (first x) '*)))
(defn- expnt? [x] (and (= (count x) 3) (contains? #{'pow 'exp} (first x))))
(defn- difference? [x] (and (>= (count x) 3) (= (first x) '-)))
;Does not allow expressions of the form (/ 3), the (/ 1 3) must be explicit
(defn- quotient? [x] (and (>= (count x) 3) (= (first x) '/)))

(defn- conv-qtnt [x] "Convert a quotient to a product of a base with an inverse"
  (list '* (second x) (list 'pow (list* '*  1 (nthnext x 1)) -1)))

;exp can also kind of be chainrulized below, it makes sense not to though since
;it takes 2 args, not one.  log base whatever is same situation
(defn- expnt [e] (nth e 2))
(defn- chainable? [x]
  (and (contains? chain-list (first x)) (= (count x) 2)))

(defn- make-sum
  "assemble a sum expression properly"
  [a1 a2]
  (cond
    (= a1 0) a2
    (= a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    ;always put numbers first
    (number? a1) (list '+ a1 a2)
    true (list '+ a2 a1)))

(defn- make-prod [a1 a2]
  "assemble a product expression properly"
  (cond
    (= a1 0) 0
    (= a2 0) 0
    (= a1 1) a2
    (= a2 1) a1
    (and (number? a1) (number? a2)) (* a1 a2)
    (number? a1) (list '* a1 a2)
    true (list '* a2 a1)))

(defn- make-expnt [b e]
  "assemble an exponent expression properly."
  (cond
    (= b 0) 0
    (= b 1) 1
    (= e 0) 1
    (= e 1) b
    (and (number? b) (number? e)) (pow b e)
    true (list 'pow b e)))

(defn deriv*
  "main sub-function for differentiation. with 2 args, takes 1st degree deriv.
  with 3, takes arbitrary degrees. contains all deriv rules for basic funcs.

  Examples:

    (deriv* '(+ x 3) 'x)
    (deriv* '(* x y) 'x)
    (deriv* '(* (* x y) '(+ x 3)) 'x)
    (deriv* '(* (* x y) (+ x 3)) 'y)

    (deriv* '(* x y (+ x 3)) 'x)
    (deriv* '(* x y (+ x 3)) 'y)

    (deriv* '(* x y (+ x 3)) 'x 2)
    (deriv* '(* x y (+ x 3)) 'x 3)

"
  ([exp v]
    (cond
      (number? exp) 0
      (same-var? exp v) 1
      (and (same-var? exp) (not= exp v)) 0
      (sum? exp) (make-sum (totemsa.core/deriv* (second exp) v) (totemsa.core/deriv* (reduce-expr exp '+) v))
      (difference? exp) (make-sum (totemsa.core/deriv* (second exp) v)
                          (totemsa.core/deriv* (make-prod -1 (reduce-expr exp '+)) v))
      (product? exp)
      (make-sum
        (make-prod (second exp)
          (totemsa.core/deriv* (reduce-expr exp '*) v))
        (make-prod (totemsa.core/deriv* (second exp) v)
          (reduce-expr exp '*)))
      (quotient? exp) (totemsa.core/deriv* (conv-qtnt exp) v)
      (expnt? exp)
      (let [u (second exp)
            n (expnt exp)]
        (make-prod (make-prod
                     (expnt exp)
                     (make-expnt (second exp) (make-sum (expnt exp) -1)))
          (totemsa.core/deriv* (second exp) v)))
      (chainable? exp)
      (let [u (first exp)
            n (second exp)]
        (cond
          (number? n) 0;things could be out-of-bounds a la log(0), but that's philosophical
          (= 'sin u) (make-prod (list 'cos n) (totemsa.core/deriv* n v))
          (= 'cos u) (make-prod (list '* -1 (list 'sin n)) (totemsa.core/deriv* n v))
          (= 'tan u) (make-prod (list 'pow (list 'cos n) -2) (totemsa.core/deriv* n v))
          ;multiply by inverse of denominator is same as numerator/denominator
          (= 'log u) (make-prod (totemsa.core/deriv* n v) (list 'pow n -1))
          (= 'exp u) (make-prod (list 'exp n) (totemsa.core/deriv* n v))
          true false));should not happen as chainable? refers to a list that
      ;we should completely specify here
      true (list 'totemsa.core/deriv* exp v);some kind of error here, return a description of
      ;"the derivative of this function" rather than the actual result
      ))
  ([exp vr degree]
    (loop [x exp v vr dgr degree]
      (if (zero? dgr) x
        (recur (totemsa.core/deriv* x v) v (dec dgr) )))))


(defmacro deriv
  "Macro for symbolic differentiation. with 2 args, takes 1st degree deriv.
  with 3, takes arbitrary degrees. contains all deriv rules for basic funcs.


  Examples:

    (deriv (+ x 3) x) ; => 1
    (deriv (* x y) x) ; => y
    (deriv (* (* x y) (+ x 3)) x) ; => (+ (* (+ x 3) y) (* x y))
    (deriv (* (* x y) (+ x 3)) y) ; => (* (+ x 3) x)

    (deriv (* x y (+ x 3)) x) ; => (+ (* y (+ x 3)) (* y x))
    (deriv (* x y (+ x 3)) y) ; => (* (+ x 3) x)

    (deriv (sin x) x) ; => (cos x)
    (deriv (cos x) x) ; => (* -1 (sin x))

    (deriv (sin (* x y)) y) ; => (* x (cos (* x y)))

    (deriv (pow x 3) x) ; => (* 3 (pow x 2))

    (deriv (pow x 3) x 2) ; => (* 3 (* 2 x))

    (deriv (* x y (+ x 3)) x 2) ; => (+ y y)
    (deriv (* x y (+ x 3)) x 3) ; => 0

    (deriv (+ (* 3 x) (* 8 x)) x) ; => 11
"
  ([exp v]
     `(totemsa.core/deriv* '~exp '~v))
  ([exp v degree]
     `(totemsa.core/deriv* '~exp '~v ~degree)))

(defn- tree-subst
"

  Examples:
    (use '(incanter core symbolic))

    (def ops {'+ clojure.core/+
		'- clojure.core/-
		'* clojure.core/*
		'/ clojure.core//
		'sin totemsa.core/sin
		'cos totemsa.core/cos
		'tan totemsa.core/tan
		'pow totemsa.core/pow
		'exp totemsa.core/exp
                'fn clojure.core/fn})

    (tree-subst '(+ (* x y) x) {'x 3, 'y 9, '* 'clojure.core/*, '+ 'clojure.core/+})
    (eval (tree-subst '(+ (* x y) x) {'x 3, 'y 9, '* 'clojure.core/*, '+ 'clojure.core/+}))

    (tree-subst (deriv (+ (* x y) x) x)  (apply assoc ops ['x 3 'y 9]))
    (eval (tree-subst (deriv (+ (* x y) x) x) (apply assoc ops ['x 3 'y 9])))

    (fn [x y] (tree-subst (deriv (+ (* x y) x) x)  (apply assoc ops ['x 3 'y 9])))
    
    ((fn [x y] (eval (tree-subst (deriv (+ (* x y) x) x) (apply assoc ops ['x 3 'y 9])))) 5 9)

    ((eval (tree-subst (list 'fn '[x y] (deriv (+ (* x y) x) x))
                       (apply assoc ops ['x (gensym 'x) 'y (gensym 'y)]))) 
      5 9)

     ((eval (tree-subst (list 'fn '[x y] (deriv* '(+ (* x y) x) 'x))
                       (apply assoc ops ['x (gensym 'x) 'y (gensym 'y)]))) 
      5 9)


"
  ([tree subst-map]
     (let [subst-fn (fn [el] 
		      (cond
		       (vector? el)
		         (apply vector (tree-subst el subst-map))
		       (coll? el) 
		         (tree-subst el subst-map) 
		       :else
		         (or (subst-map el) el)))]
       (map subst-fn tree))))


(defn deriv-fn*
"
  Examples:
    (deriv-fn* '[x y] '(+ (* x y) x) 'x)

    ((deriv-fn* '[x y] '(+ (* x y) x) 'x) 5 9)
"
  ([[& args] expr v]
     (deriv-fn* args expr v 1))
  ([[& args] expr v degree]
     (let [ops {'+ clojure.core/+
                '- clojure.core/-
                '* clojure.core/*
                '/ clojure.core//
                'sin totemsa.core/sin
                'cos totemsa.core/cos
                'tan totemsa.core/tan
                'pow totemsa.core/pow
                'exp totemsa.core/exp}] 
       (eval (tree-subst (list 'fn (apply vector args) (totemsa.core/deriv* expr v degree))
                         (apply assoc ops (interleave args (map gensym args))))))))


(defmacro deriv-fn
"  Examples:
    (deriv-fn [x y] (+ (* x y) x) x)

    ((deriv-fn [x y] (+ (* x y) x) x) 5 9)"
([[& args] expr v]
   `(totemsa.core/deriv-fn* '[~@args] '~expr '~v 1))
([[& args] expr v degree]
   `(totemsa.core/deriv-fn* '[~@args] '~expr '~v ~degree)))