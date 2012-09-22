(ns totemsa.test.core
  (:use [clojure.test]
        [totemsa.core]))

(defn generative-test-deriv-fn
  "Generate 1000 random doubles [-100,100] and verify that the computed
   derivative's evaluation at those points are within 1/1000th of the
   manually computed derivative's."
  [deriv-fn correct-fn]
  (let [check-times 1000
        ubound 100.0
        lbound -100.0
        tolerance 0.001
        domain-range (- ubound lbound)
        nargs (-> correct-fn class .getDeclaredMethods first .getParameterTypes alength)]
    (->> (fn [] (repeatedly nargs #(- (/ domain-range 2.0) (rand domain-range))))
      (repeatedly check-times)
      (map #(/ (apply correct-fn %) (apply deriv-fn %)))
      (every? #(> (+ 1.0 tolerance) % (- 1.0 tolerance))))))

(deftest test-deriv*
  (is (= (totemsa.core/deriv* '(+ x 3) 'x) 1))
  (is (= (totemsa.core/deriv* '(* x 3) 'x) 3))
  (is (= (totemsa.core/deriv* '(* x y) 'x) 'y)))

(deftest test-deriv-fn
  (is (generative-test-deriv-fn
        (deriv-fn [x] (/ 1 x) x)
        (fn [x] (* -1 (pow x -2.0)))))
  (is (generative-test-deriv-fn
        (deriv-fn [x] (* x x x) x)
        (fn [x] (* 3 (* x x)))))
  (is (generative-test-deriv-fn
        (deriv-fn [x] (sin (* x x)) x)
        (fn [x] (* 2 x (cos (* x x))))))
  (is (generative-test-deriv-fn 
        (deriv-fn [x y] (* x x y y) x)
        (fn [x y] (* y y x 2))))
  (is (generative-test-deriv-fn 
        (deriv-fn [x y] (* (* x y) (+ x 3)) x) 
        (fn [x y] (+ (* (+ x 3) y) (* x y)))))
  ;FAILS, need to properly apply chain rule
  (is (generative-test-deriv-fn
        (deriv-fn [x] (pow x x) x)
        (fn [x] (* (inc (log x)) (pow x x))))))