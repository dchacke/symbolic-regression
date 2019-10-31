; ; Todos
; 1. Make work with any fn, not just numeric functions
;    (this most likely requires turning datastructures
;    into numbers so that deltas can be calculated)
; 2. Dynamically try different functions and terminals
;    (make these dynamic sets part of the fitness
;    evaluation as well)
; 3. Dynamically vary depth
; 4. Apply fitness function with same values of target-fn
;    in every iteration so we are not comparing apples
;    and oranges
; 5. Add to fitness function quality metrics other than
;    just matching return values, such as program's
;    conciseness, big o, speed, etc
;    (see https://airbrake.io/blog/metrics/code-quality-metrics-management).
;    E.g. (* (* 2 x) (* 1 x)) can be better rewritten as
;    (* 2 x x)
;    All these criteria may be covered by making sure that
;    any addition to the syntax tree costs the program some.
;    This should be run first, before the function implementation
;    is tested.
;    fitness, which has to be offset by some functional gain
; 6. Build up a "tree of knowledge" by saving good past guesses
;    and their function implementations for reuse by making them
;    available in future function sets (and perhaps terminal sets
;    for doing functional programming)
; 7. Implement crossover
; 8. Adjust algorithm so that I have to specify less and less
;    when I run it. Eventually I should not need to specify any
;    of the following:
;    - depth
;    - functions
;    - terminals
;    as all of these can themselves be the result of trial and error.
; 9. Infer arities of created functions automatically
; 10. Handle flexible arities (such as +, -, *, etc)
; 11. Make the fitness function just add up how many invocations were
;     correct and how many were not instead of measuring "how close"
;     each invocation was to the correct result. This should make it
;     work for any datatype.

(ns first-genetic-program.core
  (:require [clojure.walk :as w]))

(defn safe-division
  "Safely divides a by b, returning 0 if b is 0"
  [a b]
  (if (zero? b)
    0
    (/ a b)))

; (def max-depth 2)
; (def functions '([+ 2] [* 2] [- 2] #_[safe-division 2]))

; This knowledge grows every time the interact fn is run.
; This is how we "educate" the genetic algorithm; through
; a growing repertoire of functions it can use. For example,
; we can "teach" it inc, and then it can evolve #(+ % 1 1)
; despite a terminal set of #{1} (i.e. not including 2) and
; while being restricted to arity 2 for +; the result will
; most likely be (inc (+ x 1)) or even (inc (inc x)).
(def knowledge (atom {'+ {:implementation +
                          :arity 2}
                      '- {:implementation -
                          :arity 2}
                      '* {:implementation *
                          :arity 2}}))
(def terminals '(x 1 #_2))

(defn knowledge->fns
  "Turns the current knowledge into a list of
  functions and their arities, formatted
  ([fn1 2] [fn2 2] ...)"
  []
  (->>
    @knowledge
    vals
    (map
      (fn [fn-info]
        [(:implementation fn-info) (:arity fn-info)]))))

(defn random-program
  "Returns a random program given max depth,
  list of functions and their arities, and
  terminals"
  [depth functions terminals]
  (cond
    (zero? depth) (rand-nth terminals)
    (< (rand) 0.2) (rand-nth terminals)
    :else
    (let [[fun arity] (rand-nth functions)]
      (cons fun
        (repeatedly arity
          #(random-program (dec depth) functions terminals))))))

(defn random-population
  "Returns a random population of programs of
  given size given their max depth, list of
  functions and their arities, and terminals"
  [size max-depth functions terminals]
  (repeatedly size #(random-program (rand-int (inc max-depth)) functions terminals)))

(defn program->fn
  "Turns a datastructure representing a program
  into an executable function"
  [program]
  (eval (list 'fn '[x] program)))

(defn fitness
  "Determines the fitness of the given program
  given a target fn, from which target fn it
  derives explicanda. The target fn is expected
  to be a function of one numeric parameter that
  returns a number."
  ; make this just add up how many invocations were
  ; correct and how many were not
  [program target-fn]
  (->>
    (repeatedly 10 #(rand-int 100))
    (map
      (fn [x]
        {:x x
         :y (target-fn x)}))
    (reduce
      (fn [acc {x :x y :y}]
        (let [f (program->fn program)
              result (f x)
              delta (Math/abs ^long (-' result y))]
          (-' acc delta)))
      0)))

(defn mutate
  "Mutates the given program"
  [program functions terminals]
  (w/postwalk
    (fn [form]
      (if
        (and
          (seq? form)
          (< (rand) 0.01))
        (random-program (rand-int 5) functions terminals)
        form))
    program))

(defn evolve
  "Evolves a given population size of
  programs of given depth towards a given target function
  using the given functions and terminals
  for a given maximum number of generations
  or until a fitness score of 0 is found,
  whichever happens sooner."
  ([target-fn population-size max-depth functions terminals max-generations]
   (let [programs (random-population population-size max-depth functions terminals)]
    (evolve programs target-fn population-size functions terminals max-generations 0)))
  ([programs target-fn population-size functions terminals max-generations curr-generation]
   (println "the function are...")
   (clojure.pprint/pprint functions)
   (clojure.pprint/pprint programs)
   (let [fittest-programs
         (->>
          programs
          (map
            (fn [program]
              {:program program
               :fitness (fitness program target-fn)}))
          (sort-by :fitness >)
          (take (-> programs count (/ 2))))
         fittest-program (first fittest-programs)]
    (cond
      (= max-generations curr-generation)
      (do
        (println "Reached max generation" max-generations "- fittest program is" fittest-program)
        fittest-programs)
      (zero? (-> fittest-programs first :fitness))
      (do
        (println (str "!! Found perfect fit at generation " curr-generation ": " fittest-program))
        fittest-programs)
      :else
      (let [new-programs
            (->>
              fittest-programs
              (map :program)
              (reduce #(conj %1 %2 %2) []) ; duplicate each program
              (map #(mutate % functions terminals)))]
        (recur new-programs target-fn population-size functions terminals max-generations (inc curr-generation)))))))

(defn interact
  "Interactively evolves a target program with the user"
  []
  (let [_ (println "What is the target function?")
        target-fn (-> (read-line) read-string eval)
        _ (println "What is the population size?")
        population-size (-> (read-line) read-string)
        _ (println "What is the max depth?")
        max-depth (-> (read-line) read-string)
        _ (println "What is the maximum number of generations?")
        max-generations (-> (read-line) read-string)
        functions (knowledge->fns)]
    (println "Starting evolution...")
    (loop [programs (evolve target-fn population-size max-depth functions terminals max-generations)]
      (println "Fittest program found:" (first programs))
      (println "Is this program good enough? y/n")
      (if (= (read-line) "y")
        (let [_ (println "What is this function called (as symbol)?")
              fn-name (-> (read-line) read-string symbol)]
          (swap! knowledge assoc fn-name {:implementation (-> programs first :program program->fn)
                                          :arity 1}))
        (do
          (println "Okay, refining these programs...")
          (recur (evolve (map :program programs) target-fn population-size functions terminals max-generations 0)))))))
