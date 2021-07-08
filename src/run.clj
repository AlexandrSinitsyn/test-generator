(ns tester.run
    (:require [tester.GrammarParser :as gp]
              [tester.TestParser :as tp]))


(defn print-test [line & lines]
    (apply println line)
    (if lines (apply print-test lines)))

(defn- main [& args]
    (gp/parse-grammar "grammar.txt")
    (println @gp/integers @gp/strings @gp/ranges @gp/commands)
    (println (clojure.string/join (repeat 120 "-")))
    (apply print-test (tp/parse-test "test.txt")))

(main)
