(ns tester.TestParser
    (:require [tester.parser :as p]
              [tester.GrammarParser :as gp]))


(def generated-result (atom []))


(declare generate-variable generate-line-break)

(defn generate-integer
    ([x] (read-string (str x)))
    ([_ x] (generate-integer x))
    ([br-o bot top br-c]
     (let [x' (generate-variable bot)
           y' (generate-variable top)
           x (if (= br-o :i) x' (+ x' 1))
           y (if (= br-c :i) y' (- y' 1))]
         (+ (rand-int (- y x)) x)))
    ([name br-o bot top br-c]
     (let [res (generate-integer br-o bot top br-c)]
         (swap! gp/integers assoc name [res])
         res)))
(defn generate-string
    ([x] x)
    ([s len] (generate-string (clojure.string/split s #"") len ""))
    ([alphabet len cur]
     (if (zero? len)
         cur
         (recur alphabet (- len 1) (clojure.string/join (cons (nth alphabet (rand-int (count alphabet))) [cur])))))
    ([_ a b _] (generate-string a (read-string b))))
(defn generate-command [& list]
    (nth list (rand-int (count list))))
(defn generate-variable [token]
    (cond
        (vector? token) (apply generate-integer token)
        (number? (read-string token)) (generate-integer "" token)
        :else (let [list (clojure.string/split token #"")
                    marker (first list)
                    word (clojure.string/join (rest list))]
                  (if (= marker ":")
                      (cond
                          (@gp/integers word) (apply generate-integer word (@gp/integers word))
                          (@gp/strings word) (apply generate-string (@gp/strings word))
                          (@gp/commands word) (apply generate-command (@gp/commands word))
                          :else (throw (IllegalArgumentException. (str "Cannot define variable, check its name: " token))))
                      (generate-string token)))))
(declare convert-for)
(defn generate-sequence [[arg & args]]
    (letfn [(gen-seq [res arg & args]
                (cond
                    (= arg ":$") (generate-line-break)
                    (= arg "for") (convert-for (vec (cons arg args)))
                    :else (swap! res conj (generate-variable arg)))
                (if args
                    (apply gen-seq res args)))]
        (apply gen-seq generated-result arg args)))
(defn generate-for-loop
    ([counter-name r body] (apply generate-for-loop counter-name [(@gp/ranges (subs r 1)) body]))
    ([counter-name br-o x y br-c body]
     (let [s (if (= br-o :i) x (+ x 1))
           e (if (= br-c :i) y (- y 1))]
         (generate-for-loop counter-name s e body)))
    ([counter-name s e body]
     (letfn [(for-iter [name cur top body]
                 (swap! gp/integers assoc name [cur])
                 (if (<= cur top)
                     (do
                         (generate-sequence body)
                         (recur name (+ cur 1) top body))))]
         (for-iter counter-name s e body))))


(defn generate-line-break
    ([] (if (empty? @generated-result) (reset! generated-result [[""]]) (apply generate-line-break [] @generated-result)))
    ([cur] (if (vector? cur)
               (swap! generated-result conj [""])
               (reset! generated-result [[cur]])))
    ([lines cur & rst]
     (cond
         (vector? cur) (apply generate-line-break (conj lines cur) rst)
         (every? vector? lines) (reset! generated-result (conj lines (vec (cons cur rst))))
         :else (reset! generated-result [(vec (cons lines (cons cur rst)))]))))



(def +all-chars (mapv char (range 0 128)))
(def +digit (p/+char "0123456789"))
(def +letter (p/+char (apply str (filter #(Character/isLetter %) +all-chars))))
(def +string #(p/+str (p/+map clojure.string/join (apply p/+seq (map (partial p/+char) (clojure.string/split % #""))))))
(def +space (p/+char " \t"))

(def *ws (p/+ignore (p/+star +space)))

(def +separator (p/+ignore (p/+opt (p/+char ","))))
(defn +skip [& list] (p/+ignore (apply p/+or (mapv +string list))))

(def *line-skip (p/+ignore (p/+seq *ws (p/+star (p/+seq (p/+char "\r\n") *ws)))))


(def ube #(throw (IllegalArgumentException. (str "Unsupported bracket exception")))) ; unsupported bracket exception
(def *br-o (p/+map #(case %
                        \( :e ; excluded
                        \[ :i ; included
                        ;\{ :o ; only todo!
                        (ube)) (p/+char "([{")))
(def *br-c (p/+map #(case %
                        \) :e ; excluded
                        \] :i ; included
                        ;\} :o ; only
                        (ube)) (p/+char "}])")))

(def *line-break (p/+map (fn [& args] (generate-line-break)) (+skip "!?")))

(def *token (p/+str (p/+map clojure.string/join (p/+plus (p/+char-not " \u0001\n\r\t,()[]{}")))))


(defn convert-for
    ([[_ counter rng [& body]]]
     (if (vector? rng)
         (let [[br-o bot top br-c] rng
               s (generate-variable bot)
               e (generate-variable top)]
             (generate-for-loop counter br-o s e br-c body))
         (generate-for-loop counter rng body))))


(def *variable (p/+map #(if (= % ":$") (generate-line-break) (swap! generated-result conj (generate-variable %))) *token))
(def +range (p/+seq *ws *br-o *ws *token *ws (p/+ignore (p/+opt +separator)) *ws *token *ws *br-c *ws))
(def *range (p/+map #(swap! generated-result conj (apply generate-integer %)) +range))
(def +command-seq (p/+plus (p/+seqn 0 (p/+or +range *token) *ws)))
(declare *for)
(def +one-line-for (p/+seqf cons +command-seq *line-skip (p/+opt (+skip "done")) *ws))
(defn flatten-one-level [coll]
    (mapcat #(if (sequential? %) % [%]) coll))
(def +multiline-for (p/+seqf cons (p/+map (comp flatten-one-level flatten-one-level)
                                          (p/+plus (p/+seq (+skip "-") *ws (p/+or (delay *for) +command-seq) *line-skip)))
                             (p/+opt (+skip "done")) *ws))
(def *for (p/+map convert-for (p/+seq *ws (+string "for") *ws *token *ws (+skip "in" "=") *ws (p/+or +range *token) *ws (+skip "do") (p/+opt *line-skip)
                                      (p/+map flatten-one-level (p/+or +multiline-for +one-line-for)))))

(defn parse-test [file-name]
    (let [file (slurp file-name)]
        ((p/+parser (p/+star (p/+seq (p/+or *for *range *line-break *variable) *line-skip))) file)
        (generate-line-break)
        @generated-result))
