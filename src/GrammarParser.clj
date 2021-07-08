(ns tester.GrammarParser
    (:require [tester.parser :as p]))

(def +all-chars (mapv char (range 0 128)))
(def +digit (p/+char "0123456789"))
(def +letter (p/+char (apply str (filter #(Character/isLetter %) +all-chars))))
(def +string #(p/+str (p/+map clojure.string/join (apply p/+seq (map (partial p/+char) (clojure.string/split % #""))))))
(def +space (p/+map (constantly " ") (p/+char (apply str (filter #(Character/isWhitespace %) +all-chars)))))

(def *ws (p/+ignore (p/+star +space)))

(def +separator (p/+ignore (p/+opt (p/+char ","))))
(defn +skip [& list] (p/+ignore (apply p/+or (mapv +string list))))

(def *token (p/+str (p/+map (comp clojure.string/join flatten) (p/+seq (p/+opt (p/+char ":")) (p/+plus (p/+or +letter +digit))))))
(def *text (p/+str (p/+map (comp clojure.string/join flatten) (p/+plus (p/+char-not " ()[]{}"))))) ; (apply str (filter #(Character/isWhitespace %) +all-chars))


(def integers (atom {"Z" [0]
                     "T" [100]
                     "N" [100]
                     "S" [100000]}))
(def strings (atom {"$" ["\n"]
                    ":" [":"]
                    "@" ["@"]}))
(def ranges (atom {}))
(def commands (atom {}))

(defn parse-variable [list]
    #(letfn [(prs
                 ([name _ nmb _] (swap! list assoc name [nmb]))
                 ([name o btm top c] (if top (swap! list assoc name [o btm top c]) (prs name o btm c))))]
         (apply prs %)))
(defn parse-command [[name & cmds]] (swap! commands assoc name cmds))


(def *line-skip (p/+seq *ws (p/+star (p/+seq (p/+char "\r\n") *ws))))

(def *comment (p/+seq (+string "//") (p/+star (p/+char-not "\r\n")) *line-skip))

(def *br-o (p/+map #(if (= % \() :e :i) (p/+char "([")))
(def *br-c (p/+map #(if (= % \)) :e :i) (p/+char "])")))

;(defn component-no-type [parse-func br-o expr-seq br-c] (p/+map (comp parse-func flatten) (p/+seq *ws *token *ws (+skip br-o) *ws expr-seq *ws (+skip br-c) *ws)))
(defn component [parse-func list expr-seq]
    (p/+map (comp parse-func flatten) (p/+seq *ws (apply +skip list) *ws (+skip ":") *ws *token *ws *br-o *ws *token *ws expr-seq *ws *br-c *ws)))

(def *integer (p/+str (p/+seq (p/+opt (p/+char "+-")) (p/+str (p/+plus +digit))
                              (p/+str (p/+map flatten (p/+opt (p/+seq (p/+char ".") (p/+plus +digit)))))
                              (p/+str (p/+opt (p/+seq (p/+char "eE") (p/+opt (p/+char "+-")) (p/+str (p/+plus +digit))))))))

(def *number (p/+or
                 (p/+map (comp (parse-variable integers)
                               (fn [[% & %&]] (cons % (cons :i (concat %& [:e]))))
                               flatten)
                         (p/+seq *ws *token *ws (+skip "=") *ws (p/+or *integer *token) *ws (+skip ".." "->") *ws (p/+or *integer *token) *ws))
                 (component (parse-variable integers) ["i" "int" "Integer"] (p/+opt (p/+seq +separator *ws *token)))))
(def *string (component (parse-variable strings) ["s" "str" "String"] (p/+opt (p/+seq +separator *ws *token))))
(def *range (component (parse-variable ranges) ["r" "rng" "Range"] (p/+seq +separator *ws *token)))

;(def *command (component-no-type parse-command "{" (p/+star (p/+seq *text *ws (p/+opt (p/+seq +separator *ws)))) "}"))
(def *command (p/+map (comp parse-command flatten) (p/+seq *ws *token *ws (+skip "{") *ws (p/+star (p/+seq *text *ws (p/+opt (p/+seq +separator *ws)))) *ws (+skip "}") *ws)))

(def *variable (p/+or *number *string *range *command))


(defn parse-grammar
    ([file-name]
     (let [file (slurp file-name)]
         ((p/+parser (p/+star (p/+seq (p/+or *variable *comment) *line-skip))) file)))
    ([file-name flag]
     (if flag
         (let [file (slurp file-name)]
             (println file)
             (println (clojure.string/join (repeat 120 "-")))
             ((p/+parser (p/+star (p/+seq (p/+or *variable *comment) *line-skip))) file)
             (println @integers)
             (println @strings)
             (println @ranges)
             (println @commands))
         (parse-grammar file-name))))
