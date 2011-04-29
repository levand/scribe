(ns scribe.template
  (:require [clojure.walk :as walk]
            [clojure.string :as s]
            [clojure.contrib.string :as cs]
            [clojure.set])
  (:use [scribe.utils :only (dbg) :as u]))

(defprotocol SoyCommand
  "The data representation of a Soy command"
  (render [this] "Returns the Soy command as a string in preperation for passing to the Tofu compiler.")
  (params [this] "Returns a seq of the param names that this command references.")
  (children [this] "Returns a seq of the SoyCommand's children."))

(defn- render-directives
  "Renders a seq of keywords as a string in Google Template directive format"
  [directives]
  (apply str (map #(str " |" (name %)) directives)))

(defn- extract-params
  "Given a Google Template expression, returns a seq of the names of all the parameters it references."
  [expression]
  (set (map #(s/replace % "$" "") (re-seq #"\$[\w_]+" (str expression)))))

(defn- get-child-params
  "Returns a set of all the params used by a seq of children."
  [children]
  (set (reduce concat (map params (filter #(satisfies? SoyCommand %) children)))))

(defn- leading-space
  "Adds a leading space to any non-empty string."
  [string]
  (if (empty? string) string (str " " string)))

(defn- map-to-attributes
  "Converts a map to a string of key='value' statements"
  [attrs]
  (s/join \space (map (fn [[k v]]
                         (str k "=" "\"" v "\""))
                       (walk/stringify-keys attrs))))

(defn- escape-string
  "Escapes a string so it will always be literal in output. It shouldn't be possible for users to manually or accidentally write raw soy"
  [s]
  (s/escape (str s) {\space "{sp}"
                       \newline "{\n}"
                       \return "{\r}"
                       \tab "{\t}"
                       \{ "{lb}"
                       \} "{rb}"}))

(defn- render-child
  "Renders a child element. Handles each kind of child as appropriate."
  ([child escape?]
     (cond (satisfies? SoyCommand child) (render child)
           (isa? (class child) String) (if escape? (escape-string child) child)
           :default (throw (RuntimeException. "Only strings and SoyCommands can be rendered."))))
  ([child] (render-child child true)))

(defn- render-soydoc
  "Given a seq of param names, returns a Soy document string for them."
  [params]
  (str "\n/**"
       (reduce str (map #(str "\n * @" % " " %) params))
       "\n */"))

(defn namespace-cmd
  "Creates a namespace SoyCommand"
  [ns]
  (reify SoyCommand
         (render [this] (str "\n{namespace " ns "}"))
         (params [this] [])
         (children [this] [])))

(defn print-cmd
  "Creates a print SoyCommand"
  [expression & directives]
  (reify SoyCommand
         (render [this] (str "{" expression (render-directives directives) "}"))
         (params [this] (extract-params expression))
         (children [this] [])))

(defn template-cmd
  "Creates a template SoyCommand"
  [name options & children]
  (let [opts (if (map? options) options {})
        kids (if (map? options) children (cons options children))]
    (reify SoyCommand
           (render [this] (str "\n"
                               (render-soydoc (get-child-params kids))
                               "\n" "{template ." name (leading-space (map-to-attributes opts)) "}"
                               "\n" (reduce str (map render-child kids))
                               "\n" "{/template}" "\n"))
           (params [this] (get-child-params kids))
           (children [this] kids))))

(defn literal-cmd
  "Creates a literal SoyCommand"
  [& children]
  (reify SoyCommand
         (render [this] (str "{literal}"
                             (reduce str (map #(render-child % false) children))
                             "{/literal}"))
         (params [this] [])
         (children [this] children)))

(defn msg-cmd
  "Creates a msg SoyCommand"
  [opts & children]
  (reify SoyCommand
         (render [this] (str "{msg" (leading-space (map-to-attributes opts)) "}"
                             (reduce str (map render-child children))
                             "{/msg}"))
         (params [this] (get-child-params children))
         (children [this] children)))

(defn if-cmd
  "Creates an if SoyCommand"
  [expression & children]
  (reify SoyCommand
         (render [this] (str "{if " expression "}"
                             (reduce str (map render-child children))
                             "{/if}"))
         (params [this] (clojure.set/union (extract-params expression) (get-child-params children)))
         (children [this] children)))

(defn elseif-cmd
  "Creates an elseif SoyCommand"
  [expression]
  (reify SoyCommand
         (render [this] (str "{elseif " expression "}"))
         (params [this] (extract-params expression))
         (children [this] [])))

(defn else-cmd
  "Creates an else SoyCommand"
  []
  (reify SoyCommand
         (render [this] "{else}")
         (params [this] [])
         (children [this] [])))

(defn switch-cmd
  "Creates a switch SoyCommand"
  [expression & children]
  (reify SoyCommand
         (render [this] (str "{switch " expression "}"
                             (reduce str (map render-child children))
                             "{/switch}"))
         (params [this] (clojure.set/union (extract-params expression) (get-child-params children)))
         (children [this] children)))

(defn case-cmd
  "Creates a case SoyCommand"
  [& expressions]
  (reify SoyCommand
         (render [this] (str "{case " (s/join ", " (map str expressions)) "}"))
         (params [this] (reduce clojure.set/union (map extract-params expressions)))
         (children [this] [])))

(defn default-cmd
  "Creates a default SoyCommand"
  []
  (reify SoyCommand
         (render [this] "{default}")
         (params [this] [])
         (children [this] [])))

(defn foreach-cmd
  "Creates a foreach SoyCommand"
  [local-var data-ref children]
  (reify SoyCommand
         (render [this] (str "{foreach "
                             local-var
                             " in "
                             data-ref "}"
                             (reduce str (render-child children))
                             "{/foreach}"
                             ))
         (params [this] (clojure.set/union (extract-params data-ref) (get-child-params children)))
         (children [this] children)))

(defn ifempty-cmd
  "Creates an ifempty SoyCommand"
  []
  (reify SoyCommand
         (render [this] "{ifempty}")
         (params [this] [])
         (children [this] [])))

(defn for-cmd
  "Creates a for SoyCommand. expressions is a seq of 1-3 expressions that is used as the argument to the GT range function."
  [local-var expressions children]
  (reify SoyCommand
         (render [this] (str "{for "
                             local-var
                             " in range("
                             (s/join ", " expressions)
                             ")}"))
         (params [this] (clojure.set/union (map clojure.set/union expressions) (get-child-params children)))
         (children [this] children)))