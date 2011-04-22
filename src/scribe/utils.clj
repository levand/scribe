(ns scribe.utils)

(defmacro dbg
  "A handy little debugging macro"
  [x]
  `(let [x# ~x] (println '~x "=" x#) x#))