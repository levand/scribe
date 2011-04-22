(ns scribe.test.core
  (:use clojure.test)
  (:use scribe.core :reload)
  (:use scribe.template :reload))

;; Test loading from files
(deftest test-from-files
  (let [tofu (from-files "./test")]
    (is (= "Hello, Luke" (render tofu "scribe.test.hello" {:name "Luke"})))))

;; Test loading via a namespace, but still from a file
(def ^{:soy true} test-soy (slurp "./test/test.soy"))
(deftest test-from-namespaces
  (let [tofu (from-namespaces ["scribe.test.core"])]
    (is (= "Hello, Luke" (render tofu "scribe.test.hello" {:name "Luke"})))))
