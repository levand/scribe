(ns scribe.template
  (:require [clojure.walk :as walk]
            [clojure.string :as str])
  (:use [scribe.utils :only (dbg) :as u]))