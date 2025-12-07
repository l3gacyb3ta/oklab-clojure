(ns build
  (:require [nextjournal.clerk :as clerk]))

(clerk/build! {:paths ["notebooks/*"] :browse nil })
