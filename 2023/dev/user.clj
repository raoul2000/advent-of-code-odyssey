(ns dev.user
  (:require
   [portal.api :as p]
   [babashka.fs :as fs]))

(def p (p/open {:launcher :vs-code}))
(add-tap #'p/submit)
(tap> "portal ready") ; Start tapping out values
