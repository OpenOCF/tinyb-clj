(def project 'iotk/tinyb)
(def version "0.1.0-SNAPSHOT")

(set-env! ;; :resource-paths #{"src"}
          ;;  :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "RELEASE"]
                            [org.clojure/math.numeric-tower "0.0.4"]
                            [org.clojure/core.async "0.2.385"]
                            [tinyb/tinyb "1.0.0"]])

(task-options!
 repl {:port 48080})
