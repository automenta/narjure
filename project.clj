(defproject narsirc "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [
                 [org.clojure/clojure "1.7.0-alpha5"] 
                 [irclj "0.5.0-alpha4"] 
                 [opennars/opennars-core "1.7-SNAPSHOT"]
                 ]
  :javac-options [ "-target" "1.8" "-source" "1.8" "-Xlint:-options"]
  :aot [narsirc.core]
  :main narsirc.core)
