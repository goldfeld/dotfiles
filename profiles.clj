{:user {:plugins [[lein-difftest "2.0.0"]]
        :dependencies [[leiningen "2.3.4"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [org.clojure/tools.trace "0.7.6"]
                       [slamhound "1.5.0"]
                       [im.chit/vinyasa "0.1.8"]
                       [io.aviso/pretty "0.1.8"]]
        :injections [(require '[clojure.repl :refer [doc dir source
                                                     find-doc apropos
                                                     root-cause]])
                     (require 'vinyasa.inject)
                     (vinyasa.inject/inject
                      'clojure.core
                      '[[vinyasa.inject inject]
                        [vinyasa.pull pull]
                        [vinyasa.lein lein]
                        [clojure.tools.trace trace deftrace
                         trace-forms trace-ns trace-vars]
                        [clojure.pprint [pprint >pprint]]
                        [clojure.tools.namespace.repl refresh]])
                     (require 'io.aviso.repl 'clojure.repl
                              'clojure.main)
                     (alter-var-root
                      #'clojure.main/repl-caught
                      (constantly @#'io.aviso.repl/pretty-pst))
                     (alter-var-root
                      #'clojure.repl/pst
                      (constantly @#'io.aviso.repl/pretty-pst))]}}
