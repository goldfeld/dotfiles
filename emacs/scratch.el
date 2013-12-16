; clojure cider init
(ns name.space)
(use 'clojure.repl)
(require '[cljs.repl.node :as node])
(node/run-node-nrepl)

(require '[cljs.closure :as cljsc])
(cljsc/build "src-cljs/dow" {:output-to "resources/public/dow.js"
	                     :target :nodejs
                             :optimizations :simple})

---

