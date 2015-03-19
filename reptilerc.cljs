(set datapath "~/datav/repo/archive/dow" "~/datav/repo/log")
(set browser "luakit")
(set project-types [:git :clojure :nodejs])
(defsymlink habits "acorn")
(defsymlink goals "plans")
(deflog journal "* %EEE %daysuf, %HH:%mm, ")
(dowp/def [:by-artist :by-album] "~/datav/file/media/albums")
(set title-align :center)
(set find/fuzzy :dmenu)
(set activity/log-to "~/datav/repo/log")
(set activity/log-format "* %yyyy-%MM-%dd %HH:%mm:%ss")
(data/dir-structure :journal "YYYY/mmm.org")
(menu "reptile" ["new buffer" buffer/new
                 "sessions" :sessions
                 "workflows" flow/list
                 "workspaces" workspace
                 "view tasks" :tasks
                 "albums" dowp/albums
                 "wrap up" :wrap-up
                 "quit" :quit])
(menu :tasks "view tasks" ["..to do" outline/tasks
                           "..in outline" nil
                           "..by area" nil
                           "     project" nil
                           "or do weekly review" :review])
(menu :wrap-up "status report" ["all code (%)" stata
                                "your code (%)" stata/authored
                                "cloned code (%)" stata/cloned])
(load "personal.cljs")
(set outline/mode :tnt)
(set outline/root "sessions/life.org")
(_flow/add-menu-hook tnt/test)
(_flow/add-menu-hook hey)
(workspace/deforder [cool third main hub rest rep])
(workspace/defcli rep {:go-to "hooker 11 && hooker 33" :move-to "hooker 12"})
(workspace/defcli main {:go-to "hooker 5" :move-to "hooker 6" :hotkey "m"})
(workspace/defcli cool {:go-to "hooker 1" :move-to "hooker 2" :hotkey "c"})
(workspace/defcli third {:go-to "hooker 3" :move-to "hooker 4" :hotkey "t"})
(workspace/defcli hub {:go-to "hooker 7" :move-to "hooker 8" :hotkey "h"})
(workspace/defcli rest {:go-to "hooker 9" :move-to "hooker 10" :hotkey "r"})
(bind gh :do (:move-to - 1) (:go-to - 1))
(bind gl :do (:move-to + 1) (:go-to + 1))
(bind o :brightness "LVDS1" 1.0)
(bind s :go stata)
(bind p :go context)
(bind a :go acorn)
(serve POST "/tab/inbox"
       (:capture [data] (:bucket data) :org-link (:url data) (:title data)))
(set flow/default-alert [[:brightness "LVDS1" 0.7]
                         [:sleep 3500 :ms]
                         [:brightness "LVDS1" 1.1]])
(flow :sprint [["side" [2 5 5 6 6 :min] :go-to cool]
               ["work" [2 6 6 7 7 :min] :go-to rest]
               ["get up and move" [:.. 8 :min] :go-to break]])
(flow :sprint-3 [["side" [2 4 5 5 :min] :go-to cool]
                 ["work" [2 5 6 6 :min] :go-to rest]
                 ["wmn" [2 4 5 6] :go-to third]
                 ["" [:.. 2 :min] :go-to hub]
                 ["meditate" [:.. 6 :min] :go-to break]])
(flow :big-project [["new code" [1 2 3 3 :min] :go-to main]
                    ["triage and fix issues" [1 2 3 3 :min]
                     {:on-start [:go-to rest] :alert [:exec "mplayer "]}]
                    ["write docstrings and articles/press-releases"
                     [1 2 3 3 :min] :go-to cool]
                    ["break" [:.. 3 :min] :go-to break]])
(flow :dualsemibout [["side" [2 3 3 3 3 3 5 3 3 5 :min] :go-to cool]
                     ["job" [2 3 4 4 3 4 6 3 4 6 :min] :go-to rest]
                     ["catchup" [:. :. :. 2 :. :. 2 :. :. 2 :min] :go-to hub]
                     ["break" [:. :. :. 4 :. :. 4 :. :. 4 :min] :go-to break]])
