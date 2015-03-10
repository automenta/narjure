;temporary code, not used yet
;this will contain the main part of narjure

 
(ns narsirc.core
  (:gen-class))


(require '[irclj.core :as irc])

(defn -_main
 []
  (def connection 
    (irc/connect "irc.freenode.net" 6667 "narjure" 
                 :callbacks {:privmsg (fn [irc type s] (prn irc type s))})
    )
  
  
  )


(defn -main2
 []
  (def connection 
     (irc/connect "irc.freenode.net" 6667 "narjure" 
                 :callbacks {
                             :privmsg (fn [irc type s] (prn irc type s))                            
                             }))
    
    (irc/join connection "#nars")
    
    
)


;(irc/join connection "#nars")
;(irc/message connection "#nars" "test")