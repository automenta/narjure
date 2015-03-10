;(ns narsirc.core
;  (:gen-class)
;  (:import [java.io DataInputStream File FileInputStream BufferedInputStream])
  ;(:import [nars.core NAR ])
  ;(:import [my.package MyClassOne MyClassTwo]) 
;)

  
;https://github.com/naduse/goofy/blob/master/src/goofy/core.clj
;https://github.com/zakwilson/minibot/blob/master/src/minibot/core.clj
  
(ns narsirc.core
  (:import [nars.core NAR ])
  (:import [nars.build Default ])
  (:import [nars.io TextOutput ])
  (:import [nars.io Texts ])
  (:import [nars.io.nlp Twokenize ])
  (:use [irclj.core]
  	    [irclj.parser]
  	    [irclj.events]
       ;[clojure.string]
  	   ;[goofy.minify]
  	   ;[goofy.calc]
  	   ;[goofy.syn]
      )
)

(defn clean [s t]
  (.trim (.substring s (+ (.length t) (.indexOf s t)))))

 (defn subseqx
   "subsequence"
   [s start end] (into (empty s) (drop-last (- (count s) end) (drop start s))))
 
 
(defn textOutput  [ nar onOutput ]
  (let [t (proxy [ TextOutput ] [nar]
      (process [channel args]         
        (let [ textRepresentation (proxy-super process channel args ) ]
          (if (not= textRepresentation nil) (onOutput channel textRepresentation) )
        )        
      )
  )] (do 
    (.setShowInput t false)
    (.setShowErrors t true)
    (.setPriorityMin t (float 0.95) )
    (.setShowStamp t false)
    t)
  )
)


;; map with essential self
(def self 
  (let [n (new NAR (new Default 4000) ) ]
  {
            :network "irc.freenode.net" 
            :port 6667 
            :name "narjure" 
            :chan "#nars"
            :nar n
            :logicFrameMS 50
            :logicCyclesPerFrame 1
            })
  )

(defn echo  [irc s chan]
  (let [st (clean s ".echo")]
    (message irc chan s)
  )
)


(defn username [u] (nth (clojure.string/split u #"[:|!]") 1 nil)  )

(defn tokenterm [token]  
    (if (or (= (.pattern token) "word")  (= (.pattern token) "wordApos"))
      (clojure.string/join "" [ "\"" (.content token) "\"" ])
      (clojure.string/join "" [
          "<\"" (.content token) "\" --> " (.pattern token) ">"
      ])
    )
    ; (Texts/escape "something")
)

(defn narsify [text user] 
  (let [tokens (map tokenterm (Twokenize/twokenize text)) ]
    (clojure.string/join "" [ 
        "<(*," user ",(*," (clojure.string/join "," tokens) ")) --> say>. :|:" ])
  )
)

(defn nal  [irc s chan]
  (let [st (clean s "nal ")]
    (.input (:nar self) st )
  )
)

(defn other [irc s chan]
  (let [st (narsify (clean s " :") (username s)) ]
     ;(message irc chan st)
    (.input (:nar self) st )
  )
)

(defn- cmd-callback
  "callback to handle incoming messages"
  [irc type s]
  (case type
    :read
      (cond
        (.contains s "PRIVMSG") ; we know it's a channel message
        (let [chan (:chan self)]
          (cond                 ; check & handle known commands
            ;(.contains s ".minify")
            ;  (shorten irc s chan)
            ;(.contains s ".syn")
            ;  (thesaurus irc s chan)))
            (.contains s ":.echo ")
              (echo irc s chan)
            (.contains s ":nal ")
              (nal irc s chan)
            :else
              (other irc s chan)
            ))
        (.contains s "INVITE")  ; handle invites since idk if it's handled in irclj/process
          (let [chan (subs s (+ 1 (.indexOf s ":#")))]
            (join irc chan)))
    :write))


 
 (defn -main [& args]   
   (let [irc (connect 
               (:network self) (:port self) (:name self) 
               :real-name (:name self) 
               :callbacks {:raw-log cmd-callback})]
     (do            
       
       ;create NAR output -> IRC output handler
       (textOutput (:nar self) 
              (fn [ signalChannel textRepresentation ]
                 
                   (do                      
                     (println textRepresentation)
                     (message irc (:chan self) textRepresentation)                       
                    )
               )
               
       )
       
       (.input (:nar self) (new java.io.File "bot1.nal" ) )
       
       ;create local output display
       ;(new TextOutput (:nar self) System/out (float 0.25) )
       
       ;start NAR reasoner
       (.start (:nar self) (:logicFrameMS self) (:logicCyclesPerFrame self) )
       
       ;join channel
       (join irc (:chan self))             
       )
   )
)