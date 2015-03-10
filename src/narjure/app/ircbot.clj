(ns narjure.app.ircbot
  (:import [nars.core NAR ])
  (:import [nars.build Default ])
  (:import [nars.io TextOutput ])
  (:import [nars.io BufferedOutput ])
  (:import [nars.io Texts ])
  (:import [nars.io.nlp Twokenize ])
  (:use [irclj.core]
  	    [irclj.parser]
  	    [irclj.events]
       ;[clojure.string]
      )
)

;REFERENCE:
;  https://github.com/naduse/goofy/blob/master/src/goofy/core.clj
;  https://github.com/zakwilson/minibot/blob/master/src/minibot/core.clj


(def self ;; map with essential self
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


(defn clean [s t]
  (.trim (.substring s (+ (.length t) (.indexOf s t)))))

 (defn subseqx
   "subsequence"
   [s start end] (into (empty s) (drop-last (- (count s) end) (drop start s))))
 
 
(defn textOutput  [ nar onOutput ]
    (proxy [ BufferedOutput ] [nar 1 500 16 ]
      (output [ items ]         
        (onOutput ( .toString this items 505 ) ) ;irc line length limit
    )
  )
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
  (let [tokens (map tokenterm (Twokenize/twokenize (.toLowerCase text) )) ]
    (clojure.string/join "" [ 
        ;"$0.3$ <(*," user ",(*," (clojure.string/join "," tokens) ")) --> say>. :|:" 
        "$0.3$ <(&/," (clojure.string/join "," tokens) ") =\\> <" user " --> say>>. :|:" 
    ])
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
            (.contains s "CHARSET=ascii") ;server login message, TODO find a better way to detect this
              (do true)
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
              (fn [ textRepresentation ]
                 
                   (do                      
                     (println textRepresentation)
                     (message irc (:chan self) textRepresentation)                       
                    )
               )
               
       )
       
       (.input (:nar self) (clojure.string/join "" [ "schizo(" (:name self) ")!"]  ))
       (.input (:nar self) (new java.io.File "bot1.nal" ) )
       
       ;create local output display
       (new TextOutput (:nar self) System/out (float 0.5) )
       
       ;start NAR reasoner
       (.start (:nar self) (:logicFrameMS self) (:logicCyclesPerFrame self) )
       
       ;join channel
       (join irc (:chan self))             
       )
   )
)