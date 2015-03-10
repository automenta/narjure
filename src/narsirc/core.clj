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
 
 
;; map with essential self
(def self 
  (let [n (new NAR (new Default) ) ]
  {
            :network "irc.freenode.net" 
            :port 6667 
            :name "narjure" 
            :chan "#nars"
            :nar n
            :narTextOut ( new TextOutput n System/out )
            })
  )

(defn echo
  [irc s chan]
  (let [st (clean s ".echo")]
    (message irc chan s)
  )
)
(defn username [u] (nth (clojure.string/split u #"[:|!]") 1 nil)  )

(defn tokenterm 
  [token]
  
    (if (or (= (.pattern token) "word")  (= (.pattern token) "wordApos"))
      (clojure.string/join "" [ "\"" (.content token) "\"" ])
      (clojure.string/join "" [
          "<\"" (.content token) "\" --> " (.pattern token) ">"
      ])
    )
    ; (Texts/escape "something")
)

(defn narsify
  [text user] 
  (let [tokens (map tokenterm (Twokenize/twokenize text)) ]
    (clojure.string/join "" [ 
        "<(*," user ",(*," (clojure.string/join "," tokens) ")) --> say>. :|:" ])
  )
)

(defn other
  [irc s chan]
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
            (.contains s ".echo")
              (echo irc s chan)
            ;(.contains s ".syn")
            ;  (thesaurus irc s chan)))
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
             (.start (:nar self) 250 1)             
             (join irc (:chan self))             
           )
         )
   
)