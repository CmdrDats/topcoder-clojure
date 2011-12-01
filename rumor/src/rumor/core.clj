(ns rumor.core)

(defn spread-to [knowledge rabbit-to rumor]
  (let [split (split-at rabbit-to knowledge)
        current (first (second split))
        begin (first split)
        end (rest (second split))]
    (apply conj (vec begin)
           (cond
            (and (= current \N) (= rumor \A)) \A
            (and (= current \B) (= rumor \A)) \Y
            (and (= current \N) (= rumor \B)) \B
            (and (= current \A) (= rumor \B)) \Y
                                        ; Corner case - shouldn't happen though...
            (= rumor \Y) \Y
            :else current
            )
           (vec end))))

(defn spread-rumor [knowledge graph level]
  (println "Spreading:" knowledge "along" graph "at level" level)
  (if (= (count knowledge) (count (filter #(= \Y %) knowledge)))
    level
    (for [rabbit (range (count knowledge))
          destrabbit (range (count knowledge))
          :let [knowrumor (get knowledge rabbit)
                destknowrumor (get knowledge destrabbit)
                connected (get (get graph rabbit) destrabbit)]]
      (cond
                                        ; No point spreading to self
       (= rabbit destrabbit) :self
                                        ; Doesn't know a rumor to spread
       (= knowrumor \N) :norumortospread
                                        ; Also no point spreading to others who know
       (= destknowrumor \Y) :alreadyknows
                                        ; Can't share with unconnected peers.
       (= connected \N) :notconnected
                                        ; Already knows the same rumor.
       (= destknowrumor knowrumor) :alreadyknowsame

                                        ; Can tell about both rumors
       (and (= knowrumor \Y) (= destknowrumor \N))
       [(delay (spread-rumor (spread-to knowledge destrabbit \A) graph (inc level)))
        (delay (spread-rumor (spread-to knowledge destrabbit \B) graph (inc level)))]

                                        ; Can tell about B
       (and (= knowrumor \Y) (= destknowrumor \A))
       [(delay (spread-rumor (spread-to knowledge destrabbit \B) graph (inc level)))]

                                        ; Can tell about A
       (and (= knowrumor \Y) (= destknowrumor \B))
       [(delay (spread-rumor (spread-to knowledge destrabbit \A) graph (inc level)))]

                                        ; Can tell about A
       (and (= knowrumor \A) (or (= destknowrumor \N) (= destknowrumor \B)))
       [(delay (spread-rumor (spread-to knowledge destrabbit \A) graph (inc level)))]

                                        ; Can tell about B
       (and (= knowrumor \B) (or (= destknowrumor \N) (= destknowrumor \A)))
       [(delay (spread-rumor (spread-to knowledge destrabbit \A) graph (inc level)))]
       
       :else :not-sure-what-to-do?
       )
      ))
  )

(defn get-minimum [knowledge graph]
  (def queue (ref [(delay (spread-rumor knowledge graph 0))]))
  (loop [item (first @queue)
         result (force item)]
    (println "..." item)
    (if (number? result) result
        (do
          (dosync
           (ref-set queue (vec (rest @queue)))
           (alter queue conj (filter #(vector? %) result))
           )
          (recur (first @queue) (force item))
          )
        
      )
    )
  )