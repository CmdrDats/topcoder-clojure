(ns rainy-road.core)

(defn map-path-pairs
  "Maps a path into pairs that can be analyzed a bit easier"
  [path]
  (apply vector (partition 2 (interleave (seq (first path)) (seq (second path)))))
  )


(defn is-reachable [path]
  (let [unreachable-parts (filter #(= "NO" %) (for [[s1 s2] (map-path-pairs path)]
                                                (if (and (= s1 \W) (= s2 \W)) "NO" "")))]
    (if (> (count unreachable-parts) 0) "NO" "YES")))

(defn alt-reachable
  "Alternative solution, without mapping to pairs first."
  [path]
  (let [reachable-seq (for [idx (range (count (first path)))
                            :let [s1 (get (first path) idx)
                                  s2 (get (second path) idx)]]
                        (if (= (str s1 s2) "WW") "NO" ""))
        unreachable-parts (filter #(= "NO" %) reachable-seq)]
    (if (> (count unreachable-parts) 0) "NO" "YES")))

(defn another-reachable
  "Yet another solution. Probably the best in terms of readability and performance"
  [path]
  (let [path (apply str (interleave (first path) (second path)))]
        (loop [pair (take 2 path)
               more (drop 2 path)]
          (cond
           (= (apply str pair) "WW") "NO"
           (empty? more) "YES"
           :else (recur (take 2 more) (drop 2 more))
           )
          )
        )
  )

(def path1 [".W.."
            "...."])
(println "Path1 is " (is-reachable path1))

(def path2 [".W..W.."
            "...WWW."])
(println "Path2 is " (is-reachable path2))

(def path3 [".W.."
            "..W."])
(println "Path3 is " (is-reachable path3))

(def path4 [".."
            "WW"])
(println "Path4 is " (is-reachable path4))

(def path5 [".WWWW."
            "WWWWWW"])
(println "Path5 is " (is-reachable path5))

(def path6 [".W.W.W."
            "W.W.W.W"])
(println "Path6 is " (is-reachable path6))

(def path7 [".............................................W."
            ".............................................W."])
(println "Path7 is " (is-reachable path7))
