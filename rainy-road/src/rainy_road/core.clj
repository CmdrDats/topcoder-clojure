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
