(ns DropCoins.core)

(def states (ref #{}))
(def queue (ref []))

(defn board-empty? [board]
  (= -1 (.indexOf (apply str board) "o"))
  )

(defn count-coins [board]
  (count (filter #(= \o %) (apply str board)))
  )

(defn move-board-left [board]
  (apply vector (for [line board] (apply str (concat (rest line) [\.]))))
  )

(defn move-board-right [board]
  (apply vector (for [line board] (apply str (concat [\.] (butlast line)))))
  )

(defn move-board-up [board]
  (conj (vec (rest board)) ".")
  )

(defn move-board-down [board]
  (vec (cons "." (butlast board)))
  )

(defn get-minimum-spin [board result level]
  ;; Prevent backing over itself.
  (if (not (nil? (get @states board))) (- -1 level)
      (do
        (dosync
         (alter states conj board)
         
         (cond
          (= result (count-coins board)) level
          (board-empty? board) (- -1 level)
          :else (let [newlevel (inc level)
                      left-board (move-board-left board)
                      left (delay (get-minimum-spin left-board result newlevel))
                      
                      right-board (move-board-right board)
                      right (delay (get-minimum-spin right-board result newlevel))
                      
                      up-board (move-board-up board)
                      up (delay (get-minimum-spin up-board result newlevel))
                      
                      down-board (move-board-down board)
                      down (delay (get-minimum-spin down-board result newlevel))]
                  (alter queue conj left right up down)
                  (- -1 level))
          )))))

(defn get-minimum [board result]
  (dosync (ref-set states #{})
          (ref-set queue [(delay (get-minimum-spin board result 0))])
          )
  (loop [item (first @queue)
         result (force item)]
    (cond
     (nil? item) -1
     (> result -1) result
     :else (do 
       (dosync (ref-set queue (vec (rest @queue))))
       (recur (first @queue) (force item)))
     )))

(def board1 [ ".o.."
              ,"oooo"
              ,"..o."])

(def board2 [ ".....o"
              ,"......"
              ,"oooooo"
              ,"oooooo"
              ,"......"
              ,"o....."])

(def board3 [ "...."
              ,".oo."
              ,".oo."
              ,"...."])

(def board4 [ "......."
              ,"..ooo.."
              ,"ooooooo"
              ,".oo.oo."
              ,"oo...oo"])

(def board5 ["................."
             ,".ooooooo...oooo.."
             ,".ooooooo..oooooo."
             ,".oo.......oo..oo."
             ,".oo.......oo..oo."
             ,".ooooo.....oooo.."
             ,".ooooooo...oooo.."
             ,".....ooo..oo..oo."
             ,"......oo..oo..oo."
             ,".ooooooo..oooooo."
             ,".oooooo....oooo.."
             ,"................."])

