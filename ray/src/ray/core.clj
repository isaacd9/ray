(ns ray.core
  (:gen-class))

(defn vec_print
  [v]
  (let [[x y z] v] (printf "%d %d %d\n" x y z)))

(defn vec_unit
  [v]
  (let [[x y z] v]
    (let [k (/ 1 (Math/sqrt (+ (* x x) (* y y ) (* z z))))]
      [(* x k) ( * y k) (* z k)])))

(defn vec_+
  [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    [(+ x1 x2) (+ y1 y2) (+ z1 z2)]))

(defn vec_-
  [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    [(- x1 x2) (- y1 y2) (- z1 z2)]))

(defn pixel
  [nx ny j i]
  (let [r (/ i nx) g (/ j ny) b 0.2]
    (let [ir (* 255.99 r) ig (* 255.99 g) ib (* 255.99 b)]
      (format "%d %d %d" (int ir) (int ig) (int ib)))))

(defn pixels
  [nx ny]
  (let [pixeln (partial pixel nx ny)]
    (let [d (fn [m] (let [[j i] m] (pixeln j i)))]
      (map d (for [j (reverse (range ny)) i (range nx)] [j i])))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [nx 200 ny 100]
    (do
      (printf "P3\n%d %d \n255\n", nx ny)
      (doall (map println (pixels nx ny))))))
