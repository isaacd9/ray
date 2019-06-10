(ns ray.core
  (:gen-class))

(defn vec_print
  [v]
  (let [[x y z] v] (printf "%d %d %d\n" x y z)))

(defn vec_length
  [v]
  (let [[x y z] v]
    (Math/sqrt (+ (* x x) (* y y) (* z z)))))

(defn sq_length
  [v]
  (let [[x y z] v]
    (+ (* x x) (* y y) (* z z))))

(defn vec_unit
  [v]
  (let [[x y z] v]
    (let [k (/ 1 (Math/sqrt (+ (* x x) (* y y ) (* z z))))]
      [(* x k) ( * y k) (* z k)])))

(defn vec_add
  [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    [(+ x1 x2) (+ y1 y2) (+ z1 z2)]))

(defn vec_sub
  [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    [(- x1 x2) (- y1 y2) (- z1 z2)]))

(defn vec_mul
  [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    [(* x1 x2) (* y1 y2) (* z1 z2)]))

(defn vec_div
  [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    [(/ x1 x2) (/ y1 y2) (/ z1 z2)]))

(defn vec_mul_scalar
  [s v]
  (let [[x y z] v] [(* x s) (* y s) (* z s)]))

(defn vec_div_scalar
  [s v]
  (let [[x y z] v] [(/ x s) (/ y s) (/ z s)]))

(defn vec_dot
  [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    (+ (* x1 x2) (* y1 y2) (* z1 z2))))

(defn vec_cross
  [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    [(- (* y1 z2) (* z1 y2))
     (* -1 (- (* x1 z2) (* z1 x2)))
     (- (* x1 y2) (* y1 x2))]))

(def ray vector)

(defn ray_point_at_param
  [ray t]
  (let [[A B] ray]
    (vec_add A (vec_mul_scalar t B))))

(defn ray_origin
  [ray]
  (let [[A B] ray] A))

(defn ray_direction
  [ray]
  (let [[A B] ray] B))

(defn color
  [r]
  (let [unit_direction (vec_unit (ray_direction r))]
    (let [t (* 0.5 (+ (nth unit_direction 1) 1))]
      (vec_add
        (vec_mul_scalar (- 1.0 t) [1.0 1.0 1.0])
        (vec_mul_scalar t [0.5 0.7 1.0])))))

(def lower_left_corner [-2.0 -1.0 -1.0])
(def horizontal [4.0 0.0 0.0])
(def vertical [0.0 2.0 0.0])
(def origin [0.0 0.0 0.0])

(defn pixel
  [nx ny j i]
  (let [r (/ i nx) g (/ j ny) b 0.2]
    (let [ir (* 255.99 r) ig (* 255.99 g) ib (* 255.99 b)]
      (format "%d %d %d" (int ir) (int ig) (int ib)))))

(defn pixels
  [nx ny pixelf]
    (let [d (fn [m] (let [[j i] m] (pixelf j i)))]
      (map d (for [j (reverse (range ny)) i (range nx)] [j i]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [nx 200 ny 100]
    (let [pixeln (partial pixel nx ny)]
    (do
      (printf "P3\n%d %d \n255\n", nx ny)
      (doall (map println (pixels nx ny pixeln)))))))
