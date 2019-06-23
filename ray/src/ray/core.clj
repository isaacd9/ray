(ns ray.core
  (:gen-class))

(defrecord Vec3 [x y z])

(defn vec_print
  [{:keys [x y z]}]
   (printf "%d %d %d\n" x y z))

(defn vec_length
  [{:keys [x y z]}]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn vec_sq_length
  [{:keys [x y z]}]
  (+ (* x x) (* y y) (* z z)))

(defn vec_unit
  [{:keys [x y z]}]
    (let [k (/ 1 (Math/sqrt (+ (* x x) (* y y ) (* z z))))]
      (Vec3. (* x k) ( * y k) (* z k))))

(defn vec_add
  [v1 v2]
  (let [{x1 :x, y1 :y, z1 :z} v1
        {x2 :x, y2 :y, z2 :z} v2]
     (Vec3. (+ x1 x2) (+ y1 y2) (+ z1 z2))))

(defn vec_sub
  [v1 v2]
  (let [{x1 :x, y1 :y, z1 :z} v1
        {x2 :x, y2 :y, z2 :z} v2]
    (Vec3. (- x1 x2) (- y1 y2) (- z1 z2))))

(defn vec_mul
  [v1 v2]
  (let [{x1 :x, y1 :y, z1 :z} v1
        {x2 :x, y2 :y, z2 :z} v2]
    (Vec3. (* x1 x2) (* y1 y2) (* z1 z2))))

(defn vec_div
  [v1 v2]
  (let [{x1 :x, y1 :y, z1 :z} v1
        {x2 :x, y2 :y, z2 :z} v2]
    (Vec3. (/ x1 x2) (/ y1 y2) (/ z1 z2))))

(defn vec_mul_scalar
  [s {:keys [x y z]}]
  (Vec3. (* x s) (* y s) (* z s)))

(defn vec_div_scalar
  [s {:keys [x y z]}]
  (Vec3. (/ x s) (/ y s) (/ z s)))

(defn vec_dot
  [v1 v2]
  (let [{x1 :x, y1 :y, z1 :z} v1
        {x2 :x, y2 :y, z2 :z} v2]
    (+ (* x1 x2) (* y1 y2) (* z1 z2))))

(defn vec_cross
  [v1 v2]
  (let [{x1 :x, y1 :y, z1 :z} v1
        {x2 :x, y2 :y, z2 :z} v2]
    (Vec3. (- (* y1 z2) (* z1 y2))
     (* -1 (- (* x1 z2) (* z1 x2)))
     (- (* x1 y2) (* y1 x2)))))

(defrecord Ray [A B])

(defn ray_origin
  [{:keys [A B]}] A)

(defn ray_direction
  [{:keys [A B]}] B)

(defn ray_point_at_param
  [ray t]
  (let [{:keys [A B]} ray]
    (vec_add A (vec_mul_scalar t B))))

(defn color
  [r]
  (let [unit_direction (vec_unit (ray_direction r))]
    (let [t (* 0.5 (+ (:y unit_direction) 1.0))]
      (vec_add
        (vec_mul_scalar (- 1.0 t) (Vec3. 1.0 1.0 1.0))
        (vec_mul_scalar t (Vec3. 0.5 0.7 1.0))))))

(def lower_left_corner (Vec3. -2.0 -1.0 -1.0))
(def horizontal (Vec3. 4.0 0.0 0.0))
(def vertical (Vec3. 0.0 2.0 0.0))
(def origin (Vec3. 0.0 0.0 0.0))

;; multicolored pixel impl
(defn pixel
  [nx ny j i]
  (let [r (/ i nx) g (/ j ny) b 0.2]
    (let [ir (* 255.99 r) ig (* 255.99 g) ib (* 255.99 b)]
      [(int ir) (int ig) (int ib)])))

;; Calls pixelf for every point in nx ny
(defn pixels
  [nx ny pixelf]
    (let [d (fn [m] (let [[j i] m] (pixelf j i)))]
      (map d (for [j (reverse (range ny)) i (range nx)] [j i]))))

;; Generates a ppm file for the given nx and ny, using pixelf on each point
(defn gen_ppm
  [nx ny pixelf]
    (do
      (printf "P3\n%d %d \n255\n", nx ny)
      (doall
        (map
          (fn [v] (let [[x y z] v] (printf "%d %d %d\n" x y z)))
          (pixels nx ny pixelf)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [nx 200 ny 100]
    (let [pixeln (partial pixel nx ny)]
      (gen_ppm nx ny pixeln))))
