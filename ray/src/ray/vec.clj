(ns ray.vec)

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

