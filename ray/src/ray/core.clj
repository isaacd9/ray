(ns ray.core
  (:use ray.vec ray.ray)
  (:import [ray.vec Vec3]
           [ray.ray Ray])
  (:gen-class))

(defn hit_sphere
  [center radius r]
  (let [oc (vec_sub (ray_origin r) center)]
    (let [a (vec_dot (ray_direction r) (ray_direction r))
          b (* 2.0 (vec_dot oc (ray_direction r)))
          c (- (vec_dot oc oc) (* radius radius))]
      (let [discriminant (- (* b b) (* 4 a c))]
        (> discriminant 0)))))

(defn gradient_color
  [r]
  (let [unit_direction (vec_unit (ray_direction r))]
    (let [t (* 0.5 (+ (:y unit_direction) 1.0))]
      (vec_add
        (vec_mul_scalar (- 1.0 t) (Vec3. 1.0 1.0 1.0))
        (vec_mul_scalar t (Vec3. 0.5 0.7 1.0))))))

(defn sphere_color
  [r]
  (if (hit_sphere (Vec3. 0 0 1) 0.25 r)
    (Vec3. 1 0 0)
    (gradient_color r)))

(def lower_left_corner (Vec3. -2.0 -1.0 -1.0))
(def horizontal (Vec3. 4.0 0.0 0.0))
(def vertical (Vec3. 0.0 2.0 0.0))
(def origin (Vec3. 0.0 0.0 0.0))

;; multicolored pixel impl
(defn multicolored_pixel
  [nx ny j i]
  (let [r (/ i nx) g (/ j ny) b 0.2]
    (let [ir (* 255.99 r) ig (* 255.99 g) ib (* 255.99 b)]
      [(int ir) (int ig) (int ib)])))

(defn gradient_pixel
  [nx ny j i]
  (let [u (/ i nx)
        v (/ j ny)]
    (let [r (Ray.  origin (vec_add (vec_add
                lower_left_corner
                (vec_mul_scalar u horizontal))
                (vec_mul_scalar v vertical)))]
      (let [col (sphere_color r)]
        (let [ir (* 255.99 (:x col))
              ig (* 255.99 (:y col))
              ib (* 255.99 (:z col))]
          [(int ir) (int ig) (int ib)])))))

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
    (let [pixeln (partial gradient_pixel nx ny)]
      (gen_ppm nx ny pixeln))))
