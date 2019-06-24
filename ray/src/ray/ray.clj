(ns ray.ray
  (:use ray.vec)
  (:import [ray.vec Vec3]))

(defrecord Ray [A B])

(defn ray_origin
  [{:keys [A B]}] A)

(defn ray_direction
  [{:keys [A B]}] B)

(defn ray_point_at_param
  [ray t]
  (let [{:keys [A B]} ray]
    (vec_add A (vec_mul_scalar t B))))

