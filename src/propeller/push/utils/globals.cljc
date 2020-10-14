(ns propeller.push.utils.globals)

;; =============================================================================
;; Values used by the Push instructions to keep computed values within
;; reasonable size limits.
;; =============================================================================

;; Used by keep-number-reasonable as the maximum magnitude of any integer/float
(def max-number-magnitude 1.0E12)

;; Used by keep-number-reasonable as the minimum magnitude of any float
(def min-number-magnitude 1.0E-10)

;; Used by reasonable-string-length? to ensure that strings don't get too large
(def max-string-length 1000)

;; Used by keep-vector-reasonable to ensure that vectors don't get too large
(def max-vector-length 1000)
