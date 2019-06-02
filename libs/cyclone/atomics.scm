;; A temporary test file, let's try to get an API going here before writing too much support code
;; 
;; see: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/compare-and-set!
;; initial operations to support:
;; - create atomic
;; - ref atomic
;; - swap, see https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/swap!
;; - compare and swap?
;; 
;; 
;; once that starts going, double-back to how to allocate shared objects effectively.
;; probably want a (make-shared)
;; may also way a way to allocate multiple shared objects at once (since a minor GC will likely be req'd)

;; (atomic? obj)
