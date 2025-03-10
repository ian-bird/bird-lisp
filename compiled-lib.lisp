(defmacro (nil? v) (list 'eq? v (list 'quote '())))

(def-comp (inc a) (+ a 1))

(def-comp (dec a) (- a 1))

(def-comp (count coll)
    (progn
      (define (count-tco x acc)
	  (if (nil? x)
	      acc
	      (count-tco (cdr x) (inc acc))))
      (count-tco coll 0)))

(def-comp (reverse coll)
    (progn
      (define (reverse-tco coll acc)
	  (if (nil? coll)
	      acc
	      (reverse-tco (cdr coll)
			   (cons (car coll) acc))))
      (reverse-tco coll '())))

(def-comp (append2 a b)
  (progn
    (define (append2-inner a b)
	(if (nil? a)
	    b
	    (append2-inner (cdr a)
			   (cons (car a) b))))
    (append2-inner (reverse a) b)))



(def-comp (foldl fn basis coll)
    (if (nil? coll)
	basis
	(foldl fn (fn basis (car coll)) (cdr coll))))

(def-comp (flip f) (lambda (a b) (f b a)))

(def-comp (foldr fn basis coll)
  (foldl (flip fn) basis (reverse coll)))

(def-comp (map fn coll)
    (reverse (foldl (lambda (acc e)
		      (cons (fn e) acc))
		    '()
		    coll)))

