(label list (lambda elements elements))

(label nil? (lambda (v) (eq? v '())))

;; needs to be updated to use gensym
(label define
       (macro args
	      (cond ((atom? (car args))  ;; the car is the name being assigned. If its an atom, then plain label.
		     (cons  'label args))
		    (else ;; else its a function definition.
		     (list 'label
			   (car (car args)) ;; the first argument in the list is the name
			   (cons 'lambda
				 (cons (cdr (car args)) ;; the rest of the first argument are the args
				       (cdr args)))))))) ;; and the rest is the fn body

;; udpate to use gensym
(define defmacro
       (macro args
	      (list 'label
		    (car (car args))
		    (cons 'macro
			  (cons (cdr (car args))
				(cdr args))))))

;; update to use gensym
(defmacro (if pred consq alt)
    (list 'cond (list pred consq)
     (list 'else alt)))

(define (count coll)
    (if (nil? coll)
	0
	(+ 1 (count (cdr coll)))))

(define (append2 a b)
    (if (nil? a)
	b
	(cons (car a) (append2 (cdr a) b))))

(define (foldl fn basis coll)
    (if (nil? coll)
	basis
	(foldl fn (fn basis (car coll)) (cdr coll))))

(define (reverse coll)
    (foldl (lambda (acc e)(cons e acc)) '() coll))

(define (flip f)(lambda (a b)(f b a)))

(define (foldr fn basis coll)
    (foldl (flip fn) basis (reverse coll)))

(define (map fn coll)
    (cond ((eq? '() coll) '())
	  (else (cons (fn (car coll))
		      (map fn (cdr coll))))))

(define (filter fn coll)
    (foldr (lambda (e acc) (if (fn e) (cons e acc) acc)) '() coll))

(define (even? x)
    (eq? 0 (% x 2)))

(define (last coll)
    (if (nil? (cdr coll))
	(car coll)
	(last (cdr coll))))

(define progn (lambda args (last args)))

;; update to use gensym
(define let (macro let-args
		   ((lambda (bindings body)
		      (if (nil? bindings)
			  (cons 'progn body)
                          (list
			   (list 'lambda
				 (list (car (car bindings)))
				 (append (list 'let (cdr bindings)) body))
			   (car (cdr (car bindings))))))
		    (car let-args)
		    (cdr let-args))))

(defmacro (eval x)
    (let ((arg (gensym)))
      (list (list 'lambda (list arg) (list (list 'macro '() arg))) x)))

(define (apply fn args)
    (eval (map (lambda (e) (list 'quote e)) (cons fn args))))

(define identity (lambda (x) x))

(define append (lambda colls (foldr append2 '() colls)))

(define (not x) (if x #f #t))

(define < (lambda operands (apply > (reverse operands))))

(define partial (lambda fn-and-args (lambda rest-of-args (apply (car fn-and-args) (append (cdr fn-and-args) rest-of-args)))))


(define or (let ((args (gensym)))
	     (macro args (foldl (lambda (true? e) (if true? #t (eval e)))
				#f
				args))))

(define and (let ((args (gensym)))
	      (macro args (foldl (lambda (true? e) (if true? (eval e) #f))
				 #t
				 args))))

(define (walk before after code)
    (cond ((before code) (before code))
	  (else (after (map (partial walk before after) code)))))

(define (any? f coll)
    (apply or (map f coll)))

(define zip (lambda lists
	      (if (any? nil? lists)
		  '()
		  (cons (map car lists) (apply zip (map cdr lists))))))

(define (all? f coll)
    (apply and (map f coll)))

(define >= (lambda args
	     (all? (lambda (x) (or (> x 0) (eq? x 0)))
		   (map (partial apply -)
			(zip args (cdr args))))))

(define <= (lambda args
	     (all? (lambda (x) (or (< x 0) (eq? x 0)))
		   (map (partial apply -)
			(zip args (cdr args))))))

(define (take-last n coll)
    (if (nil? coll)
	'()
	(if (>= n (count coll))
	    coll
	    (take-last n (cdr coll)))))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (take n coll)
    (if (and (> n 0) (not (nil? coll)))
	(cons (car coll) (take (dec n) (cdr coll)))
	'()))

(define (drop n coll)
    (if (and (> n 0) (not (nil? coll)))
	(drop (dec n) (cdr coll))
	coll))

(define (partition size step coll)
    (if (>= (count coll) size)
	(cons (take size coll)
	      (partition size step (drop step coll)))
	'()))

(define (mapcat f coll)
    (apply append (map f coll)))

;; if a subsequence passes the predicate,
;; it is replaced with the 'to' value in the
;; given string.
(define (replace-subseq-2 operation coll)
  (if (> 2 (count coll))
      coll
      (cons
       (if (operation (car coll) (car (cdr coll)))
	   (operation (car coll) (car (cdr coll)))
	   (car coll))
       (if (operation (car coll) (car (cdr coll)))
	   (replace-subseq-2 operation (cdr (cdr coll)))
	   (replace-subseq-2 operation (cdr coll))))))

(define (subseq-post-walk-2 operation coll)
    (if (atom? coll)
	coll
	(replace-subseq-2
	 operation
	 (map (partial subseq-post-walk-2 operation)
	      coll))))


(define (alist coll)
    (if (even? (count coll))
	(map (partial apply cons) (partition 2 2 coll))
	'()))

(define (get m k)
    (cond ((nil? m) '())
	  ((eq? (car (car m)) k) (cdr (car m)))
	  (else (get (cdr m) k))))

(define (nth coll index)
    (car (drop index coll)))

(define (update m k f)
    (cons (cons k (f (get m k)))
	  m))

(define (constantly x) (lambda (_) x))

(define (unassoc m k)
    (filter (lambda (pair) (not (eq? (car pair) k)))
	    m))

;; (`    (      LAMBDA (,    (car (car bindings)) (              let ,(cdr bindings) ,@ body))) (car (cdr (car bindings))))
;; to
;; (list (list 'lambda (list (car (car bindings)) (append (list 'let (cdr bindings)) body)))    (car (cdr (car bindings))))
;;
;; splicing unquote can go straight in.
;; (' ,@ (map (partial + 1) '(1 2 3)))
;; becomes
;; (append (map (partial + 1) '(1 2 3)))
;;
;; single unquote needs to be in a list to avoid expanding
;; (' , (+ 1 2))
;; becomes
;; (append (list (+ 1 2)))
;;
;; elements without any quoting need a quote added in addition to list
;; (' lambda)
;; becomes
;; (append (list (quote lambda)))
(define ` (macro quoted-elements
		 (let ((unquoted? (lambda (a b) (if (eq? ', a) (list 'unquote  b) #f)))
		       (splicing-unquoted? (lambda (a b) (if (eq?  ',@ a) (list 'splicing-unquote b) #f)))
		       (auto-gensym? (lambda (a b) (if (eq? '# b) (list 'auto-gensym a) #f)))
		       (first-pass (subseq-post-walk-2 auto-gensym?
						      (subseq-post-walk-2 splicing-unquoted?
									  (subseq-post-walk-2 unquoted? quoted-elements)))))
		   (car (cdr
			 (walk (lambda (element)
				     (cond ((atom? element) (list 'list (list 'quote element)))
					   ((eq? (car element) 'unquote) (cons 'list (cdr element)))
					   ((eq? (car element) 'splicing-unquote) (car (cdr element)))
					   ((eq? (car element) 'auto-gensym) (car (cdr element)))
					   (else #f)))
				   (lambda (element)
				     (cond ((atom? element) element)
					   (else (list 'list (cons 'append element)))))
				   first-pass))))))
