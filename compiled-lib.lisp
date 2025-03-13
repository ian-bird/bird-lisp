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

(def-comp (filter fn coll)
    (foldr (lambda (e acc) (if (fn e) (cons e acc) acc)) '() coll))

(def-comp (last coll)
    (if (nil? (cdr coll))
	(car coll)
	(last (cdr coll))))

(def-comp (identity x) x)

(def-comp (not x) (if x #f #t))

(def-comp (walk before after code)
    (cond ((before code) (before code))
	  (else (after (map (partial walk before after) code)))))

(def-comp (any? f coll)
    (foldl (lambda (acc e) (or acc e)) #f (map f coll)))

(def-comp (all? f coll)
    (foldl (lambda (acc e) (and acc e)) #t (map f coll)))

(def-comp (take-last n coll)
    (if (nil? coll)
	'()
	(if (>= n (count coll))
	    coll
	    (take-last n (cdr coll)))))

(def-comp (take n coll)
    (if (and (> n 0) (not (nil? coll)))
	(cons (car coll) (take (dec n) (cdr coll)))
	'()))

(def-comp (drop n coll)
    (if (and (> n 0) (not (nil? coll)))
	(drop (dec n) (cdr coll))
	coll))

(def-comp (partition size step coll)
    (if (>= (count coll) size)
	(cons (take size coll)
	      (partition size step (drop step coll)))
	'()))

(def-comp (mapcat f coll)
    (apply append (map f coll)))

(def-comp (replace-subseq-2 operation coll)
  (if (> 2 (count coll))
      coll
      (cons
       (if (operation (car coll) (car (cdr coll)))
	   (operation (car coll) (car (cdr coll)))
	   (car coll))
       (if (operation (car coll) (car (cdr coll)))
	   (replace-subseq-2 operation (cdr (cdr coll)))
	   (replace-subseq-2 operation (cdr coll))))))

(def-comp (subseq-post-walk-2 operation coll)
    (if (atom? coll)
	coll
	(replace-subseq-2
	 operation
	 (map (partial subseq-post-walk-2 operation)
	      coll))))

(def-comp (alist coll)
    (if (even? (count coll))
	(map (partial apply cons) (partition 2 2 coll))
	'()))

(def-comp (get m k)
    (cond ((nil? m) '())
	  ((eq? (car (car m)) k) (cdr (car m)))
	  (else (get (cdr m) k))))

(def-comp (nth coll index)
    (car (drop index coll)))

(def-comp (update m k f)
    (cons (cons k (f (get m k)))
	  m))

(def-comp (constantly x) (lambda (_) x))

(def-comp (unassoc m k)
    (filter (lambda (pair) (not (eq? (car pair) k)))
	    m))

(def-comp (range from to step)
    (if (< from to)
	(cons from (range (+ from step) to step))
	'()))

(def-comp (caar x) (car (car x)))

(def-comp (cadr x) (car (cdr x)))

(def-comp (cdar x) (cdr (car x)))

(def-comp (cddr x) (cdr (cdr x)))

(def-comp (list? x) (not (atom? x)))

(def-comp (halt f) f)

(def-comp (drop-last coll)
    (take (dec (count coll)) coll))

(def-comp (match-head template arg)
    (cond ((nil? template) arg)
	  ((nil? arg) #f)
	  ;; if a list is encountered loop back to top level matching
	  ((list? (car template)) (if (list? (car arg))
				      (match? (car template) (car arg))
				      #f))
	  ((eq? (car template) ',) (match-head (cddr template) (cdr arg)))
	  ((eq? (car template) (car arg)) (match-head (cdr template) (cdr arg)))
	  (else #f)))


(def-comp (match-tail template arg)
  (let ((reversed (reverse arg))
	(reversed-template
	 (let ((unquote (gensym)))
	   (foldl (lambda (acc e)
		    (cond ((and (not (nil? acc))
				(eq? (car acc) unquote))
			   (cons ', (cons e (cdr acc))))
			  ((eq? e ',) (cons unquote acc))
			  (else (cons e acc))))
		  '()
		  template))))
    (match-head reversed-template reversed)))



(def-comp (match-centre template arg)
    (cond ((nil? template) arg)
	  ((nil? arg) #f)
	  ((not (eq? (match-head template arg) #f)) (match-head template arg))
	  (else (match-centre template (cdr arg)))))

(def-comp (split coll on)
    (foldr (lambda (e acc)
	     (if (eq? e on)
		 (cons '() acc)
		 (cons (cons e (car acc))
		       (cdr acc))))
	   '(())
	   coll))

(def-comp (join coll with)
    (if (> (count coll) 1)
	(foldl (lambda (e acc)
		 (append e
			 (list with)
			 acc))
	       (car coll)
	       (cdr coll))
	coll))

(def-comp (match? template arg)
    (if (atom? arg) #f
	(let (;; builds up the segments that must match, ignoring splicing unquote symbols
	      (actual-groups (split (let ((splicing-unquote (gensym)))
				      (reverse
				       ;; drops the element after a splicing unquote
				       (foldl (lambda (acc e)
						(cond ((and (not (nil? acc))
							    (eq? splicing-unquote (car acc)))
						       (cons ',@ (cdr acc)))
						      ((eq? e ',@)
						       (cons splicing-unquote acc))
						      (else (cons e acc))))
					      '()
					      template)))
				    ',@))
              (head-matches? (or (eq? (car actual-groups) '())
				 (not (eq? (match-head (car actual-groups) arg) #f))))
              (tail-matches? (or (eq? (last actual-groups) '())
				 (not (eq? (match-tail (last actual-groups) arg) #f))))
              (centre-groups-match?
	       (if  (> (count actual-groups) 2)
		    ;; apply each center match to what remains of the center section
		    ;; if #f was already generated just propogate it to the end
		    (foldl (lambda (acc e)
			     (if (atom? acc)
				 acc
				 (match-centre e acc)))
			   (match-tail (last actual-groups)
				       (match-head (car actual-groups)
						   arg))
			   (drop-last (cdr actual-groups)))
		    #t)))
	  (and head-matches? tail-matches? centre-groups-match?))))
