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

(define append (lambda colls (foldr append2 '() colls)))

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

(define (not x) (if x #f #t))

(define < (lambda operands (apply > (reverse operands))))

(define partial (lambda fn-and-args (lambda rest-of-args (apply (car fn-and-args) (append (cdr fn-and-args) rest-of-args)))))


(define or (macro args (cons 'cond
			     (append (map (lambda (arg) (list arg #t))
					  args)
				     '((else #f))))))

(define and (macro args (list 'not (cons 'or (map (lambda (arg) (list 'not arg)) args)))))

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

(define ` (macro quoted-elements
		 (let ((unquoted? (lambda (a b) (if (eq? ', a) (list 'unquote  b) #f)))
		       (splicing-unquoted? (lambda (a b) (if (eq?  ',@ a) (list 'splicing-unquote b) #f)))
		       (auto-gensym? (lambda (a b) (if (eq? '# b) (list 'auto-gensym a) #f)))
		       (first-pass (subseq-post-walk-2 auto-gensym?
						      (subseq-post-walk-2 splicing-unquoted?
									  (subseq-post-walk-2 unquoted? quoted-elements))))
		       (get-gensyms-fn (lambda (tree self)
					 (if (atom? tree)
					     '()
					     (if (eq? (car tree) 'auto-gensym)
						 (cdr tree)
						 (filter identity (mapcat (lambda (sub-tree)(self sub-tree self)) tree))))))
		       (gensyms-alist (foldl (lambda (alist gensym-sym) (update alist gensym-sym (lambda (_) (gensym))))
					     '()
					     (get-gensyms-fn first-pass get-gensyms-fn))))
		   (car (cdr
			 (walk (lambda (element)
				 (cond ((atom? element)
					(list 'list (list 'quote element)))
				       ((eq? (car element) 'unquote)
					(cons 'list (cdr element)))
				       ((eq? (car element) 'splicing-unquote)
					(car (cdr element)))
				       ((eq? (car element) 'auto-gensym)
					(list 'list (list 'quote (get gensyms-alist (car (cdr element))))))
				       (else
					#f)))
			       (lambda (element)
				 (cond ((atom? element) element)
				       (else (list 'list (cons 'append element)))))
			       first-pass))))))


(define (range from to step)
    (if (< from to)
	(cons from (range (+ from step) to step))
	'()))

(define (macroexpand to-expand)
    (let ((expanded (macroexpand-1 to-expand)))
      (if  (and (eq? (type expanded) 'conscell)
		(eq? (type (eval (car expanded))) 'macro))
	   (macroexpand expanded)
	   expanded)))



(define (macroexpand-all to-expand)
    (cond ((atom? to-expand) to-expand)
	  ((and (not (atom? (car to-expand)))
		(eq? (car (car to-expand)) 'macro)) (macroexpand to-expand))
	  ((not (eq? (type (car to-expand)) 'symbol)) (map macroexpand-all to-expand))
	  ((not (bound? (car to-expand))) (map macroexpand-all to-expand))
	  ((eq? (type (eval (car to-expand))) 'macro) (map macroexpand-all (macroexpand to-expand)))
	  (else (map macroexpand-all to-expand))))

(define (caar x) (car (car x)))

(define (cadr x) (car (cdr x)))

(define (cdar x) (cdr (car x)))

(define (cddr x) (cdr (cdr x)))


(define (halt f) f)

;; drops the last element
(define (drop-last coll)
    (take (dec (count coll)) coll))

(define call-with-cont (lambda args
			 ((last args) (apply (car args) (cdr (drop-last args))))))

;; (match? ('lambda args body) '(lambda (x) (+ x 1))) => true
(defmacro (match? template arg)
    (let (;; matcher will return either a new matcher,
	  ;; true if the pattern matches, or false if it fails
	  (matcher (lambda (next-sym)
		     ())))
      (if (atom? arg)
	  (eq? (matcher arg) #t)
	  (foldl (fn (matcher arg)
		     (if (or (eq? matcher #f) (eq? matcher #t))
			 matcher
			 (matcher arg)))
		 matcher
		 args))))

(defmacro (destructuring-bind template arg body))

;; M expression converts a non-cps form
;; into cps form

;; expand m to convert primitive functions to cps
;; eg + => +&
(define (M expression)
    (cond ((and (not (atom? expression))
		(eq? (car expression) 'lambda))
	   (let ((arg (cadr expression))
		 (body (car (cddr expression)))
		 ($k (gensym)))
	     (` lambda (,@ arg) , (T body 'halt))))
	  (else expression)))

(define (T expression cont)
    (cond
      ((and (not (atom? expression))
		(eq? (car expression) 'lambda))
       (` , cont ,(M expression)))
      ((atom? expression) (` , cont ,(M expression)))
      ((eq? (car expression) 'label)
       (T (car (cddr expression))
	  (` lambda (arg #) (call-with-cont label ,(cadr expression) arg # , cont))))
      ((eq? (car expression) 'cond)
       (let ((cond-body (if  (eq? (car (last expression)) 'else)
			     (cdr expression)
			     (append (cdr expression) '(else '()))))
	     (without-else (drop-last cond-body))
	     (else-consq (cadr (last cond-body))))
	 (foldr (lambda (pred-and-consq acc)
		  (T (car pred-and-consq)
		     (` lambda (pred-sym #) (branch pred-sym #
						    ,(T (cadr pred-and-consq) cont)
						    , acc))))
		(T else-consq cont)
		without-else)))
      ((eq? (car expression) 'quote) (` , cont (quote ,(cadr expression))))
      ((>= (count expression) 2)
       (let ((gensyms (map (lambda (_) (gensym)) expression))
	     (gensyms-and-symbols (zip gensyms expression)))
	 (foldr (lambda (gensym-and-symbol acc)
		  (T (cadr gensym-and-symbol) (` lambda (,(car gensym-and-symbol)) , acc)))
		(` call-with-cont ,@ gensyms , cont)
		gensyms-and-symbols)))
      (else '())))

(define (substitute from to body)
    (cond ((eq? body from) to)
	  ((atom? body) body)
	  (else (map (partial substitute from to) body))))

(define (inline-atoms cps-form)
    (cond ((atom? cps-form) cps-form)
	  ((and (not (atom? (car cps-form)))
		(eq? 'lambda (caar cps-form))
		(atom? (cadr cps-form)))
	   (inline-atoms
	    (substitute (caar (cdar cps-form)) ; lambda arg
			(cadr cps-form) ; lambda call
			(cadr (cdar cps-form)))))  ; lambda body
	  (else (map inline-atoms cps-form))))

(define (funcall? x)
    (and (not ( atom? x))
	 (not (atom? (car x)))
	 (eq? 'lambda (caar x))))

(define (unfold-cps cps-form)
    (cond (;; function calls (lambda expression on left and args on right)
	   ;; should be expanded to have the arguments assigned, then the funarg
	   ;; assigned, and then the expansion of the lambda after the assignment
	   (funcall? cps-form)
	   (append (list (unfold-cps (cadr cps-form)))
		   (` (assign ,(caar (cdar cps-form))))
		   (unfold-cps (cadr (cdar cps-form)))))
	  (;; call with cont needs to be expanded into the function call
	   ;; and then the unfolded continuation following it
	   (and (not (atom? cps-form))
		(eq? (car cps-form) 'call-with-cont))
	   (append (list (drop-last (cdr cps-form)))
		   (unfold-cps (last cps-form))))
	  (;; lambda literals need their params assigned
	   ;; and then are followed by their unfolded body
	   (and (not (atom? cps-form))
		(eq? (car cps-form) 'lambda))
	   (append (` (assign ,(car (cadr cps-form))))
		   (unfold-cps (car (cddr cps-form)))))
	  (;; a branch is turned into an imperative
	   ;; branch instruction, followed by the unfolded
	   ;; consequent, and then a label, and the unfolded
	   ;; alternative
	   (and (not (atom? cps-form))
		(eq? (car cps-form) 'branch))
	   (` (branch ,(cadr cps-form) consq #)
	      ,@ (unfold-cps (car (cddr cps-form)))
	      (: consq #)
	      ,@ (unfold-cps (cadr (cddr cps-form)))))
	  (;; if the continuation call is to halt,
	   ;; then unfold the argument
	   ;; and put the halt instruction after it
	   (and (not (atom? cps-form))
		(eq? (car cps-form) 'halt))
	   (append (unfold-cps (cadr cps-form))
		   (list 'halt)))
	  (else (list cps-form))))

