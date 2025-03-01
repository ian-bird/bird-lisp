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

(define (list? x) (not (atom? x)))

(define (halt f) f)

;; drops the last element
(define (drop-last coll)
    (take (dec (count coll)) coll))

(define call-with-cont (lambda args
			 ((last args) (apply (car args) (cdr (drop-last args))))))

(define (match-head template arg)
    (cond ((nil? template) arg)
	  ((nil? arg) #f)
	  ;; if a list is encountered loop back to top level matching
	  ((list? (car template)) (if (list? (car arg))
				      (match? (car template) (car arg))
				      #f))
	  ((eq? (car template) ',) (match-head (cddr template) (cdr arg)))
	  ((eq? (car template) (car arg)) (match-head (cdr template) (cdr arg)))
	  (else #f)))

(define (match-tail template arg)
    (let ((reversed (reverse arg))
	  ;; need to flip unquotes
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

(define (match-centre template arg)
    (cond ((nil? template) arg)
	  ((nil? arg) #f)
	  ((not (eq? (match-head template arg) #f)) (match-head template arg))
	  (else (match-centre template (cdr arg)))))

(define (split coll on)
    (foldr (lambda (e acc)
	     (if (eq? e on)
		 (cons '() acc)
		 (cons (cons e (car acc))
		       (cdr acc))))
	   '(())
	   coll))

(define (join coll with)
    (if (> (count coll) 1)
	(foldl (lambda (e acc)
		 (append e
			 (list with)
			 acc))
	       (car coll)
	       (cdr coll))
	coll))

(define (match? template arg)
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

(define (take-while pred coll)
    (cond ((nil? coll) '())
	  ((pred (car coll))
	   (cons (car coll) (take-while pred (cdr coll))))
	  (else '())))

(define (extract-values-from-template template)
    (cond ((nil? template) '())
	  ((or (eq? (car template) ',)
	       (eq? (car template) ',@))
	   (cons (cadr template)
		 (extract-values-from-template (cddr template))))
	  ((list? (car template))
	   (append (extract-values-from-template (car template))
		   (extract-values-from-template (cdr template))))
	  (else (extract-values-from-template (cdr template)))))

(define (none? pred coll)
    (not (any? pred coll)))

(define (extract-template-value template value from)
    (cond ((nil? template) '())
	  ((not (match? template from)) panic)
	  ((nil? from) '())
	  ((eq? (car template) ',)
	   (if (eq? (cadr template) value)
	       (car from)
	       (extract-template-value (cddr template) value (cdr from))))
	  ((eq? (car template) ',@)
	   (if (eq? (cadr template) value)
	       (if (> (count template) 2)
		   (take-while (lambda (e)
				 (not (eq? (nth template 3) e)))
			       from)
		   from)
	       (extract-template-value (cddr template) value (cdr from))))
	  ((list? (car template))
	   (if (not (nil? (extract-template-value (car template) value (car from))))
	       (extract-template-value (car template) value (car from))
	       (extract-template-value (cdr template) value (cdr from))))
	  (else (extract-template-value (cdr template) value (cdr from)))))

(defmacro (destructuring-bind template from body)
    (` let ,(map (lambda (val)
		   (` , val
			(extract-template-value , template (quote , val) , from)))
		(extract-values-from-template template))
      , body))

(define match
    (macro clauses
	   (let ((match-on (car clauses))
		 (rest (cdr clauses))
		 (cases (if (eq? 'else (car (last rest)))
			    (drop-last rest)
			    rest))
		 (else-statement (if (eq? 'else (car (last rest)))
				     (list (last rest))
				     '())))
	     (` cond ,@(map (lambda (pattern)
			      (` (match? (quote ,(car pattern)) , match-on)
				 (destructuring-bind (quote ,(car pattern)) , match-on (progn ,@ (cdr pattern)))))
			    cases)
	       ,@ else-statement))))

;; M expression converts a non-cps form
;; into cps form

;; expand m to convert primitive functions to cps
;; eg + => +&
(define (M expression)
    (match expression
	   ((lambda , args , body)
	    (` lambda , args , (T body 'halt)))
	   (else expression)))

(define (T expression cont)
    (if (atom? expression)
	(` , cont ,(M expression))
	(match expression
	       ((lambda ,@ _)
		(` , cont ,(M expression)))
	       
	       ((label , sym , rterm)
		(T rterm (` lambda (arg #) (call-with-cont label , sym arg # , cont))))
	       
	       ((cond ,@ clauses (else , else-statement))
		(foldr (lambda (pred-and-consq acc)
			 (destructuring-bind '(, pred , consq) pred-and-consq
			   (T pred (` lambda (pred-sym #)
				      (branch pred-sym # ,(T consq cont) , acc)))))
		       (T else-statement cont)
		       clauses))
	       
	       ((cond ,@ clauses)
		(foldr (lambda (pred-and-consq acc)
			 (destructuring-bind '(, pred , consq) pred-and-consq
			   (T pred (` lambda (pred-sym #)
				      (branch pred-sym # ,(T consq cont) , acc)))))
		       (T '() cont)
		       clauses))
	       ((quote , quoted)
		(` , cont (quote , quoted)))
	       
	       ((, _ , _ ,@ _)
		(let ((gensyms (map (lambda (_) (gensym)) expression))
		      (gensyms-and-symbols (zip gensyms expression)))
		  (foldr (lambda (gensym-and-symbol acc)
			   (destructuring-bind '(, gensym , symbol) gensym-and-symbol
			     (T symbol (` lambda (, gensym) , acc))))
			 (` call-with-cont ,@ gensyms , cont)
			 gensyms-and-symbols)))
	       
	       (else '()))))

(define (substitute from to body)
    (cond ((eq? body from) to)
	  ((atom? body) body)
	  (else (map (partial substitute from to) body))))

(define (inline-atoms cps-form)
    (match cps-form
	   (((lambda (, param) , body) , arg)
	    (if (atom? arg)
		(inline-atoms (substitute param arg body))
		(map inline-atoms cps-form)))
	   ((,@ elements)
	    (map inline-atoms cps-form))
	   (else cps-form)))

(define (funcall? x)
    (match? '((lambda ,@ _) ,@ _) x))

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

