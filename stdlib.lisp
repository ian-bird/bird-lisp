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
    ;; we need to quote each arg and the function
    ;; since theyve already been evaluated on the way in,
    ;; and we don't want to evaluate again on the way into
    ;; the eval lambda
    (eval (map (lambda (e) (list 'quote e)) (cons fn args))))

(define identity (lambda (x) x))

(define (not x) (if x #f #t))

(define < (macro operands (cons '> (reverse operands))))

(define partial (lambda fn-and-args
		  (lambda rest-of-args
		    (apply (car fn-and-args) (append (cdr fn-and-args) rest-of-args)))))


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
                 (take (last (take-while (lambda (n)
					  (match? template (drop n from)))
					 (range 0 (inc (count from)) 1)))
		       from)
		 from)
	     (extract-template-value (cddr template)
				     value
				     (drop (last (take-while (lambda (n)
							      (match? template (drop n from)))
							     (range 0  (inc (count from)) 1)))
					   from))))
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

(define (unfold cps-form)
    (match cps-form
	   (((lambda , params , body) , arg)
	    (` ,(unfold arg)
		,@(map (lambda (param) (` assign , param))
		       params)
		,@(unfold body)))
	   
	   ((call-with-cont ,@ args , cont)
	    (` , args ,@ (unfold cont)))
	   
	   ((lambda , params , body)
	    (` ,@(map (lambda (param) (` assign , param))
		      params)
		 ,@(unfold body)))

	   ((branch , pred , consq , alt)
	    (` (branch , pred consq-label #)
	       ,@(unfold alt)
	       (-> consq-label #)
	       ,@(unfold consq)))

	   ((halt , arg)
	    (` ,(if (atom? arg) arg (unfold arg))
	       halt))

	   ((quote , quoted)
	    (` quote , quoted))

	   (else (list cps-form))))

(define (flatten coll)
    (mapcat (lambda (x)
	   (if (list? x)
	       (flatten x)
	       (list x)))
	    coll))

;; need to define how to look up bindings for sub functions
(define (insert-bindings instructions)
    (let ((extract-args (lambda (form callback)
			  (let ((these-args
				  (map last
				       (take-while (lambda (instruction)
						     (and (list? instruction)
							  (eq? (car instruction) 'assign)))
						   form))))
			    (append these-args
				    (map (lambda (x) (callback x callback))
					 (filter (lambda (instruction)
						   (and (list? instruction)
							(list? (car instruction))))
						 form))))))
	  (needs-binding (lambda (form arg)
			   (any? (partial eq? arg)
				 (flatten (filter (lambda (x)
						    (and ( list? x)
							 (list? (car x))))
						  form)))))
	  (args (extract-args instructions extract-args))
	  (to-bind (filter (partial needs-binding instructions)
			   (filter atom? args))))
      (map (lambda (instruction)
	     (if (and (list? instruction)
		     (eq? (car instruction) 'assign)
		     (any? (partial eq? (cadr instruction)) to-bind))
		 (` bind , (cadr instruction))
		 instruction))
	   instructions)))

(define (fix-assignments instructions)
    (let ((stack-value 0)
	  (gensym-alist '()))
      (map (lambda (instruction)
	     (if (and (list? instruction)
		      (eq? (car instruction) 'bind))
		 (` label ,@ (cdr instruction))
		 instruction))
	   (reverse
	    (foldl (lambda (fixed instruction)
		     (cons
		      (if (list? instruction)
			  (if (or (eq? (car instruction) 'assign)
				  (eq? (car instruction) 'bind))
			      (if (or (nil? fixed)
				      (eq? (caar fixed) 'assign)
				      (eq? (caar fixed) 'bind))
				  (progn
				    (set! 'stack-value (inc stack-value))
				    (if (eq? (car instruction) 'assign)
					(set! 'gensym-alist
					      (update gensym-alist
						      (cadr instruction)
						      (constantly (dec stack-value))))
					'())
				    (if (eq? (car instruction) 'assign)
					(` assign arg ,(dec stack-value) ,(dec stack-value))
					(` bind ,(cadr instruction) arg ,(dec stack-value))))
				  (progn
				    (set! 'stack-value (inc stack-value))
				    (set! 'gensym-alist
					  (update gensym-alist
						  (cadr instruction)
						  (constantly (dec stack-value))))
				    (` assign prev , (dec stack-value))))
			      (foldr (lambda (term acc)
				       (if (get gensym-alist term)
					   (cons '$
						 (cons (get gensym-alist term)
						       acc))
					   (cons term acc)))
				     '()
				     instruction))
			  (if (and (eq? (type instruction) 'symbol)
				   (get gensym-alist instruction))
			      (list '$ (get gensym-alist instruction))
			      instruction))
		      fixed))
		   '()
		   instructions)))))

(define (scan f basis coll)
    (reverse
     (foldl (lambda (acc e)
	      (cons (f (car acc) e) acc))
	    (list basis)
	    coll)))

(define (fix-branches instructions)
    (let ((goto-alist '()))
      (foldr (lambda (instruction-and-line-num acc)
	       (destructuring-bind '(, instruction , line-num) instruction-and-line-num
		 (match instruction
			((-> , goto)
			 (progn
			   (set! 'goto-alist (update goto-alist
						     goto
						     (constantly line-num)))
			   acc))
			((branch ,@ middle , goto)
			 (cons (` branch ,@ middle , (get goto-alist goto)) acc))
			(else
			 (cons instruction acc)))))
	     '()
	     (zip instructions
		  (scan (lambda (acc instruction)
			  (if (and (not (atom? instruction))
				   (eq? (car instruction) '->))
			      acc
			      (inc acc)))
			0
			instructions)))))

(define (add-gosubs instructions)
    (map (lambda (instruction)
	   (cond ((atom? instruction)
		  instruction)
		 ((or (eq? (car instruction) 'assign)
		      (eq? (car instruction) 'branch)
		      (eq? (car instruction) 'bind)
		      (and (eq? (count instruction) 2)
			   (eq? (car instruction) '$)))
		  instruction)
		 ((and (eq? (type (car instruction)) 'symbol)
		       (bound? (car instruction))
		       (eq? (type (eval (car instruction))) 'specialform))
		  instruction)
		 ((list? (car instruction))
		  (cons 'code instruction))
		 (else (cons 'gosub instruction))))
	 instructions))

(define (add-tailcalls instructions)
    (foldr (lambda (instruction prevs)
	     (cons (cond ((or (nil? prevs)
			     (not (eq? (car prevs) 'halt)))
			  instruction)
			((and (list? instruction)
			      (eq? (car instruction) 'gosub))
			 (cons 'tailcall (cdr instruction)))
			(else instruction))
		   prevs))
	   '()
	   instructions))

(define (mark-symbols-for-no-lookup instructions)
    (map (lambda (instruction)
	   (cond ((atom? instruction) instruction)
		 ((and (or (eq? (car instruction) 'quote)
			   (eq? (car instruction) 'label)))
		  (if (eq? (type (cadr instruction)) 'symbol)
		      (` ,(car instruction) ~ ,@(cdr instruction))
		      instruction))
		 (else instruction)))
	 instructions))

(define (optimize instructions)
    (mark-symbols-for-no-lookup
     (add-tailcalls
      (add-gosubs
       (fix-branches
	(fix-assignments
	 (insert-bindings
	  instructions)))))))

(define (to-assembly lambda-expression)
  (map (lambda (instruction)
	 (match instruction
	   ((code ,@ lines)
		    (` code ,@(optimize lines)))
		   (else instruction)))
	  (optimize
	   (unfold
	    (inline-atoms
	     (M
	      (macroexpand-all
	       lambda-expression)))))))

(define (compile lambda-expression)
    (assemble (to-assembly lambda-expression)))

(defmacro (def-comp call-form body)
    (` define ,(car call-form)
       (compile (quote (lambda ,(cdr call-form) , body)))))

