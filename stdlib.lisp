(label list (lambda elements elements))

(label nil? (macro (v) (list 'eq? v (list 'quote '()))))

;; defines new functions, using scheme formatting
(label define
       (macro args
	      (cond ((atom? (car args))  ; the car is the name being assigned. If its an atom, then plain label.
		     (cons  'label args))
		    (else ; else its a function definition.
		     (list 'label
			   (car (car args)) ; the first argument in the list is the name
			   (cons 'lambda
				 (cons (cdr (car args)) ; the rest of the first argument are the args
				       (cdr args)))))))) ; and the rest is the fn body



;; works like define but for creating new macros
(define defmacro
       (macro args
	      (list 'label
		    (car (car args))
		    (cons 'macro
			  (cons (cdr (car args))
				(cdr args))))))

;; a regular if statement
(defmacro (if pred consq alt)
    (list 'cond (list pred consq)
     (list 'else alt)))

;; returns the number of elements in the supplied list
(define (count coll)
    (if (nil? coll)
	0
	(+ 1 (count (cdr coll)))))

;; fold left, meaning, apply an operation from the
;; head of the list to the tail, carrying an accumulator
;; on each step and returning it at the end
(define (foldl fn basis coll)
    (if (nil? coll)
	basis
	(foldl fn (fn basis (car coll)) (cdr coll))))

;; flips the argument order for a 2 arg function
(define (flip f)
    (lambda (a b)(f b a)))

;; reverses order of a list
(define (reverse coll)
    (foldl (lambda (acc e)(cons e acc)) '() coll))

;; joins 2 lists into 1 larger list
(define (append2 a b)
    (foldl (flip cons) b (reverse a)))

;; fold right, works like fold left but applies
;; from tail to head
(define (foldr fn basis coll)
    (foldl (flip fn) basis (reverse coll)))

;; applies a function to each element in a list and returns
;; a list of the outputs
(define (map fn coll)
    (reverse (foldl (lambda (acc e) (cons (fn e) acc)) '() coll)))

;; removes items that evaluate to false when passed to the function
(define (filter fn coll)
    (foldr (lambda (e acc) (if (fn e) (cons e acc) acc)) '() coll))

(define (even? x)
    (eq? 0 (% x 2)))

;; returns the last item in a set
(define (last coll)
    (if (nil? (cdr coll))
	(car coll)
	(last (cdr coll))))

;; evalautes the list of arguments provided and returns the result
;; of the last one
(define progn (lambda args (last args)))

;; append2 but works with more than 2 lists
(define append (lambda colls (foldr append2 '() colls)))

;; this is equivalent to common-lisp's let*
;; it creates new bindings that can be referenced in the body
;; of the let, and later bindings in the let block can reference
;; earlier ones.
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

;; evaluates a function by doing some runtime trickery with macros
(defmacro (eval x)
    (let ((arg (gensym)))
      (list (list 'lambda (list arg) (list (list 'macro '() arg))) x)))


;; uses eval; MUST be interpreted
;;
;; takes a function and a list of arguments to be passed to it
;; and returns the output
(define (apply fn args)
    ;; we need to quote each arg and the function
    ;; since theyve already been evaluated on the way in,
    ;; and we don't want to evaluate again on the way into
    ;; the eval lambda
    (eval (cons (if (eq? (type fn) 'symbol) fn (list 'quote fn))
	   (map (lambda (e) (list 'quote e)) args))))

;; identity returns the thing passed to it, useful sometimes
(define (identity x) x)

;; returns the logical opposite of its argument
(define (not x) (if x #f #t))

;; less than
(define < (macro operands (cons '> (reverse operands))))

;; applies some of the arguments and returns a function
;; that will take the rest of them
(define partial (lambda fn-and-args
		  (lambda rest-of-args
		    (apply (car fn-and-args) (append (cdr fn-and-args) rest-of-args)))))

;; logical or
(define or (macro args (cons 'cond
			     (append (map (lambda (arg) (list arg #t))
					  args)
				     '((else #f))))))

;; logical and
(define and (macro args (cons 'cond
			      (append (map (lambda (arg) (list (list 'not arg) #f))
					   args)
				      '((else #t))))))

;; applies before and returns it if before is true,
;; otherwise continues down and applies after on the way back up
(define (walk before after code)
    (cond ((before code) (before code))
	  (else (after (map (partial walk before after) code)))))

;; returns true if f is true for any element in coll
(define (any? f coll)
    (apply or (map f coll)))

;; takes several lists, and returns the collections for each index.
;; e.g. (zip '(1 2 3) '(a b c)) would return '((1 a) (2 b) (3 c))
(define zip (lambda lists
	      (if (any? (lambda (x)(nil? x)) lists)
		  '()
		  (cons (map car lists) (apply zip (map cdr lists))))))

;; returns true if f is true for all arguments
(define (all? f coll)
    (apply and (map f coll)))

;; greater than or equal to
(define >= (lambda args
	     (all? (lambda (x) (or (> x 0) (eq? x 0)))
		   (map (partial apply -)
			(zip args (cdr args))))))

;; less than or equal to
(define <= (lambda args
	     (all? (lambda (x) (or (< x 0) (eq? x 0)))
		   (map (partial apply -)
			(zip args (cdr args))))))

;; takes the last n elements of the list
(define (take-last n coll)
    (if (nil? coll)
	'()
	(if (>= n (count coll))
	    coll
	    (take-last n (cdr coll)))))

;; add one
(define (inc x) (+ x 1))

;; add two
(define (dec x) (- x 1))

;; takes the first n elements of the list
(define (take n coll)
    (if (and (> n 0) (not (nil? coll)))
	(cons (car coll) (take (dec n) (cdr coll)))
	'()))

;; removes the first n elements of the list
(define (drop n coll)
    (if (and (> n 0) (not (nil? coll)))
	(drop (dec n) (cdr coll))
	coll))

;; splits a long array into an array of segments of it,
;; where each segment is size elements, and segments are made at step
;; intervals.
(define (partition size step coll)
    (if (>= (count coll) size)
	(cons (take size coll)
	      (partition size step (drop step coll)))
	'()))

;; map with f and coll and then concatenate the returned lists
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

;; post walk with replace subseq 2
(define (subseq-post-walk-2 operation coll)
    (if (atom? coll)
	coll
	(replace-subseq-2
	 operation
	 (map (partial subseq-post-walk-2 operation)
	      coll))))

;; create a new alist
(define (alist coll)
    (if (even? (count coll))
	(map (partial apply cons) (partition 2 2 coll))
	'()))

;; get a value from the alist in m
(define (get m k)
    (cond ((nil? m) '())
	  ((eq? (car (car m)) k) (cdr (car m)))
	  (else (get (cdr m) k))))

;; get the nth element in a list
(define (nth coll index)
    (car (drop index coll)))

;; overwrite the value stored at k in m by passing it through f
(define (update m k f)
    (cons (cons k (f (get m k)))
	  m))

;; returns a function that always returns x regardless of input
(define (constantly x) (lambda (_) x))

;; remove the value stored at k and k
(define (unassoc m k)
    (filter (lambda (pair) (not (eq? (car pair) k)))
	    m))

;;; this defines a domain-specific langauge for quasi-quoting. The syntax is as close as possible to scheme
;; comma is recognized as unquote, and comma at is recognized as splicing unquote.
;; the ordering and placement are slightly different since there isn't support for reader macros, so
;; what would be `(foo ,bar ,@baz) in scheme is (` foo , bar , baz)
;;
;; additionally however, a symbol can be followed by a # to mark that its a gensym,
;; and all matching symbols will be replaced with the same gensym.
;; this should reduce how frequently with-gensysms is needed
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

;; creates a new list of values from from to to with step size step.
;; (range 0 5 1) => '(0 1 2 3 4)
(define (range from to step)
    (if (< from to)
	(cons from (range (+ from step) to step))
	'()))

(define (list? x) (not (atom? x)))

;; uses eval; MUST be interpreted
(define (macroexpand to-expand)
    (let ((expanded (macroexpand-1 to-expand)))
      (if  (and (eq? (type expanded) 'conscell)
		(eq? (type (eval (car expanded))) 'macro))
	   (macroexpand expanded)
	   expanded)))

;; uses eval; MUST be interpreted
;;
;; expands all macros in a given list, recursively
(define (macroexpand-all to-expand)
    (cond ((atom? to-expand) to-expand)
	  ((and (list? (car to-expand))
		(eq? (car (car to-expand)) 'macro))
	   (macroexpand to-expand))
	  ((not (eq? (type (car to-expand)) 'symbol))
	   (map macroexpand-all to-expand))
	  ((not (bound? (car to-expand)))
	   (map macroexpand-all to-expand))
	  ((eq? (type (eval (car to-expand))) 'macro)
	   (map macroexpand-all (macroexpand to-expand)))
	  (else (map macroexpand-all to-expand))))

(define (caar x) (car (car x)))

(define (cadr x) (car (cdr x)))

(define (cdar x) (cdr (car x)))

(define (cddr x) (cdr (cdr x)))


(define (halt f) f) ; testing fn for cps transformation

;; drops the last element
(define (drop-last coll)
    (take (dec (count coll)) coll))

(define call-with-cont (lambda args ; testing fn for cps transformation
			 ((last args) (apply (car args) (cdr (drop-last args))))))

;; matches a pattern by only checking the front of it
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

;; matches a pattern by only checking the end of it
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

;; matches a pattern to see if it matches anywhere
(define (match-centre template arg)
    (cond ((nil? template) arg)
	  ((nil? arg) #f)
	  ((not (eq? (match-head template arg) #f)) (match-head template arg))
	  (else (match-centre template (cdr arg)))))

;; split a list into a list of lists by using 'on' as a split point.
;; returned array will not have on anywhere in any list
(define (split coll on)
    (foldr (lambda (e acc)
	     (if (eq? e on)
		 (cons '() acc)
		 (cons (cons e (car acc))
		       (cdr acc))))
	   '(())
	   coll))

;; join several lists together and add with between them when merging
(define (join coll with)
    (if (> (count coll) 1)
	(foldl (lambda (e acc)
		 (append e
			 (list with)
			 acc))
	       (car coll)
	       (cdr coll))
	coll))

;;; returns whether a supplied value matches a template given.
;;; this uses a similar syntax to scheme quasi quotes, where
;;; the arg must line up with the not unqoted parts of the template.
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

;; takes from the start until pred is false
(define (take-while pred coll)
    (cond ((nil? coll) '())
	  ((pred (car coll))
	   (cons (car coll) (take-while pred (cdr coll))))
	  (else '())))

;; pull all the unquoted variables from a quasi quote template
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

;; returns true if pred returns false for everything in coll
(define (none? pred coll)
    (not (any? pred coll)))

;; given a template and from that matches the template,
;; find the specific value in the template and pull the
;; element from from that corresponds to it.
;; e.g.
;; (extract-template-value '(lambda (, arg) ,@ body) '(lambda (x) (+ x 1)) 'arg)
;;   => 'x
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

;;; destructuring bind allows for concise extraction of several values using quasi quotation syntax.
;;; its like extract-template-value except you don't need to name the value you want, you jsut get
;;; all of them
(defmacro (destructuring-bind template from body)
    (` let ,(map (lambda (val)
		   (` , val
			(extract-template-value , template (quote , val) , from)))
		(extract-values-from-template template))
      , body))

;;; pattern matching operator. This allows you to give a value and a list of possible templates
;;; it could match, and then control passes to the first one matched, at which point
;;; you nave access to all the values from the template.
;;
;; I originally used this for some of the more complex functions in the compiler,
;; but performance was too slow, and optimizing it to use finite state machines
;; was beyond the scope of the project
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

;; lifts all values in a list into a single level deep list
(define (flatten coll)
    (mapcat (lambda (x)
	   (if (list? x)
	       (flatten x)
	       (list x)))
	    coll))

;; applies function after its been applied to all its children
(define (postwalk form fn)
    (if (list? form)
	(fn (map (lambda (e) (postwalk e fn)) form))
	(fn form)))

;; applies fn to itself then to all its children
(define (prewalk form fn)
    (if (list? form)
	(map (lambda (e) (prewalk e fn)) (fn form))
	(fn form)))

;; returns only the unique elements in a list
(define (unique coll)
    (foldr (lambda (e acc)
	     (if (any? (partial eq? e) acc)
		 acc
		 (cons e acc)))
	   '()
	   coll))

;;;
;;; the following block of code does closure conversion. lambda forms
;;; with implicit access to all enclosing scopes have all of their
;;; potentially free variables removed and replaced with explicit
;;; env-ref calls, and an environment containing these is built
;;; and attached to the lambda with make-closure.
;;;

;; given a list of free symbols that need to be referenced from a given
;; inner function, replace them with env-ref calls,
(define (add-env-ref-calls expression free-symbols)
    (if (list? expression)
	(if (or (eq? (car expression) 'make-closure)
		(eq? (car expression) 'quote))
	    expression
	    (map (lambda (e) (add-env-ref-calls e free-symbols))
		 expression))
	(if (any? (partial eq? expression) free-symbols)
	    (` env-ref , expression)
	    expression)))

;; get the variables in a lower function that its accessing from the outer scope
(define (referenced-free-variables expression free-variables)
    (unique
     (if (and (list? expression)
	      (not (eq? (car expression) 'quote)))
	 (mapcat (lambda (e) (referenced-free-variables e free-variables))
		 expression)
	 (filter (partial eq? expression) free-variables))))

;; get the labeled terms, since those are also in scope for deeper functions
(define (extract-labels form)
    (if (list? form)
	(if (eq? (car form) 'label)
	    (append (list (cadr form))
		    (if (list? (nth form 2))
			(mapcat extract-labels (nth form 2))
			'()))
	    (mapcat extract-labels form))
	'()))

;; get all the variables created locally in this function
(define (get-env-variables lambda-expression)
    (append (cadr lambda-expression)
	    (extract-labels
	     ;; dont want to read stuff from child lambdas
	     (prewalk (nth lambda-expression 2)
		      (lambda (e)
			(if (and (list? e)
				 (eq? (car e) 'lambda))
			    '()
			    e))))))

;; convert a lambda into an explicit closure, if needed.
;; gets all the variables it uses, replaces its body with env-refs,
;; and creates the enviromnment and make-closure calls
(define (convert-lambda lambda-expression free-vars)
    (let ((env-vars (get-env-variables lambda-expression))
	  (body (add-env-ref-calls
		 (convert-closures (nth lambda-expression 2) (append env-vars free-vars))
		 free-vars))
	  (closed-vars (referenced-free-variables body free-vars))
	  (replacement-lambda (` lambda , (cadr lambda-expression)
				, body)))
      (if (> (count closed-vars) 0)
	  (` make-closure , replacement-lambda (make-env ,@ closed-vars))
	  replacement-lambda)))

;; convert all the lambdas in  a general expression into explicit closures,
;; if needed.
(define (convert-closures expression free-vars)
    (if (list? expression)
	(if (eq? (car expression) 'lambda)
	    (convert-lambda expression (filter (lambda (free-var)
						 (none? (partial eq? free-var) (cadr expression)))
					       free-vars))
	    (map (lambda (e) (convert-closures e free-vars))
		 expression))
	expression))

;; remove var args so stuff doesnt explode.
(define (protect-var-args expression)
    (if (and (list? expression)
	     (eq? (car expression) 'lambda)
	     (atom? (cadr expression)))
	(` lambda (,(cadr expression)) ,@(map protect-var-args (cddr expression)))
	(if (list? expression)
	    (map protect-var-args expression)
	    expression)))

;;;;
;;;; M and T constitute the functions that perform contination passing style transformation.
;;;; this converts the implicit flow of control in normal style into an explicit one,
;;;; where all atomic operations take an additional argument in the form of a single-arg
;;;; function that receives the result of the operation when its complete.
;;;; Converting to this style gives us a linear flow of control that can then be complied
;;;; into assembly.
;;;;
;;;;

;; M expression converts a non-cps form lambda
;; into cps form

;; expand m to convert primitive functions to cps
;; eg + => +&
(define (M expression)
    (cond ((and (list? expression) ; match (lambda ,_ ,_)
		(eq? (count expression) 3)
		(eq? (car expression) 'lambda))
	   (` lambda , (cadr expression) , (T (nth expression 2) 'halt)))
	  (else expression)))

(define (substitute from to body)
    (cond ((eq? body from) to)
	  ((atom? body) body)
	  (else (map (partial substitute from to) body))))

;;
;; T takes a continuation and a complex form and breaks it down
;; into smaller forms while mainitaining flow of control towards
;; the given continatuion
;;

(define (T-cond expression cont)
    (foldr (lambda (pred-and-consq acc)
		    (if (atom? (car pred-and-consq))
			(` branch ,(car pred-and-consq) ,(T (cadr pred-and-consq) cont) , acc)
			(let ((pred-sym (gensym)))
			  (T (car pred-and-consq)
			     (` lambda (, pred-sym)
			       (branch , pred-sym ,(T (cadr pred-and-consq) cont) , acc))))))
		  (if (eq? (car (last expression)) 'else)
		      (T (cadr (last expression)) cont)
		      (T '() cont))
		  (cdr (if (eq? (car (last expression)) 'else)
			   (drop-last expression)
			   expression))))

(define (T-func expression cont)
    (let ((gensyms (map (lambda (_) (gensym)) expression))
		 (gensyms-and-symbols (zip gensyms expression)))
	     (foldr (lambda (gensym-and-symbol acc)
		      (if (atom? (cadr gensym-and-symbol))
			  acc
			  (T (cadr gensym-and-symbol)
			     (` lambda (, (car gensym-and-symbol)) , acc))))
		    (` call-with-cont ,@ (map (lambda (gensym-and-symbol)
						((if (atom? (cadr gensym-and-symbol))
						     cadr
						     car)
						 gensym-and-symbol))
					      gensyms-and-symbols) , cont)
		    gensyms-and-symbols)))

(define (T expression cont)
    (cond ((atom? expression)
	   (if (atom? cont)
	       (` , cont , expression)
	       (substitute (car (cadr cont)) expression (nth cont 2))))
	  ((and (eq? (car expression) 'lambda))  ; match (lambda ,@ _)
	   (` , cont ,(M expression)))
	  
	  ((and (eq? (count expression) 3) ; match (label , _ , _)
		(eq? (car expression) 'label))
	   (if (atom? (nth expression 2))
	       (` call-with-cont label ,(cadr expression) ,(nth expression 2) , cont)
	       (let ((arg (gensym)))
		 (T (nth expression 2)
		    (` lambda (, arg) (call-with-cont label , (cadr expression) , arg , cont))))))

	  ((eq? 'cond (car expression)) ; match (cond ,@ _)
	   (T-cond expression cont))

	  ((and (eq? (count expression) 2) ; match (quote , _)
		(eq? 'quote (car expression)))
	   (` , cont (quote , (cadr expression))))

	  ((eq? '(gensym) expression) ; match (gensym)
	   (` call-with-cont gensym , cont))

	  ((>= (count expression) 1) ; match (_ ,@ _)
	   (T-func expression cont))
	  (else '())))

;;
;; the result of CPS trasnformation is still a deeply nested structure,
;; and unfold converts it to a linear sequence of instructions.
;;

(define (unfold-lambda-call cps-form)
    (` ,(unfold (cadr cps-form))
	       ,@(map (lambda (param) (` assign , param))
		      (cadr (car cps-form)))
	,@(unfold (nth (car cps-form) 2))))

(define (unfold-branch cps-form)
    (let ((consq-label (gensym)))
	       (` (branch , (cadr cps-form) , consq-label)
		  ,@(unfold (nth cps-form 3))
		  (-> , consq-label)
		  ,@(unfold (nth cps-form 2)))))

(define (unfold cps-form)
    (let ((lambda-call? (and (list? cps-form)
			     (list? (car cps-form))
			     (eq? (caar cps-form) 'lambda)
			     (eq? (count cps-form) 2)
			     (eq? (count (car cps-form)) 3)))
	  (funcall? (and (list? cps-form)
			 (eq? (car cps-form) 'call-with-cont) 
			 (>= (count cps-form) 2)))
	  (assignment? (and (list? cps-form)
			    (eq? (car cps-form) 'lambda) 
			    (eq? (count cps-form) 3)))
	  (branch? (and (list? cps-form)
			(eq? (car cps-form) 'branch) 
			(eq? (count cps-form) 4)))
	  (halt? (and (list? cps-form)
		      (eq? (car cps-form) 'halt) 
		      (eq? (count cps-form) 2)))
	  (quote? (and (list? cps-form)
		       (eq? (car cps-form) 'quote) 
		       (eq? (count cps-form) 2))))
      (cond ((atom? cps-form) (list cps-form))
	    (lambda-call?
	     (unfold-lambda-call cps-form))
	    
	    (funcall?
	     (` ,(cdr (drop-last cps-form)) ,@(unfold (last cps-form))))
	    
	    (assignment?
	     (` ,@(map (lambda (param) (` assign , param))
		       (cadr cps-form))
		  ,@(unfold (nth cps-form 2))))
	    
	    (branch?
	     (unfold-branch cps-form))
	    
	    (halt?
	     (let ((arg (cadr cps-form)))
	       (` ,(if (atom? arg) arg (unfold arg))
		   halt)))
	    
	    (quote?
	     cps-form)
	    (else (list cps-form)))))

;; fix assignments adds in stack numbers and references
;; to stack variables, which saves us from having to look up
;; bindings in msot cases.
(define (fix-assignments instructions)
    (let ((stack-value 0)
	  (gensym-alist '()))
      (reverse
       (foldl (lambda (fixed instruction)
		(cons
		 (cond ((list? instruction)
			(cond ((list? (car instruction))
			       instruction) 
			      ((eq? (car instruction) 'assign)
			       (if (or (nil? fixed)
				       (eq? (caar fixed) 'assign))
				   (progn
				     (set! 'stack-value (inc stack-value))
				     (set! 'gensym-alist
					   (update gensym-alist
						   (cadr instruction)
						   (constantly (dec stack-value))))
				     (` assign arg# ,(dec stack-value) ,(dec stack-value)))
				   (progn
				     (set! 'stack-value (inc stack-value))
				     (set! 'gensym-alist
					   (update gensym-alist
						   (cadr instruction)
						   (constantly (dec stack-value))))
				     (` assign prev , (dec stack-value)))))
			      ((eq? (car instruction) 'make-env)
			       (foldr (lambda (term acc)
					(if (get gensym-alist term)
					    (` $ ,(get gensym-alist term) , term ,@ acc)
					    (cons term acc)))
				      '()
				      instruction))
			      (else (foldr (lambda (term acc)
					     (if (get gensym-alist term)
						 (cons '$
						       (cons (get gensym-alist term)
							     acc))
						 (cons term acc)))
					   '()
					   instruction))))
		       ((and (eq? (type instruction) 'symbol)
			      (get gensym-alist instruction))
			(list '$ (get gensym-alist instruction)))
		       (else instruction))
		 fixed))
	      '()
	      instructions))))

;; works like foldl but returns an array where each element
;; is the state of acc when it was on that element.
(define (scan f basis coll)
    (reverse
     (foldl (lambda (acc e)
	      (cons (f (car acc) e) acc))
	    (list basis)
	    coll)))

;; replace branch to gensym and arrow marker gensym
;; with branch and then the instruction number to goto,
;; with the branch label removed.
(define (fix-branches instructions)
    (let ((goto-alist '()))
      (foldr (lambda (instruction-and-line-num acc)
	       (let ((instruction (car instruction-and-line-num))
		     (line-num (cadr instruction-and-line-num)))
		 (cond ((and (list? instruction)
			     (eq? (count instruction) 2)
			     (eq? (car instruction) '->))
			(progn
			   (set! 'goto-alist (update goto-alist
						     (cadr instruction)
						     (constantly line-num)))
			   acc))

		       ((and (list? instruction)
			     (>= (count instruction) 2)
			     (eq? (car instruction) 'branch))
			(cons (` branch ,@ (cdr (drop-last instruction))
					   ,(get goto-alist (last instruction)))
			      acc))

		       (else (cons instruction acc)))))
	     '()
	     (zip instructions
		  (scan (lambda (acc instruction)
			  (if (and (not (atom? instruction))
				   (eq? (car instruction) '->))
			      acc
			      (inc acc)))
			0
			instructions)))))

;; uses eval; MUST be interpreted

;; figure out what calls are to functions and add in
;; a gosub in front, so the runtime knows to go call a function.
(define (add-gosubs instructions)
    (map (lambda (instruction)
	   (cond ((atom? instruction)
		  instruction)
		 ((or (any? (partial eq? (car instruction))
			    '(assign branch make-closure make-env env-ref))
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

;; if a gosub appears right before a halt instruction, it
;; can be replaced with a tail call, since we don't need
;; the state of any stack variables when we return.
;; so mark it as a tail call.
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

(define (get-immediate-lambdas expression)
    (if (list? expression)
	(if (eq? (car expression) 'lambda)
	    (list expression)
	    (mapcat get-immediate-lambdas expression))
	'()))

;; adds in a var arg marker for functions that take variable numbers of arguments,
;; so that the caller knows to place all its args in the first slot, rather
;; than spreading them across several.
(define (mark-var-args expression assembly)
    (let ((immediate-lambdas (get-immediate-lambdas (cddr expression)))
	  (new-asm
	   (reverse
	    (cadr
	     (foldl (lambda (prior ins)
		      (let ((im-lmb (car prior))
			    (acc (cadr prior)))
			(if (and (list? ins)
				 (eq? (car ins) 'code)
				 (list? im-lmb)
				 (not (nil? im-lmb)))
			    (list (cdr im-lmb) (cons (mark-var-args (car im-lmb) ins)
						     acc) )
			    (list im-lmb (cons ins acc)))))
		    (list immediate-lambdas '())
		    assembly)))))
      (if (and (list? expression)
	       (eq? (car expression) 'lambda)
	       (atom? (cadr expression)))
	  (let ((fixed-branches (map (lambda (ins)
				       (if (and (list? ins)
						(eq? (car ins) 'branch))
					   (append (drop-last ins)
						   (list (inc (last ins))))
					   ins))
				     new-asm)))
	    (if (eq? (car fixed-branches) 'code)
		(cons 'code (cons '(var-arg) (cdr fixed-branches)))
		(cons '(var-arg) fixed-branches)))
	  new-asm)))

;; perform all the post-unfolding work recursively across code blocks
(define (optimize instructions)
    (map (lambda (instruction)
	   (cond ((atom? instruction) instruction)
		 ((eq? (car instruction) 'code)
		  (` code ,@(optimize (cdr instruction))))
		 (else instruction)))
       (add-tailcalls
	(add-gosubs
	 (fix-branches
	  (fix-assignments
	   instructions))))))

;; get the assembly code for a lambda form provided
(define (to-assembly lambda-expression)
    (mark-var-args
     lambda-expression
     (optimize
      (unfold
       (M
	(convert-closures
	 (protect-var-args
	  (macroexpand-all
	   lambda-expression))
	 '()))))))

;; compile a quoted lambda expression
(define (compile lambda-expression)
    (assemble (to-assembly lambda-expression)))

;; creates a new compiled function
(defmacro (def-comp call-form body)
    (` define ,(car call-form)
       (compile (quote (lambda ,(cdr call-form) , body)))))
