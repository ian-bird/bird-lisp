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

;; works like foldl but returns an array where each element
;; is the state of acc when it was on that element.
(define (scan f basis coll)
    (reverse
     (foldl (lambda (acc e)
	      (cons (f (car acc) e) acc))
	    (list basis)
	    coll)))

