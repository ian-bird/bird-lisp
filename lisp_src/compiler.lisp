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
