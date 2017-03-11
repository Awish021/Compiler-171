
(define (has-duplicates? lst) 
  (cond
     ((null? lst) #f)
     ((not (not (member (car lst) (cdr lst)))) #t)
     (else (has-duplicates? (cdr lst)) )))

(define (flatten-list x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten-list (car x)) (flatten-list (cdr x))))
        (else (list x))))

(define last
	(lambda (x) 
		(let ((l (length x))
			  (vec (list->vector x)))
		(vector-ref vec (- l 1)))))

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define applic?
	(lambda (x) (and (list? x) (not (null? x)))))

	(define atom?
		(lambda (x) (or (null? x) (not (list? x)) (and (equal? 'quote (car x)) (= (length x) 2)))))

(define atom-list? 
	(lambda (x)
		(cond ((null? x) #t)
			  ((and (list? x) (=(length x) 1) (number? (car x))) #f)
			((list? x) (andmap atom? x)) 
			(else #t)
		)
	)
)

(define flaten
	(lambda (x)
		(if (not (list? x)) x
		(letrec 
			((helper 
				(lambda (y)
					(if (null? y)
						y
						(if (atom-list? y)
						(list y)
						(let 
							((first (helper (car y)))
							 (second (helper (cdr y))))
							(append (if (list? first) first (list first)) 
								(if (list? second) second (list second)))
						))))))
		(helper x)	))))



(define how-many-in-list
	(lambda (list)
		(lambda (x)
		(apply + 
		(map 
			(lambda (y) (if (equal? y x) 1 0)) list)))))


(define list->number-of-occurences
	(lambda (list)
		(map (how-many-in-list list) list)))




(define remove-duplicates
	(lambda  (list)
  (fold-right (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y))) '() list)))

(define remove-uniques ; removes from dictionary every key with value 1 (value 1 = unique)
	(lambda (dict)
		(filter
			(lambda (pair)
				(not (= (cdr pair) 1))) dict)))


(define remove-not-applications
	(lambda (lst)
		(filter (lambda (x)  (applic? x)) lst )))

(define generate-symbol-list
	(lambda (x)
		(if (= x 0) '() (cons (gensym) (generate-symbol-list (- x 1))))))

(define remove-values
	(lambda (list)
		(map car list)))

(define create-symbols-for-dictionary
	(lambda (dict)
		(map list (generate-symbol-list (length dict)) dict )))

(define dict-lookup
	(lambda (dict value)
		(cond ((null? dict) value)
			  ((equal? (cadar dict) value) (caar dict))
			  (else (dict-lookup (cdr dict) value)))))

(define dict->let*
	(lambda (dict expr)
		(cond ((null? dict) expr)
			((= (length dict )1) `(let ,dict ,expr))
			(else `(let* ,dict ,expr)))))

(define replace-with-dictionary
	(lambda (dictionary)
		(lambda (x)
			(dict-lookup dictionary x))))

(define create-dictionary
	(lambda (expr)
		(let ((flat-list (flaten expr)))
		(create-symbols-for-dictionary ( remove-not-applications (remove-values (remove-uniques (remove-duplicates (map cons flat-list (list->number-of-occurences flat-list)))))
		)))))


(define parse-expr
	(lambda (expr dict)
		(letrec ((helper
			(lambda (exp)
			(cond ((null? exp) exp)
			  ((or (atom? exp) (atom-list? exp)) (dict-lookup dict exp))
			  (else (map helper exp))))))
		(helper expr))))


(define replacer
	(lambda (var val)
		(lambda (p)
			(map (lambda (x) (if (equal? var x) val  x)) (cadr p))
		)
	)	
)

(define remover
	(lambda (dict var val)
		(let ((filtered (filter (lambda (x) (not(equal? (car x) var))) dict)))
			(map (lambda (x) (list (car x) ((replacer var val)x))) filtered) 
		)	
	)
)

(define optimize-dict
	(lambda (dict expr)
			(letrec ((helper
						(lambda (d e)
							
							(cond ((null? d) '())
								  ((and (= (length d) 1) (member (caar d) (flatten-list e)))  d)
								  ((= (length d) 1) '())
								  (else (let* ((lvar (car d))
				 					(flatexpr (flatten-list expr))
				 				(flatdict (flatten-list d))
								 (counter ((how-many-in-list flatdict) (car lvar) )))
									(cond ((member (car lvar) flatexpr) (cons lvar (helper (cdr d) e)))
										((= counter 1) (helper (cdr d) e)) 
										((= counter 2) (helper (remover d (car lvar) (cadr lvar)) expr))
													(else (cons lvar (helper (cdr d) e))))))))))
		 (if (equal? dict (helper dict expr)) dict (optimize-dict (helper dict expr) expr)))))

(define cse-eval
	(lambda (expr dict)
		(let* ((dictionary (append  dict (create-dictionary (flaten expr))))
			  (parsed-expr (parse-expr expr dictionary)))
			(if (equal? parsed-expr expr) (dict->let* (optimize-dict dictionary expr) expr) (cse-eval parsed-expr dictionary ))
				)))

(define cse
	(lambda (expr)
	(cse-eval expr '())))
		



