
(define abs
	(lambda (int)
		(if (< 0 int ) int (- int))))


(define gcd 
	(lambda (a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b))))))

(define negative->positive
	(lambda (frac)
		(if  (> 0 (denominator frac))  (fraction 
																			(-(numerator frac))
																			(- (denominator frac)))
		frac
		) ))

(define fraction->integer
	(lambda (frac)
		(if (= (denominator frac) 1) (numerator frac) frac)))

(define tzimtzum
	(lambda (frac)
		(let ((gcdiv (gcd (abs (numerator frac)) (abs (denominator frac)))))
			(if (= gcdiv 1) frac  (fraction (bin/ (numerator frac) gcdiv) (bin/ (denominator frac) gcdiv))))))

(define at-func
	(lambda (frac) (fraction->integer (negative->positive (tzimtzum frac)))))

(define fraction?
	(lambda (number)
		(and (number? number) (not (integer? number))))) 

(define bin+2
	(lambda (x y)

		(cond ((and (fraction? x) (fraction? y)) (let* ((frac (fraction 
													(bin+ (bin* (numerator x) (denominator y)) (bin* (denominator x) (numerator y)))
													(bin* (denominator x) (denominator y)))))
												(at-func frac)))
		((fraction? x) (bin+2 x (fraction y 1)))
		((fraction? y) (bin+2 (fraction x 1) y))
	(else (bin+ x y)))))


(define bin-2
	(lambda (x y)
		(cond ((and (fraction? x) (fraction? y)) (bin+2 x (fraction (bin- 0 (numerator y)) (denominator y)))) 
		((fraction? x) (bin-2 x (fraction y 1)))
		((fraction? y) (bin-2 (fraction x 1) y))
	(else (bin- x y)))))


(define bin*2
	(lambda (x y)
		(cond ((and (fraction? x ) (fraction? y)) (at-func(fraction (bin* (numerator x) (numerator y))  
																	(bin* (denominator x) (denominator y)))))
			  ((fraction? x) (bin*2 x (fraction y 1)))
			  ((fraction? y) (bin*2 (fraction x 1) y))
			  (else (bin* x y))
			)))
(define bin/2
		(lambda (x y)
		(cond ((and (fraction? x ) (fraction? y)) (at-func(fraction (bin* (numerator x) (denominator y))  
																	(bin* (denominator x) (numerator y)))))
			  ((fraction? x) (bin/2 x (fraction y 1)))
			  ((fraction? y) (bin/2 (fraction x 1) y))
			  (else (at-func (fraction x y)))
			)))



(define +
	
		(letrec ((helper
			(lambda (x) (if (null? x) 0 (bin+2 (car x) (helper (cdr x)))))))
	(lambda vars (helper vars))))



(define (length lis)
   (cond ((null? lis)
          0)
         (else
          (+ 1 (length (cdr lis))))))



(define *
	
		(letrec ((helper1
			(lambda (x) (if (null? x) 1 (bin*2 (car x) (helper1 (cdr x)))))))
	(lambda vars (helper1 vars))))

(define (one-map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
            (one-map proc (cdr lst)))))

(define (map proc . lst)
  (if (null? (car lst))
      '()
      (cons (apply proc (one-map car lst))
            (apply map proc (one-map cdr lst)))))


 (define list (lambda a a))



(define - 
	(lambda (a . s) (if (null? s) (bin-2 0 a) (bin-2 a (apply + s)))))



(define /
	(lambda (a . s) (if (null? s) (bin/2 1 a) (bin/2 a (apply *  s))))) 

(define (fold-right f init seq) 
   (if (null? seq) 
       init 
       (f (car seq) 
           (fold-right f init (cdr seq))))) 


(define bin-append
 (lambda (x y)
         (cond((null? x) y)
                (else (cons (car x) (bin-append (cdr x) y) )))))

 ;(define appendddd
;	(lambda lsts 
;		(fold-right bin-append '() lsts))) 
(define append
	(lambda lstt
		(letrec ((helper
				 (lambda (ans x) (cond ((null? x) ans)
				 						(else (helper (bin-append ans (car x)) (cdr x)))))
				))
		(helper '() lstt)
	))
)


(define vector (lambda vec (list->vector vec))) 

(define bin=2
	(lambda (x y )
		(and (bin= (numerator x) (numerator y))
			 (bin= (denominator x) (denominator y)) )))

(define bin<2
	(lambda (x y)
		(let ((sub (bin-2 x y)))
			(bin< (numerator sub) 0))))

(define bin>2
	(lambda (x y)
		(let ((sub (bin-2 x y)))
			(bin> (numerator sub) 0))))


(define order
  (lambda (<)
    (letrec ((loop
	      (lambda (a s)
		(or (null? s)
		    (and (< a (car s))
			 (loop (car s) (cdr s)))))))
      (lambda (a . s)
	(loop a s)))))

(define = (order bin=2))
(define < (order bin<2))
(define > (order bin>2))

(define (zero? x) (= x 0))


(define remainder-helper
	(lambda (x y)
		(cond ((zero? y) 0)
			((< x y) x)
			(else (remainder-helper (- x y) y)))))

(define remainder
	(lambda (x y)
		(let* ((sign (if (< x 0) -1 1))
			   (first (if (< x 0) (* x -1) x))
			   (second (if (< y 0) (* y -1) y))
			   (ans (remainder-helper first second)))
			(* sign ans)
		)
	)
)



(define rational?
	(lambda (x)
		(or (integer? x) (fraction? x))))

(define list->vector
  (lambda (s)
    (let* ((n (length s))
	   (v (make-vector n)))
      (letrec ((loop
		(lambda (s i)
		  (if (= i n) v
		      (begin
			(vector-set! v i (car s))
			(loop (cdr s) (+ i 1)))))))
	(loop s 0))))) 

(define foldr
  (lambda (binop final s)
    (letrec ((loop
	      (lambda (s)
		(if (null? s) final
		    (binop (car s) (loop (cdr s)))))))
      (loop s))))

(define compose
  (let ((binary-compose
	 (lambda (f g)
	   (lambda (x)
	     (f (g x))))))
    (lambda s
      (foldr binary-compose (lambda (x) x) s))))

(define caar (compose car car))
(define cadr (compose car cdr))
(define cdar (compose cdr car))
(define cddr (compose cdr cdr))
(define caaar (compose car caar))
(define caadr (compose car cadr))
(define cadar (compose car cdar))
(define caddr (compose car cddr))
(define cdaar (compose cdr caar))
(define cdadr (compose cdr cadr))
(define cddar (compose cdr cdar))
(define cdddr (compose cdr cddr))
(define caaaar (compose car caaar))
(define caaadr (compose car caadr))
(define caadar (compose car cadar))
(define caaddr (compose car caddr))
(define cadaar (compose car cdaar))
(define cadadr (compose car cdadr))
(define caddar (compose car cddar))
(define cadddr (compose car cdddr))
(define cdaaar (compose cdr caaar))
(define cdaadr (compose cdr caadr))
(define cdadar (compose cdr cadar))
(define cdaddr (compose cdr caddr))
(define cddaar (compose cdr cdaar))
(define cddadr (compose cdr cdadr))
(define cdddar (compose cdr cddar))
(define cddddr (compose cdr cdddr))