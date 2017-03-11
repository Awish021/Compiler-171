(load "pattern-matcher.scm")
(load "pc.scm")
(load "parser.so")	
(load "primitive.scm")



(define <digit-0-9>
  (range #\0 #\9))

(define <a-f>
  (range #\a #\f))

(define <A-F>
  (range #\A #\F))

(define <a-z>
  (range #\a #\z))

(define <A-Z>
  (range #\A #\Z))


(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <whitespaces>
	(new
		(*parser <whitespace>)
		*star
		done))


(define <Natural>
	(new
	(*parser <digit-0-9>)
	*plus
	(*pack (lambda(c) (string->number
			(list->string c))))
		done
		)
)

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (-- n) (- n)))

       (*parser <Natural>)

       (*disj 3)

       done))

	
(define <boolean>
		(new
			(*parser (char #\#))
			(*parser (char #\t))	
			(*pack (lambda(_) #t))
			(*parser (char #\f))
			(*pack (lambda(_) #f) )
			(*disj 2)
			(*caten 2)
			(*pack-with 
				(lambda (h b) b))
			done) 
		)


(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (if (= den 0) 
	  (string->symbol (string-append (number->string num) "/" "0")) (/ num den) )))
       done))

(define <Number>
	(new 
		(*parser <whitespaces>)
		(*parser <Fraction>)
		(*parser <Integer>)
		(*disj 2)
		(*parser <whitespaces>)
		(*caten 3)
		(*pack-with (lambda (a b c ) b ))
		done))


(define <CharPrefix>
	(new
		(*parser (char #\#))
		(*parser (char #\\))
		(*caten 2)
		done
		)
	)

(define <VisibleSimpleChar>
	(range #\! #\xff)
	)

(define <NamedChar>
	(new
		(*parser (word-ci "lambda"))
		(*pack
			(lambda (a)	
	 			  (integer->char 955)))
		(*parser (word-ci "newline"))
		(*pack
			(lambda (a)	
	 			  #\newline))
		(*parser (word-ci "nul"))
		(*pack
			(lambda (a)	
	 			  #\nul))
		(*parser (word-ci "page"))
		(*pack
			(lambda (a)	
	 			  #\page))
		(*parser (word-ci "return"))
		(*pack
			(lambda (a)	
	 			  #\return))
		(*parser (word-ci "space"))
		(*pack
			(lambda (a)	
	 			  #\space))
		(*parser (word-ci "tab"))
		(*pack
			(lambda (a)	
	 			  (integer->char 9)))
		(*disj 7)
		done
		)
	)

(define hex-chars-to-number 
	(lambda (num)
		(cond 	((char=? num #\0) 0)
				((char=? num #\1) 1)
				((char=? num #\2) 2)
				((char=? num #\3) 3)
				((char=? num #\4) 4)
				((char=? num #\5) 5)
				((char=? num #\6) 6)
				((char=? num #\7) 7)
				((char=? num #\8) 8)
				((char=? num #\9) 9)
				((char=? num #\a) 10)
				((char=? num #\b) 11)
				((char=? num #\c) 12)
				((char=? num #\d) 13)
				((char=? num #\e) 14)
				((char=? num #\f) 15)
				((char=? num #\A) 10)
				((char=? num #\B) 11)
				((char=? num #\C) 12)
				((char=? num #\D) 13)
				((char=? num #\E) 14)
				((char=? num #\F) 15)

			)))

(define convert
 (lambda (lst len)
 	(letrec ((helper
 		(lambda (lst len counter)
 			(if (= len -1) counter 
 				(helper (cdr lst) 
 					(- len 1) 
 					(+ counter (* (expt 16 len) (car lst))))))))
 	(helper lst (- len 1) 0))))


(define <HexUnicodeChar>
	(new
		(*parser (char #\x))
		(*parser (char #\X))
		(*disj 2)
		(*parser <digit-0-9>)
		(*parser <a-f>)
		(*parser <A-F>)
		(*disj 3)
		*plus
		(*guard
			(lambda(h)
				(let((num (string->number (list->string `(#\# #\x ,@h))))
					(maxUnicode (string->number "#x10ffff")))
				(< num maxUnicode ))))
		
		(*caten 2)
		(*pack-with
			(lambda (a h)
				(integer->char (string->number (list->string `(#\# #\x ,@h))))))
				
		done
		)
	)

(define <Char>
	(new
		(*parser <CharPrefix>)
		(*parser <HexUnicodeChar>)
		(*parser <NamedChar>)
		(*parser <VisibleSimpleChar>)
		(*disj 3)
		(*caten 2)
		(*pack-with
			(lambda (a s) 
				s	))
	 			  
		done
		)
	)
(define <SpaceOrNot>
	(new
		(*parser <whitespace>)
		(*parser <epsilon>)
		(*disj 2)
		done))

(define <StringLiteralChar>
	(new 
		(*parser <any-char>)
		(*parser (char #\\))
		*diff
		done
		)
	)

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))

(define <stringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page))
       (*disj 6)
       done))

(define <StringHexChar>
	(new
		(*parser (char #\\))
		(*parser (char #\x))
		(*caten 2)
		(*parser <digit-0-9>)
		(*parser <a-f>)
		(*parser <A-F>)
		(*disj 3)
		*star
		(*parser (char #\;))
		(*caten 3)
		(*pack-with
			(lambda (a s b)
				 (integer->char (convert (map hex-chars-to-number s) (length s)))))
		done
		)
	)

(define <StringChar>
	(new
		(*parser <StringHexChar>)
		(*parser <stringMetaChar>)
		(*parser <StringLiteralChar>)
		(*disj 3)
		done
		)
	)

(define <String>
  (new (*parser (char #\"))
       (*parser <StringChar>)
       (*parser (char #\"))
       *diff
       *star
       (*parser (char #\"))
       (*caten 3)
       (*pack-with
			(lambda (open-delim chars close-delim)
	  			(list->string chars)))

       done
       )
  )

(define <SymbolChar>
	(new 
	(*parser <digit-0-9>)
	(*parser <a-z> )
	(*parser <A-Z>)
	(*parser (char #\!))
	(*parser (char #\$))
	(*parser (char #\^))
	(*parser (char #\*))
	(*parser (char #\-))
	(*parser (char #\_))
	(*parser (char #\=))
	(*parser (char #\+))
	(*parser (char #\<))
	(*parser (char #\>))
	(*parser (char #\?))
	(*parser (char #\/))
	(*disj 15)
	done
		))


(define <Symbol>
 	(new 
 		(*parser <SymbolChar>)
 		*plus
 		(*pack
 			(lambda (symbol)
 				(if (string->number (list->string symbol)) (string->number (list->string symbol))
 				(string->symbol (string-downcase (list->string symbol))))))
 		done))



(define <ImproperList>
	(new
		(*parser (word "("))
		(*delayed (lambda() <sexpr>))
		*plus
		(*parser (char #\.))
		(*delayed (lambda() <sexpr>))
		(*parser (word ")"))
		(*caten 5)
		(*pack-with
			(lambda (open-bracket sexpr-star dot sexpr-second close-bracket)
	  			(fold-right cons  sexpr-second sexpr-star)))
		done
		)
	)

(define <Unquoted>
	(new
		(*parser (word ","))
		(*delayed (lambda() <sexpr>))
		(*caten 2)
		(*pack-with
			(lambda (delim sexpr)
	  			    (list 'unquote sexpr)))
		done
		)
	)


(define <Quoted>
	(new
		(*parser (word "'"))
		(*delayed (lambda() <sexpr>))
		(*caten 2)
		(*pack-with
			(lambda (delim sexpr)
	  			    (list 'quote sexpr)))
		done
		)

	)

(define <QuasiQuoted>
	(new
		(*parser (word "`"))
		(*delayed (lambda() <sexpr>))
		(*caten 2)
		(*pack-with
			(lambda (delim sexpr)
	  			    (list 'quasiquote sexpr)))
		done
		)
	)

(define <UnquotedAndSpliced>
	(new
		(*parser (word ",@"))
		(*delayed (lambda() <sexpr>))
		(*caten 2)
		(*pack-with
			(lambda (delim sexpr)
	  			    (list 'unquote-splicing sexpr)))
		done
		)
	)


(define <ProperList>
	(new
		(*parser (word "("))
		(*delayed (lambda() <sexpr>))
		*star
		(*parser (word ")"))
		(*caten 3)
		(*pack-with 
			(lambda (open-brackets sexpr close-brackets)  sexpr))
		done

			))

(define <Vector>
	(new
		(*parser (char #\#))
		(*parser <ProperList>)
		(*caten 2)
		(*pack-with
			(lambda (hash sexpr ) (list->vector sexpr)))
		done
		))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))


(define <skip>
  (disj <comment>
	<whitespace>))

(define <Infix-comment>
	(new
		(*parser (word "#;"))
		(*delayed (lambda() <InfixExpression>))
		(*caten 2)
		done
		)
	)

(define <Infixskip>
	(new
		(*parser <whitespace>)
		(*parser <line-comment>)
		(*parser <Infix-comment>)
		(*disj 3)
		done
		)
	)
  


(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))
(define ^<skipped-Infix*> (^^<wrapped> (star <Infixskip>)))




(define <PowerSymbol>
	(new 
		(*parser (char #\^))
		(*parser (word "**"))
		(*disj 2)
		(*pack (lambda (n) (string->symbol "expt") ))
		done)
	)

(define <InfixPrefixExtensionPrefix>
	(new 
		(*parser (word "##"))
		(*parser (word "#%"))
		(*disj 2)
		done)
	)

(define <InfixSexprEscape>
	(new
		(*parser <InfixPrefixExtensionPrefix>)
		(*delayed (lambda() <sexpr>))
		(*caten 2)
		(*pack-with (lambda (a b) b))
		done)
	)


(define <InfixExpression>
  (
    new
    (*delayed (lambda () <InfixAddSub>))
    done)
  )

(define <SymbolInfix>
	(new 
	(*parser <digit-0-9>)
	(*parser <a-z> )
	(*parser <A-Z>)
	(*parser (char #\!))
	(*parser (char #\$))
	(*parser (char #\_))
	(*parser (char #\=))
	(*parser (char #\<))
	(*parser (char #\>))
	(*parser (char #\?))
	(*disj 10)
	*plus
	(*pack
 			(lambda (symbol)
 				(if (string->number (list->string symbol)) (string->number (list->string symbol))
 				(string->symbol (string-downcase (list->string symbol))))))
	done
		))


(define <InfixSymbol>
		(new
		(*parser <SymbolInfix>)
		done
		)
	)

(define <neg>
	(new
		(*parser (word "-"))
		(*delayed (lambda() 
			(new
				(*parser <func_or_arr>)
				(*parser <InfixParenthis>) 
				(*disj 2)
				done)))
		(*caten 2)
		(*pack-with (lambda (first end) 
			(list '- end)))
		done
		))

(define <InfixParenthis>
	(^<skipped-Infix*>
	(new
		
		(*parser (char #\( ))
		(*parser <InfixExpression>)
		(*parser (char #\) ))
		(*caten 3)
		(*pack-with (lambda (a b c)b))
		(*parser <Fraction>)
		(*parser <InfixSymbol>)
		(*parser <Number>)
		(*parser <InfixSexprEscape>)
				(*parser <neg>)
		(*disj 6)
		done
		)
	))

(define <InfixArrayGet>
	(new
		(*parser (^<skipped-Infix*> (word "[" )))
		(*parser <InfixExpression>)
		(*parser (^<skipped-Infix*> (word "]" )))
		(*caten 3)
		(*pack-with (lambda ( open-brackets e2 close-brackets)
			(lambda(x) (list 'vector-ref x e2))))
		done))


(define <InfixArgList>
	(new
		(*parser <InfixExpression>)
		(*parser (^<skipped*> (word "," )))
		(*parser <InfixExpression>)
		(*caten 2)
		(*pack-with (lambda (a b ) b))
		*star
		(*caten 2)
		(*pack-with (lambda (exp1 exp2) (append (list exp1) exp2)))
		(*parser <epsilon>)
		(*disj 2)
		done

		))

(define <InfixFuncall>
	(new
		(*parser (^<skipped-Infix*> (word "(" )))
		(*parser <InfixArgList>)
		(*parser (^<skipped-Infix*> (word ")" )))
		(*caten 3)
		(*pack-with (lambda (a b c ) (lambda(x) (append (list x) b))))
		done
		)
	)



(define <func_or_arr>
	(^<skipped-Infix*> (new

		(*parser <InfixParenthis>)
		(*parser <InfixFuncall>)
		(*parser <InfixArrayGet>)
		(*disj 2)
		*plus
		(*caten 2)
		(*pack-with (lambda (x y)
							(fold-left (lambda (a b) 
												(b a))
										x
										y)))
		(*parser <InfixParenthis>)
		(*disj 2)
		done
	)
		))

(define PowFold
	(lambda (first lst)
		(if (> (length lst) 0)  (list (string->symbol "expt") first (PowFold (cadar lst) (cdr lst) ))  first
			)))

(define <InfixPow>
	(new
		(*parser <func_or_arr>)
		(*parser <PowerSymbol>)
		(*parser <func_or_arr>)
		(*caten 2)
		*star
		(*caten 2)
		(*pack-with 
			(lambda (first end) 
				 (PowFold first end)))
		
		
	done))


(define <InfixMulDiv>
	(new
		(*parser <InfixPow>)
		(*parser (word "*"))
		(*parser (word "/"))
		(*disj 2)
		(*pack (lambda (n) (string->symbol (list->string n))))
		(*parser <InfixPow>)
		(*caten 2)
		*star
		(*caten 2)
		(*pack-with (lambda (first end) (fold-left (lambda (x y) 
				(list (car y)   x (cadr y)  )) first end)))
		done
		)
	)


(define <InfixAddSub>
	(new
		(*parser <InfixMulDiv>)
		(*parser (word "+"))
		(*parser (word "-"))
		(*disj 2)
		(*pack (lambda (n) (string->symbol (list->string n))))

		(*parser <InfixMulDiv>)
		(*caten 2)
		*star
		(*caten 2)
		(*pack-with (lambda (first end) (fold-left (lambda (x y) 
				(list (car y)   x (cadr y)  )) first end)))
		done
		)
	)

(define <InfixExtention>
		(new 
		(*parser <InfixPrefixExtensionPrefix>)
		(*parser <InfixExpression>)
		(*caten 2)
		(*pack-with (lambda (a b )b ))
		done))


(define <sexpr1>
	(^<skipped*>
		(new
		(*parser <boolean>) 
		(*parser <Char>)
		(*parser <Symbol>)
		(*parser <Number>)
		(*parser <String>)
		(*parser <ImproperList>)
		(*parser <ProperList>)
		(*parser <Vector>)
		(*parser <Quoted>)
		(*parser <QuasiQuoted>)
		(*parser <Unquoted>)
		(*parser <UnquotedAndSpliced>)
		(*parser <InfixExtention>)
		(*disj 13)
		done
		)
	))



(define with (lambda (s f) (apply f s)))

(define (has-duplicates? lst) 
  (cond
     ((null? lst) #f)
     ((not (not (member (car lst) (cdr lst)))) #t)
     (else (has-duplicates? (cdr lst)) )))

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define const-lambda
	(lambda (x) `(const ,x)))

(define quoted? 
	(lambda (x)
	(and (pair? x) (symbol?  (car x))  (equal? (car x) 'quote))))

(define quote-const-lambda
	(lambda (x)
		`(const ,@(cdr x))))

(define void?
	(lambda (x)
		(equal? (void) x)))



(define const-pattern
			(compose-patterns
				(pattern-rule (? 'NIL null?) 
				const-lambda)
				(pattern-rule (? 'Vector vector?) const-lambda)
				(pattern-rule (? 'Boolean boolean?) const-lambda)
				(pattern-rule (? 'Char char?) const-lambda)
				(pattern-rule (? 'Number number?) const-lambda)
				(pattern-rule (? 'String string?) const-lambda)
				(pattern-rule (? 'Quote quoted? ) quote-const-lambda) 
				(pattern-rule (? 'Void void? ) const-lambda) 
				))



(define not-empty? 
	(lambda (x) (not (null? x))))

(define var?
	(lambda (x)
		(and (symbol? x) (not (member x *reserved-words*)))))

(define var-pattern
			(pattern-rule (? 'var: var? )
			(lambda (x) `(var ,x))))

(define if-symbol?
	(lambda (x)
		(equal? 'if x)))

(define if-pattern
	(compose-patterns
		(pattern-rule `(,(? 'if if-symbol?) ,(? 'test ) ,(? 'dit ) ,(? 'dif))
			(lambda (if-symbol test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif)) ))
		(pattern-rule `(,(? 'if if-symbol?) ,(? 'test ) ,(? 'dit ))
			(lambda (if-symbol test dit )  `(if3 ,(parse test) ,(parse dit) ,(parse (void))) )))
	)


(define or?
	(lambda (x) (equal? 'or x)))

(define or-pattern
	(compose-patterns
		(pattern-rule `(,(? 'or or?)) (lambda (x) `(const #f)) )
		(pattern-rule `(,(? 'or or?) ,(? 'exp)) (lambda (or-symbol exp) (parse exp)))
		(pattern-rule `(, (? 'or or?) . ,(? 'b) ) (lambda (or-exp exps) `(or ,(map parse  exps))))
	))




(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond ((null? argl) (ret-simple '()))
			  ((symbol? argl) (ret-var argl))
			  (else
			  	(identify-lambda
			  		(cdr argl)
			  		(lambda (s) (ret-simple `(,(car argl) ,@s)))
			  		(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
			  		(lambda (var) (ret-opt `(,(car argl)) var))))
				)))

(define lambda?
	(lambda (x)
		(equal? 'lambda x)))

(define last-in-improper-list
	(lambda (x)
		(if (not (pair? x)) x (last-in-improper-list (cdr x)))))

(define list-in-improper-list
	(lambda (x)
		(if (not (pair? x)) '() (cons (car x) (list-in-improper-list (cdr x))))))

(define arglist?
	(lambda (x) (cond ((list? x) (not (has-duplicates? x)))
					  ((pair? x) (and (not (has-duplicates? (list-in-improper-list x))) (not (member (last-in-improper-list x) (list-in-improper-list x)))))
					  (else (symbol? x)))))

(define same-tag?
	(lambda (symbol)
		(lambda (x)
			(equal? symbol x))))

(define begin? (same-tag? 'begin))

(define begin-help
	(lambda (x)
		(cond 	((null? x ) x)
				((and (list? (car x)) (begin?(caar x))) (append (begin-help (cdar x)) (begin-help (cdr x))))
			  (else (cons (car x) (begin-help (cdr x))))
		)))

(define begin-pattern
	(pattern-rule `(,(? 'begin (same-tag? 'begin)) . ,(? 'rest ))   
		(lambda (a b)
			(cond ((null? b) (parse (void)))
				((= (length b) 1) (parse (car b)))
			(else `(seq ,(map parse (begin-help b))))))
	))

(define conds?
	(lambda (x)
		(and (list? x) (not (null? x)) (andmap (lambda(y) (> (length y )1)) x))))

(define split-body
	(lambda (body) 
		(letrec ((helper
					(lambda (x list1)
						(if (and (list? (car x)) (equal? (caar x) 'define)) (helper (cdr x) (cons (car x) list1) )
								 (list list1 x)))))
	 (helper body '()))))

(define defmit->def
	(lambda (exps)
		(if (and (equal? (car exps) 'define) (and (pair? (cadr exps)) (var? (car(cadr exps)))) (not(null? (caddr exps)))) 
			`(define ,(car (cadr exps)) (lambda ,(cdr (cadr exps)) ,(caddr exps)))
			exps
		)))

(define eliminate-nested
	(lambda (seq)
		(let ((splitted (append (list(map defmit->def (car (split-body (begin-help(cdr seq)))))) (cdr (split-body (begin-help(cdr seq)))))))
			 (if (null? (car splitted)) seq
				  `(letrec ,(reverse (map list (map cadr (car splitted)) (map caddr (car splitted)))) ,@(cadr splitted))))))

(define eliminate-nested-defines (lambda (x) x))

(define lambda-pattern
	(pattern-rule `(,(? 'lambda lambda?) ,(? 'args arglist?) ,(? 'a ) . ,(? 'b))
			(lambda (lambda-symbol arglist first-exp rest-exp)
			(identify-lambda arglist 
			(lambda (x) `(lambda-simple ,x ,(parse (eliminate-nested `(begin ,first-exp ,@rest-exp)))))
			(lambda (x opt) `(lambda-opt ,x ,opt ,(parse (eliminate-nested `(begin ,first-exp ,@rest-exp)))))
			(lambda (var) `(lambda-var ,var ,(parse (eliminate-nested `(begin ,first-exp ,@rest-exp)))))))
			)
)

(define define?
	(lambda (x)
		(equal? 'define x)))

(define mit-define-vars?
	(lambda (x) (and (pair? x) (var? (car x)))))

(define define-pattern 
	(compose-patterns
		(pattern-rule `(,(? 'define define?) ,(? 'var var?) ,(? 'exp))
			(lambda (def-symbol var exp) `(def ,(parse var) ,(parse exp))) )
		(pattern-rule `(,(? 'define define?) ,(? 'var var?) . ,(? 'exps))
			(lambda (def-symbol var exps) `(def ,(parse var) ,(parse `(begin ,@exps)))))
		(pattern-rule `(,(? 'define define?) ,(? 'vars mit-define-vars? ) . ,(? 'exp ))
			(lambda (def-symbol vars exps) `(def ,(parse (car vars)) ,(parse `(lambda ,(cdr vars) ,@exps)) )))

		))

(define applic?
	(lambda (x) (and (not (member (car x) *reserved-words*)) (list? x) (not (null? x)))))

(define applic-helper
	(lambda (x)
		(if (= (length x) 1)
			`(applic ,(parse (car x)) ())
			`(applic ,(parse (car x)) ,(map parse (cdr x)))
		)))

(define applic-pattern
	(pattern-rule (? 'app applic?) applic-helper ))

(define let?
	(lambda (x) (equal? 'let x)))

(define letrec?
	(lambda (x) (equal? 'letrec x)))

(define let*?
	(lambda (x) (equal? 'let* x)))

(define let-vars?
	(lambda (x)
	(or (null? x) 
		(and (list? x) (andmap list? x) (andmap (lambda(y) (= (length y )2)) x)
		 (andmap var? (map car x)) (not (has-duplicates? (map car x)))))))

(define let-helper
	(lambda (let-symbol vars exprs)
		(if (null? vars)
			`((lambda () ,@exprs))
			`((lambda ,(map car vars) ,@exprs) ,@(map cadr vars))
		)))


(define generate-list
	(lambda (x symbol)
		(if (= x 0) '() (cons symbol (generate-list (- x 1) symbol)))))

(define letrec-helper
	(lambda (let-symbol vars exprs)
		(if (null? vars)
			`((lambda () (let() ,@exprs)))
			(let ((var-list (map car vars) )
				  (val-list (map cadr vars))
				  (len (length vars)))
			`(let ,(map list var-list (generate-list len #f)) ,@(map list (generate-list len 'set!) var-list val-list)  (let (),@exprs)) 
		))))

(define let*-vars?
	(lambda (x)
	(or (null? x) 
		(and (list? x) (andmap list? x) (andmap (lambda(y) (= (length y )2)) x)
		 (andmap var? (map car x))))))

(define let*-helper
	(lambda (let-symbol vars exprs)
		(if (null? vars)
			`(let () ,@exprs)
			(letrec 
				((helper (lambda (vars)
						(if (null? vars) `(begin ,@exprs) `(let (,(car vars)) ,(helper (cdr vars)))))))
			(helper vars)
			 
		))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define tagged?
  (lambda (e tag)
    (and (pair? e) (equal? tag (car e)))))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))
(define pe-const?
        (lambda (pe)
                (tagged? pe 'const)))
(define pe-fvar?
	(lambda (pe) (tagged? pe 'fvar)))


(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))


(define and-helper
	(lambda (and-symbol exps)
			(letrec((helper 
				(lambda (exprs)
					(cond ((null? exprs) #t)
						   ((= (length exprs) 1 ) (car exprs))
							(else `(if ,(car exprs) ,(helper (cdr exprs) )#f))))))
			(helper exps)
		)))

(define cond-helper
	(lambda (cond-symbol conds)
		(letrec ((helper
				(lambda (exprs)
					(cond ((null? exprs) #f)
						((equal? 'else (caar exprs)) `(begin ,@(cdar exprs)))
						((= (length exprs) 1) `(if ,(caar exprs) (begin ,@(cdar exprs)) ,(void)))
						(else `(if ,(caar exprs) (begin ,@(cdar exprs)) ,(helper (cdr exprs))))
						))))
	(helper conds))))

(define macro-expansion
	(compose-patterns
		(pattern-rule `(,(? 'let let?) ,(? 'vars let-vars?) . ,(? 'expr not-empty?)) (lambda (a b c) (parse (let-helper a b c))))
		(pattern-rule `(,(? 'letrec letrec?) ,(? 'vars let-vars?) . ,(? 'expr not-empty?)) (lambda (a b c) (parse (letrec-helper a b c))))
		(pattern-rule `(,(? 'let* let*?) ,(? 'vars let*-vars?) . ,(? 'expr not-empty?)) (lambda (a b c) (parse (let*-helper a b c))))
		(pattern-rule `(,(? 'and (same-tag? 'and) ) . ,(? 'rest )) (lambda (a b) (parse (and-helper a b ))))
		(pattern-rule `(,(? 'cond (same-tag? 'cond)) . ,(? 'conds conds?)) (lambda (a b) (parse (cond-helper a b))))
		(pattern-rule `(,(? 'quasiquote (same-tag? 'quasiquote)) ,(? 'list )) (lambda (a b) (parse (expand-qq b))))
		
	))

(define set!-pattern
	(pattern-rule `(,(? 'set! (same-tag? 'set!)) ,(? 'var var?) ,(? 'exp))
		(lambda (set!-symbol var exp) `(set ,(parse var) ,(parse exp))))
)

(define any-lambda?
	(lambda (x) (or (equal? x 'lambda-simple) (equal? x 'lambda-opt) (equal? x 'lambda-var))))

(define minor 
	(lambda (var l x)
		(if (equal? var (car l)) x (minor var (cdr l) (+ x 1)))))

(define list-without-last
	(lambda (l)
		(if (null? l) l (reverse(cdr (reverse l))))))

(define list-last
	(lambda (l)
		(if (null? l) l (car (reverse l)))))

(define fbp-var
	(lambda (var env arglist)
			(if (member var arglist) `(pvar ,var ,(minor var arglist 0))
				  (letrec ((helper 
				  			(lambda (var env counter)
				  				(cond ((null? env) `(fvar ,var))
				  					  ((member var (car env)) `(bvar ,var ,counter ,(minor var (car env) 0)))
				  					  (else (helper var (cdr env) (+ counter 1)))))))
				(helper var env 0))

			)))

(define pe->lex-pe
	(lambda (ast)
		(letrec ((helper (lambda (x prev arglist)
									
									(cond
										((or (null? x) (not (list? x))) x)
										((equal? 'var (car x)) (fbp-var (cadr x) prev arglist))
										((equal? 'lambda-simple (car x)) (map (lambda (y) (helper y (cons arglist prev) (cadr x))) x))
										((equal? 'lambda-var (car x)) (map (lambda (y) (helper y (cons arglist prev) (list (cadr x)))) x))
										((equal? 'lambda-opt (car x)) (map (lambda (y) (helper y (cons arglist prev) (append (cadr x) (list (caddr x))))) x))
										(else (map (lambda (y) (helper y prev arglist)) x)))
								)))

			(helper ast '() '())
		)))

(define seq? (same-tag? 'seq))

(define seq-help
	(lambda (x)
		(cond 	((null? x ) x)
				((and (list? (car x)) (seq?(caar x))) (append (seq-help (cadar x)) (seq-help (cdr x))))
			  (else (cons (car x) (seq-help (cdr x))))
		)))

(define remove-nested-seq
	(lambda (ast)
		(letrec ((helper 
						(lambda (x)
							(cond 
							((or (null? x) (not (list? x))) x)
							((equal? 'seq (car x) ) `(seq ,(helper (seq-help (cadr x))))) 
						(else (map helper x))))))
		(helper ast))))

(define remove-applic-lambda-nil
	(lambda (ast)	
		(letrec ((helper (lambda (x)
			(cond 
				((or (null? x) (not (list? x))) x)
				((and (equal? (car x) 'applic) (equal? (caadr x) 'lambda-simple) (null? (cadadr x)) (null? (caddr x))) (helper (car (cddadr x))))  
				(else (map helper x))))))
		    (remove-nested-seq (helper ast)))))

(define has-bvar-occ?
	(lambda (var body)
		(letrec ((helper 
					(lambda (x expr counter)
						(cond 
							((or (null? expr) (not (list? expr))) #f)
							((and (equal? (car expr) 'bvar) (equal? (cadr expr) x ) (= counter (caddr expr))) #t)
							((any-lambda? (car expr)) (ormap (lambda (y) (helper x y (+ counter 1))) expr))
							(else (ormap (lambda (y) (helper x y counter )) expr))))))
					(helper var body -1 )
					)))

(define check-var-with-counter
	(lambda (var var-expr counter)
		(cond 
			((not (equal? var (cadr var-expr))) #f)
			((and (equal? (car var-expr) 'pvar) (= counter -1)) #t)
			((and (equal? (car var-expr) 'bvar) (= counter (caddr var-expr))) #t)
			(else #f))))

 (define has-write?
 	(lambda (var body)
 		(letrec ((helper 
					(lambda (x expr counter)
						(cond 
							((or (null? expr) (not (list? expr))) #f)
							((and (equal? (car expr) 'set) (check-var-with-counter x (cadr expr) counter)) #t)
							((any-lambda? (car expr)) (ormap (lambda (y) (helper x y (+ counter 1))) expr))
							(else (ormap (lambda (y) (helper x y counter)) expr))))))
					(helper var body -1)
					)))
  (define has-read?
 	(lambda (var body)
 		(letrec ((helper 
					(lambda (x expr counter)
						(cond 
							((or (null? expr) (not (list? expr))) #f)
							((equal? (car expr) 'set) (helper x (caddr expr) counter))
							((and (or (equal? (car expr) 'bvar) (equal? (car expr) 'pvar)) (check-var-with-counter x expr counter)) #t)
							((any-lambda? (car expr)) (ormap (lambda (y) (helper x y (+ counter 1))) expr))
							(else (ormap (lambda (y) (helper x y counter)) expr))))))
					(helper var body -1)
					)))

  (define change-lambda
 	(lambda (var body)
 		(letrec ((helper 
					(lambda (x expr counter)
						(cond 
							((or (null? expr) (not (list? expr))) expr)
							((and (equal? (car expr) 'set) (check-var-with-counter x (cadr expr) counter)) `(box-set ,(cadr expr) ,(helper x (caddr expr) counter)))
							((and (or (equal? (car expr) 'bvar) (equal? (car expr) 'pvar)) (check-var-with-counter x expr counter)) `(box-get ,expr))
							((any-lambda? (car expr)) (map (lambda (y) (helper x y (+ counter 1))) expr))
							(else (map (lambda (y) (helper x y counter)) expr))))))
					(helper var body -1)
					)))

(define need-box?
	(lambda (var body)
			(and (has-bvar-occ? var body) (has-write? var body) (has-read? var body))))

(define check-seq
	(lambda (body)
		(if (null? body) 
			body
			(if (equal? 'seq (car body)) (cadr body) body))))

(define fix-body
	(lambda (body need-boxing)
				(letrec ((helper 
							(lambda (x body)
								(if (null? x) body
									(helper (cdr x) 
									(change-lambda (car x) body))
								))))
				(helper need-boxing body))
	))


(define box-lambda
	(lambda (lambda-ast arglist body)
		(let ((need-boxing (filter (lambda (x) (need-box? x body)) arglist))
			  (check (equal? 'seq (car body)))
			  (hhh (check-seq body)))
			(cond 
			((null? need-boxing) lambda-ast)
			((or (equal? 'lambda-simple (car lambda-ast))  (equal? 'lambda-var (car lambda-ast))) 
				(if (not check)
					`(,(car lambda-ast) ,(cadr lambda-ast) (seq ,`(,@(map (lambda (x) `(set (var ,x) (box (var ,x)))) need-boxing) ,(fix-body hhh need-boxing))))
					`(,(car lambda-ast) ,(cadr lambda-ast) (seq ,`(,@(map (lambda (x) `(set (var ,x) (box (var ,x)))) need-boxing) ,@(fix-body hhh need-boxing))))
				))
			(else (if (not check)
					`(,(car lambda-ast) ,(cadr lambda-ast) ,(caddr lambda-ast) (seq ,`(,@(map (lambda (x) `(set (var ,x) (box (var ,x)))) need-boxing) ,(fix-body hhh need-boxing))))
					`(,(car lambda-ast) ,(cadr lambda-ast) ,(caddr lambda-ast) (seq ,`(,@(map (lambda (x) `(set (var ,x) (box (var ,x)))) need-boxing) ,@(fix-body hhh need-boxing)))))
				))
		)))



(define change-vars
	(lambda (ast)
		(letrec ((helper 
						(lambda (x)
							(cond 
							((or (null? x) (not (list? x))) x)
							((or (equal? 'bvar (car x))(equal? 'pvar (car x))(equal? 'fvar (car x))) `(var ,(cadr x)))
						(else (map helper x))))))
	(helper ast))))

(define box-set
	(lambda (ast)
			(letrec ((helper 
						(lambda (x)
							(cond 
							((or (null? x) (not (list? x))) x)
							((equal? 'lambda-simple (car x)) (map helper (box-lambda x (cadr x) (caddr x))))
							((equal? 'lambda-var (car x)) (map helper (box-lambda x (list (cadr x)) (caddr x))))
							((equal? 'lambda-opt (car x))  (map helper (box-lambda x (append (cadr x) (list (caddr x))) (car (cdddr x)))))
						(else (map helper x))))))
		(change-vars (helper (pe->lex-pe ast))))
		))

(define annotate
	(lambda (expr tp?)
		(cond 
		((or (equal? (car expr) 'const) (equal? (car expr) 'pvar) (equal? (car expr) 'bvar) (equal? (car expr) 'fvar) (equal? (car expr) 'var)) expr)
		((equal? 'applic (car expr)) (if tp? 
										`(tc-applic ,(annotate (cadr expr) #f) ,(map (lambda (x) (annotate x #f)) (caddr expr))) 
										`(applic ,(annotate (cadr expr) #f) ,(map (lambda (x) (annotate x #f)) (caddr expr)))))
		((equal? 'or (car expr)) `(or  ,`( ,@(map (lambda (x) (annotate x #f)) (list-without-last (cadr expr))) ,(annotate (list-last (cadr expr )) tp?))))
		((equal? 'if3 (car expr)) `(if3 ,(annotate (cadr expr) #f) ,(annotate (caddr expr) tp?) ,(annotate (cadddr expr) tp?)))
		((equal? 'def (car expr)) `(def ,(cadr expr) ,(annotate (caddr expr) #f)))
		((or (equal? 'lambda-simple (car expr)) (equal? 'lambda-var (car expr))) `(,(car expr) ,(cadr expr) ,(annotate (caddr expr) #t)))
		((equal? 'lambda-opt (car expr)) `(,@(list-without-last expr) ,(annotate (list-last expr) #t)))
		((equal? 'seq (car expr)) `(seq ,`(,@(map (lambda (x) (annotate x #f)) (list-without-last (cadr expr))) ,(annotate (list-last (cadr expr)) tp?))))
		(else `(,(car expr) ,@(map (lambda (x) (annotate x #f)) (cdr expr))))
	)))

(define annotate-tc
	(lambda (ast)
		(annotate ast #f)))

(define parse
	(letrec ((parser
		(compose-patterns
			const-pattern var-pattern set!-pattern if-pattern  or-pattern lambda-pattern define-pattern begin-pattern applic-pattern macro-expansion 
		 )
	))
	(lambda (sexpr)
		 (parser sexpr (lambda () "ERROR")))))

(define get-pe (lambda (x) 
		    (annotate-tc
		      (pe->lex-pe
			(box-set
			  (remove-applic-lambda-nil
			    (eliminate-nested-defines (parse x))))))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;					;;;;;;;;;;;
;;;;;;;;;;;;;	CODE GENERATION ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define label-gen
	(lambda (prefix)
		(lambda ()
			(let ((n 0))
				(lambda ()
					(set! n (+ n 1))
					 (string-append prefix (number->string n) ))))))

(define if-label ((label-gen "L_IF3_")))
;(define if-true-label (label-gen "L_IF3_TRUE_"))
(define if-else-label ((label-gen "L_IF3_ELSE_")))
(define if-exit-label ((label-gen "L_IF3_EXIT_")))

(define or-label ((label-gen "L_OR_")))
(define or-exit-label ((label-gen "L_OR_EXIT_")))
(define box-get-label ((label-gen "L_BOX_GET_")))
(define box-label ((label-gen "L_BOX_")))
(define box-set-label ((label-gen "L_BOX_SET_")))
(define set-label ((label-gen "L_SET_")))
(define lambda-simple-label ((label-gen "L_LAMBDA_SIMPLE_")))
(define closure-body-label ((label-gen "L_CLOSURE_BODY_")))
(define closure-exit-label ((label-gen "L_CLOSURE_EXIT_")))
(define malloc-label ((label-gen "L_MALLOC_")))
(define applic-label ((label-gen "L_APPLIC_")))
(define lambda-opt-label ((label-gen "L_LAMBDA_OPT_")))
(define fix-stack-label ((label-gen "L_FIX_STACK_")))
(define fix-stack-exit-label ((label-gen "L_FIX_STACK_EXIT_")))
(define tc-label ((label-gen "L_TC_")))
(define tc-clear-frame-label ((label-gen "L_TC_CLEAR_FRAME_")))
(define tc-clear-frame-exit-label ((label-gen "L_TC_CLEAR_FRAME_EXIT_")))
(define opt-args-loop ((label-gen "L_OPT_LOOP_")))
(define opt-args-loop-exit ((label-gen "L_OPT_LOOP_EXIT_")))
(define env-loop ((label-gen "L_ENV_LOOP_")))
(define env-loop-exit ((label-gen "L_ENV_LOOP_EXIT_")))
(define null-label ((label-gen "OPT_NULL_")))
(define finish-sexpr-label ((label-gen "FINISH_EXPR_")))




(define cmp-to-false "CMP(INDD(R0,1),0);\n")

(define gen-if3
	(lambda (expr maj)
		(let ((else_label (if-else-label))
			  (exit_label (if-exit-label)))
		(string-append
			(if-label) ":\n" (code-gen (cadr expr) maj) "\n" cmp-to-false  "JUMP_EQ(" else_label ");\n" 
		(code-gen (caddr expr) maj) "\n" "JUMP(" exit_label ");\n" else_label ":\n" (code-gen (cadddr expr) maj) "\n" exit_label ":\n" ))))

(define gen-seq
	(lambda (expr maj)
		
			(cond ((= (length expr) 1) (code-gen (car expr) maj))
				  (else (string-append (code-gen (car expr) maj) (gen-seq (cdr expr) maj))))))


(define gen-or
	(lambda (expr maj)
		(let ((exitl (or-exit-label)))
			 (letrec ((helper 
			 			(lambda (exp)

			 				(if (null? exp) ""
			 				(string-append (code-gen (car exp) maj) cmp-to-false "JUMP_NE(" exitl ");\n" (helper (cdr exp)) )))))
			(string-append (or-label) ":\n" (helper expr) exitl ":\n")))))

(define gen-set
	(lambda (var expr maj)

		(let ((vtype (car var))
			  (result (code-gen expr maj))
			  (number (cond ((equal? (car var) 'pvar) (number->string (+ 2 (caddr var)))) 
			  				((equal? (car var) 'bvar) (number->string (caddr var)))
			  				(else 0))))

		(string-append (set-label) ":\n" result 
			(cond ((equal? vtype 'pvar) (string-append "MOV(FPARG(" number  "),R0);\n" "MOV(R0,IMM(SOB_VOID));\n" ) )
				  ((equal? vtype 'bvar) (string-append "MOV(R1,R0);\n" "MOV(R0,FPARG(0));\n" "MOV(R0,INDD(R0," number  "));\n" 
				  		"MOV(INDD(R0," (number->string (cadddr var)) "),R1);\n" "MOV(R0,IMM(SOB_VOID));\n"))
				(else (string-append "\n" (code-gen expr maj) 
									 "\n MOV(IND(" (number->string (lookup-in-fvar-table (cadr var) fvar-table)) "),R0);
										 MOV(R0,IMM(2));\n" ))
		)))))

(define gen-box-set
	(lambda (var expr maj)
		(let ((vtype (car var))
			  (number (cond ((equal? (car var) 'pvar) (number->string (+ 2 (caddr var)))) 
			  				((equal? (car var) 'bvar) (number->string (caddr var)))
			  				(else 0))))
		(string-append (box-set-label) ":\n" 
			(code-gen expr maj)
			(cond ((equal? vtype 'pvar) (string-append "MOV(R1,FPARG(" number "));\n" "MOV(IND(R1),R0);\n")) 
								((equal? vtype 'bvar) (string-append "MOV(R1,FPARG(0));\n" "MOV(R1,INDD(R1," number  "));\n" "MOV(R1,INDD(R1,"
								 (number->string (cadddr var)) "));\n" "MOV(IND(R1),R0);\n" ))
								(else (string-append 
										"MOV(R1,IND(" (number->string (lookup-in-fvar-table (cadr var) fvar-table)) "));\n" )
										"\n MOV(IND(R1),R0);"))
			"
			MOV(R0,IMM(2)); //VOID \n"))))




(define gen-box
	(lambda (var maj)
		(let ((vtype (car var))
			  (number (cond ((equal? (car var) 'pvar) (number->string (+ 2 (caddr var)))) 
			  				((equal? (car var) 'bvar) (number->string (caddr var)))
			  				(else 0))))
		(string-append (box-label) ":\n" 
			"PUSH(IMM(1));
			 CALL(MALLOC);
			 DROP(1);"
			(cond ((equal? vtype 'pvar) (string-append "MOV(R1,FPARG(" number "));\n" "MOV(IND(R0),R1);\n")) 
								((equal? vtype 'bvar) (string-append "MOV(R1,FPARG(0));\n" "MOV(R1,INDD(R1," number  "));\n" "MOV(R1,INDD(R1,"
								 (number->string (cadddr var)) "));\n" "MOV(IND(R0),R1);\n" ))
								(else (string-append 
										"MOV(R1,IND(" (number->string (lookup-in-fvar-table (cadr var) fvar-table)) "));
										MOV(IND(R0),R1);\n")))))))

(define gen-box-get
	(lambda (var maj)
		(let ((vtype (car var))
			  (number (cond ((equal? (car var) 'pvar) (number->string (+ 2 (caddr var)))) 
			  				((equal? (car var) 'bvar) (number->string (caddr var)))
			  				(else 0))))
		(string-append (box-get-label) ":\n" (cond ((equal? vtype 'pvar) (string-append "MOV(R0,FPARG(" number  "));\n" "MOV(R0,IND(R0));\n")) 
								((equal? vtype 'bvar) 
									(string-append  "MOV(R0,FPARG(0));\n" "MOV(R0,INDD(R0," number  "));\n" "MOV(R0,INDD(R0,"
								 (number->string (cadddr var)) "));\n" "MOV(R0,IND(R0));\n" ))
								(else (string-append 
										(code-gen var maj)
										"\n MOV(R0,IND(R0));")))))))

(define create-extended-env
	(lambda (maj reg1 reg2 i)
		
		(if (= i maj) "" (string-append "MOV(INDD(" reg1 "," (number->string (+ i 1)) ")," "INDD(" reg2 "," (number->string i) "));\n"
						 (create-extended-env maj reg1 reg2 (+ i 1))))
			))

(define gen-lambda-simple
	(lambda (args body maj)
		(let ((bodylabel (closure-body-label))
			  (exitlabel (closure-exit-label))
			   (envloop (env-loop))
				(envloopexit (env-loop-exit))
				(nulllabel (null-label)))

			(string-append "\n" (lambda-simple-label) ":\n" 
			
			"		
			MOV(R1,FPARG(0)); // R1= ENV \n" (malloc-label) ":\n" 
			 "MOV(R4," (number->string (+ maj 1)) ") ; /* R4= maj + 1*/ \n
			 			" "PUSH(R4);\n" "CALL(MALLOC);\n" "DROP(1);\n" 
						"MOV(R2,R0);  /*POINTER TO ENV */\n /* CREATE ENV */\n" (create-extended-env maj  "R2" "R1" 0) "
						/*ENV CREATION IS COMPLETED */
						//MOV(R3,"(number->string (length args))");\n 
						 MOV(R3,FPARG(1));


						 MOV(R4,FPARG(1));
						 ADD(R4,2);
						 MOV(R9,FPARG(R4));
						CMP(R9,SOB_NIL);
						JUMP_NE("nulllabel");
						INCR(R3);
						"nulllabel":

						 "   "PUSH(R3);\n" "CALL(MALLOC);\n"
						"DROP(1);\n" "MOV(INDD(R2,0),R0);\n"

						 "
						 \r\n
						 /*COPY ARGS TO ENV, r2[0] has new vector waiting for them */
						 MOV(R4,0); //i 
						MOV(R7,INDD(R2,0)); // pointer to vector
						"envloop":
						CMP(R4,R3);

						JUMP_EQ("envloopexit");

						MOV(R6,R4);
						ADD(R6,IMM(2));
						MOV(R5,R6);
						MOV(INDD(R7,R4),FPARG(R5));
						MOV(R5,FPARG(R5));
						INCR(R4);
						JUMP("envloop");
					"envloopexit":



						MOV(IND(R2),R7);
						 " "MOV(R8,3);\n " "PUSH(R8);\n " "CALL(MALLOC);\n" "DROP(1);\n" 
						"MOV(IND(R0),T_CLOSURE);\n" "MOV(INDD(R0,1),R2);\n " "MOV(INDD(R0,2),LABEL(" bodylabel "));\n" "JUMP(" exitlabel ");\n"
						;CODE A UNTIL HERE
						bodylabel ":\n" "PUSH(FP);\n " "MOV(FP,SP);\n"  "
						MOV(R1,FPARG(1));
						MOV(R2,"(number->string (length args))");
						CMP(R1,R2);\n 
						" 
					
					 
					"JUMP_NE(L_error_lambda_args_count);" (code-gen body (+ maj 1)) "POP(FP);\n RETURN;\n" exitlabel ":
					
				\n\n")
						 )))

(define gen-applic
	(lambda (proc params maj)
		(letrec ((appliclabel (applic-label))
				(helper 
					(lambda (arglist)
						
						 (if (null? arglist) "PUSH(T_NIL);\n" (string-append (helper (cdr arglist)) (code-gen (car arglist) maj) "PUSH(R0);\n")))))
		(string-append appliclabel ":\n /* PUSING ARGUMENTS\n */" (helper  params) "\n/* end of pushing arguments for "appliclabel"*/\n
				" "PUSH(" (number->string (length params)) "); /*number of arguments*/\n"
			(code-gen proc maj) " CMP(IND(R0), IMM(T_CLOSURE));\n" "JUMP_NE(L_error_cannot_apply_non_clos);\n"
			"PUSH(INDD(R0,1));\n " "CALLA(INDD(R0,2));\n" "DROP(1);\n" "POP(R1);\n" "DROP(R1);\n" "DROP(1);\n"))))

(define gen-lambda-opt
	(lambda (args opt body maj)
		(let ((bodylabel (closure-body-label))
			  (exitlabel (closure-exit-label))
			  (fixstack (fix-stack-label))
			  (exitfixstack (fix-stack-exit-label))
			  (optloop (opt-args-loop))
			  (optloopexit (opt-args-loop-exit))
			(envloop (env-loop))
				(envloopexit (env-loop-exit))
				(nulllabel (null-label)))
		(string-append (lambda-opt-label) ":

			MOV(R1,FPARG(0)); // R1= ENV \n" (malloc-label) ":\n" 
			 "MOV(R4," (number->string (+ maj 1)) ") ; /* R4= maj + 1*/ \n
			 			" "PUSH(R4);\n" "CALL(MALLOC);\n" "DROP(1);\n" 
						"MOV(R2,R0); /*POINTER TO ENV */\n /* CREATE ENV */\n" (create-extended-env maj  "R2" "R1" 0) "
						/*ENV CREATION IS COMPLETED */
						//MOV(R3,"(number->string (length args))");\n 
						 MOV(R3,FPARG(1)); 
						 MOV(R4,FPARG(1));
						 ADD(R4,2);
						 MOV(R9,FPARG(R4));
						CMP(R9,SOB_NIL);
						JUMP_NE("nulllabel");
						INCR(R3);
						"nulllabel":

						 "   "PUSH(R3);\n" "CALL(MALLOC);\n"
						"DROP(1);\n" "MOV(INDD(R2,0),R0);\n"

						 "
						 \r\n
						 /*COPY ARGS TO ENV, r2[0] has new vector waiting for them */
						 MOV(R4,0); //i 
						MOV(R7,INDD(R2,0)); // pointer to vector
						"envloop":
						CMP(R4,R3);

						JUMP_EQ("envloopexit");
						MOV(R6,R4);
						ADD(R6,IMM(2));
						MOV(R5,R6);
						MOV(INDD(R7,R4),FPARG(R5));
						MOV(R5,FPARG(R5));
						
						INCR(R4);
						JUMP("envloop");
					"envloopexit":
						MOV(IND(R2),R7);
						 " "MOV(R8,3);\n " "PUSH(R8);\n " "CALL(MALLOC);\n" "DROP(1);\n" 
						"MOV(IND(R0),T_CLOSURE);\n" "MOV(INDD(R0,1),R2);\n" "MOV(INDD(R0,2),LABEL(" bodylabel "));\n" "JUMP(" exitlabel ");\n"
						;CODE A UNTIL HERE
						bodylabel ": 
						 PUSH(FP);\n " "MOV(FP,SP);\n 
						MOV(R1,SOB_NIL);
						MOV(R2,FPARG(1)); /*R2=NUMBER OF ARGS */
												MOV(R4,R2); /* R4= NUMBER OF ARGS */ 
						MOV(R5,R4);/* NUMBER OF ARGS */
						MOV(R3," (number->string (length args)) "); /*R3= NUMBER OF MUST VALUES*/
						SUB(R2,R3); /* R2 = NUMBER OF OPTIONAL VALUES */
						CMP(R2,0);
					
						JUMP_LT(L_OPT_NUMBER_OF_ARGS_ERROR);
						INCR(R4);
						INCR(R3);
						MOV(R0,SOB_NIL);

						"
						optloop ": \n"
						"

						CMP(R4,R3); 
						JUMP_EQ("optloopexit");
						PUSH(R1);
						PUSH(FPARG(R4));
						//PUSH(R1);
						PUSH(IMM(2));
						PUSH(666);

						CALL(CONS);
						DROP(4);
						MOV(R1,R0);
						DECR(R4);
						JUMP("optloop");\n"
					optloopexit":\n
					
						
						//PUSH(R0);
						//CALL(WRITE_SOB);
						//DROP(1);


						MOV(R3," (number->string (length args)) "); /*R3= NUMBER OF MUST VALUES*/
						MOV(R5,FP);
						
						SUB(R5,R3);
						SUB(R5,IMM(5));

						MOV(STACK(R5),R1);
						ADD(R5,IMM(" (number->string (length args)) "));
						INCR(R5);
						
						
						MOV(R0,SP);
						ADD(R1,IMM(" (number->string (length args)) "));
						SUB(R0,R1);
						MOV(R2,FP);

						" 

						(code-gen body (+ maj 1))
						"

						POP(FP);
						RETURN;\n" 
						exitlabel ":\n\n"))))


(define gen-tc-applic

	(lambda (proc params maj)
		(letrec ((helper 
					(lambda (arglist)
						(if (null? arglist) "PUSH(T_NIL);\n"  (string-append (helper (cdr arglist)) (code-gen (car arglist) maj) "PUSH(R0);\n"))))
				(fixframe (tc-clear-frame-label))
				(exitfixframe (tc-clear-frame-exit-label))
				(tclabel (tc-label)))
		;(display (cadr proc))(newline)
		 (string-append tclabel ":\n" 
			
			(helper params) 

			"MOV(R7," (number->string (length params)) ");\n /* end pushing args for "tclabel"*/ 
			 PUSH(R7); /*R7=m*/
			
			 "
			(code-gen proc maj) "CMP(IND(R0), IMM(T_CLOSURE));\n" "JUMP_NE(L_error_cannot_apply_non_clos);\n"

			"

			MOV(R1,INDD(R0,1)); //environment of the proc.
			";printf(\""(if (list? (cadr proc)) (symbol->string (cadr (cadr proc))) (symbol->string (cadr proc)))"\\n\");
"
			PUSH(R1); 
			MOV(R1,FPARG(-1)); //Return Address 
			PUSH(R1);
			
			MOV(R3,FP);
			SUB(R3,FPARG(-2));
			MOV(R4,IMM(" (number->string (length params)) "));//NUMBER OF CURRENT ARGS
			ADD(R4,3);
			MOV(R5,R4);
			MOV(R1,IMM(0)); //i 
			MOV(R6,FP);
			INCR(R6);

			MOV(R10,FP);
			SUB(R10,FPARG(1));
			SUB(R10,4);
			MOV(R11,FPARG(-2)); //old fp
			MOV(FP,R10);
			"
			fixframe ":\n"
			"CMP(R1,R4);
			 JUMP_EQ(" exitfixframe ");
			 MOV(R7,FP);
			 ADD(R7,R1);
			 MOV(R8,R6);
			 ADD(R8,R1);
			 MOV(STACK(R7),STACK(R8));
			 INCR(R1);
			
			 JUMP(" fixframe ");\n"
			 exitfixframe ":\n"
			 "
			 MOV(SP,FP);
			 ADD(SP,R5);
			 MOV(FP,R11);
			  JUMPA(INDD(R0,2));
			"	))))

(define mr0 
        (lambda (val)
                (string-append "MOV(R0," val ");\n")))

(define code-gen-const
        (lambda (pe maj)

                (let* ((val (cadr pe))
                        (mem_add (string-append "IMM(" (number->string (lookup-in-constants-table val constants-table)) ")" ))
                        )
               (mr0 mem_add))))

(define code-gen-fvar
	(lambda (var maj)
		(string-append 
			"MOV(R0,IND(" (number->string (lookup-in-fvar-table var fvar-table)) "));\n" )

		))

(define code-gen-define
	(lambda (var expr maj)

		(string-append
			"/*define code for " (symbol->string (cadr var)) " */\n"
			(code-gen expr maj)

			"MOV(IND(" (number->string (lookup-in-fvar-table (cadr var) fvar-table)) "),R0);\n"
			"MOV(R0,IMM(2));\n" )))

(define code-gen-prep
	(lambda (expr maj)
		(let ((label (finish-sexpr-label)))
		(string-append 
			(code-gen expr maj)
			"
			CMP(R0,IMM(2));
			JUMP_EQ("label");
			PUSH(R0);
					CALL(WRITE_SOB);
					DROP(1);
					CALL(NEWLINE);
					"label":"
			))))

(define code-gen
	(lambda (expr maj)
		;(display expr) (newline) (newline )
		 (cond ((equal? (car expr) 'const)  (code-gen-const expr maj))
			  ((equal? (car expr) 'if3) (gen-if3 expr maj))
			  ((equal? (car expr) 'seq) (gen-seq (cadr expr) maj))
			  ((equal? (car expr) 'or) (gen-or (cadr expr)maj) )
			  ((equal? (car expr) 'set) (gen-set (cadr expr) (caddr expr) maj) )
			  ((equal? (car expr) 'box-set) (gen-box-set (cadr expr) (caddr expr) maj)) 
			  ((equal? (car expr) 'box) (gen-box (cadr expr) maj))
			  ((equal? (car expr) 'box-get) (gen-box-get (cadr expr) maj))
			  ((equal? (car expr) 'pvar) (string-append "MOV(R0,FPARG(" (number->string (+ 2 (caddr expr))) "));\n") )
			  ((equal? (car expr) 'bvar) (string-append "MOV(R0,FPARG(0));\n" "MOV(R0,INDD(R0," (number->string  (caddr expr))
			  		"));\n" "MOV(R0,INDD(R0," (number->string (cadddr expr)) "));\n"))
			  ((equal? (car expr) 'lambda-simple) (gen-lambda-simple (cadr expr) (caddr expr) maj ))
			  ((equal? (car expr) 'applic) (gen-applic (cadr expr) (caddr expr) maj))
			  ((equal? (car expr) 'lambda-opt) (gen-lambda-opt (cadr expr) (caddr expr) (cadddr expr) maj))
			  ((equal? (car expr) 'lambda-var) (gen-lambda-opt '() (cadr expr) (caddr expr) maj) )
			  ((equal? (car expr) 'tc-applic ) (gen-tc-applic (cadr expr) (caddr expr) maj))
			  ((equal? (car expr) 'fvar) (code-gen-fvar (cadr expr) maj))
			  ((equal? (car expr) 'def) (code-gen-define (cadr expr) (caddr expr) maj))
			  (else "")
			  )))

(define (generate-impls)
	(fold-right string-append "" (map (lambda (x) (string-append x "\n")) primitive-scm-procedures)))

(define (add-runtime)
	(string-append " #include <stdio.h>
#include <stdlib.h>
#include \"cisc.h\" 
#define SOB_FALSE 4
#define SOB_NIL 3
#define SOB_VOID 2
#define MAKE_CLOSURE(FUNC) PUSH(LABEL(FUNC)) ;PUSH(666);CALL(MAKE_SOB_CLOSURE); DROP(2);
int main()
{
START_MACHINE ;
JUMP(AFTER_DEF);
\n"


"#include \"char.lib\"
#include \"io.lib\"
#include \"math.lib\"
#include \"string.lib\"
#include \"system.lib\"
#include \"scheme.lib\"
			/* PRIMITIVE PROCEDURES IMPLEMENTATIONS */\n
 "


(generate-impls)

"\n/* OUR ASM*/ \n"
our-asm-code

"AFTER_DEF:
ADD(IND(0),1000);	
"))
(define (flatten x)
    (cond ((null? x) '())
         ((not (pair? x)) (list x))
          (else (append (car x)
                        (flatten (cdr x))))))


(define fvar-undefined
	(lambda (table)

		(string-append (if (null? table) "" 
			(string-append "
							PUSH(IMM(117));
						 	PUSH(IMM(110));
						 	PUSH(IMM(100));
						 	PUSH(IMM(101));
						 	PUSH(IMM(102));
						 	PUSH(IMM(105));
						 	PUSH(IMM(110));
						 	PUSH(IMM(101));
						 	PUSH(IMM(100));
						 	PUSH(IMM(9));
						 	CALL(MAKE_SOB_STRING);
						 	DROP(10);
						 	MOV(IND("(number->string (cadr(car table)))"),R0);
						 	"(fvar-undefined (cdr table)))))))

(define fvar-table '())

(define get-all-symbols-from-consts-table
	(lambda (constants-table)
		(letrec ((helper
				(lambda (consts lst)
					(cond ((null? consts) lst)
						  ((symbol? (cadar consts)) (helper (cdr consts) (cons (caddar consts) lst)))
						  (else (helper (cdr consts) lst))))))
	(map cadr (helper constants-table '())))))

(define generate-smbl-nodes
	(lambda (symbls)
		(string-append
			"MOV(R1,IND(1));
			"
			(letrec ((helper (lambda (lst)
								(if (null? lst) "MOV(INDD(R1,1),T_NIL);"
												
												(string-append "
												MOV(IND(R1)," (number->string (car lst)) ");
												PUSH(2);
												CALL(MALLOC);
												DROP(1);
												MOV(INDD(R1,1),R0); /*CREATION OF NEW NODE */
												MOV(R1,R0); /*JUMP TO THE NEXT NODE */
												
												" (helper (cdr lst)))))))
		(helper symbls)))))

(define init-symtable
	(lambda (const-table)
		(string-append
		"
		PUSH(2);
		CALL(MALLOC);
		DROP(1);
		MOV(IND(1),R0);"
		(let ((lst (get-all-symbols-from-consts-table constants-table)))
			(if (null? lst) "MOV(IND(R0),3);\n MOV(INDD(R0,1),T_NIL);\n"
							(generate-smbl-nodes lst) ))
		" 
		")))

(define string->sexpr
  (lambda (string)
  	;(display "bbbbbbb   ") (display string) (newline )
    (<sexpr> (string->list string)
	    (lambda (e s)
	      `( ,e ,@(string->sexpr (list->string s))))
	    (lambda (w)  w))))

(define compile-scheme-file
	(lambda (input-file output-file)
		
		(delete-file output-file)
		(set! constants-table *initial-constant-table*)
		(let* ((f (open-output-file output-file))
			  (expr (map all (append (string->sexpr (file->string "Runtime.scm")) (string->sexpr (file->string input-file)))))
			  ;(expr  (map all (string->sexpr (file->string input-file))))
			  (new-const-table (fix-known-symbols-pointers (add_topo_list_to_const_table (pe-list->flat-const-list expr) (next-free-mem constants-table) constants-table)))
			  (new-fvar-table (make-fvar-table (next-free-mem new-const-table) expr )))
		;(display expr)
		
			(set! constants-table new-const-table)
			(set! fvar-table new-fvar-table)
			(display (string-append
				

				(add-runtime) 
				"/*SYMBOL TABLE INITIALIZATION */\n"
				(init-symtable constants-table)
				"\n"
				"/* CONSTANT TABLE */"

					 (copy-const-table-to-memory (flatten (map append (map caddr constants-table))) 2)  

					
					
					"/*PRIMITIVE FVAR TABLE */\n"
					(fvar-undefined fvar-table)
					"/*  bindings */"
					(bindings (next-free-mem constants-table)) "\n"

					"/* CODE GENERATION */"
					(fold-right string-append "" (map (lambda (x) (code-gen-prep x 0 )) expr ))

					"
					STOP_MACHINE;\n}") f) 
				 (close-output-port f))))


(define all (lambda (x) 
		    (annotate-tc
		      (pe->lex-pe
			(box-set
			  (remove-applic-lambda-nil
			    (eliminate-nested-defines (parse x))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;CONSTANT TABLE ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define starting-point 10)


(define *initial-constant-table*
`((2 ,(if #f #t) (937610))
  (3 () (722689))
  (4 ,#f (741553 0))
  (6 ,#t (741553 1)))  )

(define constants-table *initial-constant-table*)

;Location of the void object.
(define void-loc "IMM(2)")

;Location of the nil object.
(define nil-loc "IMM(3)")

;Location of the true boolean.
(define true-loc "IMM(4)")

;Location of the false boolean.
(define false-loc "IMM(6)")

(define fraction?
	(lambda (x)
		(and (rational? x) (not (integer? x)))))


(define topo-sort
  (lambda (e)
          (cond ((pair? e)
                  `(,@(topo-sort (car e))
                    ,@(topo-sort (cdr e))
                    ,e))
                ((vector? e)
                 `(,@(apply append (map topo-sort (vector->list e)))
                   ,e))
                ((symbol? e) `(,@(topo-sort (symbol->string e)) ,e))
                ((fraction? e)
                `(,@(topo-sort (numerator e))
            	  ,@(topo-sort (denominator e))
            		,e))
                (else `(,e)))))

(define lookup-in-constants-table
	(lambda (val const-table)
		(letrec ((helper
			(lambda (table)
				(cond ((null? table) #f)
					  ((equal? (cadr (car table)) val) (caar table))
					  (else (helper (cdr table)))))))
		(helper const-table))))



(define lookup-in-fvar-table
	(lambda (val fvar-table)
		(letrec ((helper
			(lambda (table)
				(cond ((null? table) #f)
					  ((equal? (caar table) val) (cadar table))
					  (else (helper (cdr table)))))))
		(helper fvar-table))))




(define find-consts 
    (lambda (lst) 
      (if (pair? lst)
	  (if (pe-const? lst)
	      `(,(cadr lst))
	      (append (find-consts (car lst)) (find-consts (cdr lst))))
	  '())))

(define find-fvar 
    (lambda (lst) 

      (if (pair? lst)
	  (if (pe-fvar? lst)
	      `(,(cadr lst))
	      (append (find-fvar (car lst)) (find-fvar (cdr lst))))
	  '())))

(define pe-list->flat-const-list (lambda (pe-list)
                                          (remove-duplicates (apply append (map topo-sort (find-consts pe-list))))))

              
(define get_type_id
  (lambda (smbl)
  (cond 
          ((null? smbl) 722689)
          ((boolean? smbl) 741553)
          ((char? smbl) 181048)
          ((integer? smbl) 945311)
          ((string? smbl) 799345)
          ((symbol? smbl) 368031)
          ((pair? smbl) 885397)
          ((vector? smbl) 335728)
          ((procedure? smbl) 276405)
          ((fraction? smbl) 544512)
          (else 937610)
          )))

(define get_size_of_smbl
  (lambda (smbl)
           (cond
                        ((boolean? smbl) 2)
                        ((char?  smbl)  2)
                        ((integer? smbl) 2)
                        ((pair? smbl) 3)
                        ((symbol? smbl) 2)
                        ((fraction? smbl) 3)
                        ((string? smbl) (+ 2 (string-length smbl)))
                        ((vector? smbl) (+ 2 (vector-length smbl)))
                        (else 3))))
(define next-free-mem
	(lambda (const-table)
		(let* ((last-const (car (reverse const-table)))
			   (last-mem (car last-const))
				(last-length (length (caddr last-const))))
		(+ last-length last-mem))))

(define convert_string_to_val
	(lambda (str)
		(map char->integer (string->list str))))

(define convert_vector_to_val
	(lambda (vec table)
		(map (lambda (x) (lookup-in-constants-table x table)) (vector->list vec))))

(define add_topo_list_to_const_table
	(lambda (topo-list next-mem newlist)

		(if (null? topo-list) newlist
		(let* ((smbl (car topo-list))
			    (type_expr (get_type_id smbl))
				(next_free (+ (get_size_of_smbl smbl) next-mem))
				(exists? (lookup-in-constants-table smbl newlist))
				(value (if exists? `() 
							(cond 

								((fraction? smbl) `(,type_expr ,(lookup-in-constants-table (numerator smbl) newlist) ,(lookup-in-constants-table (denominator smbl) newlist)))
								((pair? smbl) `(,type_expr ,(lookup-in-constants-table (car smbl) newlist)
							 						,(lookup-in-constants-table (cdr smbl) newlist)))
									((string? smbl) `(,type_expr ,(string-length smbl) ,@(convert_string_to_val smbl)))
									((number? smbl) `(,type_expr ,smbl))
									((char? smbl)   `(,type_expr ,(char->integer smbl)))
									((symbol? smbl) `(,type_expr 0))
									
									((vector? smbl) `(,type_expr ,(vector-length smbl) ,@(convert_vector_to_val smbl newlist)))
								(else `(,type_expr)))))
				(new_var (if exists? `() `((,next-mem ,smbl ,value)))))
			(if (= (length (cdr topo-list)) 0)
			(append newlist new_var) 
		(add_topo_list_to_const_table (cdr topo-list) (if exists? next-mem next_free) (append newlist new_var)))))))

(define fix-known-symbols-pointers
		(lambda (constants-table)
		(letrec ((helper
				(lambda (constants-table prev)
					(cond ((null? constants-table) '())
				  ((symbol? (cadr (car constants-table))) (cons `(,(caar constants-table) ,(cadar constants-table) 
				  													(368031 ,(lookup-in-constants-table (symbol->string (cadar constants-table)) prev)))
																		(helper (cdr constants-table) prev)))
				  (else (cons (car constants-table) (helper (cdr constants-table) prev )))

			   ))))
		(helper constants-table constants-table)
	)))

(define copy-const-table-to-memory (lambda (table index)
(if (null? table)
	(string-append "//END OF memory allocation\n " ) 

  (let* 
  	((exp (car table))
	(e (if (number? exp)
		(number->string exp)
		(symbol->string exp))))
	 (string-append  "MOV(IND(" (number->string index) ") , IMM(" e "));\n" 
(copy-const-table-to-memory (cdr table) (+ index 1)))
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;FVAR TABLE;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define find-define 
    (lambda (lst) 
      (if (pair? lst)
	  (if (tagged? lst 'def)
	      `(,(cadadr lst))
	      (append (find-define (car lst)) (find-define (cdr lst))))
	  '())))


(define bind
  (lambda(label mem)
	  (string-append "
//This is the bindings for " label "
MAKE_CLOSURE(" label");
MOV(IND(" (number->string mem) "),R0);"

)))

(define primitive-list
	'( 
			(bin+ "BIN_PLUS" 0)
			(apply "APPLY" 1) 
			(bin- "BIN_MINUS" 2)
			(bin* "BIN_MUL" 3 )
			(bin/ "BIN_DIV" 4)
			(bin> "GRATER_THAN" 5)
			(bin= "BIN_EQUAL" 6)
			(bin< "LESS_THAN" 7)
			(car "CAR" 8)
			(cdr "CDR" 9)
			(numerator "NUMERATOR" 10)
			(denominator "DENOMINATOR" 11)
			(rational? "IS_RATIONAL" 12)
			(string->symbol "STRING_TO_SYMBOL" 13)
			(procedure? "IS_CLOSURE" 14)
			(vector?  "IS_VECTOR" 15)
			(symbol?  "IS_SYMBOL" 16)
			(string?  "IS_STRING" 17)
			(char?  "IS_CHAR" 18)
			(number?  "IS_NUMBER" 19)
			(integer? "IS_INTEGER" 20)
			(boolean?  "IS_BOOLEAN" 21)
			(pair?  "IS_PAIR" 22)
			(null? "IS_NULL" 23)
			(integer->char "INTEGER_TO_CHAR" 24)
			(char->integer "CHAR_TO_INTEGER" 25)
			(string-length "STRING_LENGTH" 26)
			(vector-length "VECTOR_LENGTH" 27)
			(make-string "MAKE_STRING" 28)
			(make-vector "MAKE_VECTOR" 29)
			(not "NOT" 30)
			(symbol->string "SYMBOL_TO_STRING" 31)
			(set-car! "SET_CAR" 32)
			(set-cdr! "SET_CDR" 33)
			(cons "CONS" 34)
			(string-set! "STRING_SET" 35)
			(vector-set! "VECTOR_SET" 36)
			(string-ref "STRING_REF" 37)
			(vector-ref "VECTOR_REF" 38)
			(fraction   "MAKE_SOB_FRACTION" 39)
			(eq? "EQQ" 40)
			))


(define bindings 
  (lambda (next-free)
      (apply string-append
		(map 
			(lambda (x) (bind (cadr x) (+ (caddr x) next-free)))
			primitive-list
			))))

(define builtin_Scheme_procedures 	'(bin+ bin- bin* bin/ bin< bin= bin> apply
					 procedure? numerator denominator rational? car cdr string->symbol vector? symbol?
					string? char? number? boolean? pair? null? integer->char
					char->integer string-length symbol->string set-car! set-cdr!
					cons make-string vector-length string-set! make-vector string-ref
					vector-ref eq? integer? vector-set! not fraction
))

(define primitive-scm-procedures
	`(,apply-code ,is-rational-code ,numerator-code  ,denominator-code ,string-to-symbol-code ,symbol-to-string-code
		
	 ,cons-code ,cdr-code ,car-code ,bin+-code ,bin*-code ,bin>-code ,bin<-code ,bin--code ,IS_NUMBER-code ,IS_INTEGER-code
	 ,bin/-code ,bin=-code ,IS_NULL-code ,IS_STRING-code ,IS_SYMBOL-code ,IS_VECTOR-code ,CHAR_TO_INTEGER-code 
		,integer->char-code ,make-string-code ,eq?-code ,IS_BOOLEAN-code ,IS_PAIR-code ,IS_PROCEDURE-code ,IS_CHAR-code ,string-length-code ,vector-length-code ,set-car!-code ,set-cdr!-code ,string-ref!-code ,string-set!-code ,not-code ,make-vector-code ,vector-ref-code ,vector-set!-code))


(define make-initial-fvar-table
	(lambda (first-mem)
		(map (lambda (x) (list (car x) (+ first-mem (caddr x)))) primitive-list)
		))

(define remove-duplicates
	(lambda  (lst)
  (fold-right (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y))) '() lst)))

(define make-fvar-table 
	(lambda (first-free-mem expr)
		
		(let ((fvar-table (make-initial-fvar-table first-free-mem))
			(next-mem (+ first-free-mem (length primitive-list)))
			  (fvars (remove-duplicates(find-fvar expr)))
			  )
	(add-fvars fvar-table next-mem fvars))))

(define add-fvars
	(lambda (table mem fvars)
		(letrec ((helper 
					(lambda (vars memory lst)
						
						(if (null? vars) lst 
										(let ((exists? (lookup-in-fvar-table (car vars) table )))
											(if exists? (helper (cdr vars) memory lst)
												(helper (cdr vars) (+ memory 1) (append lst `((,(car vars) ,memory))))


										))))))
	(append table (helper fvars mem '())))))