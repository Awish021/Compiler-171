(define remww
  (letrec ((read-from cadr)
           (write-to caddr)
           ;; checks if a single register is removable
           (reg-removable?
             (lambda (reg rest-insts)
               (cond ((null? rest-insts) #f)
                     ((member reg (read-from (car rest-insts))) #f)
                     ((member reg (write-to (car rest-insts))) #t)
                     (else (reg-removable? reg (cdr rest-insts))))))
           
           (remove-inst-if-possible
             (lambda (inst rest)
               ;; if all write-regs are removable - remove current instruction
               (if (andmap (lambda (reg) (reg-removable? reg rest)) (write-to inst))
                   rest
                   (cons inst rest)))))
    
    (lambda (inst-lst)
      (if (null? inst-lst)
          '()
          (remove-inst-if-possible (car inst-lst)
                                   (remww (cdr inst-lst)))))))