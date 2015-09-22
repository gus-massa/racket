(module member '#%kernel
  (#%require "cond.rkt" "qq-and-or.rkt"
             "performance-hint.rkt"
             (for-syntax '#%kernel "qq-and-or.rkt"))
  (#%provide memq2 memq2x memv2 member2 member2x)

  ;; helper for memq/v/ber error cases
  (define-values (bad-list)
    (λ (who orig-l)
      (raise-mismatch-error who "not a proper list: " orig-l)))

  ;; Create the mem functions
  (define-syntaxes (mk)
    (λ (stx)
      (define-values (forms) (syntax-e stx))
      (define-values (id eq?)
        (values (syntax-e (cadr forms))
                (syntax-e (caddr forms))))
      (datum->syntax
       stx
       `(let-values ([(,id)
                      (lambda (v orig-l)
                        (let loop ([ls orig-l])
                          (cond
                            [(null? ls) #f]
                            [(not (pair? ls))
                             (bad-list ',id orig-l)]
                            [(,eq? v (car ls)) ls]
                            [else (loop (cdr ls))])))])
          ,id))))

  (define-syntaxes (mk3)
    (λ (stx)
      (define-values (forms) (syntax-e stx))
      (define-values (id)
        (values (syntax-e (cadr forms))))
      (datum->syntax
       stx
       `(let-values ([(,id)
                      (lambda (v orig-l eq?)
                        (let loop ([ls orig-l])
                          (cond
                            [(null? ls) #f]
                            [(not (pair? ls))
                             (bad-list ',id orig-l)]
                            [(,eq? v (car ls)) ls]
                            [else (loop (cdr ls))])))])
          ,id))))

  (define-values (memq2x) (mk memq eq?))
  (begin-encourage-inline
    (define-values (memq2) (mk memq eq?))
    (define-values (memv2) (mk memq eqv?))
    ;; Note that this uses `mk-member`
    (define-values (member2) (mk member equal?))
    (define-values (member/default) (mk member equal?))
    (define-values (member/any) (mk3 member))
    (define-values (member/test)
                    (lambda (v orig-l eq?) 
                       (if (and (procedure? eq?)
                                (procedure-arity-includes? eq? 2))
                           (void)
                           (raise-argument-error
                            'member
                            "(procedure-arity-includes/c 2)"
                            eq?))
                       (member/any v orig-l eq?)))  
    (define-values (member2x)
                    (case-lambda
                      ([v orig-l] (member/default v orig-l))
                      ([v orig-l eq?] (member/test v orig-l eq?))))))
