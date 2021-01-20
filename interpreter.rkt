#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require global)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define symbol-table (make-hash))

(define-tokens a (num id str))
(define-empty-tokens b (ob cb semi-colon new-line end do while then if else
                            print return assignment greater less not-equal equal minus multiply div open-paren close-paren null 
                            true false comma open-sq-bracket close-sq-bracket plus eof))
(define while 
  (lambda (cond body)
    (when (cond) (begin (body) (while cond body))
    )))

(define globalism
  (lambda
    (name expr)
    (hash-set! symbol-table name expr)
    expr))

(define smart-eval 
  (lambda 
    (expr)
    (if (string? expr)
        (hash-ref symbol-table expr)
       (expr))))


(define basic-lexer
  (lexer
   [(eof) (token-eof)]
   [";" (token-semi-colon)]
   ["end" (token-end)]
   ["do" (token-do)]
   ["while" (token-while)]
   ["then" (token-then)]
   ["if" (token-if)]
   ["else" (token-else)]
   ["return" (token-return)]
   ["print" (token-print)]
   ["=" (token-assignment)]
   [">" (token-greater)]
   ["<" (token-less)]
   ["!=" (token-not-equal)]
   ["==" (token-equal)]
   ["-" (token-minus)]
   ["+" (token-plus)]
   ["*" (token-multiply)]
   ["/" (token-div)]
   ["(" (token-open-paren)]
   [")" (token-close-paren)]
   ["null" (token-null)]
   ["true" (token-true)]
   ["false" (token-false)]
   ["," (token-comma)]
   ["ob" (token-ob)]
   ["cb" (token-cb)]
   ["[" (token-open-sq-bracket)]
   ["]" (token-close-sq-bracket)]
   [(:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-num (string->number lexeme))]
   [(:+ alphabetic) (token-id lexeme)]
   [(:: #\" alphabetic #\") (token-str lexeme)]
   [whitespace (basic-lexer input-port)]

))
(define in-while? #f)

(define basic-parser
           (parser
            (start cmd)
            (end eof)
            (error void)
            (tokens a b)
            ;;; (precs (left - +))
            (grammar
             
             (cmd ((keyword) $1)
                  ((cmd semi-colon keyword) (lambda () ($1) ($3))));;gg
             (keyword ((if_statement) $1)
                      ((while_statement) $1) 
                      ((assignment_statement) $1)
                      ((return_statement) $1)
                      ((print_statement) $1))
             (if_statement ((if exp then cmd else cmd end) (lambda () (if ($2) ($4) ($6)))))
             (while_statement ((while exp do cmd end) (lambda () (while $2 $4))))
             (assignment_statement ((id assignment exp) (lambda () (globalism $1 ($3))))) ;; ez
             (return_statement ((return exp) (lambda () ($2))))
             (print_statement ((print exp) (lambda () (displayln ($2)))))
             
             (exp ((aexp) $1) 
                   ((aexp greater aexp) (lambda () (> ($1) ($3))))
                   ((aexp less aexp) (lambda () (< ($1) ($3))))
                   ((aexp equal aexp) (lambda () (equal? ($1) ($3))))
                   ((aexp not-equal aexp) (lambda () (not (equal? ($1) ($3))))))

             (aexp ((bexp) $1) 
                   ((bexp minus aexp) (lambda () (- ($1) ($3))))
                   ((bexp plus aexp) (lambda () (+ ($1) ($3)))))
             (bexp ((cexp) $1) 
                   ((cexp multiply bexp) (lambda () (* ($1) ($3))))
                   ((cexp div bexp) (lambda () (/ ($1) ($3)))))
             (cexp ((minus cexp) (lambda () (* -1 ($2))))
                   
                   ((open-paren exp close-paren) (lambda () ($2)))
                   
                   ((num) (lambda () $1))
                   ((null) (lambda () null))
                   ((id) (lambda () (smart-eval $1)))
                   ((true) (lambda () #t))
                   ((false) (lambda () #f))
                   ((str) (lambda () $1))
                   ((list) (lambda () $1)))
             (list ((open-sq-bracket list_values close-sq-bracket) null)
                   ((open-sq-bracket close-sq-bracket) null))
             (list_values ((exp) null) ((exp comma list_values) null))
             (list_member ((exp) null) ((ob exp cb list_member) null)))))

(define evaluate (lambda (path)
  (define lex-this (lambda (lexer input) (lambda () (lexer input))))
  (define my-lexer (lex-this basic-lexer (open-input-string (file->string path))))
  (let ((parser-res (basic-parser my-lexer))) (parser-res))
))

(evaluate "code.pam")

(provide interpreter)