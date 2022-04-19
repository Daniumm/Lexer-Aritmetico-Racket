#lang racket

(require racket/port) 
(require "Act3.rkt" parser-tools/lex)

(define (write-to-a-file-new-line path txt)
  (call-with-output-file path
    (lambda (output-port)
      (writeln txt output-port))
    #:exists 'append))

(define (write-to-a-file path txt)
  (call-with-output-file path
    (lambda (output-port)
      (print txt output-port))
    #:exists 'append))

(define (create-file path)
  (define out (open-output-file "input-output.txt"))
  (close-output-port out))


 
(define (lex-test ip)
  (if(file-exists? "input-output.txt")
    (delete-file "input-output.txt")
    (create-file "input-output.txt")
  )
  (printf "Token    Tipo\n")
  (write-to-a-file-new-line "input-output.txt" "Token    Tipo")
  (port-count-lines! ip)
  (letrec ([one-line
            (lambda ()
              (let ([result (lexerAritmetico ip)])
                (unless (equal? result 'EOF)
                  (printf "~a    |     " (token-value result))
                  (printf "~a\n" (token-name result))
                  (printf "\n")
                  (write-to-a-file "input-output.txt" (token-value result))
                  (write-to-a-file "input-output.txt" "      |     ")
                  (write-to-a-file "input-output.txt" (token-name result))
                  (write-to-a-file-new-line "input-output.txt" "")
                  (one-line)
                  )))])
    (one-line)))
 
(define (my-lex-test str)
    (lex-test (open-input-string str)))
    
(provide my-lex-test)

(define input-contents
    (port->string (open-input-file "Act 3.2/input.txt") #:close? #t))

(my-lex-test input-contents)

