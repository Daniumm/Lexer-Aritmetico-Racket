#lang racket
(require racket/port)
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))



(define-tokens value-tokens (ENTERO REAL VARIABLE ASIGNACION MULTIPLICACION COMENTARIO SUMA RESTA POTENCIA DIVISION PARENTESISABRE PARENTESISCIERRA ERROR))
(define-empty-tokens op-tokens (newline = + - * / ^ EOF))


(define-lex-abbrevs
  [var_symbs     (:or (:or (:/ "a" "z") (:/ #\A #\Z) ) (:+ "_"))]
  [digit      (:/ #\0 #\9)])

(define lexerAritmetico
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space) (lexerAritmetico input-port)]
   [#\newline (token-newline)]
   ["=" (token-ASIGNACION (string->symbol lexeme))]
   ["+" (token-SUMA (string->symbol lexeme))]
   ["-" (token-RESTA (string->symbol lexeme))]
   ["*" (token-MULTIPLICACION (string->symbol lexeme))]
   ["/" (token-DIVISION (string->symbol lexeme))]
   [(concatenation (repetition 2 2 "/") (repetition 0 +inf.0 (union (char-range #\a #\z) (:or #\tab #\space) (char-range #\A #\Z) (char-range #\0 #\9))) ) (token-COMENTARIO (string->symbol lexeme))]
   ["^" (token-POTENCIA (string->symbol lexeme))]
   
   ["(" (token-PARENTESISABRE lexeme)]
   [")" (token-PARENTESISCIERRA lexeme)]
   
   [(concatenation (union (char-range #\a #\z) (char-range #\A #\Z)) (repetition 0 +inf.0 (union (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9) (:+ "_")))) (token-VARIABLE (string->symbol lexeme))]
   [(:: (:* "-") (:+ digit)) (token-ENTERO (string->number lexeme))]
   [(:: (:* "-") (:+ digit) (:* (:: #\. (:+ digit) ))) (token-REAL (string->number lexeme))]
   [(:: (:* "-") (:+ digit) (:* (:: #\. (:+ digit) )) (:* (:: (:: #\E (:* "-") ))) (:+ digit) ) (token-REAL (string->number lexeme))]))
   

(provide value-tokens op-tokens lexerAritmetico)


