#lang racket

(provide (all-defined-out))

;; În acest fișier vă definiți constructorii și
;; operatorii tipului Collection.
;; În etapele anterioare, colecțiile erau de fapt
;; liste.
;; În definițiile de mai jos, veți considera că
;; o colecție este implementată ca flux.

; Întrucât stream-cons nu este o funcție obișnuită, 
; ci este definită ca o sintaxă specială, astfel
; încât ea să nu își evalueze argumentele înainte 
; de apel (comportament pe care ni-l dorim și pentru 
; collection-cons), nu putem folosi o definiție
; de tipul
;    (define collection-cons stream-cons)
; (genul acesta de definiție generează o eroare).
; Nici varianta
;    (define (collection-cons x xs) (stream-cons x xs))
; nu este o soluție, întrucât funcțiile definite de noi
; în Racket sunt funcții stricte, iar x și xs vor fi
; evaluate înainte de a intra în corpul funcției
; collection-cons și a descoperi că ele vor fi
; argumentele unui stream-cons.
; Modul de a defini collection-cons pentru a reproduce
; întocmai comportamentul lui stream-cons este:
(define-syntax-rule (collection-cons x xs) (stream-cons x xs))
; Obs: puteți schimba numele funcției, dacă nu vă
; place "collection-cons". Este o funcție folosită doar
; de voi în fișierul etapa4.rkt, nu de cheyker.

; TODO
; Scrieți în continuare restul definițiilor
; (care nu necesită o sintaxă specială).
(define-syntax-rule (collection-first x) (stream-first x))
(define-syntax-rule (collection-rest x) (stream-rest x))
(define (collection-empty? x) (stream-empty? x))
(define-syntax-rule (collection? x) (stream? x))
(define-syntax-rule (collection-caddr x) (stream-caddr x))
(define-syntax-rule (collection-cadr x) (stream-cadr  x))
(define-syntax-rule (collection-filter x xy) (stream-filter x xy))
(define-syntax-rule (collection-map x xy) (stream-map x xy))
(define-syntax-rule (collection-tail x xx) (stream-tail x xx))
(define (list->collection L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->collection (cdr L)))))
(define collection-empty
  (stream))