#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.


(define (longest-common-prefix w1 w2)
  (if (or (null? w1) (null? w2))
      (list '() w1 w2)
      (if (equal? (car w1) (car w2))
          (cons (cons (car w1) (car (longest-common-prefix (cdr w1) (cdr w2)))) ; primul cons pune langa lista de prefix listele cu ce a mai ramas din cuvinte
                (cdr (longest-common-prefix (cdr w1) (cdr w2)))) ; al doilea cons tot adauga in lista de litere ale prefixului
          (list '() w1 w2)); daca nu sunt literele la fel returnez ce a ramas din cuvinte si array gol
      )
  )

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.

(define (searcher result word-list)
    (if (null? word-list)
        (car result) ; primul element din lista de 3 liste e cel cu prefixul
        (searcher (longest-common-prefix (car result) (car word-list))
              (cdr word-list)
              ) ; apelez searcher de prefix si celalalt element din lista de cuvinte
        )
  )

(define (longest-common-prefix-of-list words)
  (if (null? words)
      '()
      (searcher (list (car words)) (cdr words)) ; primul element din words devine lista pt car de la car result
      )
  )

;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

(define (match-pattern-with-label st pattern)
  (let* ((branch (get-ch-branch st (car pattern)))) ; caut branch care are primul caracter, primul caracter din pattern
    (if (not branch)
        (list #f '()) ; daca nu gasesc niciuna returnez fals
        (let* ((label (get-branch-label branch))
               (common-prefix (longest-common-prefix pattern label)))
          (cond
            ((equal? (car common-prefix) pattern) #t) ; daca cel mai lung prefix e patternul returnez instant true
            ((equal? (car common-prefix) label) ; daca cel mai lung prefix e egal cu label
             (if (pair? (cadr common-prefix)) ; verific daca a mai ramas ceva din pattern
                 (list label (cadr common-prefix) (get-branch-subtree branch)) ; returnez label, ce a mai ramas din pattern si subtree
                 (list #f '()))) ; daca nu a mai ramas nimic din pattern returnez false si pattern lista goala
            (else (list #f (car common-prefix)))))))) ; daca nu se potriveste returnez fals si cel mai lung prefix comun


; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.

(define (st-has-pattern? st pattern)
  (let ((result (match-pattern-with-label st pattern))) ; se face match
    (cond
      ((equal? result #t) #t) ; daca rezult e true atunci returnez direct true
      ((and (list? result) (equal? (car result) #f))
       #f);altfel returnez false 
      ((and (list? result) (not (null? result))) ; daca lista nu e vida
       (let ((label (car result));fac extragerile necesare
             (remaining-pattern (cadr result))
             (subtree (caddr result)))
         (if (null? remaining-pattern) ; daca am ajuns la final returnez rezultatul functiei apelate
             result
             (st-has-pattern? subtree remaining-pattern)))) ; apelez recursiv
      (else #f)))) ;  daca nimic nu e true returnez false