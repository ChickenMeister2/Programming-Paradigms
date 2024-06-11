#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")
(require racket/stream)

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (if (or (null? w1) (null? w2))
      (list '() w1 w2)
      (if (equal? (car w1) (car w2))
          (cons (cons (car w1) (car (longest-common-prefix (cdr w1) (cdr w2)))) ; primul cons pune langa lista de prefix listele cu ce a mai ramas din cuvinte
                (cdr (longest-common-prefix (cdr w1) (cdr w2)))) ; al doilea cons tot adauga in lista de litere ale prefixului
          (list '() w1 w2)); daca nu sunt literele la fel returnez ce a ramas din cuvinte si array gol
      )
  )



; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection

(define (searcher result word-list)
  (let loop ((result result) (word-list word-list))
    (if (collection-empty? word-list)
        (car result) ; primul element din lista de 3 liste e cel cu prefixul
        (loop (longest-common-prefix (car result) (collection-first word-list))
              (collection-rest word-list))))) ; apelez searcher de prefix si celalalt element din lista de cuvinte


(define (longest-common-prefix-of-collection words)
  (if (null? words)
      collection-empty
      (searcher (list (collection-first words)) (collection-rest words)) ; primul element din words devine lista pt car de la car result
      )
  )


;startog
;(define (match-pattern-with-label st pattern)
;  (let* ((branch (get-ch-branch st (collection-first pattern)))) ; caut branch care are primul caracter, primul caracter din pattern
;    (if (not branch)
;        (list #f '()) ; daca nu gasesc niciuna returnez fals
;        (let* ((label (get-branch-label branch))
;               (common-prefix (longest-common-prefix pattern label)))
;         (cond
;            ((equal? (car common-prefix) pattern) #t) ; daca cel mai lung prefix e patternul returnez instant true
;            ((equal? (car common-prefix) label) ; daca cel mai lung prefix e egal cu label
;             (if (pair? (cadr common-prefix)) ; verific daca a mai ramas ceva din pattern
;                 (list label (cadr common-prefix) (get-branch-subtree branch)) ; returnez label, ce a mai ramas din pattern si subtree
;                 (list #f '()))) ; daca nu a mai ramas nimic din pattern returnez false si pattern lista goala
;            (else (list #f (car common-prefix)))))))) ; daca nu se potriveste returnez fals si cel mai lung prefix comun


;(define (st-has-pattern? st pattern)
;  (if (collection-empty? pattern) ; if pattern is empty, return #f
;      #f
;      (let ((result (match-pattern-with-label st pattern))) ; matchuim label cu tructura
;        (cond
;          ((equal? result #t) #t) ; daca e true returnez instant true
;          ((and (collection? result) (equal? (collection-first result) #f)) ; daca e false la inceput si lista returnez false instant
;           #f)
;          ((and (collection? result) (not (collection-empty? result))) ; daca rezultatul nu e lista goala apelez recursiv cu noile valori
;           (st-has-pattern? (collection-rest (collection-rest result)) (collection-rest (collection-first result))))
;          (else #f))))) ; daca am o lista goala ca rezultat returnez direct false
;og

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


(define (st-has-pattern? st pattern)
  (let ((result (match-pattern-with-label st pattern)))
    (cond
      ((equal? result #t) #t)
      ((and (list? result) (equal? (car result) #f))
       #f)
      ((and (list? result) (not (null? result)))
       (let ((label (car result))
             (remaining-pattern (cadr result))
             (subtree (caddr result)))
         (if (null? remaining-pattern)
             result
             (st-has-pattern? subtree remaining-pattern))))
      (else #f))))

(define (get-suffixes text)
  (if (collection-empty? text)
      collection-empty
      (collection-cons text  (get-suffixes (collection-rest text)))
      )
  )


(define (get-ch-words words ch)
  (collection-filter (lambda (x) (and (not (collection-empty? x)) (eq? (collection-first x) ch))) words))


(define (ast-func suffixes)
  (if (collection-empty? suffixes)
      '()
      (cons (list (collection-first (collection-first suffixes))) (collection-map (lambda (x) (if (collection-empty? x) '() (collection-rest x))) suffixes))))


(define (cst-func suffixes)
  (let* ((prefix (longest-common-prefix-of-collection suffixes)))
    (cons  prefix (collection-map (lambda (x) (collection-tail x (length prefix))
                                    )suffixes ))  ))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (let* ((by-first-char (collection-map (λ (ch) (get-ch-words suffixes ch)) alphabet))
         (useful-ch-lists (collection-filter (compose not collection-empty?) by-first-char))
         (branches (collection-map labeling-func useful-ch-lists)))
    (collection-map (λ (branch) (cons (car branch)
                                      (suffixes->st labeling-func (cdr branch) alphabet)))
                    branches)))
 
 
 

; nu uitați să convertiți alfabetul într-un flux
(define (text->st labeling-func) ; curried : doar un argument pe rand
  (lambda (text)
    (let* ((suffixes (get-suffixes (append text (list #\$)))) ; toate sufixele si adaug la final $
           (alphabet (list->collection (sort (remove-duplicates (append text (list #\$))) char<?)))) ; alfabetul (literle) fara duplicate din text
      (suffixes->st labeling-func suffixes alphabet)))) ; cream suffix tree-ul folosind sufixele



(define text->ast (text->st ast-func)) 


(define text->cst (text->st cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let ((st (text->ast text)))
    (st-has-pattern? st pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let loop ((st (text->cst text)) (need-len len) (result '()))
    (cond ((st-empty? st) #f)
          ((<= need-len 0) (take result len))
          (else
           (let* ((branch (first-branch st)) (label (get-branch-label branch)) (subtree (get-branch-subtree branch)))
             (or (loop subtree (- need-len (length label)) (append result label))
                 (loop (other-branches st) need-len result)))))))