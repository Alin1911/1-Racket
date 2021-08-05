#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
   (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (update-rec f counters index '())
  )

(define (update-rec f counters index acc)
  (cond ((null? counters) acc)
        ((= (counter-index (car counters)) index) (update-rec f (cdr counters) index (append  acc (list (f (car counters))))))
        (else (update-rec f (cdr counters) index (append acc (list (car counters)))))
        ))
;aplica o functie pe  o lista si returneaza lista
(define (update-all f counters acc)
    (cond ((null? counters) acc)
          (else (update-all f (cdr counters) (append  acc (list (f (car counters))))))
          )
  )

(define tt+ 
  (lambda(minutes)
    (lambda(C)
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [index index] [et et] [tt (+ tt minutes)] [queue queue])])
      )
    )
  )

(define et+
  (lambda(minutes)
   (lambda(C)
    (match C
    [(counter index tt et queue)
     (struct-copy counter C [index index] [tt tt] [et (+ et minutes)] [queue queue])])
    )
  )
)

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
    [(counter index tt et queue)
        (if
         (queue-empty? queue)
         (struct-copy counter C [index index] [tt (+ tt items)] [et ( + et items)] [queue (enqueue (cons name items) queue)])
         (struct-copy counter C [index index] [tt (+ tt items)] [et et] [queue (enqueue (cons name items) queue)])
         )
     ]
     )
    )
  )

(define (general-func f counters C)
      (cond
      ((null? counters) C)
      ((and (equal? counter-et f)(queue-empty? (counter-queue (car counters))))(general-func f (rest counters) C))
      ((< (f (first counters)) (cdr C)) (general-func f (rest  counters) (cons (counter-index (first counters)) (f (first counters)))))
      ((> (f (first counters)) (cdr C)) (general-func f (rest  counters) C))
      ((= (f (first counters)) (cdr C)) (if (<  (counter-index (first counters)) (car C))
                                                            (general-func f (rest counters) (cons (counter-index (first counters)) (f (first counters))))
                                                            (general-func f (rest counters) C)
                                                            )
                                        )
      )
  )


; folosind funcția de mai sus
(define (min-tt counters)
  (general-func counter-tt counters (cons 1000 1000))
  )

; folosind funcția de mai sus
(define (min-et counters)
  (general-func counter-et counters (cons 1000 1000))
  )

(define (remove-first-from-counter C)   ; testată de checker
   (match C
       [(counter index tt et queue)
        (if (queue-empty? queue)
            (struct-copy counter C [index index] [tt 0] [et 0] [queue queue])
            (if (queue-empty? (dequeue queue))
                (struct-copy counter C [index index] [tt 0] [et 0] [queue (dequeue queue)])
                (struct-copy counter C [index index] [tt (- tt et)] [et (cdr (top (dequeue queue)))] [queue (dequeue queue)])
                )
            )
     ]
    )
  )


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!


(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
       [(counter index tt et queue)
        (if (< tt minutes)
            (struct-copy counter C [index index] [tt 0] [et 0] [queue empty-queue])
            (struct-copy counter C [index index] [tt (- tt minutes)] [et (- et minutes)] [queue queue])
            )
     ]
    )
   )
  )
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.


(define (ttmed list nr sum)
  (cond ((null? list)(/ sum nr))
        (else (ttmed (cdr list) (+ nr 1) (+ sum (counter-tt (car list)))))
        )
  )

(define (max-index list acc)
    (cond ((null? list) acc)
          ((<= acc (counter-index (car list)))(max-index (cdr list) (counter-index (car list))))
          (else (max-index (cdr list) acc))
   )   
  )




(define (serve requests fast-counters slow-counters)
  (serve1 requests fast-counters slow-counters '())
  )

(define (serve1 requests fast-counters slow-counters client)
  (if (null? requests)
      (cons client (append fast-counters slow-counters))
      (match (car requests)
        
        [(list string numar)
         (cond
           ((equal? string 'ensure) (if( < numar (ttmed (append fast-counters slow-counters) 0 0) )
                (serve1 requests fast-counters (append slow-counters (list (empty-counter (+ 1(max-index (append fast-counters slow-counters) 0))))) client)
                (serve1 (cdr requests) fast-counters slow-counters client)
                ))
           (else (if (< numar (+ ITEMS 1))
                (serve1 (cdr requests) (update (add-to-counter string numar) fast-counters (car (min-tt (append fast-counters slow-counters))))
                        (update (add-to-counter string numar) slow-counters (car (min-tt (append fast-counters slow-counters))))
                        client
                        )
            
                 (serve1 (cdr requests) fast-counters
                        (update (add-to-counter string numar) slow-counters (car (min-tt slow-counters)))
                        client
                        )
                 )
             )
           )
         ]
        
         [(list 'delay index minutes) (serve1 (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index)
                                             (update (tt+ minutes) (update (et+ minutes) slow-counters index) index) client)
                                         ]

         [minute (serve1 (cdr requests) (update-all (trece-timpul minute) fast-counters '()) (update-all (trece-timpul minute) slow-counters '())
                         (au-plecat (append fast-counters slow-counters) minute client))
                 ]
          )
      )
  )
; functie care se se aplica pe o casa si actualizeaza casa
(define (trece-timpul minutes)
  (λ (C)
    (match C
       [(counter index tt et queue)
        (cond
          ((<= tt minutes)(struct-copy counter C [index index] [tt 0] [et 0] [queue empty-queue]))
          ((> et minutes)(struct-copy counter C [index index] [tt (- tt minutes)] [et (- et minutes)] [queue queue]))
          ((= et minutes)(struct-copy counter C [index index] [tt (- tt minutes)] [et (cdr (top (dequeue queue)))] [queue (dequeue queue)]))
          (else (elimina-rec (struct-copy counter C [index index] [tt (- tt minutes)] [et et] [queue queue]) minutes))
            )
     ]
    )
   )
  )
; functie care modifica coada de clienti
(define (elimina-rec C minute)
  (match C
    [(counter index tt et queue)
        (cond
          ((= minute 0) C)
          ((> et minute)(struct-copy counter C [index index] [tt tt] [et (- et minute)] [queue queue]))
          ((= et minute)(struct-copy counter C [index index] [tt tt] [et (cdr (top (dequeue queue)))] [queue (dequeue queue)]))
          ((> minute et)(elimina-rec (struct-copy counter C [index index] [tt tt] [et (cdr (top (dequeue queue)))] [queue (dequeue queue)]) (- minute et)))
          )
        ]
    )
  )

;functie care  actualizeaza casele cu 1 minute si actualizeaza lista de clienti plecati
 (define (au-plecat counters minute acc)
   (cond
     ((= minute 1)(pleaca counters acc))
     (else (au-plecat (update-all (trece-timpul 1) counters '()) (- minute 1)
                      (pleaca counters acc))
           )
     )
   )

;functie care verifica ce clienti pleaca
 (define (pleaca counters acc)
   (if (null? counters)
       acc
       (match (car counters)
         [(counter index tt et queue)
          (cond
            ((queue-empty? queue)(pleaca (cdr counters) acc))
            ((= et 1) (pleaca (cdr counters) (append acc (list (cons index (car (top queue)))))))
            (else (pleaca (cdr counters) acc))
            )
          ]
         )
       )
    )
   

        
