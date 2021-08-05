#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue
(define-struct counter (index tt et queue stare) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 empty-queue 1))

;==========================================================================================

(define closeC 
  (lambda(schimba)
    (lambda(C)
      (match C
        [(counter index tt et queue stare)
         (struct-copy counter C [index index] [et et] [tt tt] [queue queue] [stare schimba])])
      )
    )
  )

(define (update f counters index)
  (update-rec f counters index '())
  )

(define (update-rec f counters index acc)
  (cond ((null? counters) acc)
        ((= (counter-index (car counters)) index) (update-rec f (cdr counters) index (append  acc (list (f (car counters))))))
        (else (update-rec f (cdr counters) index (append acc (list (car counters)))))
        )
  )

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
        [(counter index tt et queue stare)
         (struct-copy counter C [index index] [et et] [tt (+ tt minutes)] [queue queue] [stare stare])])
      )
    )
  )

(define et+
  (lambda(minutes)
   (lambda(C)
    (match C
    [(counter index tt et queue stare)
     (struct-copy counter C [index index] [tt tt] [et (+ et minutes)] [queue queue][stare stare])])
    )
  )
)

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
    [(counter index tt et queue stare)
        (if
         (queue-empty? queue)
         (struct-copy counter C [index index] [tt (+ tt items)] [et ( + et items)] [queue (enqueue (cons name items) queue)] [stare stare])
         (struct-copy counter C [index index] [tt (+ tt items)] [et et] [queue (enqueue (cons name items) queue)][stare stare])
         )
     ]
     )
    )
  )

(define (general-func f counters C)
      (cond
        ((null? counters) C)
        ((and (equal? counter-tt f)(= 0 (counter-stare (car counters))))(general-func f (rest counters) C))
        ((and (equal? counter-et f)(queue-empty? (counter-queue (car counters))))(general-func f (rest counters) C))
        ((< (f (first counters)) (cdr C)) (general-func f (rest  counters) (cons (counter-index (first counters)) (f (first counters)))))
        ((> (f (first counters)) (cdr C)) (general-func f (rest  counters) C))
        ((= (f (first counters)) (cdr C)) (if (<  (counter-index (first counters)) (car C))
                                              (general-func f (rest counters) (cons (counter-index (first counters)) (f (first counters))))
                                              (general-func f (rest counters) C)
                                              ))
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
;======================================================================================================================================
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei


(define (ttmed list nr sum)
  (cond ((null? list)(/ sum nr))
        ((= (counter-stare (car list)) 1) (ttmed (cdr list) (+ nr 1) (+ sum (counter-tt (car list)))))
        (else (ttmed (cdr list) nr sum))
        )
  )

(define (max-index list acc)
  (cond ((null? list) acc)
        ((<= acc (counter-index (car list)))(max-index (cdr list) (counter-index (car list))))
        (else (max-index (cdr list) acc))
        )   
  )
;compune lista de clienti la final inex.queue
(define (final counters acc)
  (if (null? counters)
      acc
      (match (car counters)
        [(counter index tt et queue stare)
         (if (queue-empty? queue)
             (final (cdr counters)  acc)
             (final (cdr counters) (append acc (list (cons index queue))))
             )
         ]
        )
      )
  )

 


(define (serve requests fast-counters slow-counters)
  (serve1 requests fast-counters slow-counters '())
  )

(define (serve1 requests fast-counters slow-counters client)
  
  (if (null? requests)
      (cons client (final (append fast-counters slow-counters) '()))
      
      (match (car requests)
        
        [(list string numar)
         (cond
           
           ((equal? string 'ensure) (if( < numar (ttmed (append fast-counters slow-counters) 0 0) )
                (serve1 requests fast-counters (append slow-counters (list (empty-counter (+ 1(max-index (append fast-counters slow-counters) 0))))) client)
                (serve1 (cdr requests) fast-counters slow-counters client)
                ))
           
           ((equal? string 'close)(serve1 (cdr requests) (update (closeC 0) fast-counters numar)
                                             (update (closeC 0) slow-counters  numar) client))
           
           (else (if (< numar (+ ITEMS 1))
                     (serve1 (cdr requests) (update (add-to-counter string numar) fast-counters (car (min-tt (append fast-counters slow-counters))))
                             (update (add-to-counter string numar) slow-counters (car (min-tt (append fast-counters slow-counters))))
                             client)
            
                     (serve1 (cdr requests) fast-counters
                             (update (add-to-counter string numar) slow-counters (car (min-tt slow-counters)))
                             client)
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
       [(counter index tt et queue stare )
        (cond
          ((<= tt minutes)(struct-copy counter C [index index] [tt 0] [et 0] [queue empty-queue] [stare stare]))
          ((> et minutes)(struct-copy counter C [index index] [tt (- tt minutes)] [et (- et minutes)] [queue queue] [stare stare]))
          ((= et minutes)(struct-copy counter C [index index] [tt (- tt minutes)] [et (cdr (top (dequeue queue)))] [queue (dequeue queue)] [stare stare]))
          (else (elimina-rec (struct-copy counter C [index index] [tt (- tt minutes)] [et et] [queue queue] [stare stare]) minutes))
          )
     ]
    )
   )
  )
; functie care modifica coada de clienti
(define (elimina-rec C minute)
  (match C
    [(counter index tt et queue stare)
        (cond
          ((= minute 0) C)
          ((> et minute)(struct-copy counter C [index index] [tt tt] [et (- et minute)] [queue queue] [stare stare]))
          ((= et minute)(struct-copy counter C [index index] [tt tt] [et (cdr (top (dequeue queue)))] [queue (dequeue queue)] [stare stare]))
          ((> minute et)(elimina-rec (struct-copy counter C [index index] [tt tt] [et (cdr (top (dequeue queue)))] [queue (dequeue queue)] [stare stare]) (- minute et)))
          )
        ]
    )
  )

;functie care  actualizeaza casele cu 1 minute si actualizeaza lista de clienti plecati
 (define (au-plecat counters minute acc)
   (cond
     ((= minute 1)(pleaca counters acc))
     (else (au-plecat (update-all (trece-timpul 1) counters '()) (- minute 1)
                      (pleaca counters acc)))
     )
   )

;functie care verifica ce clienti pleaca
 (define (pleaca counters acc)
   (if (null? counters)
       acc
       (match (car counters)
         [(counter index tt et queue stare)
          (cond
            ((queue-empty? queue)(pleaca (cdr counters) acc))
            ((= et 1) (pleaca (cdr counters) (append acc (list (cons index (car (top queue)))))))
            (else (pleaca (cdr counters) acc))
            )
          ]
         )
       )
    )
   

        
