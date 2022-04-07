#lang racket

;función de ejemplo para la selección aleatoria de elementos desde un conjunto, asignación aleatoria de cartas a ;jugadores, ordenamiento aleatorio de cartas en la pila, etc. Esta función garantiza transparencia referencial. Puede crear su propia función o usar esta.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define modFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

(define elements (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "V" "W" "X" "Y" "Z"))
;;TDA cards
;;Descripción: TDA que contiene los elementos de las cartas
;;Dom: cartas (list) X jugador (str) 
;;Rec: lista de cartas de un jugador (list)
;;Ejemplo: (tdaCards (list (list "A" "B" "C") (list "A" "C" "E")) "juan")
(define tdaCards (lambda (cards player)
          (list cards player)))

;Ejemplo: (recortaLista elements 1 3)
(define recortaLista (lambda (lista numCont numRecorte)
                       (if (and (< numCont (+ numRecorte 1)) (not (null? lista)))
                           (recortaLista (cdr lista) (+ numCont 1) numRecorte)
                           lista)))

;Ejemplo: (obtenerDato elements 1 3)
(define obtenerDato (lambda (lista numCont numRecorte)
                       (if (and (< numCont numRecorte) (not (null? lista)))
                           (obtenerDato (cdr lista) (+ numCont 1) numRecorte)
                           (if (null? lista)
                               (list "A")
                               (car lista))
                           )))

;Ejemplo: (for1 elements 3 1 null)
(define for1 (lambda (elements nElementCards nCont card)
                       (if (or (< nCont (+ nElementCards 1)) (= nCont (+ nElementCards 1)))
                           (for1 (cdr elements) nElementCards (+ nCont 1) (cons (car elements) card))
                           (reverse card)
                           )))

;Ejemplo: (for3 elements 3 1 1 null)
(define for3 (lambda (elements nElementCards nContJ nContW card)
                       (if (< nContW (+ nElementCards 1))
                           (for3 elements nElementCards nContJ (+ nContW 1) (cons (obtenerDato elements 1 (+ (* nElementCards nContJ) (+ nContW 1))) card))
                           (reverse card)
                           )))

;Ejemplo: (for2 elements 3 1 null null)
(define for2 (lambda (elements nElementCards nCont card cards)
                       (if (< nCont (+ nElementCards 1))
                             (for2 elements nElementCards (+ nCont 1) card (cons (for3 elements nElementCards nCont 1 (cons (car elements) card)) cards))
                             (reverse cards)
                       )))


;Ejemplo: (for4 elements 3 1 null null)
(define for4 (lambda (elements nElementCards nCont card cards)
                       (if (< nCont (+ nElementCards 1))
                             (for4 elements nElementCards (+ nCont 1) card (reverse (for5 elements nElementCards nCont 1 card cards)))
                             (reverse cards)
                       )))

;Ejemplo: (for5 elements 3 1 1 null null)
(define for5 (lambda (elements nElementCards nContI nContJ card cards)
                       (if (< nContJ (+ nElementCards 1))
                             (for5 elements nElementCards nContI (+ nContJ 1) card (cons (for6 elements nElementCards nContI nContJ 1 (cons (obtenerDato elements 1 (+ 1 nContI)) card)) cards))
                             (reverse cards)
                       )))

;Ejemplo: (for6 elements 3 1 1 1 null)
(define for6 (lambda (elements nElementCards nContI nContJ nContW card)
                       (if (< nContW (+ nElementCards 1))
                           (for6 elements nElementCards nContI nContJ (+ nContW 1) (cons (obtenerDato elements 1 (+ (+ nElementCards 2) (* nElementCards (- nContW 1)) (modulo (+ (* (- nContI 1) (- nContW 1)) (- nContJ 1)) nElementCards))) card))
                           (reverse card)
                           )))

;Ejemplo: (creaConjunto elements 3 1 null null)
(define creaConjunto (lambda (elements nElementCards nCont card cards)
             (append (cons (for1 elements nElementCards nCont card) (for2 elements nElementCards nCont card cards)) (for4 elements nElementCards nCont card cards))
             ))

;Ejemplo: (limitaCartas elements 3 1 null null)
(define limitaCartas (lambda (cards newCards nCont limit)
                       (if (< limit 1)
                           cards
                           (if (> nCont limit)
                                  (reverse newCards)
                                  (limitaCartas (cdr cards) (cons (car cards) newCards) (+ nCont 1) limit)))
                           ))
  
;;TDA cardsSet - Constructor
;;Descripción: Función constructora de conjuntos válidos de cartas para el juego Dobble.
;;Dom: elements (list) X numE (int) X maxC (int) X mdFn
;;Rec: lista de cartas con conjuntos válidos
;;Ejemplo: (cardsSet elements 3 5 null)
;;Ejemplo: (cardsSet elements 3 -1 null)
(define cardsSet (lambda (elements numE maxC mdFN)
                  (limitaCartas (creaConjunto elements numE 1 null null) null 1 maxC)))
                   