#lang racket

;función de ejemplo para la selección aleatoria de elementos desde un conjunto, asignación aleatoria de cartas a ;jugadores, ordenamiento aleatorio de cartas en la pila, etc. Esta función garantiza transparencia referencial. Puede crear su propia función o usar esta.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define modFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

;;TDA cards
;;Descripción: TDA que contiene los elementos de las cartas
;;Dom: cartas (list) X jugador (str) 
;;Rec: lista de cartas de un jugador (list)
;;Ejemplo: (tdaCards (list (list "A" "B" "C") (list "A" "C" "E")) "juan")
(define tdaCards (lambda (cards player)
          (list cards player)))

;Ejemplo: (recortaLista (list "A" "B" "C" "D" "E" "F" "G") 1 3)
(define recortaLista (lambda (lista numCont numRecorte)
                       (if (< numCont (+ numRecorte 1))
                           (recortaLista (cdr lista) (+ numCont 1) numRecorte)
                           lista)))

;Ejemplo: (obtenerDato (list "A" "B" "C" "D" "E" "F" "G") 1 3)
(define obtenerDato (lambda (lista numCont numRecorte)
                       (if (< numCont numRecorte)
                           (obtenerDato (cdr lista) (+ numCont 1) numRecorte)
                           lista)))

;Ejemplo: (for1 (list "A" "B" "C" "D" "E" "F" "G") 3 1 null)
(define for1 (lambda (elements nElementCards nCont card)
                       (if (or (< nCont (+ nElementCards 1)) (= nCont (+ nElementCards 1)))
                           (for1 (cdr elements) nElementCards (+ nCont 1) (cons (car elements) card))
                           (reverse card)
                           )))

;Ejemplo: (for3 (list "A" "B" "C" "D" "E" "F" "G") 3 1 null)
(define for3 (lambda (elements nElementCards nContJ nContW card)
                       (if (or (< nContW (+ nElementCards 1)) (= nContW (+ nElementCards 1)))
                           (for3 (recortaLista elements 1 (+ (* nElementCards nContJ) (+ nContW 1))) nElementCards nContJ (+ nContW 1) (cons (obtenerDato elements 1 (+ (* nElementCards nContJ) (+ nContW 1))) card))
                           (reverse card)
                           )))

;Ejemplo: (for2 (list "A" "B" "C" "D" "E" "F" "G") 3 1 null)
(define for2 (lambda (elements nElementCards nCont card)
                       (cond ((or (< nCont (+ nElementCards 1)) (= nCont (+ nElementCards 1)))
                              (cons 1 card)
                              (for3 elements nElementCards nCont 1 card))
                       (else card))))
                              

;;TDA cardsSet - Constructor
;;Descripción: Función constructora de conjuntos válidos de cartas para el juego Dobble.
;;Dom: elements (list) X numE (int) X maxC (int) X mdFn
;;Rec: lista de cartas con conjuntos válidos
;;Ejemplo: (cardsSet (list "A" "B" "C" "D" "E" "F" "G") 3 5 modFn)
;(define cardsSet (lambda (elements numE maxC mdFn)
                   
                   