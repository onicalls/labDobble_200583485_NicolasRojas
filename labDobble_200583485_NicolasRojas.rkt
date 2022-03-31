#lang racket
;función de ejemplo para la selección aleatoria de elementos desde un conjunto, asignación aleatoria de cartas a ;jugadores, ordenamiento aleatorio de cartas en la pila, etc. Esta función garantiza transparencia referencial. Puede crear su propia función o usar esta.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define modFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

;conjunto de elementos/símbolos con los que se podrían generar ls cartas. Esta lista es solo un ejemplo. En la práctica podría albergar cualquier ;tipo de elemento y de cualquier tipo de dato.
(define elementsSet (list 0 1 2 3 4 5 6 7 8 9 ))

;cantidad de jugadores de la partida
(define numPlayers 4)

;cantidad de elementos requeridos para cada carta
(define numElements 3)

;máxima cantidad de cartas a generar
(define maxCards 5)  ;para generar la cantidad necesaria de cartas para un set válido



;;TDA cards
;;Descripción: TDA que contiene los elementos de las cartas
;;Dom: cartas (list) X jugador (str) 
;;Rec: lista de cartas de un jugador (list)
;;Ejemplo: (tdaCards (list 1 3 4 2 5 7) "juan")
(define tdaCards (lambda (cards player)
          (list cards player)))

;;TDA cardsSet - Constructor
;;Descripción: Función constructora de conjuntos válidos de cartas para el juego Dobble.
;;Dom: elements (list) X numE (int) X maxC (int) X mdFn
;;Rec: lista de cartas con conjuntos válidos
;;Ejemplo: (cardsSet elementsSet numElements maxCards modFn)
(define cardsSet (lambda (elements numE maxC mdFn)
                   
                   