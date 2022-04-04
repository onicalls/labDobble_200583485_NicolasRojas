#lang racket
;;TDA game
;;Descripción: TDA que contiene los elementos de la partida
;;Dom: lista de jugadores (list) X set de cartas (list) X jugada (list) X turno (int) X estado del juego (int)
;;Rec: lista de con los elementos del juego (list)
;;Ejemplo: (tdaCards (list 1 3 4 2 5 7) "juan")
(define tdaGame (lambda (cards player)
          (list cards player)))