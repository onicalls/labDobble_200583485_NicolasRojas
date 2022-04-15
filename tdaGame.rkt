#lang racket
(require "tdaCards.rkt")
(provide (all-defined-out))

;;TDA game
;;Descripción: TDA que contiene los elementos de la partida.
;;Dom: lista de jugadores (list) X set de cartas (list) X modo de juego (fn) X funcion de random (fn)
;;Rec: lista de con los elementos del juego (list)
;;Ejemplo: (game 4 (cardsSet elements 3 5 null) null null)
;;(define game1 (game numPlayers dobbleSet0 null null))

(define game (lambda (nPlayers cardsSet mode rndFn)
          (list null null (make-list 7 "") cardsSet null )))


;;TDA game
;;Descripción: TDA que contiene los elementos de la partida
;;Dom: lista de jugadores (list) X set de cartas (list) X jugada (list) X turno (int) X estado del juego (int)
;;Rec: lista de con los elementos del juego (list)
;;Ejemplo: (game 4 (cardsSet elements 3 5 null) null null)
;;(define game1 (game numPlayers dobbleSet0 null null))
