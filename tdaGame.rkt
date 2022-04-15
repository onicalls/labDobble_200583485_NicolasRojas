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
          (list null null (make-list nPlayers "") cardsSet null)))

;getPlayers
;Descripción: Función que retorna la lista con los jugadores
;Dom: game (list)
;Rec: players (list)
;Ejemplo (getPlayers (game 4 (cardsSet elements 3 5 null) null null))
(define getPlayers (lambda (game)
  (caddr game)))


;;TDA game - register
;;Descripción: TDA que contiene los elementos de la partida
;;Dom: lista de jugadores (list) X set de cartas (list) X jugada (list) X turno (int) X estado del juego (int)
;;Rec: lista de con los elementos del juego (list)
;;Ejemplo: (register "juan" (game 4 (cardsSet elements 3 5 null) null null))
;;(define game1 (game numPlayers dobbleSet0 null null))
(define register (lambda (user game)
                   (define register-cola (lambda (name players)
                                           (if (null? players)
                                               players
                                               (if (eq? (car players) "")
                                                   (cons user (cdr players))
                                                   (cons (car players) (register-cola name (cdr players)))))))
                   (list (car game) (cadr game) (register-cola user (getPlayers game)) (cadddr game) (car (cddddr game)))))

(define game0 (game 4 (cardsSet elements 3 5 null) null null))
(define game1 (register "juan" game0))
(define game2 (register "pato" game1))