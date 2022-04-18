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
               (if (eq? mode stackMode)
                   (list (mode cardsSet) null (make-list nPlayers "") cardsSet null "" null)
                   null
                   )))

;getAreaJuego
(define getAreaJuego (lambda (game)
                       (car game)))

;getCartasJugadores
(define getCartasJugadores (lambda (game)
                       (cadr game)))

;getPlayers
;Descripción: Función que retorna la lista con los jugadores
;Dom: game (list)
;Rec: players (list)
;Ejemplo (getPlayers (game 4 (cardsSet elements 3 5 null) null null))
(define getPlayers (lambda (game)
  (caddr game)))

;getCartasDisponibles
(define getCartasDisponibles (lambda (game)
                       (cadddr game)))
;getEstadoJuego
(define getEstadoJuego (lambda (game)
                       (car (cddddr game))))
;getTurno
(define getTurno (lambda (game)
                       (cadr (cddddr game))))
;getPuntuacion
(define getPuntuacion (lambda (game)
                       (caddr (cddddr game))))

;;TDA game - stackMode
;;Descripción: Función que permite retirar y voltear las dos cartas superiores del stack de cartas en el juego y las dispone en el área de juego.
;;Dom: cartas del mazo (list)
;;Rec: retira dos cartas del mazo (list)
;;Ejemplo: (register "juan" (game 4 (cardsSet elements 3 5 null) null null))
;;(define game1 (game numPlayers dobbleSet0 null null))
(define stackMode (lambda (cardsSet)
                    (if (null? cardsSet)
                        cardsSet
                        (if (null? (car cardsSet))
                            cardsSet
                            (append (list (car cardsSet) (cadr cardsSet)))))))

;;TDA game - register
;;Descripción: TDA que contiene los elementos de la partida
;;Dom: lista de jugadores (list) X set de cartas (list) X jugada (list) X turno (int) X estado del juego (int)
;;Rec: 
;;Ejemplo: (register "juan" (game 4 (cardsSet elements 3 5 null) null null))
;;(define game1 (game numPlayers dobbleSet0 null null))
(define register (lambda (user game)
                   (define register-cola (lambda (name players)
                                           (if (null? players)
                                               players
                                               (if (eq? (car players) "")
                                                   (cons user (cdr players))
                                                   (cons (car players) (register-cola name (cdr players)))))))
                   (list (getAreaJuego game) (getCartasJugadores game) (register-cola user (getPlayers game)) (getCartasDisponibles game) (getEstadoJuego game) (getTurno game) (getPuntuacion game))))

;;TDA game - whoseTurnIsIt?
;;Descripción: Función que retorna el usuario a quién le corresponde jugar en el turno.
;;Dom: argumentos del juego (game)
;;Rec: nombre del jugador del turno (str)
;;Ejemplo: (register "juan" (game 4 (cardsSet elements 3 5 null) null null))
;;(define game1 (game numPlayers dobbleSet0 null null))
(define whoseTurnIsIt? (lambda (game)
                         (if (eq? (cadr (cddddr game)) "")
                             (car (caddr game))
                             (cadr (cddddr game)))))



(define game0 (game 2 dobbleSet0 stackMode null))
(define game1 (register "juan" game0))
(define game2 (register "pato" game1))
(whoseTurnIsIt? game2)