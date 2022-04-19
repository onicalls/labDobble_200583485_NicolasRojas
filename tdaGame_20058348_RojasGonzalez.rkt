#lang racket
;getAreaJuego
;Descripción: Función que retorna una lista con el area de juego
;Dom: game (list)
;Rec: area de juego (list)
(define getAreaJuego (lambda (game)
                       (car game)))

;getCartasJugadores
;Descripción: Función que retorna una lista con las cartas de los jugadores
;Dom: game (list)
;Rec: cartas del jugador (list)
(define getCartasJugadores (lambda (game)
                       (cadr game)))

;getPlayers
;Descripción: Función que retorna una lista con jugadores
;Dom: game (list)
;Rec: players (list)
;Ejemplo (getPlayers (game 4 (cardsSet elements 3 5 null) null null))
(define getPlayers (lambda (game)
  (caddr game)))

;getCartasDisponibles
;Descripción: Función que retorna una lista con las cartas disponibles
;Dom: game (list)
;Rec: mazo de cartas (list)
(define getCartasDisponibles (lambda (game)
                       (cadddr game)))
;getEstadoJuego
;Descripción: Función que retorna el estado de la partida
;Dom: game (list)
;Rec: estado de la partida (str)
(define getEstadoJuego (lambda (game)
                       (car (cddddr game))))
;getTurno
;Descripción: Función que retorna el nombre del jugador al que corresponde el turno
;Dom: game (list)
;Rec: player (str)
(define getTurno (lambda (game)
                       (cadr (cddddr game))))
;getPuntuacion
;Descripción: Función que retorna una lista con las puntuaciones de los jugadores
;Dom: game (list)
;Rec: puntuaciones (list)
(define getPuntuacion (lambda (game)
                       (caddr (cddddr game))))


(provide (all-defined-out))
;(define play (lambda (game function)
