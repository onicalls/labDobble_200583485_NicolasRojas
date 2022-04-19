#lang racket
(require "tdaCards_20058348_RojasGonzalez.rkt")
(require "tdaGame_20058348_RojasGonzalez.rkt")

;;TDA cardsSet - Constructor
;;Descripción: Función constructora de conjuntos válidos de cartas para el juego Dobble.
;;Dom: elements (list) X numE (int) X maxC (int) X mdFn
;;Rec: lista de cartas con conjuntos válidos
;;Tipo de recursión: cola
;;Ejemplo: (cardsSet elements 3 5 null)
;;Ejemplo: (cardsSet elements 3 -1 null)
(define cardsSet (lambda (elements numE maxC mdFn)
                   (define makeShuffle (lambda (cards veces resultado)
                                         (if (eq? veces 0)
                                             resultado
                                             (makeShuffle cards (- veces 1) (shuffle cards)))))
                   (makeShuffle (limitaCartas (creaConjunto elements numE 1 null null) null 1 maxC) (modulo (mdFn numE) numE) null)))

;;TDA cardsSet - dobble?
;;Descripción: Función que permite verificar si el conjunto de cartas en el conjunto corresponden a un conjunto válido.
;;Dom: cards (list) 
;;Rec: true or false (boolean)
;;Tipo de recursión: natural
;;Ejemplo: (dobble? (cardsSet elements 3 5 null))
(define dobble? (lambda (cards)
                  (if (null? (cdr cards))
                      #t
                      (if (validaDiferencias (car cards))
                          (if (comparaCartas (car cards) (car (cdr cards)) null 0)
                              (dobble? (cdr cards))
                              #f)
                          #f)
                      )))

;;TDA cardsSet - numCards
;;Descripción: Función que permite determinar la cantidad de cartas en el set.
;;Dom: Mazo de Cartas (cardsSet) 
;;Rec: Número de cartas que hay en el mazo (int)
;;Ejemplo: (numCards (cardsSet elements 3 5 null))
(define numCards (lambda (cards)
                   (length cards)))

;;TDA cardsSet - nthCard
;;Descripción: función que obtiene la n-ésima (nth) carta desde el conjunto de cartas partiendo desde 0 hasta (totalCartas-1).
;;Dom: mazo de Cartas (cardsSet) X número de Carta (int)
;;Rec: carta (card)
;;Ejemplo: (nthCard (cardsSet elements 3 5 null) 0)
(define nthCard (lambda (cards int)
                  (list-ref cards int)))

;;TDA cardsSet - findTotalCards
;;Descripción: Función que a partir de una carta de muestra, determina la cantidad total de cartas que se deben producir para construir un conjunto válido.
;;Dom: carta (card)
;;Rec: cantidad de cartas (int)
;;Ejemplo: (findTotalCards (nthCard (cardsSet elements 3 5 null) 0))
(define findTotalCards (lambda (card)
                         (+ (* (- (numCards card) 1) (- (numCards card) 1)) (numCards card))))

;;TDA cardsSet - requiredElements
;;Descripción: Función que a partir de una carta de muestra, determina la cantidad total de elementos necesarios para poder construir un conjunto válido.
;;Dom: carta (card)
;;Rec: cantidad de elementos restantes (int)
;;Ejemplo: (requiredElements (nthCard (cardsSet elements 3 5 null) 0))
(define requieredElements (lambda (card)
                            (+ (* (- (numCards card) 1) (- (numCards card) 1)) (numCards card))))
                                                                   
;;TDA cardsSet - missingCards
;;Descripción: Función que a partir de un conjunto de cartas retorna el conjunto de cartas que hacen falta para que el set sea válido.
;;Dom: mazo de cartas (cards)
;;Rec: cartas faltantes del mazo (cards)
;;Ejemplo: (missingCards (cardsSet elements 3 5 null))
(define missingCards (lambda (cards)
                       (define missingCola (lambda (cards elements aux)
                                             (if (and (= (numCards aux) (requieredElements (nthCard cards 0))) (dobble? aux))
                                                 aux
                                                 (missingCola cards elements (append cards (cardsSet elements (numCards (nthCard cards 0)) (- (requieredElements (nthCard cards 0)) (numCards cards)) randomFn))))))                         
                       (missingCola cards (shuffle (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")) cards)))

;;TDA cardsSet - cardsSet->string
;;Descripción: Función que convierte un conjunto de cartas a una representación basada en strings que posteriormente pueda visualizarse a través de la función display.
;;Dom: mazo de cartas (cards)
;;Rec: cartas del mazo (str)
;;Ejemplo: (cardsSet->string (cardsSet elements 3 5 null))
;(cardsSet->string dobbleSet0)
(define cardsSet->string (lambda (cards)
                           (define cardsStringCola (lambda (cards contador lista)
                                                     (if (null? cards)
                                                         lista
                                                         (cardsStringCola (cdr cards) (+ contador 1) (cons (string-append "Carta " (number->string contador) ": " (string-join (car cards) " ") "\n") lista)))))
                           (define displayCards (lambda (card cards)
                                                  (if (null? cards)
                                                      (display card)
                                                      (displayCards (cdr cards) (car cards)))))
                           (displayCards (reverse (cardsStringCola cards 1 null)) null)))

;;TDA game
;;Descripción: TDA que contiene los elementos de la partida.
;;Dom: numero de jugadores (int) X mazo de cartas (list) X modo de juego (fn) X funcion de random (fn)
;;Rec: lista de con los elementos del juego (list)
;;Ejemplo: (game 4 (cardsSet elements 3 5 null) null null)
;;(define game1 (game numPlayers dobbleSet0 null null))
(define game (lambda (nPlayers cardsSet mode rndFn)
               (if (eq? mode stackMode)
                   (list (mode cardsSet) null (make-list nPlayers "") cardsSet "" "" null)
                   null
                   )))

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
;;Descripción: Función para registrar a un jugador en un juego.
;;Dom: nombre del jugador a registrar (list) X datos de la partida (list)
;;Rec: 
;;Ejemplo: (register "juan" (game 4 (cardsSet elements 3 5 null) null null))
;;(define game1 (game numPlayers dobbleSet0 null null))
(define register (lambda (user game)
                   (define register-cola (lambda (name players)
                                           (if (null? players)
                                               players
                                               (if (eq? (car players) "")
                                                   (cons user (cdr players))
                                                   (if (eq? user (car players))
                                                       players
                                                       (cons (car players) (register-cola name (cdr players))))
                                                   ))))
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


;función de ejemplo para la selección aleatoria de elementos desde un conjunto, asignación aleatoria de cartas a ;jugadores, ordenamiento aleatorio de cartas en la pila, etc. Esta función garantiza transparencia referencial. Puede crear su propia función o usar esta.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                   )
  )

;EJEMPLOS

;EJEMPLO 1 
;(define elementsSet (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
;(define numPlayers 2)
;(define numElementsPerCard 3)
;(define maxCards 5)
;(define dobbleSet0 (cardsSet elementsSet numElementsPerCard maxCards randomFn))
;(dobble? dobbleSet0)
;(numCards dobbleSet0)
;(findTotalCards (nthCard dobbleSet0 1))
;(requieredElements (nthCard dobbleSet0 1))
;(missingCards dobbleSet0)
;(define dobbleSet1 (cardsSet elementsSet numElementsPerCard -1 randomFn))
;(define game1 (game numPlayers dobbleSet1 stackMode randomFn))
;(define game2 (register "juan" game1))
;(define game3 (register "juan" game2))
;(define game4 (register "pipo" game3))
;(define game5 (register "pana" game4))
;(cardsSet->string dobbleSet1)
;(whoseTurnIsIt? game5)

;EJEMPLO 2 
;(define elementsSet (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
;(define numPlayers 3)
;(define numElementsPerCard 4)
;(define maxCards 7)
;(define dobbleSet0 (cardsSet elementsSet numElementsPerCard maxCards randomFn))
;(dobble? dobbleSet0)
;(numCards dobbleSet0)
;(findTotalCards (nthCard dobbleSet0 3))
;(requieredElements (nthCard dobbleSet0 4))
;(missingCards dobbleSet0)
;(define dobbleSet1 (cardsSet elementsSet numElementsPerCard -1 randomFn))
;(define game1 (game numPlayers dobbleSet1 stackMode randomFn))
;(define game2 (register "que" game1))
;(define game3 (register "pasa" game2))
;(define game4 (register "pipo" game3))
;(define game5 (register "pi" game4))
;(cardsSet->string dobbleSet1)
;(whoseTurnIsIt? game5)

;EJEMPLO 3
;(define elementsSet (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
;(define numPlayers 4)
;(define numElementsPerCard 5)
;(define maxCards 3)
;(define dobbleSet0 (cardsSet elementsSet numElementsPerCard maxCards randomFn))
;(dobble? dobbleSet0)
;(numCards dobbleSet0)
;(findTotalCards (nthCard dobbleSet0 2))
;(requieredElements (nthCard dobbleSet0 2))
;(missingCards dobbleSet0)
;(define dobbleSet1 (cardsSet elementsSet numElementsPerCard -1 randomFn))
;(define game1 (game numPlayers dobbleSet1 stackMode randomFn))
;(define game2 (register "proyecto" game1))
;(define game3 (register "de" game2))
;(define game4 (register "lab" game3))
;(define game5 (register "uno" game4))
;(cardsSet->string dobbleSet1)
;(whoseTurnIsIt? game5)