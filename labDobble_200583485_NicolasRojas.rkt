#lang racket
(require "tdaCards.rkt")
(require "tdaGame.rkt")


;;TDA cardsSet - Constructor
;;Descripción: Función constructora de conjuntos válidos de cartas para el juego Dobble.
;;Dom: elements (list) X numE (int) X maxC (int) X mdFn
;;Rec: lista de cartas con conjuntos válidos
;;Ejemplo: (cardsSet elements 3 5 null)
;;Ejemplo: (cardsSet elements 3 -1 null)
(define cardsSet (lambda (elements numE maxC mdFn)
                   (define makeShuffle (lambda (cards veces resultado)
                                         (if (eq? veces 0)
                                             resultado
                                             (makeShuffle cards (- veces 1) (shuffle cards)))))
                   (makeShuffle (limitaCartas (creaConjunto elements numE 1 null null) null 1 maxC) (modulo (mdFn numE) numE) null)))
                   ;(limitaCartas (creaConjunto elements numE 1 null null) null 1 maxC)))


;;TDA cardsSet - dobble?
;;Descripción: Función que permite verificar si el conjunto de cartas en el conjunto corresponden a un conjunto válido.
;;Dom: cards (list) 
;;Rec: true or false (boolean)
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
;;Descripción: Función que obtiene la n-ésima (nth) carta desde el conjunto de cartas partiendo desde 0 hasta (totalCartas-1).
;;Dom: Mazo de Cartas (cardsSet) X Número de Carta (int)
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
                   (list (mode cardsSet) null (make-list nPlayers "") cardsSet null "" null)
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



;EJEMPLOS
;conjunto de elementos/símbolos con los que se podrían generar ls cartas. Esta lista es solo un ejemplo. En la práctica podría albergar cualquier ;tipo de elemento y de cualquier tipo de dato.
(define elementsSet (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
;cantidad de jugadores de la partida
(define numPlayers 4)

;cantidad de elementos requeridos para cada carta
(define numElementsPerCard 4)

;máxima cantidad de cartas a generar
(define maxCards 5)  ;para generar la cantidad necesaria de cartas para un set válido

;se genera un conjunto de cartas incompleto
(define dobbleSet0 (cardsSet elementsSet numElementsPerCard maxCards randomFn))

;se consulta si el set generado es un set válido del juego dobble
(dobble? dobbleSet0)

;retorna la cantidad de cartas en el set
(numCards dobbleSet0)

;retorna la máxima cantidad de cartas que se podrían generar a partir de la información recogida de una carta
(findTotalCards (nthCard dobbleSet0 1))
;para los valores anteriores el resultado debería ser 7 

;retorna la cantidad de elementos que se requieren generar a partir de la información recogida de una carta
(requieredElements (nthCard dobbleSet0 1))
;para los valores anteriores el resultado debería ser 7 

;retorna las cartas que faltan
(missingCards dobbleSet0)
;;para los valores anteriores me debería devolver las 2 cartas que faltan para completar las 7

;generar un set completo de cartas lo que queda estipulado con maxCards = -1
(define dobbleSet1 (cardsSet elementsSet numElementsPerCard -1 randomFn))
;para los valores anteriores, esto debe dar como resultado un conjunto con las 7 cartas

;crea una partida del juego con el conjunto de cartas, en modo tradicional y con la función aleatoria
;definida para la asignación de cartas y ordenamiento en la pila.
(define game1 (game numPlayers dobbleSet1 stackMode randomFn))

;registra 3 usuarios con nombres de usuario diferentes
(define game2 (register "user1" game1))
(define game3 (register "user2" game2))
(define game4 (register "user3" game3))

;intenta registrar al usuario “user3” que ya fue registrado
(define game5 (register "user3" game4))

(define game6 (register "user4" game5))

;intenta registrar a un quinto jugador
(define game7 (register "user5" game5))

;inicia una partida
(cardsSet->string dobbleSet1)

;retorna el nombre de usuario a quien corresponde el turno
(whoseTurnIsIt? game7)

