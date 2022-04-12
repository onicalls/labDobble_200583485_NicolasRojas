#lang racket

;función de ejemplo para la selección aleatoria de elementos desde un conjunto, asignación aleatoria de cartas a ;jugadores, ordenamiento aleatorio de cartas en la pila, etc. Esta función garantiza transparencia referencial. Puede crear su propia función o usar esta.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

(define elements (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "V" "W" "X" "Y" "Z"))
(define nelements (list 1 2 3 4 5 6 7 8 9))

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

;Ejemplo: (obtenerDato elements 1 3 elements)
(define obtenerDato (lambda (lista numCont numRecorte aux)
                       (if (and (< numCont numRecorte) (not (null? lista)))
                           (obtenerDato (cdr lista) (+ numCont 1) numRecorte aux)
                           (if (null? lista)
                               (append (list (car aux)) (list (obtenerDato aux (+ numCont 1) numRecorte aux)))
                               (car lista))
                           )))

;Ejemplo: (for1 elements 3 1 null)
(define for1 (lambda (elements nElementCards nCont card)
                       (if (or (< nCont nElementCards) (= nCont nElementCards))
                           (for1 (cdr elements) nElementCards (+ nCont 1) (cons (car elements) card))
                           (reverse card)
                           )))

;Ejemplo: (for3 elements 3 1 1 null)
(define for3 (lambda (elements nElementCards nContJ nContW card)
                       (if (< nContW nElementCards)
                           (for3 elements nElementCards nContJ (+ nContW 1) (cons (obtenerDato elements 1 (+ (* nElementCards nContJ) (+ nContW 1)) elements) card))
                           (reverse card)
                           )))

;Ejemplo: (for2 elements 3 1 null null)
(define for2 (lambda (elements nElementCards nCont card cards)
                       (if (< nCont nElementCards)
                             (for2 elements nElementCards (+ nCont 1) card (cons (for3 elements nElementCards nCont 1 (cons (car elements) card)) cards))
                             (reverse cards)
                       )))


;Ejemplo: (for4 elements 3 1 null null)
(define for4 (lambda (elements nElementCards nCont card cards)
                       (if (< nCont nElementCards)
                             (for4 elements nElementCards (+ nCont 1) card (reverse (for5 elements nElementCards nCont 1 card cards)))
                             (reverse cards)
                       )))

;Ejemplo: (for5 elements 3 1 1 null null)
(define for5 (lambda (elements nElementCards nContI nContJ card cards)
                       (if (< nContJ nElementCards)
                             (for5 elements nElementCards nContI (+ nContJ 1) card (cons (for6 elements nElementCards nContI nContJ 1 (cons (obtenerDato elements 1 (+ 1 nContI) elements) card)) cards))
                             (reverse cards)
                       )))

;Ejemplo: (for6 elements 3 1 1 1 null)
(define for6 (lambda (elements nElementCards nContI nContJ nContW card)
                       (if (< nContW nElementCards)
                           (for6 elements nElementCards nContI nContJ (+ nContW 1) (cons (obtenerDato elements 1 (+ (+ nElementCards 2) (* nElementCards (- nContW 1)) (modulo (+ (* (- nContI 1) (- nContW 1)) (- nContJ 1)) nElementCards)) elements) card))
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

;Ejemplo: (validaDiferencias (list 1 2 3))
(define validaDiferencias (lambda (card)
                            (if (null? card)
                                #t
                                (if (memq (car card) (cdr card))
                                    #f
                                    (validaDiferencias (cdr card))
                                    ))))

;Ejemplo: (comparaCartas (list 1 2 3) (list 1 4 5) null 0)
(define comparaCartas (lambda (card card2 acum nAcum)
                        (if (null? card)
                            (validaDiferencias (reverse acum))
                            (if (memq (car card) card2)
                                (comparaCartas (cdr card) card2 (cons -1 acum) nAcum)
                                (comparaCartas (cdr card) card2 (cons (+ nAcum 1) acum) (+ nAcum 1)))
                            )))
                        
                            
;;TDA cardsSet - Constructor
;;Descripción: Función constructora de conjuntos válidos de cartas para el juego Dobble.
;;Dom: elements (list) X numE (int) X maxC (int) X mdFn
;;Rec: lista de cartas con conjuntos válidos
;;Ejemplo: (cardsSet elements 3 5 null)
;;Ejemplo: (cardsSet elements 3 -1 null)
(define cardsSet (lambda (elements numE maxC mdFN)
                  (limitaCartas (creaConjunto elements numE 1 null null) null 1 maxC)))


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
(define requiredElements (lambda (card)
                (+ (* (- (numCards card) 1) (- (numCards card) 1)) (numCards card))))

;;TDA cardsSet - missingCards
;;Descripción: Función que a partir de un conjunto de cartas retorna el conjunto de cartas que hacen falta para que el set sea válido.
;;Dom: mazo de cartas (cards)
;;Rec: cartas faltantes del mazo (cards)
;;Ejemplo: (missingCards (cardsSet elements 3 5 null))
(define missingCards (lambda (cards)
                (if (= (numCards cards) (requiredElements (nthCard cards 0)))
                    cards
                    (append cards(cardsSet (build-list (* (requiredElements (nthCard cards 0)) 2) values) (numCards (nthCard cards 0)) (- (requiredElements (nthCard cards 0)) (numCards cards)) null)))))

                    
;función de ejemplo para la selección aleatoria de elementos desde un conjunto, asignación aleatoria de cartas a ;jugadores, ordenamiento aleatorio de cartas en la pila, etc. Esta función garantiza transparencia referencial. Puede crear su propia función o usar esta.
;conjunto de elementos/símbolos con los que se podrían generar ls cartas. Esta lista es solo un ejemplo. En la práctica podría albergar cualquier ;tipo de elemento y de cualquier tipo de dato.
(define elementsSet (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "V" "W" "X" "Y" "Z"))

;cantidad de jugadores de la partida
(define numPlayers 3)

;cantidad de elementos requeridos para cada carta
(define numElementsPerCard 3)

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
