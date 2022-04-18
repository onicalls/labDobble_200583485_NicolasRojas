#lang racket
(provide (all-defined-out))

;función de ejemplo para la selección aleatoria de elementos desde un conjunto, asignación aleatoria de cartas a ;jugadores, ordenamiento aleatorio de cartas en la pila, etc. Esta función garantiza transparencia referencial. Puede crear su propia función o usar esta.
(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

;Función que recorta una lista.
;Dominio: lista de elementos (list) X posI (int) X posF (int)
;Recorrido: lista recortada (list)
;Tipo de recursión: natural
;Ejemplo: (recortaLista elementsSet 1 3)
(define recortaLista (lambda (lista numCont numRecorte)
                       (if (and (< numCont (+ numRecorte 1)) (not (null? lista)))
                           (recortaLista (cdr lista) (+ numCont 1) numRecorte)
                           lista)))

;Función que obtiene un dato de una lista.
;Dominio: lista de elementos (list) X posI (int) X posF (int) X lista auxiliar (list)
;Recorrido: lista recortada (list)
;Tipo de recursión: natural
;Ejemplo: (obtenerDato elementsSet 1 3 elementsSet)
(define obtenerDato (lambda (lista numCont numRecorte aux)
                       (if (and (< numCont numRecorte) (not (null? lista)))
                           (obtenerDato (cdr lista) (+ numCont 1) numRecorte aux)
                           (if (null? lista)
                               (append (list (car aux)) (list (obtenerDato aux (+ numCont 1) numRecorte aux)))
                               (car lista))
                           )))

;Función que crea un conjunto de cartar a partir del algoritmo de Dobble.
;Dominio: lista de elementos (list) X cantidad de elementos (int) X contador (int) X lista carta (list) X lista cartas (list)
;Recorrido: lista de cartas generadas (list)
;Tipo de recursión: natural
;Ejemplo: (creaConjunto elementsSet 3 1 null null)
(define creaConjunto (lambda (elements nElementCards nCont card cards)
                       (define for1 (lambda (elements nElementCards nCont card)
                                      (if (or (< nCont nElementCards) (= nCont nElementCards))
                                          (for1 (cdr elements) nElementCards (+ nCont 1) (cons (car elements) card))
                                          (reverse card)
                                          )))
                       (define for2 (lambda (elements nElementCards nCont card cards)
                                      (define for3 (lambda (elements nElementCards nContJ nContW card)
                                                     (if (< nContW nElementCards)
                                                         (for3 elements nElementCards nContJ (+ nContW 1) (cons (obtenerDato elements (+ nContJ 1) (+ (* nElementCards nContJ) (+ nContW 1)) elements) card))
                                                         (reverse card)
                                                         )))
                                      (if (< nCont nElementCards)
                                          (for2 elements nElementCards (+ nCont 1) card (cons (for3 elements nElementCards nCont 1 (cons (car elements) card)) cards))
                                          (reverse cards)
                                          )))
                       (define for4 (lambda (elements nElementCards nCont card cards)
                                      (define for5 (lambda (elements nElementCards nContI nContJ card cards)
                                                     (define for6 (lambda (elements nElementCards nContI nContJ nContW card)
                                                                    (if (< nContW nElementCards)
                                                                        (for6 elements nElementCards nContI nContJ (+ nContW 1) (cons (obtenerDato elements (+ nContW 1) (+ (+ nElementCards 2) (* nElementCards (- nContW 1)) (modulo (+ (* (- nContI 1) (- nContW 1)) (- nContJ 1)) (- nElementCards 1))) elements) card))
                                                                        (reverse card)
                                                                        )))
                                                     (if (< nContJ nElementCards)
                                                         (for5 elements nElementCards nContI (+ nContJ 1) card (cons (for6 elements nElementCards nContI nContJ 1 (cons (obtenerDato elements 1 (+ 1 nContI) elements) card)) cards))
                                                         (reverse cards)
                                                         )))
                                      (if (< nCont nElementCards)
                                          (for4 elements nElementCards (+ nCont 1) card (reverse (for5 elements nElementCards nCont 1 card cards)))
                                          (reverse cards)
                                          )))
                       (append (cons (for1 elements nElementCards nCont card) (for2 elements nElementCards nCont card cards)) (for4 elements nElementCards nCont card cards))
                       ))

;Función que limita la cantidad de cartas que debe tener un conjunto
;Dominio: lista de cartas (list) X nueva lista de cartas (list) X contador (int) X número de límite (int)
;Recorrido: lista de cartas limitadas (list)
;Tipo de recursión: natural
;Ejemplo: (creaConjunto elementsSet 3 1 null null)
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
                                    (validaDiferencias (cdr card))))))

;Ejemplo: (validaElementoIgual (list -1 2 3))
(define validaElementoIgual (lambda (card)
                              (if (null? card)
                                  #f
                                  (if (eq? (car card) -1)
                                      #t
                                      (validaElementoIgual (cdr card))))))

;Ejemplo: (comparaCartas (list 1 2 3) (list 1 4 5) null 0)
(define comparaCartas (lambda (card card2 acum nAcum)
                        (if (null? card)
                            (and (validaElementoIgual (reverse acum)) (validaDiferencias (reverse acum)))
                            (if (memq (car card) card2)
                                (comparaCartas (cdr card) card2 (cons -1 acum) nAcum)
                                (comparaCartas (cdr card) card2 (cons (+ nAcum 1) acum) (+ nAcum 1)))
                            )))

(provide (all-defined-out))