#|
     Proyecto #2, Paradigmas de Programacion
     Andres Romero Hernandez
     Estefania Murillo Romero
|#

#lang racket

;Despliegue de polinomios, la funcion mostrara el polinomio utilizando
;la notacion estandar

(define display-p
  (lambda (polinomio) ;Ej: '(0 2 3 1) = 2𝑥+3𝑥^2+𝑥^3
      (string-append* "" (transformar polinomio 0)))) ;Hace append de cada uno de los resultados en un solo string


(define transformar                 ;Procesa la lista y la convierte en su polinomio correspondiente.
  (lambda (polinomio potencia)      ;Recibe 2 parametros, una lista y la potencia actual.
    (if (null? polinomio) '()       ;Si la lista es nula devuelva una lista vacia (Caso base)
        (if (= (car polinomio) 0) (transformar (cdr polinomio) (+ potencia 1)) ;Valida que numero no sea cero, de otra forma no se toma en cuenta.
        (cons ;Else:
         (cond
           [(= potencia 0) (adjuntar-signo polinomio)] ;Si la potencia es cero se adjunta el numero solo.
               [(= (car polinomio) 1) (string-append "+X^" (number->string potencia))]
                    [(string-append (adjuntar-signo polinomio) (if (= potencia 1) "X"
                        (string-append "X^" (number->string potencia))))])
         (transformar (cdr polinomio) (+ potencia 1)))))))


(define adjuntar-signo ;Adjunta el signo correspondiente positivo/negativo al CAR de la lista...
  (lambda (polinomio)
    (if (> (car polinomio) 0)
                            (string-append "+" (number->string (car polinomio)))
                            (string-append "-" (number->string (- (car polinomio))))
      )
    )
  )

;Suma de Polinomios:
;La función mostrará el polinomio utilizando una notación estándar.

(define +p
  (lambda (listaPolinomios)
    (display-p (sumar-polinomios listaPolinomios '()))
   ))

(define sumar-polinomios
  (lambda (listaPolinomios auxiliar)
         (cond
           [(null? listaPolinomios) auxiliar]
               [(null? auxiliar) (sumar-polinomios (cdr listaPolinomios) (car listaPolinomios))]
               [else
                (sumar-polinomios (cdr listaPolinomios) (sumar (car listaPolinomios) auxiliar))
               ])))

(define sumar
  (lambda (p1 p2)
    (if (= (length p1) (length p2)) (map + p1 p2)
        (cond
          [(null? p1) p2]
          [(null? p2) p1]
          [else
           (cons (+ (car p1) (car p2)) (sumar (cdr p1) (cdr p2)))
          ]
        ))))


;Resta de Polinomios

(define -p
  (lambda (listaPolinomios)
    (display-p (restar-polinomios listaPolinomios '()))
   ))

(define restar-polinomios
  (lambda (listaPolinomios auxiliar)
         (cond
           [(null? listaPolinomios) auxiliar]
               [(null? auxiliar) (restar-polinomios (cdr listaPolinomios) (car listaPolinomios))]
               [else
                (restar-polinomios (cdr listaPolinomios) (sumar (negar-polinomio(car listaPolinomios)) auxiliar))
               ])))


(define negar-polinomio
  (lambda (polinomio)
    (cond
      [(null? polinomio) polinomio]
      [else
       (cons (cambiar-signo (car polinomio)) (negar-polinomio (cdr polinomio)))])))

(define cambiar-signo ;Cambia el signo de cada numero, devuelve un numero
  (lambda (numero)
    (if (>  numero 0)
        (string->number (string-append "-" (number->string numero)))
        (string->number (string-append "+" (number->string (- numero)))))
      ))
    
 

;Multiplicacion de Polinomios
(define *p
  (lambda (listaPolinomios)
    (display-p (multiplicar-polinomios listaPolinomios '()))
   ))

(define multiplicar-polinomios
  (lambda (listaPolinomios auxiliar)
         (cond
           [(null? listaPolinomios) auxiliar]
               [(null? auxiliar) (multiplicar-polinomios (cdr listaPolinomios) (car listaPolinomios))]
               [else
                (multiplicar-polinomios (cdr listaPolinomios) (multiplicar (car listaPolinomios) auxiliar))
               ])))

(define multiplicar
  (lambda (p1 p2)
    (if (= (length p1) (length p2)) (map * p1 p2)
        (cond
          [(null? p1) p2]
          [(null? p2) p1]
          [else
           (cons (* (car p1) (car p2)) (multiplicar (cdr p1) (cdr p2)))
          ]
        ))))




    