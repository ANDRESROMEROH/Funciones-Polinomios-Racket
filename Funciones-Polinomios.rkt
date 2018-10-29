#lang racket

#|
     II Ciclo, 2018
     Proyecto #2, Paradigmas de Programación 3pm
     Andrés Romero Hernández 402300958
     Estefanía Murillo Romero 117000387
|#


;Despliegue de polinomios, la función mostrará el polinomio utilizando la notación estándar
;(display-p '(0 2 3 1)) = 2x+3x^2+x^3
(define display-p
  (lambda (polinomio) 
      (string-append* "" (transformar polinomio 0)))) ;Une cada uno de los resultados en un solo string


(define transformar                 ;Procesa la lista y la convierte en su polinomio correspondiente.
  (lambda (polinomio potencia)      ;Recibe 2 parámetros, una lista y la potencia actual.
    (if (null? polinomio) '()       ;Si la lista es nula devuelva una lista vacía (Caso base)
        (if (= (car polinomio) 0) (transformar (cdr polinomio) (+ potencia 1)) ;Valida que número no sea cero, de otra forma no se toma en cuenta.
        (cons 
         (cond
           [(= potencia 0) (adjuntar-signo polinomio)] ;Si la potencia es cero se adjunta el número solo.
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

;------------------------------------------------------------------------------------------------------------------------------------------------------

;Suma de Polinomios:
;La función mostrará el polinomio utilizando una notación estándar.
;(+p '(7 8 9) '(3 4 5) '(1 1 1 7)) => "+11+13X+15X^2+7X^3"

(define +p
  (lambda listaPolinomios
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

;---------------------------------------------------------------------------------------------------------------------------------------------------------

;Resta de Polinomios
;(-p '(3 1) '(-5 2) '(-1 6)) => "+9-7X"

(define -p
  (lambda listaPolinomios
    (display-p (restar-polinomios listaPolinomios '()))
   ))

(define restar-polinomios ;Permite realizar la resta.
  (lambda (listaPolinomios auxiliar)
         (cond
           [(null? listaPolinomios) auxiliar]
               [(null? auxiliar) (restar-polinomios (cdr listaPolinomios) (car listaPolinomios))]
               [else
                (restar-polinomios (cdr listaPolinomios) (sumar (negar-polinomio(car listaPolinomios)) auxiliar))
               ])))


(define negar-polinomio ;Cambia los signos de cada uno de los elementos del polinomio
  (lambda (polinomio)
    (cond
      [(null? polinomio) polinomio]
      [else
       (cons (cambiar-signo (car polinomio)) (negar-polinomio (cdr polinomio)))])))

(define cambiar-signo ;Cambia el signo de cada número, devuelve un número
  (lambda (numero)
    (if (>  numero 0)
        (string->number (string-append "-" (number->string numero)))
        (string->number (string-append "+" (number->string (- numero)))))
      ))


;-------------------------------------------------------------------------------------------------------------------------------------------------------

;Multiplicación de Polinomios:
;Ejemplo: (*p '(-5 -6 -9) '(3 1)) = "-15-23X-33X^2-9X^3"

(define *p ;Recibe una cantidad arbitraria de polinomios y devuelve la multiplicación entre ellos.
  (lambda listaPolinomios
    (display-p (multiplicar listaPolinomios '()))
   ))

(define multiplicar ;Permite realizar la multiplicación de múltiples polinomios.
  (lambda (listaPolinomios auxiliar)
  (cond
    [(null? listaPolinomios) auxiliar]
    [(null? auxiliar) (multiplicar (cdr listaPolinomios) (car listaPolinomios))]
    [else
     (multiplicar (cdr listaPolinomios) (sumar-polinomios (multp-pol (car listaPolinomios) auxiliar 0) '()))
     ])))

(define multp-pol ;Forma las listas de polinomios multiplicados.
  (lambda (p1 p2 i)
    (cond
      [(null? p1) p1]
      [(null? p2) p1]
      [else
       (cons (completar-pol(num-por-pol p2 (car p1)) i) (multp-pol (cdr p1) p2 (+ i 1)))]))) 


(define completar ;Devuelve una cantidad arbitraria de 0.
  (lambda (x)
    (cond
      [(< x 1) '()]
      [else
       (cons 0 (completar(- x 1)))])))


    
(define completar-pol ;Completa el polinomio con 0 por si no hay valores.
  (lambda (pol i)
    (cond
      [(null? pol) pol]
      [else
       (append (completar i) pol)])))

    

(define num-por-pol ;Multiplica un número por un polinomio y devuelve una lista de valores.
  (lambda (pol x)
    (cond
      [(null? pol) pol]
      [else
       (cons (* (car pol) x) (num-por-pol (cdr pol) x))])))


;-------------------------------------------------------------------------------------------------------------------------------------------------

;Derivación de Polinomios:
;(derivar-polinomios '((0 2 3 1))) = 2𝑥+3𝑥^2+𝑥^3 => ("2" "6x^1" "3x^2")
;(derivar-polinomios '((0 2 3 1) (1 2 4 5))) => '(("2" "6X^1" "3X^2")("2" "8X^1" "15X^2"))

(define derivar-polinomios 
  (lambda (listaPolinomios)
         (cond
               [(null? listaPolinomios) '()]
               [(= (length listaPolinomios) 1) (list (derivar (car listaPolinomios) 0))]
               [else
                (cons (derivar (car listaPolinomios) 0) (derivar-polinomios (cdr listaPolinomios)))
               ])))

(define derivar                 
  (lambda (polinomio potencia)    
    (if (null? polinomio) '()       
        (if (= (car polinomio) 0) (derivar (cdr polinomio) (+ potencia 1))
            (if (= potencia 0) (derivar (cdr polinomio) (+ potencia 1))
        (cons 
        (cond
           [(= (car polinomio) 1) (string-append (number->string potencia) (string-append "X^" (number->string (- potencia 1))))]
           [(string-append (number->string (* (car polinomio) potencia)) (if (= potencia 1) ""
            (string-append "X^" (number->string (- potencia 1)))))])
        (derivar (cdr polinomio) (+ potencia 1))))))))


;------------------------------------------------------------------------------------------------------------------------------------------------

;Evaluación de Polinomios
;(eval-p '(9 5 8 12 3) 3) => 663

(define eval-p
  (lambda (p x)
    (cond
      [(= (length p) 1) p]
      [else
       (suma-basica (evaluar p x 0))])))


(define suma-basica ;Suma todos los elementos de una lista
  (lambda (p)
    (if
      (null? p)
      0
      (+ (car p) (suma-basica (cdr p))))))

(define evaluar ;Multiplica cada elemento del polinomio por el valor de la x dado
  (lambda (p x i)
    (cond
      [(null? p) p]
      [else
       (cons (* (car p) (desarrollo-x x i)) (evaluar (cdr p) x (+ 1 i)))])))


(define desarrollo-x ;Ejecuta la multiplicación del monomio, X es el valor e I es la potencia.
  (lambda (x i)
    (cond
      [(= x 1) x]
      [(= i 0) 1]
      [else
       (* x (desarrollo-x x (- i 1)))])))
    

;--------------------------------------------------------------------------------------------------------------------------------------------------

;División de Polinomios
;-15+31x-23x^2+20 / -2+5x => Cociente: 5-3x+4x^2, Residuo: -5

;Calcula el resultado, tanto cociente como residuo:
;(/-p '(-15 31 -23 20) '(-2 5)) => '(5 -3 4), '(-5 0)

;Calcula el cociente:
;(qt-p '(-15 31 -23 20) '(-2 5)) => '(5 -3 4)

;Calcula el residuo:
;(rem-p '(-15 31 -23 20) '(-2 5)) => '(-5 0)

(define ultimo-item ;Retorna el último elemento de un polinomio.
  (lambda (polinomio)
    (list-ref polinomio (grado-polinomio polinomio))))


(define const-lista ;Construye una lista de n 0s y el número indicado al final.
  (lambda (numero ceros)
    (append (completar ceros) (list numero))))


(define grado-polinomio ;Determina el grado de un polinomio según la cantidad de coeficientes.
  (lambda (p)
  (for/fold ([d -inf.0]) ([(pi i) (in-indexed p)])
    (if (zero? pi) d i))))


(define pol-mult-cociente ;Lleva a cabo la multiplicación de un cociente indicado con su potencia, 
        (lambda (cociente grado polinomio) ;por cada uno de los respectivos coeficientes del denominador.
          (append (make-list grado 0) (map (lambda (x) (* x cociente)) polinomio))))


(define (residuo-resta a p b q) ;Calcula el residuo al efectuar la resta del numerador y
  (cond [(< (grado-polinomio p) 0) q] ;el resultado de las multiplicaciones efectuadas entre el denominador y los elementos del cociente.
        [(< (grado-polinomio q) 0) p]
        [(< (grado-polinomio p) (grado-polinomio q)) (residuo-resta b q a p)]
        [else
        (if (= (length p) (length q)) (map (lambda (x y) (+ (* a x) (* b y))) p q)
            (cons (+ (* a (car p)) (* b (car q))) (residuo-resta a (cdr p) b (cdr q))))]))


#|Funciones que ayudan a la transformación entre números positivos y negativos:|#
(define (division+ p q) (residuo-resta 1 p  1 q))
(define (division- p q) (residuo-resta 1 p -1 q))
#|-----------------------------------------------------------------------------|#


(define (qt-p numerador denominador) ;Calcula el cociente de la división entre 2 polinomios
  (cond
    [(< (grado-polinomio numerador) (grado-polinomio denominador)) 0]
    [else    
         (division+
          (const-lista
           (/ (ultimo-item numerador) (ultimo-item denominador)) (- (grado-polinomio numerador) (grado-polinomio denominador)))
            (qt-p
             (division- numerador
              (pol-mult-cociente
               (/ (ultimo-item numerador) (ultimo-item denominador)) (- (grado-polinomio numerador) (grado-polinomio denominador)) denominador)) denominador))
        ]))


(define (rem-p numerador denominador) ;Calcula el residuo de la división entre 2 polinomios
  (cond
    [(< (grado-polinomio numerador) (grado-polinomio denominador)) numerador]
    [else    
          (rem-p
           (division- numerador
             (pol-mult-cociente
              (/ (ultimo-item numerador) (ultimo-item denominador)) (- (grado-polinomio numerador) (grado-polinomio denominador)) denominador)) denominador)
          ]))


#|Esta función calcula el resultado de la división de los polinomios recibidos como parámetro.
El resultado es una lista que contiene el cociente y el residuo de la división:|#

(define (/-p numerador denominador)
  (values (qt-p numerador denominador) (rem-p numerador denominador)))


;--------------------------------------------------------------------------------------------------------------------------------------

;Factorización
;Polinomio de grado 2 (fact-p '(4 4 1)) => '((2 1) (2 1))
;                     (fact-p '(-6 1 1)) => '((3 1) (-2 1))
;Polinomio de grado 3 (fact-p '(-72 -6 7 1)) => '((-3 1) (4 1) (6 1))
;Polinomio de grado superior (fact-p '(72 -42 -39 6 3)) o (fact-p '(24 -14 -13 2 1)) =>'((-1 1) (-3 1) (2 1) (4 1))

(define fact-p
  (lambda (pol)
    (cond
      [ (= (length pol) 3) (fact-2g pol)]
      [else
       (factorizar pol)])))


(define fact-2g ;Realiza la factorización de polinomios de grado 2
(lambda (pol)
  (cond
    [(= (caddr pol) 0) pol]
    [(null? pol) pol]
    [else
     (cons (list (cambiar-signo (formula-resta pol)) 1) (list (list (cambiar-signo (formula-suma pol)) 1)))])))
  

(define formula-resta ;Esta función equivale a la fórmula general de la ecuación cuadrática
  (lambda (pol)
    (cond
      [(= (caddr pol) 0) pol]
      [(null? pol) pol]
      [else (/ (- (cambiar-signo (cadr pol)) (raiz pol)) (* 2 (caddr pol)))])))

(define formula-suma ;Esta función equivale a la fórmula general de la ecuación cuadrática
  (lambda (pol)
    (cond
      [(= (caddr pol) 0) pol]
      [(null? pol) pol]
      [else (/ (+ (cambiar-signo (cadr pol)) (raiz pol)) (* 2 (caddr pol)))])))


(define raiz ;Calcula la raíz que se utilizará en la fórmula general
  (lambda (pol)
    (cond
     [(null? pol) pol]
     [else
      (sqrt(- (desarrollo-x (cadr pol) 2) (* 4 (* (caddr pol) (car pol)))))])))

(define divisores-positivos ;Consigue los divisores positivos de un número
  (lambda (num i lista)
    (cond
      [(= i 0) lista]
      [(= (modulo num i) 0) (cons i (divisores-positivos num (- i 1) lista))]
      [else
       (divisores-positivos num (- i 1) lista)])))

(define factorizar ;Método que llama al que devuelve la factorización.
  (lambda (pol)
    (cond
      [(= (car pol) 0) (metodo (reverse (cdr pol)) (divisores (hacer-positivo (cadr pol))) (list (list 0 1)))]
      [else
        (metodo (reverse pol) (divisores (hacer-positivo (car pol))) '())])))


(define metodo ;Devuelve la factorización de un polinomio
  (lambda (pol divisores resultados)
    (cond
      [(null? pol) resultados]
      [(null? divisores) resultados]
      [else
       (cond
         [(equal? (verificar pol (car divisores) '()) #t) (metodo pol (cdr divisores) (cons (list (cambiar-signo (car divisores)) 1) resultados))]
         [(equal? (verificar pol (car divisores) '()) #f) (metodo pol (cdr divisores) resultados)])])))



(define divisores ;Consigue la lista de divisores de un número
  (lambda (num)
   (append (map (lambda (x) (* x -1)) (divisores-positivos num num '())) (divisores-positivos num num '()))
    ))

(define verificar ;Devuelve true si el car del resultado es 0
  (lambda (pol div res)
    (cond
      [(= (car (operacion-principal pol div res)) 0) #t]
      [else
       #f])))

(define operacion-principal ;Realiza la operación principal del método de Ruffini
  (lambda (pol div res)
    (cond
      [(null? pol) res]
      [(null? res) (operacion-principal (cdr pol) div (list (car pol)))]
      [else
       (operacion-principal (cdr pol) div (append (list (desarrolla-operacion div (car res) (car pol))) res))])))


(define desarrolla-operacion
  (lambda (divisor res num-pol) ; res = resultado, num-pol = coeficiente del polinomio
    (+ num-pol (* res divisor))))


(define hacer-positivo ;Cambia el signo de un número si este es negativo, devuelve un número
  (lambda (numero)
    (if (>  numero 0)
        numero
        (string->number (string-append "+" (number->string (- numero)))))
      ))






