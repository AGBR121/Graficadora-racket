#lang racket

#|
- Fecha:  04/06/2024
- Hora de Publicacion: 8:10pm
- Version del código: 3.0
- Autor: Ing(c) Burbano Rodriguez Angel Gabriel
- Lenguaje utilizado: Racket
- Versión del lenguaje: 8.11.1
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Descripcion del programa: Este programa consiste en un algoritmo para graficar una función matemática que
le ingreses, tambien, debe ingrsar el dominio y los saltos que realizará.El programa mostrará en modo gráfico los
coeficientes y exponentes, la evaluación de los datos y la gráfica.
- SALVEDAD: El programa debe ser usado correctamente (Valores de ingreso válidos) y las funciones donde y=n no las grafica bien (Funciona en windows)
|#

;Llamamos la libreria graphics
(require graphics/graphics)
;Abrimos los graficos
(open-graphics)
;Definimos la ventana que recibirá los datos
(define Formulario (open-viewport "Formulario" 600 260))
;Dibujará en la ventana lo que requerimos
((draw-line Formulario)(make-posn 10 50)(make-posn 590 50) "Red")
((draw-string Formulario) (make-posn 240 40) "DATOS A INGRESAR" "Red")
((draw-string Formulario) (make-posn 0 100) "ECUACION A GRAFICAR: " "Red")
((draw-string Formulario) (make-posn 0 150) "INICIO DEL DOMINIO: " "Red")
((draw-string Formulario) (make-posn 0 200) "FIN DEL DOMINIO: " "Red")
((draw-string Formulario) (make-posn 0 250) "INCREMENTO DE VALORES: " "Red")

#|------------------------------------------------------------------------
Funcion FunctionX que recibe la ecuación que el usuario escribirá.
-Identificador de bajo nivel string: es el que va guardando en un string lo que el usuario escriba
|#
(define (FunctionX string)
((draw-solid-rectangle Formulario) (make-posn 190 85) 500 50 "White")
((draw-string Formulario) (make-posn 190 100) (string-append string "_") "Black")
;Identificador text que recibe la tecla
(define text (get-key-press Formulario))
;Identificador letter que identifica que colocó el usuario
(define letter (key-value text))
(if (and (equal? string "") (equal? letter #\backspace))
    (FunctionX string)
    ;De lo contrario
(if (and (not (equal? letter 'divide))(not (equal? letter 'multiply))(not (equal? letter 'release))(not (equal? letter 'rshift))
         (not (equal? letter 'shift)) (not (equal? letter 'control)) (not (equal? letter 'menu)) (not (equal? letter 'rcontrol))) ; Verifica si la acción no es 'release'
    (if (equal? letter #\backspace)
          (FunctionX (substring string 0 (- (string-length string) 1)))
        ;De lo contrario
    (if (equal? letter #\return)
        (begin
          ((draw-solid-rectangle Formulario) (make-posn 190 85) 500 50 "White")
          ((draw-string Formulario) (make-posn 190 100) string "Black")
          string
        );Fin begin
        ;De lo contrario
        (FunctionX (string-append string (if (equal? letter 'subtract)
                                            "-"
                                            ;De lo contrario
                                            (if (equal? letter 'add)
                                                "+"
                                                ;De lo contrario
                                                (cond
                                                  ((equal? letter 'numpad1)
                                                   "1"
                                                  )
                                                  ((equal? letter 'numpad2)
                                                   "2"
                                                  )
                                                  ((equal? letter 'numpad3)
                                                   "3"
                                                  )
                                                  ((equal? letter 'numpad4)
                                                   "4"
                                                  )
                                                  ((equal? letter 'numpad5)
                                                   "5"
                                                  )
                                                  ((equal? letter 'numpad6)
                                                   "6"
                                                  )
                                                  ((equal? letter 'numpad7)
                                                   "7"
                                                  )
                                                  ((equal? letter 'numpad8)
                                                   "8"
                                                  )
                                                  ((equal? letter 'numpad9)
                                                   "9"
                                                  )
                                                  ((equal? letter 'numpad0)
                                                   "0"
                                                  )
                                                  ((equal? letter #\x)
                                                   "X"
                                                  )
                                                  ((equal? letter #\X)
                                                   "X"
                                                  )
                                                  ((equal? letter #\()
                                                   "("
                                                  )
                                                  ((equal? letter #\))
                                                   ")"
                                                  )
                                                  ((char-numeric? letter )
                                                   (~a letter)
                                                  )
                                                  (else "")
                                                );Fin cond
                                            );Fin if (equal? letter 'add)
                                         );Fin if (equal? letter 'subtract)
                                  ))
    );Fin if (equal? letter #\return)
    );Fin if (equal? letter #\backspace)
    ;De lo contrario
    (FunctionX string)    
);Fin if 
);Fin if (and (equal? string "") (equal? letter #\backspace))
);Fin función FunctionX

#|------------------------------------------------------------------------
Funcion Minimo que recibe el valor minimo de X que el usuario escribirá.
-Identificador de bajo nivel string: es el que va guardando en un string lo que el usuario escriba
|#
(define (Minimo string)
((draw-solid-rectangle Formulario) (make-posn 170 135) 500 50 "White")
((draw-string Formulario) (make-posn 170 150) (string-append string "_") "Black")
;Identificador text que recibe la tecla
(define text (get-key-press Formulario))
;Identificador letter que identifica que colocó el usuario
(define letter (key-value text))
(if (and (equal? string "") (equal? letter #\backspace))
    (Minimo string)
    ;De lo contrario
(if (and (not (equal? letter 'divide))(not (equal? letter 'multiply))(not (equal? letter 'release))(not (equal? letter 'rshift))
         (not (equal? letter 'shift)) (not (equal? letter 'control)) (not (equal? letter 'menu)) (not (equal? letter 'rcontrol))
         ) ;Verifica si la acción no es 'release'
    (if (equal? letter #\backspace)
        (Minimo (substring string 0 (- (string-length string) 1)))
        ;De lo contrario
    (if (equal? letter #\return)
        (begin
          ((draw-solid-rectangle Formulario) (make-posn 170 135) 500 50 "White")
          ((draw-string Formulario) (make-posn 170 150) string "Black")
          string
        );Fin begin
        ;De lo contrario
        (Minimo (string-append string (if (equal? letter 'subtract)
                                            "-"
                                            ;De lo contrario
                                            (if (equal? letter 'add)
                                                ""
                                                ;De lo contrario
                                                (cond
                                                  ((equal? letter 'numpad1)
                                                   "1"
                                                  )
                                                  ((equal? letter 'numpad2)
                                                   "2"
                                                  )
                                                  ((equal? letter 'numpad3)
                                                   "3"
                                                  )
                                                  ((equal? letter 'numpad4)
                                                   "4"
                                                  )
                                                  ((equal? letter 'numpad5)
                                                   "5"
                                                  )
                                                  ((equal? letter 'numpad6)
                                                   "6"
                                                  )
                                                  ((equal? letter 'numpad7)
                                                   "7"
                                                  )
                                                  ((equal? letter 'numpad8)
                                                   "8"
                                                  )
                                                  ((equal? letter 'numpad9)
                                                   "9"
                                                  )
                                                  ((equal? letter 'numpad0)
                                                   "0"
                                                  )
                                                  ((char-numeric? letter )
                                                   (~a letter)
                                                  )
                                                  (else "")
                                                );Fin cond
                                            );Fin if (equal? letter 'add)
                                         );Fin if (equal? letter 'subtract)
                                  ))
    );Fin if (equal? letter #\return)
    );Fin if (equal? letter #\backspace)
    ;De lo contrario
    (Minimo string)    
);Fin if 
);Fin if (and (equal? string "") (equal? letter #\backspace))
);Fin función Minimo

#|------------------------------------------------------------------------
Funcion Maximo que recibe el valor Maximo de X que el usuario escribirá.
-Identificador de bajo nivel string: es el que va guardando en un string lo que el usuario escriba
|#
(define (Maximo string)
((draw-solid-rectangle Formulario) (make-posn 145 185) 500 50 "White")
((draw-string Formulario) (make-posn 145 200) (string-append string "_") "Black")
;Identificador text que recibe la tecla
(define text (get-key-press Formulario))
;Identificador letter que identifica que colocó el usuario
(define letter (key-value text))
(if (and (equal? string "") (equal? letter #\backspace))
    (Maximo string)
    ;De lo contrario
(if (and (not (equal? letter 'divide))(not (equal? letter 'multiply))(not (equal? letter 'release))(not (equal? letter 'rshift))
         (not (equal? letter 'shift)) (not (equal? letter 'control)) (not (equal? letter 'menu)) (not (equal? letter 'rcontrol))
         ) ; Verifica si la acción no es 'release'
    (if (equal? letter #\backspace)
        (Maximo (substring string 0 (- (string-length string) 1)))
        ;De lo contrario
    (if (equal? letter #\return)
        (begin
          ((draw-solid-rectangle Formulario) (make-posn 145 185) 500 50 "White")
          ((draw-string Formulario) (make-posn 145 200) string "Black")
          string
        );Fin begin
        ;De lo contrario
        (Maximo (string-append string (if (equal? letter 'subtract)
                                            "-"
                                            ;De lo contrario
                                            (if (equal? letter 'add)
                                                ""
                                                ;De lo contrario
                                                (cond
                                                  ((equal? letter 'numpad1)
                                                   "1"
                                                  )
                                                  ((equal? letter 'numpad2)
                                                   "2"
                                                  )
                                                  ((equal? letter 'numpad3)
                                                   "3"
                                                  )
                                                  ((equal? letter 'numpad4)
                                                   "4"
                                                  )
                                                  ((equal? letter 'numpad5)
                                                   "5"
                                                  )
                                                  ((equal? letter 'numpad6)
                                                   "6"
                                                  )
                                                  ((equal? letter 'numpad7)
                                                   "7"
                                                  )
                                                  ((equal? letter 'numpad8)
                                                   "8"
                                                  )
                                                  ((equal? letter 'numpad9)
                                                   "9"
                                                  )
                                                  ((equal? letter 'numpad0)
                                                   "0"
                                                  )
                                                  ((char-numeric? letter )
                                                   (~a letter)
                                                  )
                                                  (else "")
                                                );Fin cond
                                            );Fin if (equal? letter 'add)
                                         );Fin if (equal? letter 'subtract)
                                  ))
    );Fin if (equal? letter #\return)
    );Fin if (equal? letter #\backspace)
    ;De lo contrario
    (Maximo string)    
);Fin if 
);Fin if (and (equal? string "") (equal? letter #\backspace))
);Fin función Maximo

#|------------------------------------------------------------------------
Funcion Saltos que recibe el valor de los intervalos de X que el usuario escribirá.
-Identificador de bajo nivel string: es el que va guardando en un string lo que el usuario escriba
|#
(define (Saltos string)
((draw-solid-rectangle Formulario) (make-posn 210 235) 500 50 "White")
((draw-string Formulario) (make-posn 212 250) (string-append string "_") "Black")
;Identificador text que recibe la tecla
(define text (get-key-press Formulario))
;Identificador letter que identifica que colocó el usuario
(define letter (key-value text))
(if (and (equal? string "") (equal? letter #\backspace))
    (Saltos string)
    ;De lo contrario
(if (and (not (equal? letter 'divide))(not (equal? letter 'multiply))(not (equal? letter 'release))(not (equal? letter 'rshift))
         (not (equal? letter 'shift)) (not (equal? letter 'control)) (not (equal? letter 'menu)) (not (equal? letter 'rcontrol))
         ) ; Verifica si la acción no es 'release'
    (if (equal? letter #\backspace)
        (Saltos (substring string 0 (- (string-length string) 1)))
    (if (equal? letter #\return)
        (begin
          ((draw-solid-rectangle Formulario) (make-posn 210 235) 500 50 "White")
          ((draw-string Formulario) (make-posn 212 250) string "Black")
          string
        );Fin begin
        (Saltos (string-append string (if (equal? letter 'subtract)
                                            ""
                                            (if (equal? letter 'add)
                                                ""
                                                (cond
                                                  ((equal? letter 'numpad1)
                                                   "1"
                                                  )
                                                  ((equal? letter 'numpad2)
                                                   "2"
                                                  )
                                                  ((equal? letter 'numpad3)
                                                   "3"
                                                  )
                                                  ((equal? letter 'numpad4)
                                                   "4"
                                                  )
                                                  ((equal? letter 'numpad5)
                                                   "5"
                                                  )
                                                  ((equal? letter 'numpad6)
                                                   "6"
                                                  )
                                                  ((equal? letter 'numpad7)
                                                   "7"
                                                  )
                                                  ((equal? letter 'numpad8)
                                                   "8"
                                                  )
                                                  ((equal? letter 'numpad9)
                                                   "9"
                                                  )
                                                  ((equal? letter 'numpad0)
                                                   "0"
                                                  )
                                                  ((char-numeric? letter )
                                                   (~a letter)
                                                  )
                                                  (else "")
                                                );Fin cond
                                            );Fin if (equal? letter 'add)
                                         );Fin if (equal? letter 'subtract)
                                  ))
    );Fin if (equal? letter #\return)
    );Fin if (equal? letter #\backspace)
    ;De lo contrario
    (Saltos string)    
);Fin if 
);Fin if (and (equal? string "") (equal? letter #\backspace))
);Fin función Saltos

;Identificador funcionGraficar que guarda el string de la funcion a descomponer y operar
(define funcionGraficar (FunctionX ""))
;Identificador minimoX que guarda el valor minimo de X
(define minimoX (string->number(Minimo "")))
;Identificador maximoX que guarda el valor maximo de X
(define maximoX (string->number(Maximo "")))
;Identificador intervalos que guarda el intervalo de X
(define intervalos (string->number(Saltos "")))

#|------------------------------------------------------------------------
Funcion AddNumero que agrega un número a un string
-Identificador de bajo nivel string: es el que guarda el string
-Identificador de bajo nivel number: es el que guarda el numero a agregar en el string
|#
(define (AddNumero string number)
(string-append string (~a number))
);Fin funcion AddNumero

#|------------------------------------------------------------------------
Funcion Ecuacion que descompone la funcion dada guardando en un vector de vectores los coeficientes y exponentes de la
funcion.
-Identificador de bajo nivel string: es el que guarda el string.
-Identificador de bajo nivel counter: es el que cuenta las veces que llevamos de la funcion.
-Identificador de bajo nivel coeficientetemp: es el que guarda el número del coeficiente mientras se sigue encontrando más
números del coeficiente hasta que no hayan más.
-Identificador de bajo nivel exponentestemp: es el que guarda el número del exponente mientras se sigue encontrando más
números del exponente hasta que no hayan más.
-Identificador de bajo nivel coeficientes: es el que guarda en un vector los coeficientes encontrados.
-Identificador de bajo nivel exponentes: es el que guarda en un vector los exponentes encontrados
-Identificador de bajo nivel X: es el que verifica si pasó por una X.
-Identificador de bajo nivel signo: es el que verifica si pasó por un signo.
-Identificador de bajo nivel parentesis: es el que verifica si pasó por un parentesis.
|#
(define (Ecuacion string counter coeficientetemp exponentestemp coeficientes exponentes X signo parentesis)
(if (< counter (string-length string))
    (if (equal? (string-upcase (~a (string-ref string counter))) "X")
        (Ecuacion string (+ 1 counter) "" exponentestemp (vector-append coeficientes (make-vector 1 (if (or (equal? coeficientetemp "") (equal? coeficientetemp "-") ) (string->number(string-append coeficientetemp "1")) (string->number coeficientetemp)))) exponentes #t signo parentesis)
        ;De lo contrario
        (if (and (equal? (~a (string-ref string counter)) "-") (not X) (not signo))
            (Ecuacion string (+ 1 counter) (AddNumero coeficientetemp (string-ref string counter)) exponentestemp coeficientes exponentes X #t parentesis)
            ;De lo contrario
            (if (and (equal? (~a (string-ref string counter)) "+") (not X) (not signo))
                (Ecuacion string (+ 1 counter) coeficientetemp exponentestemp coeficientes exponentes X #t parentesis)
                ;De lo contrario
                (if (and (equal? (~a (string-ref string counter)) "("))
                    (Ecuacion string (+ 1 counter) coeficientetemp exponentestemp coeficientes exponentes X signo #t)
                    ;De lo contrario
                    (if (and (equal? (~a (string-ref string counter)) ")"))
                        (Ecuacion string (+ 1 counter) coeficientetemp "" coeficientes (vector-append exponentes (make-vector 1 (string->number exponentestemp) )) #f #f #f)
                        ;De lo contrario
                        (if (and (equal? (~a (string-ref string counter)) "-") X signo (not parentesis))
                                (Ecuacion string (+ 1 counter) (AddNumero coeficientetemp (string-ref string counter)) "" coeficientes (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "1")) (string->number exponentestemp)))) #f #t parentesis)
                                ;De lo contrario
                                (if (and (equal? (~a (string-ref string counter)) "+") X signo (not parentesis))
                                    (Ecuacion string (+ 1 counter) coeficientetemp "" coeficientes (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "1")) (string->number exponentestemp)))) #f #t parentesis)
                                    ;De lo contrario
                                    (if (and (equal? (~a (string-ref string counter)) "-") (not X) signo (not parentesis))
                                        (Ecuacion string (+ 1 counter) (AddNumero "" (string-ref string counter)) "" (vector-append coeficientes (make-vector 1 (if (or (equal? coeficientetemp "") (equal? coeficientetemp "-") ) (string->number(string-append coeficientetemp "1")) (string->number coeficientetemp)))) (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "0")) (string->number exponentestemp)))) #f #t parentesis)
                                        ;De lo contrario
                                        (if (and (not (or (equal? (~a (string-ref string counter)) "+") (equal? (~a (string-ref string counter)) "-"))) (not X) signo (not parentesis))
                                            (Ecuacion string (+ 1 counter) (AddNumero coeficientetemp (string-ref string counter)) "" coeficientes exponentes #f #t parentesis)
                                            ;De lo contrario
                                            (if (and X signo parentesis (not (or (equal? (~a (string-ref string counter)) "+") (equal? (~a (string-ref string counter)) "-"))))
                                                (Ecuacion string (+ 1 counter) coeficientetemp (AddNumero exponentestemp (string-ref string counter)) coeficientes exponentes X signo parentesis)
                                                ;De lo contrario
                                                (if (and X signo (not parentesis) (not (or (equal? (~a (string-ref string counter)) "+") (equal? (~a (string-ref string counter)) "-"))))
                                                    (Ecuacion string (+ 1 counter) coeficientetemp (AddNumero exponentestemp (string-ref string counter)) coeficientes exponentes X signo parentesis)
                                                    ;De lo contrario
                                                    (if (and (equal? (~a (string-ref string counter)) "-") X signo parentesis)
                                                        (Ecuacion string (+ 1 counter) coeficientetemp (AddNumero exponentestemp (string-ref string counter)) coeficientes exponentes X signo parentesis)
                                                        ;De lo contrario
                                                        (if (and  (not X) (not signo) (not parentesis))
                                                            (Ecuacion string (+ 1 counter) (AddNumero coeficientetemp (string-ref string counter)) "" coeficientes exponentes #f #t parentesis)
                                                            ;De lo contrario
                                                            (if (and (equal? (~a (string-ref string counter)) "+") X (not signo) (not parentesis))
                                                                (Ecuacion string (+ 1 counter) coeficientetemp "" coeficientes (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "1")) (string->number exponentestemp)))) #f #t parentesis)
                                                                ;De lo contrario
                                                                (if (and (equal? (~a (string-ref string counter)) "-") X (not signo) (not parentesis))
                                                                    (Ecuacion string (+ 1 counter) (AddNumero coeficientetemp (string-ref string counter)) "" coeficientes (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "1")) (string->number exponentestemp)))) #f #t parentesis)
                                                                    ;De lo contrario
                                                                    (if (and (equal? (~a (string-ref string counter)) "+") (not X) signo (not parentesis))
                                                                        (Ecuacion string (+ 1 counter) "" "" (vector-append coeficientes (make-vector 1 (if (or (equal? coeficientetemp "") (equal? coeficientetemp "-") ) (string->number(string-append coeficientetemp "1")) (string->number coeficientetemp)))) (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "0")) (string->number exponentestemp)))) X signo parentesis)
                                                                        ;De lo contrario
                                                                        (if (and X (not signo) (not parentesis))
                                                                            (Ecuacion string (+ 1 counter) coeficientetemp (AddNumero exponentestemp (string-ref string counter)) coeficientes exponentes X signo parentesis)
                                                                            ;De lo contrario
                                                                            (if (and X (not signo) parentesis)
                                                                                (Ecuacion string (+ 1 counter) coeficientetemp (AddNumero exponentestemp (string-ref string counter)) coeficientes exponentes X signo parentesis)
                                                                                ;De lo contrario
                                                                                (Ecuacion string (+ 1 counter) (AddNumero "" (string-ref string counter)) "" (vector-append coeficientes (make-vector 1 (if (or (equal? coeficientetemp "") (equal? coeficientetemp "-") ) (string->number(string-append coeficientetemp "1")) (string->number coeficientetemp)))) (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "0")) (string->number exponentestemp)))) X signo parentesis) 
                                                                            );Fin if (and X (not signo) parentesis)
                                                                        );Fin if (and X (not signo) (not parentesis))
                                                                    );Fin if (and (equal? (~a (string-ref string counter)) "+") (not X) signo (not parentesis))
                                                                );Fin (and (equal? (~a (string-ref string counter)) "-") X (not signo) (not parentesis))
                                                            );Fin if (and (equal? (~a (string-ref string counter)) "+") X (not signo) (not parentesis))
                                                        );Fin if (and  (not X) (not signo) (not parentesis))
                                                    );Fin if (and (equal? (~a (string-ref string counter)) "-") X signo parentesis)
                                                );Fin if (and X signo (not parentesis) (not (or (equal? (~a (string-ref string counter)) "+") (equal? (~a (string-ref string counter)) "-"))))
                                            );Fin if (and X signo parentesis (not (or (equal? (~a (string-ref string counter)) "+") (equal? (~a (string-ref string counter)) "-"))))
                                        );Fin if (and (not (or (equal? (~a (string-ref string counter)) "+") (equal? (~a (string-ref string counter)) "-"))) (not X) signo (not parentesis))
                                    );Fin if (and (equal? (~a (string-ref string counter)) "-") (not X) signo (not parentesis))
                                );Fin if (and (equal? (~a (string-ref string counter)) "+") X signo (not parentesis)) 
                        );Fin if (and (equal? (~a (string-ref string counter)) "-") X signo (not parentesis))
                    );Fin if (and (equal? (~a (string-ref string counter)) ")"))
                );Fin if (and (equal? (~a (string-ref string counter)) "("))
           );Fin if (and (equal? (~a (string-ref string counter)) "+") (not X) (not signo))
        );Fin if (and (equal? (~a (string-ref string counter)) "-") (not X) (not signo))
    );Fin if (equal? (string-upcase (~a (string-ref string counter))) "X")
    ;De lo contrario
    (if (and (equal? coeficientetemp "") (equal? exponentestemp "") (not X))
        (vector coeficientes exponentes)
        ;De lo contrario
        (if (equal? coeficientetemp "")
            (vector coeficientes (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "1")) (string->number exponentestemp)))))
            ;De lo contrario
            (vector (vector-append coeficientes (make-vector 1 (if (or (equal? coeficientetemp "") (equal? coeficientetemp "-") ) (string->number(string-append coeficientetemp "0")) (string->number coeficientetemp)))) (vector-append exponentes (make-vector 1 (if (or (equal? exponentestemp "") (equal? exponentestemp "-") ) (string->number(string-append exponentestemp "0")) (string->number exponentestemp)))))
        );Fin if (equal? coeficientetemp "")
    );Fin if (and (equal? coeficientetemp "") (equal? exponentestemp "") (not X))
);Fin if (< counter (string-length string))  
);Fin funcion Ecuacion

;Identificador ecuation que guarda el vector de vectores para posteriormente ser usado
(define ecuation (Ecuacion funcionGraficar 0 "" "" (vector) (vector) #f #f #f))

#|------------------------------------------------------------------------
Funcion SegundoPunto que crea una ventana y dibuja en ella cuales son los coeficientes y los exponentes.
-Identificador de bajo nivel ecuacionDibujar: Es el vector de vectores el cual extraeremos los valores.
|#
(define (SegundoPunto ecuacionDibujar)
;Definimos la ventana que mostrará los coeficientes y exponentes
(define ventanaPartes (open-viewport "Coeficientes y Exponentes" 320 (+ 25 (* (vector-length (vector-ref ecuacionDibujar 1)) 25))))
((draw-line ventanaPartes)(make-posn 0 0)(make-posn 320 0) "Teal")
((draw-rectangle ventanaPartes) (make-posn 0 0) 160 25 "Teal")
((draw-rectangle ventanaPartes) (make-posn 160 0) 160 25 "Teal")
((draw-string ventanaPartes) (make-posn 10 18) "COEFICIENTES" "Red")
((draw-string ventanaPartes) (make-posn 170 18) "EXPONENTES" "Red")

  #|------------------------------------------------------------------------
Funcion local TablaPartes que dibujará la tabla con los coeficientes y los respectivos exponentes.
-Identificador de bajo nivel tabla: Es el vector de vectores el cual extraeremos los valores.
-Identificador de bajo nivel counter: Es el contador de la función.
|#
(define (TablaPartes tabla counter)
(if (<= counter (vector-length (vector-ref tabla 1)))
    [begin
      ((draw-rectangle ventanaPartes) (make-posn 0 (* counter 25)) 160 25 "Teal")
      ((draw-rectangle ventanaPartes) (make-posn 160 (* counter 25)) 160 25 "Teal")
      ((draw-string ventanaPartes) (make-posn 10 (+ 18 (* counter 25))) (~a (vector-ref (vector-ref tabla 0) (- counter 1))) "Black")
      ((draw-string ventanaPartes) (make-posn 170 (+ 18 (* counter 25))) (~a (vector-ref (vector-ref tabla 1) (- counter 1))) "Black")
      (TablaPartes tabla (+ counter 1))
    ];Fin begin
    ;De lo contrario
    (void)  
);Fin if
);Fin funcion TablaPartes
;Hacemos llamado a la funcion TablaPartes
(TablaPartes ecuacionDibujar 1)  
);Fin función SegundoPunto

#|------------------------------------------------------------------------
Funcion Cantidad que verifica cuantos valores hay en el eje x.
-Identificador de bajo nivel minimo: Es el menor valor de x.
-Identificador de bajo nivel maximo: Es el mayor valor de x.
-Identificador de bajo nivel saltos: Es el intervalo de x.
|#
(define (Cantidad minimo maximo saltos numero)
(if (< minimo maximo)
    (Cantidad (+ minimo saltos) maximo saltos (+ numero 1))
    ;De lo contrario
    numero
);Fin if (< minimo maximo)
);Fin funcion Cantidad

#|------------------------------------------------------------------------
Funcion TercerPunto que dibujará la Evaluación de los valores.
-Identificador de bajo nivel tabla: Es el vector de vectores el cual extraeremos los valores.
-Identificador de bajo nivel minimo: Es el menor valor de x.
-Identificador de bajo nivel maximo: Es el mayor valor de x.
-Identificador de bajo nivel saltos: Es el intervalo de x.
|#
(define (TercerPunto tabla minimo maximo saltos)
;Identificador valores que obtiene la cantidad de datos
(define valores (Cantidad minimo maximo saltos 1))
(define ventanaEvaluacion (open-viewport "Evaluación" 520 (+ 25 (* valores 25))))
((draw-rectangle ventanaEvaluacion) (make-posn 0 0) 160 25 "Teal")
((draw-rectangle ventanaEvaluacion) (make-posn 160 0) 220 25 "Teal")
((draw-rectangle ventanaEvaluacion) (make-posn 380 0) 160 25 "Teal")
((draw-string ventanaEvaluacion) (make-posn 10 18) "X" "Red")
((draw-string ventanaEvaluacion) (make-posn 170 18) "EVALUACIÓN" "Red")
((draw-string ventanaEvaluacion) (make-posn 390 18) "Y" "Red")

;Identificador cantidadOperaciones que obtiene el número de operaciones a realizar.
(define cantidadOperaciones (vector-length (vector-ref tabla 1)))

#|------------------------------------------------------------------------
Funcion local Operar que operará la función de acuerdo a los valores dados.
-Identificador de bajo nivel tabla: Es el vector de vectores el cual extraeremos los valores.
-Identificador de bajo nivel counter: Es el contador de la funcion.
-Identificador de bajo nivel minimo: Es el menor valor de x.
|#  
(define (Operar tabla counter numero minimo)
(if (< counter cantidadOperaciones)
    (if (and (< (vector-ref (vector-ref tabla 1) counter) 0) (= minimo 0))
        "Indefinido"
        ;De lo contrario
        (Operar tabla (+ counter 1) (+ numero (* (vector-ref (vector-ref tabla 0) counter) (expt minimo (vector-ref (vector-ref tabla 1) counter)) )) minimo)
    );Fin if (and (< (vector-ref (vector-ref tabla 1) counter) 0) (= minimo 0))
    ;De lo contrario
    numero
);Fin if (< counter cantidadOperaciones)
);Fin funcion Operar

#|------------------------------------------------------------------------
Funcion local DibujarOperaciones que guardará los datos de la tabla de evaluación.
-Identificador de bajo nivel tabla: Es el vector de vectores el cual extraeremos los valores.
-Identificador de bajo nivel counter: Es el contador de la funcion.
-Identificador de bajo nivel string: Es el que guarda el string a usar.
-Identificador de bajo nivel minimo: Es el menor valor de x.
|#  
(define (DibujarOperaciones tabla counter string minimo)
(if (< counter cantidadOperaciones)
    (if (and (< (vector-ref (vector-ref tabla 1) counter) 0) (= minimo 0))
        "Indefinido"
        ;De lo contrario
        (if (= (vector-ref (vector-ref tabla 1) counter) 0)
            (DibujarOperaciones tabla (+ counter 1) (string-append string (if (= counter 0) "" (if (< (vector-ref (vector-ref tabla 0) counter) 0) "" "+" )) (~a (vector-ref (vector-ref tabla 0) counter)) ) minimo)
            ;De lo contrario
            (DibujarOperaciones tabla (+ counter 1) (string-append string (if (= counter 0) "" (if (< (vector-ref (vector-ref tabla 0) counter) 0) "" "+" )) (if (= (vector-ref (vector-ref tabla 0) counter) 1 ) "" (~a (vector-ref (vector-ref tabla 0) counter))) "(" (~a minimo)")" "^" (if (< (vector-ref (vector-ref tabla 1) counter) 0) "(" "" ) (~a (vector-ref (vector-ref tabla 1) counter)) (if (< (vector-ref (vector-ref tabla 1) counter) 0) ")" "" ) ) minimo)
        );Fin if (= (vector-ref (vector-ref tabla 1) counter) 0)
    );Fin if (and (< (vector-ref (vector-ref tabla 1) counter) 0) (= minimo 0))
    ;De lo contrario
    (string-append string " = " (~a (Operar tabla 0 0 minimo)))
);Fin if (< counter cantidadOperaciones)
);Fin funcion DibujarOperaciones

#|------------------------------------------------------------------------
Funcion local DibujarDatos que Dibujará los datos de la tabla de evaluación.
-Identificador de bajo nivel tabla: Es el vector de vectores el cual extraeremos los valores.
-Identificador de bajo nivel counter: Es el contador de la funcion.
-Identificador de bajo nivel string: Es el que guarda el string a usar.
-Identificador de bajo nivel minimo: Es el menor valor de x.
-Identificador de bajo nivel maximo: Es el mayor valor de x.
-Identificador de bajo nivel saltos: Es el intervalo de x.
-Identificador de bajo nivel vectorOperaciones: Es el que guarda el vector de los resultados para ser usados.
|#    
(define (DibujarDatos tabla minimo maximo saltos counter vectorOperaciones)
(if (<= minimo maximo)
    [begin
      ((draw-string ventanaEvaluacion) (make-posn 10 (+ 18 (* counter 25))) (~a minimo) "Black")
      ((draw-string ventanaEvaluacion) (make-posn 170 (+ 18 (* counter 25))) (DibujarOperaciones tabla 0 "" minimo) "Black")
      ((draw-string ventanaEvaluacion) (make-posn 390 (+ 18 (* counter 25))) (~a (Operar tabla 0 0 minimo)) "Black")
      ((draw-rectangle ventanaEvaluacion) (make-posn 0 (* counter 25)) 160 25 "Teal")
      ((draw-rectangle ventanaEvaluacion) (make-posn 160 (* counter 25)) 220 25 "Teal")
      ((draw-rectangle ventanaEvaluacion) (make-posn 380 (* counter 25)) 160 25 "Teal")
      (DibujarDatos tabla (+ minimo saltos) maximo saltos (+ 1 counter) (vector-append vectorOperaciones (make-vector 1 (Operar tabla 0 0 minimo))))
    ];Fin begin
    ;De lo contrario
    vectorOperaciones
);Fin if (<= minimo maximo)
);Fin funcion funcion DibujarDatos
;Llamamos a la funcion DibujarDatos
(DibujarDatos tabla minimo maximo saltos 1 (vector))
);Fin funcion TercerPunto

;Llamamos a la funcion SegundoPunto.
(SegundoPunto ecuation)
;Identificador vectorCalculos que guardará el vector de los resultados.
(define vectorCalculos (TercerPunto ecuation minimoX maximoX intervalos))

#|------------------------------------------------------------------------
Funcion ValMaxY que saca el valor mayor de Y.
-Identificador de bajo nivel valores: Es el vector el cual extraeremos los valores.
-Identificador de bajo nivel counter: Es el contador de la funcion.
-Identificador de bajo nivel string: Es el que guarda el string a usar.
-Identificador de bajo nivel max: Es el mayor valor de Y.
|#   
(define (ValMaxY max valores counter)
(if (< counter (vector-length valores))
    (if (equal? (vector-ref valores counter) "Indefinido")
        (ValMaxY max valores (+ counter 1))
        ;De lo contrario
        (if (> (vector-ref valores counter) max)
            (ValMaxY (vector-ref valores counter) valores (+ counter 1))
            ;De lo contrario
            (ValMaxY max valores (+ counter 1))
        );Fin if (> (vector-ref valores counter) max)
    );Fin if (equal? (vector-ref valores counter) "Indefinido")
    ;De lo contrario
    max
);Fin if (< counter (vector-length valores))
);Fin funcion ValMaxY

#|------------------------------------------------------------------------
Funcion ValMinY que saca el valor menor de Y.
-Identificador de bajo nivel valores: Es el vector el cual extraeremos los valores.
-Identificador de bajo nivel counter: Es el contador de la funcion.
-Identificador de bajo nivel string: Es el que guarda el string a usar.
-Identificador de bajo nivel min: Es el menor valor de Y.
|#  
(define (ValMinY min valores counter)
(if (< counter (vector-length valores))
    (if (equal? (vector-ref valores counter) "Indefinido")
        (ValMinY min valores (+ counter 1))
        ;De lo contrario
        (if (< (vector-ref valores counter) min)
            (ValMinY (vector-ref valores counter) valores (+ counter 1))
            ;De lo contrario
            (ValMinY min valores (+ counter 1))
        );Fin if (< (vector-ref valores counter) min)
    );Fin if (equal? (vector-ref valores counter) "Indefinido")
    ;De lo contrario
    min
);Fin if (< counter (vector-length valores))
);Fin funcion ValMinY

#|------------------------------------------------------------------------
Funcion VectorX que obtiene los valores de X y los coloca en un vector.
-Identificador de bajo nivel valores: Es el vector el cual tiene los valores.
-Identificador de bajo nivel counter: Es el contador de la funcion.
-Identificador de bajo nivel string: Es el que guarda el string a usar.
-Identificador de bajo nivel minimo: Es el menor valor de x.
-Identificador de bajo nivel maximo: Es el mayor valor de x.
|#  
(define (VectorX counter valores minimo maximo)
  (if (<= minimo maximo)
      (VectorX (+ counter 1) (vector-append valores (make-vector 1 minimo)) (+ minimo intervalos) maximo )
      ;De lo contrario
      valores
  );Fin if (<= minimo maximo)
);Fin funcion VectorX

;Identificador vectorX que guarda el vector de los valores de X.
(define vectorX (VectorX 0 (vector) minimoX maximoX))
;Identificador maximoY que guarda el valor maximo de Y.
(define maximoY (ValMaxY (vector-ref vectorCalculos 0) vectorCalculos 0))
;Identificador minimoY que guarda el valor minimo de Y.
(define minimoY (ValMinY maximoY vectorCalculos 0))
;Identificador pixelX que guarda el valor de pixeles para moverse en el eje X.
(define pixelX (/ 800 (if (= (abs (- minimoX maximoX)) 0) 1 (abs (- minimoX maximoX)) ) ))
;Identificador pixelY que guarda el valor de pixeles para moverse en el eje Y.
(define pixelY (/ 800 (if (= (abs (- minimoY maximoY)) 0) 1 (abs (- minimoY maximoY)) ) ))
;Identificador nLineasY que guarda el valor de lineas horizontales que habrá en el gráfico.
(define nLineasY (- (/  (abs (- minimoX maximoX)) intervalos) 1))
;Identificador valorLinea que guarda el valor dentro del eje Y de la linea horizontal.
(define valorLinea (/ (abs (- minimoY maximoY)) nLineasY))

#|------------------------------------------------------------------------
Funcion DibujarGrafica que dibujará la funcion en el plano.
-Identificador de bajo nivel valores: Es el vector el cual extraeremos los valores.
|#  
(define (DibujarGrafica valores)
;Identificador ejeX que guarda el valor hipotético del eje X.
(define ejeX (* (- minimoY) pixelY))
;Identificador ejeY que guarda el valor hipotético del eje Y.
(define ejeY (* (- minimoX) pixelX))
;Definimos la ventana que mostrará la gráfica.
(define grafica (open-viewport "Grafica" 830 825))

#|------------------------------------------------------------------------
Funcion local DibujarFuncion que dibujará la línea por donde pasa funcion en el plano.
-Identificador de bajo nivel valores: Es el vector el cual extraeremos los valores.
-Identificador de bajo nivel counter: Es el contador de la funcion.
|# 
(define (DibujarFuncion valores counter)
(if (< (+ counter 1) (vector-length valores))
    (if (or (equal? (vector-ref valores counter) "Indefinido") (equal? (vector-ref valores (+ 1 counter)) "Indefinido"))
        (DibujarFuncion valores (+ counter 1))
        ;De lo contrario
        [begin
          ((draw-string grafica) (make-posn (+ (* (vector-ref vectorX counter) pixelX)5 ejeY) (- 805 (+ ejeX (- (* (vector-ref valores counter) pixelY)7)))) "X" "RoyalBlue")
          ((draw-string grafica) (make-posn (+ (* (vector-ref vectorX (+ counter 1)) pixelX)5 ejeY) (- 805 (+ ejeX (- (* (vector-ref valores (+ counter 1)) pixelY)7)))) "X" "RoyalBlue")
          ((draw-line grafica) (make-posn (+ (* (vector-ref vectorX counter) pixelX)10 ejeY) (- 803 (+ ejeX (- (* (vector-ref valores counter) pixelY)5)))) (make-posn (+ 10 ejeY(* (vector-ref vectorX (+ counter 1)) pixelX)) (- 803 (+ ejeX (- (* (vector-ref valores (+ counter 1) ) pixelY)5)))) "RoyalBlue")
          (DibujarFuncion valores (+ counter 1))            
        ];Fin begin
    );Fin if (or (equal? (vector-ref valores counter) "Indefinido") (equal? (vector-ref valores (+ 1 counter)) "Indefinido"))
    ;De lo contrario
    (void)
);Fin if (< (+ counter 1) (vector-length valores))
);Fin funcion DibujarFuncion

#|------------------------------------------------------------------------
Funcion local DibujarLineasX que dibujará las lineas verticales en el plano.
-Identificador de bajo nivel valores: Es el vector el cual extraeremos los valores.
-Identificador de bajo nivel counter: Es el contador de la funcion.
|#   
(define (DibujarLineasX valores counter)
(if (< counter (vector-length valores))
    (if (or (equal? (vector-ref valores counter) "Indefinido") )
        (DibujarLineasX valores (+ counter 1))
        ;De lo contrario
        [begin
          ((draw-line grafica) (make-posn (+ (* (vector-ref vectorX counter) pixelX)10 ejeY) 0) (make-posn (+ 10 ejeY(* (vector-ref vectorX counter) pixelX)) 830) "LightGray")
          ((draw-string grafica) (make-posn (+ (* (vector-ref vectorX counter) pixelX)4 ejeY) (- 800 (- ejeX 20))) (~a (vector-ref vectorX counter)) "Black")          
          (DibujarLineasX valores (+ counter 1))            
        ];Fin begin
    );Fin if (or (equal? (vector-ref valores counter) "Indefinido") )
    ;De lo contrario
    (void)
);Fin if (< counter (vector-length valores))
);Fin funcion DibujarLineasX

#|------------------------------------------------------------------------
Funcion local DibujarLineasY que dibujará las lineas horizontales en el lado positivo en el plano.
-Identificador de bajo nivel valores: Es el valor con el que trabajaremos.
-Identificador de bajo nivel counter: Es el contador de la funcion.
|# 
(define (DibujarLineasY valores counter)
(if (<= counter nLineasY)
    (if (or (equal? valores "Indefinido") )
        (DibujarLineasY valores (+ counter 1))
        ;De lo contrario
        [begin
          ((draw-line grafica) (make-posn 0 (- 803 (+ (* valores pixelY) (- ejeX 5))) ) (make-posn 830 (- 803 (+ (* valores pixelY) (- ejeX 5))) ) "LightGray")
          ((draw-string grafica) (make-posn (+ -40 ejeY) (- 813 (+ (* valores pixelY) (- ejeX 5)))) (~a valores) "Black")          
          (DibujarLineasY (+ valores valorLinea) (+ counter 1))            
        ];Fin begin
    );Fin if (or (equal? valores "Indefinido") )
    ;De lo contrario 
    (void)
);Fin if (<= counter nLineasY)
);Fin funcion DibujarLineasY
  
#|------------------------------------------------------------------------
Funcion local DibujarLineasYNegativas que dibujará las lineas horizontales en el lado negativo en el plano.
-Identificador de bajo nivel valores: Es el valor con el que trabajaremos.
-Identificador de bajo nivel counter: Es el contador de la funcion.
|# 
(define (DibujarLineasYNegativas valores counter)
(if (<= counter nLineasY)
    (if (or (equal? valores "Indefinido") )
        (DibujarLineasYNegativas valores (+ counter 1))
        ;De lo contrario
        [begin
          ((draw-line grafica) (make-posn 0 (- 803 (+ (* valores pixelY) (- ejeX 5))) ) (make-posn 830 (- 803 (+ (* valores pixelY) (- ejeX 5))) ) "LightGray")
          ((draw-string grafica) (make-posn (+ -40 ejeY) (- 813 (+ (* valores pixelY) (- ejeX 5)))) (~a valores) "Black")          
          (DibujarLineasYNegativas (- valores valorLinea) (+ counter 1))            
        ];Fin begin
    );Fin if (or (equal? valores "Indefinido") )
    ;De lo contrario
    (void)
);Fin if (<= counter nLineasY)
);Fin if DibujarLineasYNegativas
;Llamamos a la funcion DibujarLineasX
(DibujarLineasX vectorX 0)
;Llamamos a la funcion DibujarLineasYNegativas
(DibujarLineasYNegativas (- valorLinea) 0)
;Llamamos a la funcion DibujarLineasY
(DibujarLineasY valorLinea 0)
;Dibujamos el eje Y
((draw-line grafica) (make-posn (+ 10 ejeY) 0) (make-posn (+ 10 ejeY) 830) "Black")
;Dibujamos el eje X
((draw-line grafica) (make-posn 0 (- 803 (- ejeX 5))) (make-posn 815 (- 803 (- ejeX 5))) "Black")
;Llamamos a la funcion DibujarFuncion
(DibujarFuncion valores 0)
);Fin funcion DibujarGrafica
;Llamamos a la funcion DibujarGrafica
(DibujarGrafica vectorCalculos)