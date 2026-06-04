;; ========================================================
;; FUNCIÓN: sistema-semaforo
;; NATURALEZA: Impura (realiza impresiones en pantalla)
;; ESTRATEGIA: Selectiva y Modular
;; IMPACTO: No destructiva
;; ========================================================

(defun sistema-semaforo (estadoActual cambiar segundos)
			(if (and (symbolp estadoActual)(symbolp cambiar) (numberp segundos))

			    (progn
			        (print "=== SISTEMA DE SEMAFOROS ===")
			        (print (transicion estadoActual cambiar))
			        (print (timer segundos))
			        (logging-auditoria segundos estadoActual cambiar)
			    )
			    (print "argumentos invalidos")
			)	
)

;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Selectiva y Modular
;; IMPACTO: No destructiva
;; ========================================================
(defun transicion (estadoActual cambiar)
					(cond
						((and (equal estadoActual 'en-rojo) (equal cambiar 'verde))
								(list estadoActual "cambiar a verde"))

						((and (equal estadoActual 'en-amarillo) (equal cambiar 'rojo))
								(list estadoActual "cambiar a rojo"))
						((and (equal estadoActual 'en-verde) (equal cambiar 'amarillo))
								(list estadoActual "cambiar a amarillo"))
						(t 
							(list estadoActual "accion por defecto")
						)
					)
)


;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Selectiva
;; IMPACTO: No destructiva
;; ========================================================
(defun timer (segundos)
				(let
					((tiempo-ciclo (mod segundos 216)))

					(cond
						((and (>= tiempo-ciclo 0) (<= tiempo-ciclo 89)) 'ROJO)
						((and (>= tiempo-ciclo 90) (<= tiempo-ciclo 95)) 'AMARILLO)
						((and (>=  tiempo-ciclo 96) (<= tiempo-ciclo 215)) 'VERDE)
						(t 'ERROR)
					)
				)
)

;; ========================================================
;; FUNCIÓN: logging-auditoria
;; NATURALEZA: Impura
;; ESTRATEGIA: Modular
;; IMPACTO: No destructiva
;; ========================================================
(defun logging-auditoria (segundos estadoActual cambiar)
	
	(format t 
		"~%Tiempo ~a: la luz ha cambiado de ~a a ~a~%" segundos estadoActual cambiar)

		'REGISTRADO-EXITOSAMENTE

)

;; ==========================
;; CASOS DE PRUEBA
;; ==========================

;; Caso normal
(sistema-semaforo 'en-rojo 'verde 50)

;; Caso normal
(sistema-semaforo 'en-verde 'amarillo 150)

;; Caso alternativo
(sistema-semaforo 'en-rojo 'amarillo 80)

;; Caso de error
(sistema-semaforo 5 'verde 50)

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Impura (realiza salida por pantalla mediante
;; "format")
;; ESTRATEGIA: Función simple (no recursiva, no utiliza
;; funciones de orden superior)
;; IMPACTO: No destructiva (no modifica ninguna estructura
;; de datos existente)
;; ========================================================

(defun duracion-ciclo(rojo amarillo verde)
  (cond
    (
    	(not (and (integerp rojo)(integerp amarillo)(integerp verde)) )
     "Error: uno de los parametros no es un numero entero"
    )
    (t
     (let ((tiempo-un-ciclo (+ rojo amarillo verde)) )
       (format t "El ciclo dura ~a's~%" tiempo-un-ciclo)
       ;;se retorna el valor calculado 
       ;;por si es necesaria su utilizacion posteriormente
       tiempo-un-ciclo
      )
    )
  )
)

;; ==========================
;; CASOS DE PRUEBA
;; ==========================

;;caso normal
(duracion-ciclo 30 10 60)

;;caso normal

;;caso error
(duracion-ciclo 1.2 20 1.2)

;;caso error
(duracion-ciclo 'veinte 'cuarenta 'cincuenta)

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Impura (realiza salida por pantalla)
;; ESTRATEGIA: Función simple (implementada mediante cond)
;; IMPACTO: No destructiva (no modifica estructuras de
;; datos existentes)
;; ========================================================

(defun recomendacion-ciclo(total-ciclo)
	(cond 
		((> 35 total-ciclo) "recomendacion: aumentar el tiempo del ciclo para obtener entre 35 a 150 segundos")
		((and (>= total-ciclo 35) (>= 150 total-ciclo)) "¡su tiempo esta en los estandares optimos!" )
		((> total-ciclo 150) "recomendacion: disminuya su tiempo paraa obtener un ciclo entre 35 a 150 segundos") 

	)
)
;; ==========================
;; CASOS DE PRUEBA
;; ==========================

;;caso normal
(recomendacion-ciclo 120)


;;caso normal
(recomendacion-ciclo 20)


;; Caso de ciclo demasiado corto
(recomendacion-ciclo 160)



;;caso error exepcion
;;ACLARACION: este tipo de error no sucederia 
;;ya q se usaria la funcion "(duracion-ciclo)"
;;la cual ya verifica si los parametros son unicamente enteros
(recomendacion-ciclo 120.3)

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmética
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
    (if (numberp minutos)
        (floor
            (/ (* minutos 60) 216)
        )
        'ERROR
    )
)
;; Caso normal
;; 15 minutos = 900 segundos
;; 900 / 216 = 4 ciclos completos
(ciclos-por-tiempo 15)

;; Resultado esperado:
;; 4

;; Caso alternativo
;; Tiempo menor a un ciclo completo
;; 3 minutos = 180 segundos
(ciclos-por-tiempo 3)

;; Resultado esperado:
;; 0

;; Caso de error
;; Se ingresa un símbolo
(ciclos-por-tiempo 'hola)

;; Resultado esperado:
;; ERROR

;; ========================================================
;; FUNCIÓN: informe-distribucion-60min
;; NATURALEZA: Pura (Realiza cálculos matemáticos sin efectos secundarios)
;; ESTRATEGIA: Modular (Llamada a funciones de apoyo)
;; IMPACTO: No destructiva
;; ========================================================
(defun informe-distribucion-60min ()
	  (let ((ciclo-total (+ 90 6 120))) ; Duración de un ciclo completo (216s)
	    (list 
	      (list 'ROJO     (calcular-porcentaje 90 ciclo-total))
	      (list 'AMARILLO (calcular-porcentaje 6 ciclo-total))
	      (list 'VERDE    (calcular-porcentaje 120 ciclo-total))
		)
	 )
)

;; Caso normal
(informe-distribucion-60min)

;; ========================================================
;; FUNCIÓN: calcular-porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA: Función de Orden Superior (aritmética básica)
;; IMPACTO: No destructiva
;; ========================================================
(defun calcular-porcentaje (tiempo-color tiempo-ciclo)
  (let ((resultado (* (/ tiempo-color (float tiempo-ciclo)) 100)))
    (/ (round (* resultado 100)) 100.0)))

;; ========================================================
;; FUNCIÓN: analisis-de-ciclos
;; NATURALEZA: Pura
;; ESTRATEGIA: Función de Orden Superior (aritmética básica)
;; IMPACTO: No destructiva
;; ========================================================
