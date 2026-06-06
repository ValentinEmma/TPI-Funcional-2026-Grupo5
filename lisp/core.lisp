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
			        (print (logging-auditoria segundos estadoActual cambiar))
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
	(if (and (symbolp estadoActual) (symbolp cambiar))
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
		'ERROR
	)
)

;; Caso normal
;(transicion 'en-rojo 'verde)

;; Resultado esperado:
;; (EN-ROJO "cambiar a verde")

;; Caso normal
;(transicion 'en-verde 'amarillo)

;; Resultado esperado:
;; (EN-VERDE "cambiar a amarillo")

;; Caso alternativo
;(transicion 'en-rojo 'amarillo)

;; Resultado esperado:
;; (EN-ROJO "accion por defecto")

;; Caso de error
;(transicion 10 'verde)

;; Resultado esperado:
;; ERROR


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

;; Caso normal
;(timer 50)

;; Resultado esperado:
;; ROJO

;; Caso normal
;(timer 92)

;; Resultado esperado:
;; AMARILLO

;; Caso normal
;(timer 150)

;; Resultado esperado:
;; VERDE

;; Caso alternativo
;; Comienza un nuevo ciclo
;(timer 216)

;; Resultado esperado:
;; ROJO

;; Caso de error
;(timer 'hola)

;; Resultado esperado:
;; Error

;; ========================================================
;; FUNCIÓN: crea-informe
;; NATURALEZA: Impura (escribe en archivo)
;; ESTRATEGIA: Modular
;; IMPACTO: No destructiva
;; ========================================================
(defun crea-informe()
 	(with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :error :if-does-not-exist :create)
 		(format stream "Informe de Ejecución del Sistema Semafórico~%")
   		(format stream "=========================================~%")
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

;; CASO NORMAL
;(crea-informe)
;(logging-auditoria 100 'en-rojo 'amarillo)

;; Se debe recibir:
;NIL (respuesta de que se creó el informe con el encabezado)
;"Tiempo 100: la luz ha cambiado de EN-ROJO a AMARILLO" (linea que se muestra en el archivo) 

;; Caso alternativo
;(logging-auditoria 'en-rojo 'amarillo 80)

;; Caso de error
;llamar a crear-informe cuando ya fue creado, devolverá "ERROR"
;(logging-auditoria 5 'verde 50)

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
;(duracion-ciclo 30 10 60)

;;caso normal

;;caso error
;(duracion-ciclo 1.2 20 1.2)

;;caso error
;(duracion-ciclo 'veinte 'cuarenta 'cincuenta)

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
		((and (>= total-ciclo 35) (<= 150 total-ciclo)) "¡su tiempo esta en los estandares optimos!" )
		((> total-ciclo 150) "recomendacion: disminuya su tiempo paraa obtener un ciclo entre 35 a 150 segundos") 

	)
)
;; ==========================
;; CASOS DE PRUEBA
;; ==========================

;;caso normal
;(recomendacion-ciclo 120)


;;caso de ciclo demasiado corto
;(recomendacion-ciclo 20)


;; Caso de ciclo demasiado largo
;(recomendacion-ciclo 160)



;;caso error excepcion
;;ACLARACION: este tipo de error no sucederia 
;;ya que se usaria la funcion "(duracion-ciclo)"
;;la cual ya verifica si los parametros son unicamente enteros
;(recomendacion-ciclo 120.3)

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
;(ciclos-por-tiempo 15)

;; Resultado esperado:
;; 4

;; Caso alternativo
;; Tiempo menor a un ciclo completo
;; 3 minutos = 180 segundos
;(ciclos-por-tiempo 3)

;; Resultado esperado:
;; 0

;; Caso de error
;; Se ingresa un símbolo
;(ciclos-por-tiempo 'hola)

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
;(informe-distribucion-60min)

;; Resultado esperado:
;; ((ROJO 41.67)
;;  (AMARILLO 2.78)
;;  (VERDE 55.56))

;; Caso alternativo
;; Ejecutar varias veces para verificar que siempre
;; devuelve los mismos porcentajes
;(informe-distribucion-60min)

;; Resultado esperado:
;; ((ROJO 41.67)
;;  (AMARILLO 2.78)
;;  (VERDE 55.56))

;; ========================================================
;; FUNCIÓN: calcular-porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA: Función de Orden Superior (aritmética básica)
;; IMPACTO: No destructiva
;; ========================================================
(defun calcular-porcentaje (tiempo-color tiempo-ciclo)
  (let ((resultado (* (/ tiempo-color (float tiempo-ciclo)) 100)))
    (/ (round (* resultado 100)) 100.0)))
