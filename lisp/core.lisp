;; ========================================================
;; FUNCIÓN: sistema-semaforo-ext-1
;; NATURALEZA: Impura (realiza impresiones en pantalla)
;; ESTRATEGIA: Selectiva y Modular
;; IMPACTO: No destructiva
;; ========================================================

(defun sistema-semaforo-ext-1 (estadoActual cambiar segundos)
			(if (and (symbolp estadoActual)(symbolp cambiar) (numberp segundos))

			    (progn
			        (print "=== SISTEMA DE SEMAFOROS ===")
			        (print (transicion-ext-1 estadoActual cambiar))
			        (print (timer-ext-1 segundos))
			        (print (logging-auditoria segundos estadoActual cambiar))
			    )
			    (print "argumentos invalidos")
			)	
)

;; ========================================================
;; FUNCIÓN: transicion-ext-1
;; NATURALEZA: Pura
;; ESTRATEGIA: Selectiva y Modular
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion-ext-1 (estadoActual cambiar)
  (cond 
    ( (not (and (symbolp estadoActual) (symbolp cambiar)))
    	(pprint "error en los datos ingresados"))

    ( (and (equal estadoActual 'en-rojo) (equal cambiar 'intermitente))
         (list estadoActual "cambiar a intermitente"))

    ( (and (equal estadoActual 'intermitente) (equal cambiar 'amarillo))
         (list estadoActual "cambiar a amarillo"))

    ( (and (equal estadoActual 'en-amarillo)(equal cambiar 'intermitente))
         (list estadoActual "cambiar a intermitente"))

    ( (and (equal estadoActual 'intermitente)(equal cambiar 'verde))
         (list estadoActual "cambiar a verde"))

    ( (and (equal estadoActual 'verde)(equal cambiar 'intermitente))
         (list estadoActual "cambiar a intermitente"))

    ( (and (equal estadoActual 'intermitente)(equal cambiar 'rojo))
         (list estadoActual "cambiar a intermitente"))

    (t
         (list estadoActual "accion por defecto"))
   )
)



;; Caso normal
;(transicion-ext-1 'en-rojo 'intermitente)
;; Resultado esperado: (EN-ROJO "cambiar a intermitente")

;; Caso normal
;(transicion-ext-1 'intermitente 'en-amarillo)
;; Resultado esperado: (INTERMITENTE "cambiar a amarillo")

;; Caso alternativo
;(transicion-ext-1 'en-rojo 'en-amarillo)
;; Resultado esperado: (EN-ROJO "accion por defecto")

;; Caso de error
;(transicion-ext-1 10 'en-verde)
;; Resultado esperado: "error en los datos ingresados"

;; ========================================================
;; FUNCIÓN: timer-ext-1
;; NATURALEZA: Pura
;; ESTRATEGIA: Selectiva
;; IMPACTO: No destructiva
;; ========================================================

(defun timer-ext-1 (segundos)
  (let ((tiempo-ciclo (mod segundos 225)))

    (cond
      ((and (>= tiempo-ciclo 0)(<= tiempo-ciclo 89))'ROJO)
      ((and (>= tiempo-ciclo 90)(<= tiempo-ciclo 92))'INTERMITENTE)
      ((and (>= tiempo-ciclo 93)(<= tiempo-ciclo 98))'AMARILLO)
      ((and (>= tiempo-ciclo 99)(<= tiempo-ciclo 101))'INTERMITENTE)
      ((and (>= tiempo-ciclo 102)(<= tiempo-ciclo 221))'VERDE)
      ((and (>= tiempo-ciclo 222) (<= tiempo-ciclo 224)) 'INTERMITENTE)
      (t 'ERROR))))


;; Caso normal
;(timer-ext-1 50)
;; Resultado esperado: ROJO

;; Caso normal (Cae justo en la primera intermitencia)
;(timer-ext-1 91)
;; Resultado esperado: INTERMITENTE

;; Caso normal
;(timer-ext-1 95)
;; Resultado esperado: AMARILLO

;; Caso normal
;(timer-ext-1 150)
;; Resultado esperado: VERDE

;; Caso alternativo (Comienza un nuevo ciclo exacto)
;(timer-ext-1 225)
;; Resultado esperado: ROJO

;; Caso de error
;(timer-ext-1 'hola)
;; Resultado esperado: ERROR


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
 	(let ((fechainicio (local-time:encode-timestamp
 						0    ; nanosegundos
 						00   ; segundos
 						00   ; minutos
 						12   ; hora
 						1    ; día
 						1    ; mes
 						2026)))

	(with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :append)   			
   			(format stream 
   				"~a - La luz ha cambiado de ~a a ~a~%"
					(local-time:format-timestring nil
 					(local-time:timestamp+ fechainicio segundos :sec)
 							:format '((:year 4) "-"
				  						(:month 2) "-"
				  						(:day 2) "-"
				  						" "
				  						(:hour 2) ":"
				  						(:min 2) ":"
				  						(:sec 2))
   				)
   			estadoActual
   			cambiar	
   			)
   )
   )
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
;; FUNCIÓN: duracion-ciclo-ext-1
;; NATURALEZA: Impura (realiza salida por pantalla mediante
;; "format")
;; ESTRATEGIA: Función simple (no recursiva, no utiliza
;; funciones de orden superior)
;; IMPACTO: No destructiva (no modifica ninguna estructura
;; de datos existente)
;; ========================================================


  (defun duracion-ciclo-ext-1 (rojo amarillo verde intermitente)
  (if (and (integerp rojo) (integerp amarillo) (integerp verde) (integerp intermitente))
      (let ((tiempo-un-ciclo (+ rojo amarillo verde (* intermitente 3)))) ; 
        (format t "El ciclo dura ~a segundos~%" tiempo-un-ciclo)
        tiempo-un-ciclo)
      "Error: uno de los parametros no es un numero entero")
  )

;; ==========================
;; CASOS DE PRUEBA
;; ==========================
;; Caso normal (90s rojo, 6s amarillo, 120s verde, 3s intermitencia)
;(duracion-ciclo-ext-1 90 6 120 3)
;; Resultado esperado: "El ciclo dura 225 segundos" y devuelve 225.

;; Caso alternativo / Error (se ingresa un decimal)
;(duracion-ciclo-ext-1 90.5 6 120 3)
;; Resultado esperado: "Error: uno de los parametros no es un numero entero"

;; Caso de error (se ingresan palabras)
;(duracion-ciclo-ext-1 'noventa 'seis 'ciento-veinte 'tres)
;; Resultado esperado: "Error: uno de los parametros no es un numero entero"




;; ========================================================
;; FUNCIÓN: recomendacion-ciclo -ext-1
;; NATURALEZA: Impura (realiza salida por pantalla)
;; ESTRATEGIA: Función simple (implementada mediante cond)
;; IMPACTO: No destructiva (no modifica estructuras de
;; datos existentes)
;; ========================================================

(defun recomendacion-ciclo-ext-1(total-ciclo)
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
;(recomendacion-ciclo-ext-1 120)


;; Caso alternativo (ciclo demasiado corto)
;(recomendacion-ciclo-ext-1 20)
;; Resultado esperado: "recomendacion: aumentar el tiempo del ciclo para obtener entre 35 a 150 segundos"

;; Caso alternativo (ciclo demasiado largo)
;(recomendacion-ciclo-ext-1 225)
;; Resultado esperado: "recomendacion: disminuya su tiempo para obtener un ciclo entre 35 a 150 segundos"


;;caso error excepcion
;;ACLARACION: este tipo de error no sucederia 
;;ya que se usaria la funcion "(duracion-ciclo)"
;;la cual ya verifica si los parametros son unicamente enteros
;(recomendacion-ciclo 120.3)

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo-ext-1
;; NATURALEZA: Pura
;; ESTRATEGIA: Aritmética
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo-ext-1 (minutos)
    (if (numberp minutos)
        (floor
            (/ (* minutos 60) 225)
        )
        'ERROR
    )
)
;; Caso normal
;; 15 minutos = 900 segundos
;; 900 / 225 = 4 ciclos exactos
;(ciclos-por-tiempo-ext-1 15)

;; Resultado esperado:
;; 4

;; Caso alternativo
;; Tiempo menor a un ciclo completo
;; 3 minutos = 180 segundos
;(ciclos-por-tiempo-ext-1 3)

;; Resultado esperado:
;; 0

;; Caso de error
;; Se ingresa un símbolo
;(ciclos-por-tiempo-ext-1 'hola)

;; Resultado esperado:
;; ERROR

;; ========================================================
;; FUNCIÓN: informe-distribucion-60min-ext-1
;; NATURALEZA: Pura (Realiza cálculos matemáticos sin efectos secundarios)
;; ESTRATEGIA: Modular (Llamada a funciones de apoyo)
;; IMPACTO: No destructiva
;; ========================================================
(defun informe-distribucion-60min-ext-1 ()
	  (let ((ciclo-total (+ 90 6 120 9))) ; Duración de un ciclo completo (225)
	    (list 
	      (list 'ROJO     (calcular-porcentaje 90 ciclo-total))
	      (list 'AMARILLO (calcular-porcentaje 6 ciclo-total))
	      (list 'VERDE    (calcular-porcentaje 120 ciclo-total))
          (list 'INTERMITENTE (calcular-porcentaje 9 ciclo-total)))
		)
	 )


;; Caso normal
;(informe-distribucion-60min-ext-1)

;; Resultado esperado:
;; ((ROJO 40.0)
;;  (AMARILLO 2.67)
;;  (VERDE 53.33)
;;  (INTERMITENTE 4.0))

;; Caso alternativo
;; Ejecutar varias veces para verificar que siempre
;; devuelve los mismos porcentajes
;(informe-distribucion-60min)

;; Resultado esperado:
;; ((ROJO 40.0)
;;  (AMARILLO 2.67)
;;  (VERDE 53.33)
;;  (INTERMITENTE 4.0))

;; ========================================================
;; FUNCIÓN: calcular-porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA: Función de Orden Superior (aritmética básica)
;; IMPACTO: No destructiva
;; ========================================================
(defun calcular-porcentaje (tiempo-color tiempo-ciclo)
  (let ((resultado (* (/ tiempo-color (float tiempo-ciclo)) 100)))
    (/ (round (* resultado 100)) 100.0)))
   

