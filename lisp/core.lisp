;====================================================0

;ACLARACION
;usamos el razonamiento en base a la clase virtual
;en tema de ciclo de semaforo
;el ciclo mas logico al q utilizamos es:
; ROJO- AMARILLO- VERDE- AMARILLO / ROJO- AMARILLO - VERDE- AMARILLO

;===================================================







;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Selectiva y Modular
;; IMPACTO: No destructiva
;; ========================================================
(defun transicion (estadoActual cambiar)
	(if (and (symbolp estadoActual) (symbolp cambiar))  ;verifica si es simbol
		(cond
			((and (equal estadoActual 'en-rojo) (equal cambiar 'verde))
				(list estadoActual "cambiar a verde"))

			((and (equal estadoActual 'en-amarillo) (equal cambiar 'rojo))
				(list estadoActual "cambiar a rojo"))
			((and (equal estadoActual 'en-verde) (equal cambiar 'amarillo))  ;ejecuta en caso de cumplir la condicion
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
		((and (>= tiempo-ciclo 0) (<= tiempo-ciclo 89)) 'ROJO)      ;condicionales para saber si el resto de la division en 216
		((and (>= tiempo-ciclo 90) (<= tiempo-ciclo 95)) 'AMARILLO) ;entra en alguna de estas condiciones
		((and (>=  tiempo-ciclo 96) (<= tiempo-ciclo 215)) 'VERDE)
		(t 'ERROR)
	)
)
)

; Caso norma
;(timer 50)

; Resultado esperado:
; ROJO

; Caso normal
;(timer 92)

; Resultado esperado:
; AMARILLO

; Caso normal
;(timer 150)

; Resultado esperado:
; VERDE

; Caso alternativo
; Comienza un nuevo ciclo
;(timer 216)

;; Resultado esperado:
;; ROJO

;; Caso de error
;(timer 'hola)

;; Resultado esperado:
;; Error

;; ========================================================
;; FUNCIÓN: logging-auditoria
;; NATURALEZA: Impura
;; ESTRATEGIA: Modular
;; IMPACTO: No destructiva
;; ========================================================
(defun logging-auditoria (segundos estadoActual cambiar) ;funcion modular q imprime el registro
	(format t 
		"~%Tiempo ~a: la luz ha cambiado de ~a a ~a~%" segundos estadoActual cambiar)

		'REGISTRADO-EXITOSAMENTE

)

;; ==========================
;; CASOS DE PRUEBA
;; ==========================

;; Caso normal
;(sistema-semaforo 'en-rojo 'verde 50)

;; Caso normal
;(sistema-semaforo 'en-verde 'amarillo 150)

;; Caso alternativo
;(sistema-semaforo 'en-rojo 'amarillo 80)

;; Caso de error
;(sistema-semaforo 5 'verde 50)

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
	(if 
		(and (integerp rojo)(integerp amarillo)(integerp verde))
			(let ( (tiempo-un-ciclo (+ rojo (* 2 amarillo) verde)) ) ;guarda en tiempu un ciclo cuanto dura un ciclo
				(format t "El ciclo dura ~a's~%" tiempo-un-ciclo)							
				tiempo-un-ciclo ;esta linea es para q devuelva el valor sumado
		)"Error: uno de los parametros no es un numero entero"
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


(defun recomendacion-ciclo (total-ciclo)
    (cond
        ((< total-ciclo 35)"recomendacion: aumentar el tiempo del ciclo para obtener entre 35 a 150 segundos")    ;verificacion de si es menora al numero
        ((and (>= total-ciclo 35) (<= total-ciclo 150)) "su tiempo esta en los estandares optimos")               ;verifica si esta en estandar
        (t "recomendacion: disminuya su tiempo para obtener un ciclo entre 35 a 150 segundos")					  ;si no es ninguno entonces es mayor
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



;;caso error exepcion
;;caso error excepcion
;;ACLARACION: este tipo de error no sucederia 
;;ya q se usaria la funcion "(duracion-ciclo)"
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
		(/ (* minutos 60) 222)       ;dividido 222 q es la suma del ciclo logico (ROJO-AMARILLO-VERDE-AMARILLO)
	)
	'ERROR
	)
)
;; Caso normal
;; 15 minutos = 900 segundos
;; 900 / 216 = 4 ciclos completos;(ciclos-por-tiempo 15)

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
	(let ((ciclo-total (+ 90 12 120))) ; Duración de un ciclo completo (222s)
		(list 
			(list 'ROJO     (calcular-porcentaje 90 ciclo-total))
			(list 'AMARILLO (calcular-porcentaje 12 ciclo-total))
			(list 'VERDE    (calcular-porcentaje 120 ciclo-total)) ;usamos funcion q esta debajo
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
	(let ((resultado (* (/ tiempo-color (float tiempo-ciclo)) 100)))    ;proceso logico para sacar porcentaje
		(/ (round (* resultado 100)) 100.0)
	)
)                        												
