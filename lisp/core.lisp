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
;; FUNCIÓN: informe-distribucion-60min
;; NATURALEZA: Pura (Realiza cálculos matemáticos sin efectos secundarios)
;; ESTRATEGIA: Modular (Llamada a funciones de apoyo)
;; IMPACTO: No destructiva
;; ========================================================
(defun informe-distribucion-60min ()
  "Calcula el porcentaje de cada color en 1 hora (3600 segundos)"
  (let* ((tiempo-total 3600)
         (ciclo-total (+ 90 6 120)) ; Duración de un ciclo completo (216s)
         (proporcion (/ tiempo-total ciclo-total))) ; Cuántas veces entra el ciclo en 1h
    
    (list 
      (list 'ROJO     (calcular-porcentaje 90 ciclo-total))
      (list 'AMARILLO (calcular-porcentaje 6 ciclo-total))
      (list 'VERDE    (calcular-porcentaje 120 ciclo-total)))))

;; ========================================================
;; FUNCIÓN: calcular-porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA: Función de Orden Superior (aritmética básica)
;; IMPACTO: No destructiva
;; ========================================================
(defun calcular-porcentaje (tiempo-color tiempo-ciclo)
  "Retorna el porcentaje redondeado a dos decimales"
  (let ((resultado (* (/ tiempo-color (float tiempo-ciclo)) 100)))
    (/ (round (* resultado 100)) 100.0)))
