;; ========================================================
;; FUNCIÓN: sistema-semaforo
;; NATURALEZA: Impura (realiza impresiones en pantalla)
;; ESTRATEGIA: Selectiva y Modular
;; IMPACTO: No destructiva
;; ========================================================

(defun sistema-semaforo (estadoActual cambiar segundos)
			(if (and (symbolp estadoActual)(numberp cambiar) (symbolp segundos))

			    (progn
			        (print "=== SISTEMA DE SEMAFOROS ===")
			        (print (transicion estadoActual cambiar))
			        (print (timer segundos))
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

