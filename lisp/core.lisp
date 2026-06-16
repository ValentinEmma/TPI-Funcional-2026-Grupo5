;;--DOCUMENTACION--
;;Como grupo, decidimos dejar las funciones por separado y no realizar una 
;;función principal de forma que el cliente pueda 
;;elegir como las usará y cuántas veces llamará a las funciones.

;;Las funciones que tienen "ext-1" indican que fueron actualizadas para
;;incluir el requerimiento de las luces intermitentes.

;====================================================0

;ACLARACION
;usamos el razonamiento en base a la clase virtual
;en tema de ciclo de semaforo
;el ciclo mas logico al q utilizamos es:
; ROJO -INTERMITENTE - AMARILLO- INTERMITENTE -  VERDE- INTERMITENTE - AMARILLO -INTERMITENTE / ROJO -INTERMITENTE - 
; AMARILLO- INTERMITENTE -  VERDE- INTERMITENTE - AMARILLO -INTERMITENTE 

;===================================================








;; ========================================================
;; FUNCIÓN: transicion-ext-1
;; NATURALEZA: Pura
;; ESTRATEGIA: Selectiva y Modular
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion-ext-1 (estadoActual cambiar)
  (cond 
      ((not (and (symbolp estadoActual) (symbolp cambiar)))
    	(pprint "error en los datos ingresados"))
	  
	  ((and (equal estadoActual 'en-rojo) (equal cambiar 'intermitente))
         (list estadoActual "cambiar a intermitente"))
	  
	  ((and (equal estadoActual 'intermitente) (equal cambiar 'amarillo))
         (list estadoActual "cambiar a amarillo"))
	  
	  ((and (equal estadoActual 'en-amarillo)(equal cambiar 'intermitente))
         (list estadoActual "cambiar a intermitente"))
	  
	  ((and (equal estadoActual 'intermitente)(equal cambiar 'verde))
         (list estadoActual "cambiar a verde"))
	  
	  ((and (equal estadoActual 'verde)(equal cambiar 'intermitente))
         (list estadoActual "cambiar a intermitente"))
	  
	  ((and (equal estadoActual 'intermitente)(equal cambiar 'rojo))  ;condicionales para cada uno de los casos posibles q son aceptados
         (list estadoActual "cambiar a intermitente"))
	  
	  (t (list estadoActual "accion por defecto")))) ;caso contrario muestra este mensaje

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
  (let ((tiempo-ciclo (mod segundos 234)))
   	(cond
   		((and (>= tiempo-ciclo 0)(<= tiempo-ciclo 89))'ROJO)   ;misma logica que el anterior iterancia
   		((and (>= tiempo-ciclo 90)(<= tiempo-ciclo 92))'INTERMITENTE) ;pero con 234 como mod. ya q es la suma de
   		((and (>= tiempo-ciclo 93)(<= tiempo-ciclo 98))'AMARILLO)			;rojo= 90 + intermitente=4*3 + amarillo=2*6 + verde =120
  		((and (>= tiempo-ciclo 99)(<= tiempo-ciclo 101))'INTERMITENTE)
  		((and (>= tiempo-ciclo 102)(<= tiempo-ciclo 221))'VERDE)
  		((and (>= tiempo-ciclo 222) (<= tiempo-ciclo 224)) 'INTERMITENTE)
  		((and (>= tiempo-ciclo 225)(<= tiempo-ciclo 233)) 'ROJO)
 		  (t 'ERROR)
 		)
 	)
)

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

;;--DOCUMENTACION--
;;La función crea-informe fue agregada para crear el archivo y 
;;que el encabezado del informe se escriba una única vez, porque no pudimos encontrar
;;una forma de que el informe escriba una sola vez el encabezado.

;; ========================================================
;; FUNCIÓN: crea-informe
;; NATURALEZA: Impura (escribe en archivo)
;; ESTRATEGIA: Modular
;; IMPACTO: No destructiva
;; ========================================================
(defun crea-informe()
 	(with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :error :if-does-not-exist :create)
 		(format stream "Informe de Ejecución del Sistema Semafórico~%")
   		(format stream "=========================================~%")))

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
	(with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :append)   			;se abre el archivo
   			(format stream 
   				"~a - La luz ha cambiado de ~a a ~a~%" ;se
					(local-time:format-timestring nil ;en caso de que no exista
 					(local-time:timestamp+ fechainicio segundos :sec) ;en caso de q si
 							:format '((:year 4) "-"
				  						(:month 2) "-"
				  						(:day 2) "-"
				  						" "
				  						(:hour 2) ":"
				  						(:min 2) ":"
				  						(:sec 2)))
   			estadoActual
   			cambiar	)))) ;valores a tomar

;; ==========================
;; CASOS DE PRUEBA
;; ==========================

;; CASO NORMAL
;(crea-informe)
;(logging-auditoria 100 'en-rojo 'amarillo)

;; Se debe recibir:
;NIL (respuesta de que se creó el informe con el encabezado)
;"2026-01-01- 12:01:40: la luz ha cambiado de EN-ROJO a AMARILLO" (linea que se muestra en el archivo) 

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
  (if (and (integerp rojo) (integerp amarillo) (integerp verde) (integerp intermitente)) ;se verifica los parametros
      (let ((tiempo-un-ciclo (+ rojo (* 2 amarillo) verde (* intermitente 4)))) ;se suma el ciclo r.i.a.i.v.i.a.i...
        (format t "El ciclo dura ~a segundos~%" tiempo-un-ciclo)
        tiempo-un-ciclo)   ;se devuelve la suma
      "Error: uno de los parametros no es un numero entero"))

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

(defun recomendacion-ciclo-ext-1 (total-ciclo)
  (cond
    ((< total-ciclo 35) "recomendacion: aumentar el tiempo del ciclo para obtener entre 35 a 150 segundos")
    ((and (>= total-ciclo 35) (<= total-ciclo 150)) "¡su tiempo esta en los estandares optimos!")
    ((> total-ciclo 150) "recomendacion: disminuya su tiempo para obtener un ciclo entre 35 a 150 segundos")
    (t "error: valor invalido")  ;misma funcion q evalua en q condicion entra el ciclo total ya calculado previamente
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
            (/ (* minutos 60) 234)) ;nuevo numero para dividir (234)
        'ERROR))
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
;; FUNCIÓN: calcular-porcentaje
;; NATURALEZA: Pura
;; ESTRATEGIA: Función de Orden Superior (aritmética básica)
;; IMPACTO: No destructiva
;; ========================================================
(defun calcular-porcentaje (tiempo-color ciclo-total)
  (/ (round (* (/ tiempo-color (float ciclo-total)) 10000)) 100.0))

;; ========================================================
;; FUNCIÓN: informe-distribucion-60min-ext-1
;; NATURALEZA: Pura (Realiza cálculos matemáticos sin efectos secundarios)
;; ESTRATEGIA: Modular (Llamada a funciones de apoyo)
;; IMPACTO: No destructiva
;; ========================================================
(defun informe-distribucion-60min-ext-1 ()
	  (let ((ciclo-total (+ 90 12 120 12))) ; Duración de un ciclo completo (234)
	    (list 
	      (list 'ROJO     (calcular-porcentaje 90 ciclo-total) '%)     ;se usa funcion calcular porcentaje q esta codeado abajo
	      (list 'AMARILLO (calcular-porcentaje 12 ciclo-total) '%)
	      (list 'VERDE    (calcular-porcentaje 120 ciclo-total) '%)
          (list 'INTERMITENTE (calcular-porcentaje 12 ciclo-total) '%))))


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
;(informe-distribucion-60min-ext-1)

;; Resultado esperado:
;; ((ROJO 40.0)
;;  (AMARILLO 2.67)
;;  (VERDE 53.33)
;;  (INTERMITENTE 4.0))

;; Caso ERROR
;;(calcular-porcentaje 'noventa 225)

;; Resultado esperado: 
;; Error: 'noventa is not of type NUMBER'


   
