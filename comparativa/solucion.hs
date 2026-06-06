{-
esta funcion recibe el color actual del semaforo y el color
al que se desea cambiar y devuelve una accion a realizar

FUNCION: transicion
NATURALEZA: Pura
ESTRATEGIA: Guards
IMPACTO: No destructiva
-}
transicion :: String -> String -> String
transicion colorActual cambiarA
    | colorActual == "en rojo" && cambiarA == "verde" = "cambiar a verde"
    | colorActual == "en amarillo" && cambiarA == "rojo" = "cambiar a rojo"
    | colorActual == "en verde" && cambiarA == "amarillo" = "cambiar a amarillo"
    | otherwise = "accion por defecto"


{-
Calcula en qué segundo del ciclo del semáforo se encuentra 
el sistema utilizando mod
-}

tiempoCiclo :: Int -> Int
tiempoCiclo segundos = mod segundos 216

{-
Determina qué color debe mostrar el semáforo según el 
segundo actual dentro del ciclo.

FUNCIÓN: timer
NATURALEZA: Pura
ESTRATEGIA: Selectiva mediante guards
IMPACTO: No destructiva
PROPÓSITO: Determinar el color activo del semáforo
           según un tiempo dado en segundos.
-}
timer :: Int -> String
timer segundos
    | tiempoCiclo >= 0 && tiempoCiclo <= 89 = "ROJO"
    | tiempoCiclo >= 90 && tiempoCiclo <= 95 = "AMARILLO"
    | tiempoCiclo >= 96 && tiempoCiclo <= 215 = "VERDE"
    | otherwise = "ERROR" 
    where
        tiempoCiclo = mod segundos 216

{-
where sirve para definir variables locales dentro de una funcion

CASOS DE PRUEBA:

1. TRANSICION:
Caso normal
transicion "en rojo" "verde"
RESULTADO: "cambiar a verde"

Caso alternativo
transicion "en rojo" "amarillo"
RESULTADO: "accion por defecto"

2. TIMER:
Caso normal
timer 92
RESULTADO: "AMARILLO"

Caso alternativo
timer 216
RESULTADO: "ROJO"
-}
