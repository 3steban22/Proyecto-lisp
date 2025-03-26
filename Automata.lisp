(defun vecindario (l)
 ;Busca los vecinos de cada posición del arreglo. y la salida es el vecindario de toda la generación
  (mapcar 'list
          (append (last l) (butlast l))
          l
          (append (cdr l) (list (car l)))))

(defun size (l)
;Devuelve el tamañode una lista
  (length l))

(defun binario (r)
;Convierte un número decimal a una lista binaria y la completa hasta n caracteres, donde n es el tamaño de la generación padre.
  (let ((tamano (decimal-a-binario r)))
    (concatenar-cero tamano (- 8 (size tamano)))))

(defun concatenar-cero (lista cantidad)
;Agrega ceros a la izquierda para que la regla tenga la misma cantidad de reglas de las que se requiere
  (if (= 0 cantidad)
      lista
      (cons 0 (concatenar-cero lista (- cantidad 1)))))

(defun decimal-a-binario (r)
;Convierte un número decimal a binario.
  (if (= r 0)
      nil
    (append (decimal-a-binario (truncate (/ r 2))) (list (mod r 2)))))

(defun decimal (l)
;Convierte una lista  de números binarios a decimal.
  (if (null l)
      0
      (+ (* (car l) (expt 2 (- (size l) 1)))
         (decimal (cdr l)))))

(defun contar (l)
 ;Convierte una lista de listas de números binarios a una lista de números enteros del 0 al 7 utilizando la función decimal.
  (if (null l)
      nil
    (cons (decimal (car l)) (contar (cdr l)))))

(defun aplicaRegla (vecinos regla)
 ;Aplica la regla a un vecino y devuelve el valor en la posición que daba la regla.
  (let ((indice (decimal vecinos)))
    (nth indice regla)))

(defun aplica (generacion regla)
 ;Aplica la regla a cualquier generación y devuelve la nueva generación.
  (let ((veci (vecindario generacion))
        (reglabi (binario regla)))
    (mapcar (lambda (v) (aplicaRegla v (reverse reglabi))) veci)))

(defun itera (generacion regla pasos)
 ;Itera el autómata celular por la cantidad de generaciones dadas por el usuario.
  (if (= pasos 0)
      (list generacion)
    (let ((new (aplica generacion regla)))
      (cons generacion (itera new regla (1- pasos))))))

(defun imprimir (n generaciones)
;Imprime las generacione hasta llegar a 0.
  (if generaciones
    (impri (car generaciones)))
  (terpri)
  (if generaciones
    (imprimir (1- n) (cdr generaciones))))

(defun impri (generacion)
 ;Imprime una generación del autómata celular.
  (if generacion
      (princ (if (= (car generacion) 0) " " "*")))
  (if generacion 
    (impri (cdr generacion))))

(defun run (generacion-inicial regla pasos)
 ;Inicializa y ejecuta el autómata celular.
  (let ((generaciones (itera generacion-inicial regla pasos)))
    (imprimir pasos generaciones)))

(run '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 30 150)
