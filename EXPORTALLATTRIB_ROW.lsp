
(defun c:EXPORTALLATTRIB_ROW (/ blk fname ss i ent attList csvData f) 

  ;; Demander le nom du bloc
  (setq blk (getstring "\nNom du bloc a extraire : "))

  ;; Chemin du fichier CSV
  (setq fname (getfiled "Nom du fichier CSV" "export.csv" "csv" 1))

  ;; Sélection de tous les blocs du type demandé
  (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 blk))))

  (if ss 
    (progn 
      (princ (strcat "\nNombre de blocs trouves : " (itoa (sslength ss))))

      ;; En‑têtes CSV
      (setq csvData "HANDLE;BLOC;TAG;VALEUR\n")

      ;; Parcours des blocs
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq attList (vlax-invoke (vlax-ename->vla-object ent) 'GetAttributes))

        ;; Parcours des attributs
        (foreach a attList 
          (setq csvData (strcat 
                          csvData
                          (cdr (assoc 5 (entget ent)))
                          ";" ; Handle du bloc
                          blk
                          ";" ; Nom du bloc
                          (vla-get-TagString a)
                          ";" ; Nom du tag
                          (vla-get-TextString a)
                          "\n" ; Valeur de l’attribut
                        )
          )
        )

        (setq i (1+ i))
      )

      ;; Écriture du fichier
      (setq f (open fname "w"))
      (write-line csvData f)
      (close f)
      (princ (strcat "\nFichier genere : " fname))
    )
    (princ "\nAucun bloc correspondant trouve.")
  )

  (princ)
)
