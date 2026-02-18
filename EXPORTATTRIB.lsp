
(defun c:EXPORTATTRIB (/ blk tag fname ss i ent attList att tagVal csvData f) 

  ;; Demander le nom du bloc
  (setq blk (getstring "\nNom du bloc a extraire : "))

  ;; Demander le nom de l'attribut
  (setq tag (strcase (getstring "\nTag de l'attribut a extraire : ")))

  ;; Demander le chemin du fichier CSV
  (setq fname (getfiled "Nom du fichier CSV" "export.csv" "csv" 1))

  ;; Sélection des blocs dans le dessin
  (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 blk))))

  (if ss 
    (progn 
      (princ (strcat "\nNombre de blocs trouves : " (itoa (sslength ss))))

      ;; Préparer la première ligne CSV
      (setq csvData "HANDLE;BLOC;TAG;VALEUR\n")

      ;; Parcours des blocs
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq attList (vlax-invoke (vlax-ename->vla-object ent) 'GetAttributes))

        ;; Parcours des attributs d’un bloc
        (foreach a attList 
          (if (= (strcase (vla-get-TagString a)) tag) 
            (progn 
              (setq tagVal (vla-get-TextString a))
              (setq csvData (strcat 
                              csvData
                              (cdr (assoc 5 (entget ent)))
                              ";" ; Handle
                              blk
                              ";" ; Nom du bloc
                              tag
                              ";" ; Tag de l’attribut
                              tagVal
                              "\n" ; Valeur
                            )
              )
            )
          )
        )

        (setq i (1+ i))
      )

      ;; Écriture dans le fichier
      (setq f (open fname "w"))
      (write-line csvData f)
      (close f)

      (princ (strcat "\nExport termine ! Fichier genere : " fname))
    )
    (princ "\nAucun bloc correspondant n'a ete trouve !")
  )

  (princ)
)
