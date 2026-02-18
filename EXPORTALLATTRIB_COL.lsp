(vl-load-com)

(defun c:EXPORTALLATTRIB_COL (/ blk fname ss i ent vEnt attVar attList tags tag 
                              csvData f handle csvRow valsAlist 
                              ; helpers
                              _sa->list _csv-escape
                             ) 

  ;; --- helpers -------------------------------------------------------------

  ;; Convertit un Variant/SafeArray (GetAttributes) en liste AutoLISP
  (defun _sa->list (v) 
    (cond 
      ((= (type v) 'LIST) v)
      ((= (type v) 'VARIANT)
       (vlax-safearray->list (vlax-variant-value v))
      )
      ((= (type v) 'SAFEARRAY)
       (vlax-safearray->list v)
      )
      (t nil)
    )
  )

  ;; Échappe pour CSV (séparateur ;), supprime CR/LF, double les guillemets
  (defun _csv-escape (s / s1 needQuote) 
    (if (/= (type s) 'STR) 
      (setq s (vl-prin1-to-string s))
    )
    (setq s1 (vl-string-translate "\n\r" "  " s))
    (setq needQuote (or (wcmatch s1 "*;*") 
                        (wcmatch s1 "*\"*")
                        (wcmatch s1 "*,*")
                        (wcmatch s1 "* *")
                    )
    )
    (if needQuote 
      (strcat "\"" (vl-string-subst "\"\"" "\"" s1) "\"")
      s1
    )
  )

  ;; ------------------------------------------------------------------------

  ;; Demander le nom du bloc
  (setq blk (getstring T "\nNom du bloc a extraire : "))

  ;; Chemin du fichier CSV
  (setq fname (getfiled "Nom du fichier CSV" "export.csv" "csv" 1))

  ;; Sélectionner tous les INSERT du bloc donné
  (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 blk))))

  (if ss 
    (progn 
      (princ (strcat "\nNombre de blocs trouves : " (itoa (sslength ss))))

      ;; 1) PREMIER PASSAGE : collecter la liste unique des TAGs
      (setq tags '())
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq attList (vlax-invoke (vlax-ename->vla-object ent) 'GetAttributes))
        (if attList 
          (foreach a attList 
            (setq tag (vla-get-TagString a))
            (if (not (member tag tags)) 
              (setq tags (cons tag tags))
            )
          )
        )
        (setq i (1+ i))
      )
      ;; Tri alphabétique via strcmp
      (setq tags (vl-sort tags '<))

      ;; Construire l’en-tête CSV : HANDLE;BLOC;TAG1;TAG2;...
      (setq csvData (apply 'strcat 
                           (append 
                             (list "HANDLE;BLOC")
                             (mapcar 
                               (function 
                                 ;  (lambda (tag) (strcat ";" (_csv-escape tag)))
                                 (lambda (tag) (strcat ";" tag))
                               )
                               (if (listp tags) tags '())
                             )
                             (list "\n")
                           )
                    )
      )

      ;; 2) SECOND PASSAGE : une ligne par INSERT
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq vEnt (vlax-ename->vla-object ent))
        (setq handle (cdr (assoc 5 (entget ent))))

        ;; Associer TAG -> VALEUR pour cet insert (alist simple)
        (setq attVar (vlax-invoke vEnt 'GetAttributes))
        (setq attList (_sa->list attVar))
        (setq valsAlist '())
        (if attList 
          (foreach a attList 
            ;; Les TAGs dans un bloc sont uniques : on empile simplement
            (setq valsAlist (cons 
                              (cons (vla-get-TagString a) 
                                    (vla-get-TextString a)
                              )
                              valsAlist
                            )
            )
          )
        )

        ;; Construire la ligne : HANDLE;BLOC;val(TAG1);val(TAG2);...
        ; (setq csvRow (strcat (_csv-escape handle) ";" (_csv-escape blk)))
        (setq csvRow (strcat handle ";" blk))
        (foreach tag tags 
          (setq csvRow (strcat csvRow 
                               ";"
                               ;  (_csv-escape (cdr (assoc tag valsAlist)))
                               (cdr (assoc tag valsAlist))
                       )
          )
        )
        (setq csvData (strcat csvData csvRow "\n"))

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