
(defun c:GOHANDLE (/ h en ss) 
  (prompt "\nEntrez le handle (hex) de l'objet, ex: 1A2B : ")
  (setq h (strcase (getstring)))
  (setq en (handent h)) ; retourne l'ename si le handle existe
  (cond 
    (en
     (setq ss (ssadd en))
     (sssetfirst nil ss) ; met l'objet en sélection
     (command "._REGEN")
     (command "._ZOOM" "_Object" en) ; zoom sur l'objet
     (princ (strcat "\nObjet trouvé. Handle: " h))
    )
    (t (princ (strcat "\nAucun objet avec le handle " h " dans ce dessin.")))
  )
  (princ)
)
