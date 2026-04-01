;;; adapter-16x20-part1.lsp
;;; Adapter — part 1: hollow sleeve 20 mm OD, 16 mm ID; bore height ends 4 mm
;;; below the top of the bottom stack (solid cap closes bore for shaft attachment);
;;; total bottom height 34.68 mm (+Z); solid shaft 14 mm OD, 3 in long above (76.2 mm).
;;; AutoCAD 2012+ — WCS, CYLINDER + SUBTRACT + UNION (no COM).
;;;
;;; New drawing file (recommended):
;;;   1) File > New (e.g. acad3d.dwt), File > Save As … adapter-part1.dwg
;;;   2) UNITS: decimal, millimeters (1 unit = 1 mm) — see repo README.md
;;;   3) APPLOAD this file  →  ADP16P1
;;;
(defun adp1--hollow-sleeve-z ( r_out r_in h / cen p2 o i sso ssi )
  ;; Hollow cylinder axis +Z, base at origin, open ends at z=0 and z=h.
  (setq cen '(0.0 0.0 0.0))
  (setq p2 (list 0.0 0.0 h))
  (command "._CYLINDER" "_non" cen r_out "_Axis" "_non" p2)
  (setq o (entlast))
  (command "._CYLINDER" "_non" cen r_in "_Axis" "_non" p2)
  (setq i (entlast))
  (setq sso (ssadd o))
  (setq ssi (ssadd i))
  (command "._SUBTRACT" sso "" ssi "")
  (entlast)
)

(defun c:ADP16 (/ r_out r_in h_sleeve h_cap h_hollow r_shaft h_shaft z_top
                    e_hollow e_cap e_bottom e_shaft ss old_os p0 p1)
  (setq r_out     10.0              ; 20 mm OD / 2
        r_in      8.0               ; 16 mm ID / 2
        h_sleeve  34.68             ; hollow + solid cap (bottom stack)
        h_cap     4.0               ; solid at top of bottom (full OD; bore stops below)
        h_hollow  (- h_sleeve h_cap)
        r_shaft   7.0               ; 14 mm OD / 2
        h_shaft   (* 3.0 25.4)      ; 3 in → mm
        z_top     (+ h_sleeve h_shaft)
  )
  (setq old_os (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "._UNDO" "_Begin")
  (command "._UCS" "_W")

  (setq e_hollow (adp1--hollow-sleeve-z r_out r_in h_hollow))
  (setq p0 (list 0.0 0.0 h_hollow))
  (setq p1 (list 0.0 0.0 h_sleeve))
  (command "._CYLINDER" "_non" p0 r_out "_Axis" "_non" p1)
  (setq e_cap (entlast))
  (setq ss (ssadd e_hollow))
  (ssadd e_cap ss)
  (command "._UNION" ss "")
  (setq e_bottom (entlast))

  (command "._CYLINDER" "_non" (list 0.0 0.0 h_sleeve) r_shaft "_Axis" "_non"
           (list 0.0 0.0 z_top))
  (setq e_shaft (entlast))
  (setq ss (ssadd e_bottom))
  (ssadd e_shaft ss)
  (command "._UNION" ss "")

  (command "._UNDO" "_End")
  (command "._UCS" "_W")
  (setvar "OSMODE" old_os)
  (princ
    (strcat "\nADP16P1: bore " (rtos h_hollow 2 4) " mm + solid cap "
            (rtos h_cap 2 4) " mm (OD " (rtos (* 2.0 r_out) 2 2) "); shaft OD 14, L "
            (rtos h_shaft 2 4) " mm. Total Z ≈ " (rtos z_top 2 4) " mm. "
    )
  )
  (princ)
)

(princ "\nLoaded adapter-16x20-part1.lsp — run ADP16P1 in a new mm drawing. ")
(princ)
