;;; sleeve-bracket-16.lsp
;;; THIS FILE ONLY (no copy): C:/Users/Steven Alexson/Code/autocad/sleeve-bracket-16.lsp
;;; In AutoCAD run: (load "C:/Users/Steven Alexson/Code/autocad/sleeve-bracket-16.lsp")
;;; build: 2026-03-30 — sleeve axis +Z (= length). Bracket on +X OD; backplate follows cylinder and runs
;;; the full bracket span in Z (parallel to sleeve), centered on sleeve length. Not on end openings.
;;;
;;; 2" (50.8 mm) hollow sleeve — bore/OD as TCF16 (16.2 mm ID, 22.2 mm OD).
;;; Footprint: 84.25 mm tangent (Y), 40.05 mm along Z (sleeve length); 18.45 mm radial (+X);
;;; 2 mm solid back; hollow ±Y walls in outer radial band.
;;;
;;; Optional UNION gap: (setq sb16-od-bite 0.05) then reload.
;;; AutoCAD 2012+ — CYLINDER + BOX + UNION/SUBTRACT (no COM).
;;;
;;; APPLOAD; command: SB16

(setq sb16-od-bite 0.0)

(defun sb16--hollow-sleeve-z ( r_out r_in len / cen p2 o i sso ssi )
  ;; Hollow cylinder axis +Z; bore/openings at z=0 and z=len (annular faces).
  (setq cen '(0.0 0.0 0.0))
  (setq p2 (list 0.0 0.0 len))
  (command "._CYLINDER" "_non" cen r_out "_Axis" "_non" p2)
  (setq o (entlast))
  (command "._CYLINDER" "_non" cen r_in "_Axis" "_non" p2)
  (setq i (entlast))
  (setq sso (ssadd o))
  (setq ssi (ssadd i))
  (command "._SUBTRACT" sso "" ssi "")
  (entlast)
)

(defun sb16--box-lwh ( p0 dx dy dz )
  (command "._BOX" "_non" p0 "_Length" (rtos dx 2 10) "_Width" (rtos dy 2 10) "_Height" (rtos dz 2 10)))

(defun sb16--union-ents ( lst / ss e )
  (if (and lst (cdr lst))
    (progn
      (setq ss (ssadd))
      (foreach e lst (setq ss (ssadd e ss)))
      (command "._UNION" ss "")
      (entlast))
    (car lst)))

(defun sb16--side-bracket-xplus ( r_out y0 y1 z0 z1 back_th bt wall_y slice bite
                                  / y_in_lo y_in_hi ya yb ymid xin yi_lo yi_hi e-back e-out e-in
                                    sso ssi strip-ents e-strip dz )
  ;; +X side of OD: Y-slices; inner X = sqrt(r^2 - y^2). Each strip spans full Z (z0..z1) = back along sleeve.
  (setq y_in_lo (+ y0 wall_y))
  (setq y_in_hi (- y1 wall_y))
  (setq dz (- z1 z0))
  (setq strip-ents nil)
  (setq ya y0)
  (while (< ya y1)
    (setq yb (min (+ ya slice) y1))
    (setq ymid (/ (+ ya yb) 2.0))
    (setq xin
      (if (<= (abs ymid) r_out)
        (sqrt (max 0.0 (- (* r_out r_out) (* ymid ymid))))
        r_out))
    (if (> bite 0.0)
      (setq xin (- xin bite)))
    ;; Length +X radial, Width +Y tangent, Height +Z along sleeve (full backplate length on OD)
    (sb16--box-lwh (list xin ya z0) back_th (- yb ya) dz)
    (setq e-back (entlast))
    (sb16--box-lwh (list (+ xin back_th) ya z0) (- bt back_th) (- yb ya) dz)
    (setq e-out (entlast))
    (setq e-strip e-out)
    (setq yi_lo (max ya y_in_lo))
    (setq yi_hi (min yb y_in_hi))
    (if (< yi_lo yi_hi)
      (progn
        (sb16--box-lwh (list (+ xin back_th) yi_lo z0) (- bt back_th) (- yi_hi yi_lo) dz)
        (setq e-in (entlast))
        (setq sso (ssadd e-out))
        (setq ssi (ssadd e-in))
        (command "._SUBTRACT" sso "" ssi "")
        (setq e-strip (entlast))))
    (setq sso (ssadd e-back))
    (setq sso (ssadd e-strip sso))
    (command "._UNION" sso "")
    (setq e-strip (entlast))
    (setq strip-ents (cons e-strip strip-ents))
    (setq ya yb))
  (sb16--union-ents (reverse strip-ents)))

(defun c:SB16 (/ sock_id wall od r_out r_in sleeve_inch sleeve_len
                 bw bh bt back_th wall_y slice y0 y1 z0 z1 y_in_lo y_in_hi bite
                 old_os ssm e_sleeve e_brack)
  (setq bite (if (boundp 'sb16-od-bite) sb16-od-bite 0.0))
  (setq
    sock_id     16.2
    wall        3.0
    od          (+ sock_id (* 2.0 wall))
    r_out       (/ od 2.0)
    r_in        (/ sock_id 2.0)
    sleeve_inch 2.0
    sleeve_len  (* sleeve_inch 25.4)   ;; sleeve length = +Z
    bw          84.25                  ;; around pipe = Y
    bh          40.05                  ;; along sleeve = Z (centered)
    bt          18.45                  ;; radial +X from OD
    back_th     2.0
    wall_y      2.0
    slice       1.0
  )
  (setq old_os (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "._UNDO" "_Begin")
  (command "._UCS" "_W")

  (sb16--hollow-sleeve-z r_out r_in sleeve_len)
  (setq e_sleeve (entlast))
  (setq y0 (- (* 0.5 bw)))
  (setq y1 (* 0.5 bw))
  ;; Center bracket along sleeve length (Z), clear of annular openings at z=0 and z=L
  (setq z0 (/ (- sleeve_len bh) 2.0))
  (setq z1 (+ z0 bh))
  (setq y_in_lo (+ y0 wall_y))
  (setq y_in_hi (- y1 wall_y))
  (if (or (<= y_in_hi y_in_lo) (>= back_th bt) (< z0 0.0) (> z1 sleeve_len))
    (princ "\nSB16: check bw/bh/bt/back_th/wall_y and that bh fits sleeve length. ")
    (progn
      (setq e_brack
        (sb16--side-bracket-xplus r_out y0 y1 z0 z1 back_th bt wall_y slice bite))
      (if e_brack
        (progn
          (setq ssm (ssadd e_sleeve))
          (setq ssm (ssadd e_brack ssm))
          (command "._UNION" ssm ""))))

  (command "._UNDO" "_End")
  (command "._UCS" "_W")
  (setvar "OSMODE" old_os)

  (princ (strcat "\nSB16 [+Z sleeve length, +X bracket, Z centered] L=" (rtos sleeve_len 2 2)
                 " mm; bracket Z " (rtos z0 2 2) ".." (rtos z1 2 2)
                 " mm; bore " (rtos sock_id 2 2) " OD " (rtos od 2 2)
                 "; od-bite " (rtos bite 2 3) ". "))
  (princ)
)

(princ "\nLoaded: Code/autocad/sleeve-bracket-16.lsp | sleeve +Z | bracket +X | SB16 ")
(princ)
