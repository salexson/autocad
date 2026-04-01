;;; sleeve-bracket-16.lsp
;;; THIS FILE ONLY (no copy): C:/Users/Steven Alexson/Code/autocad/sleeve-bracket-16.lsp
;;; In AutoCAD run: (load "C:/Users/Steven Alexson/Code/autocad/sleeve-bracket-16.lsp")
;;; build: 2026-04-01 - WCS build: sleeve axis +Y, bracket on +X; then ROTATE3D about WCS Y through origin (default 90 deg;
;;; Z-up WCS: built +X toward -Z = bracket under tube). Use 270 for bracket on +Z (top). Override: (setq sb16-rotate-y-deg <deg>).
;;; Bracket backplate along sleeve; centered on sleeve length.
;;;
;;; 2 in (50.8 mm) hollow sleeve - bore/OD as TCF16 (16.2 mm ID, 22.2 mm OD).
;;; Footprint: 84.25 mm tangent (Z), 40.05 mm along Y (sleeve length); 18.75 mm radial (+X);
;;; 2 mm solid back (duplicate +X by bt); outer band shortened in Z; Z-end caps; void ledges one Y opening only (y0).
;;; OD-to-backplate wedge fill (Z slices) from tube surface to inner back face xin.
;;;
;;; Optional UNION gap: (setq sb16-od-bite 0.05) then reload.
;;; AutoCAD 2012+ - CYLINDER + BOX + UNION/SUBTRACT (no COM).
;;;
;;; APPLOAD; command: SB16

(setq sb16-od-bite 0.0)

(defun sb16--hollow-sleeve-y ( r_out r_in len / cen p2 o i sso ssi )
  ;; Hollow cylinder axis +Y; bore/openings at y=0 and y=len (annular faces).
  (setq cen '(0.0 0.0 0.0))
  (setq p2 (list 0.0 len 0.0))
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
  ;; AC2012 BOX: after _Length, next prompts are width then height (no _Width/_Height keywords).
  (command "._BOX" "_non" p0 "_Length" (rtos dx 2 10) (rtos dy 2 10) (rtos dz 2 10)))

(defun sb16--union-ent-list ( lst / ss e )
  (cond
    ((not lst) nil)
    ((not (cdr lst)) (car lst))
    (t
      (setq ss (ssadd))
      (foreach e lst (setq ss (ssadd e ss)))
      (command "._UNION" ss "")
      (entlast))))

(defun sb16--rotate3d-about-y ( e ang_deg / ss )
  ;; Axis = WCS Y through origin (0,0,0)-(0,1,0). ang_deg is AutoCAD ROTATE3D (right-hand about +Y).
  (if (and e (entget e))
    (progn
      (setq ss (ssadd e))
      (command "._ROTATE3D" ss "" "_non" '(0.0 0.0 0.0) "_non" '(0.0 1.0 0.0) (rtos ang_deg 2 10)))))

(defun sb16--fill-od-back-gap ( r_out x_plate y0 y1 t0 t1 slice / dy ta tb tc td zfar x_cyl dx strip-ents rlim )
  ;; Inner back x=x_plate; tube OD in +X is x=sqrt(r^2-z^2) for |z|<=r. Clip each strip to |z|<=r;
  ;; use max|z| on that segment so x_cyl is the shallowest OD point on the strip (no stair-step gaps).
  (setq dy (- y1 y0))
  (setq strip-ents nil)
  (setq rlim r_out)
  (setq ta t0)
  (while (< ta t1)
    (setq tb (min (+ ta slice) t1))
    (setq tc (max ta (- rlim)))
    (setq td (min tb rlim))
    (if (< tc td)
      (progn
        (setq zfar (max (abs tc) (abs td)))
        (setq x_cyl (sqrt (max 0.0 (- (* rlim rlim) (* zfar zfar)))))
        (setq dx (- x_plate x_cyl))
        (if (> dx 1e-6)
          (progn
            (sb16--box-lwh (list x_cyl y0 tc) dx dy (- td tc))
            (setq strip-ents (cons (entlast) strip-ents))))))
    (setq ta tb))
  (sb16--union-ent-list (reverse strip-ents)))

(defun sb16--side-bracket-xplus ( r_out y0 y1 t0 t1 back_th bt wall_t bite
                                  / t_in_lo t_in_hi tmid xin ti_lo ti_hi dz dy z0 z1 dz_out
                                    e-back e-back2 e-out e-in sso ssi e-strip e-main e-cap-lo e-cap-hi
                                    e-body e-ld-in e-ld-out e-fill zcap dz_void )
  ;; +X side of OD (sleeve axis +Y). Pocket opens along Y at y0 and y1; ledges only at y0 opening (not at y1).
  (setq zcap back_th)
  (setq t_in_lo (+ t0 wall_t))
  (setq t_in_hi (- t1 wall_t))
  (setq dy (- y1 y0))
  (setq dz (- t1 t0))
  (setq z0 (+ t0 zcap))
  (setq z1 (- t1 zcap))
  (setq dz_out (- z1 z0))
  (setq tmid (/ (+ t0 t1) 2.0))
  (setq xin
    (if (<= (abs tmid) r_out)
      (sqrt (max 0.0 (- (* r_out r_out) (* tmid tmid))))
      r_out))
  (if (> bite 0.0)
    (setq xin (- xin bite)))
  (if (<= dz_out 0.0)
    (progn
      (princ "\nSB16: bracket tangent width must exceed 2*back_th for Z caps. ")
      nil)
    (progn
      (sb16--box-lwh (list xin y0 t0) back_th dy dz)
      (setq e-back (entlast))
      (sb16--box-lwh (list (+ xin bt) y0 t0) back_th dy dz)
      (setq e-back2 (entlast))
      (sb16--box-lwh (list (+ xin back_th) y0 z0) (- bt back_th) dy dz_out)
      (setq e-out (entlast))
      (setq e-strip e-out)
      (setq ti_lo (max t_in_lo z0))
      (setq ti_hi (min t_in_hi z1))
      (if (< ti_lo ti_hi)
        (progn
          (sb16--box-lwh (list (+ xin back_th) y0 ti_lo) (- bt back_th) dy (- ti_hi ti_lo))
          (setq e-in (entlast))
          (setq sso (ssadd e-out))
          (setq ssi (ssadd e-in))
          (command "._SUBTRACT" sso "" ssi "")
          (setq e-strip (entlast))))
      (setq sso (ssadd e-back))
      (setq sso (ssadd e-back2 sso))
      (setq sso (ssadd e-strip sso))
      (command "._UNION" sso "")
      (setq e-main (entlast))
      ;; End bands: outer shell absent here; caps show as new Z faces (full bt x dy)
      (sb16--box-lwh (list xin y0 t0) bt dy zcap)
      (setq e-cap-lo (entlast))
      (sb16--box-lwh (list xin y0 (- t1 zcap)) bt dy zcap)
      (setq e-cap-hi (entlast))
      (setq sso (ssadd e-main))
      (setq sso (ssadd e-cap-lo sso))
      (setq sso (ssadd e-cap-hi sso))
      (command "._UNION" sso "")
      (setq e-body (entlast))
      (if (< ti_lo ti_hi)
        (progn
          (setq dz_void (- ti_hi ti_lo))
          ;; Full pocket depth in +Z; 2mm +Y strip at y0 only (other Y opening at y1 has no ledges)
          (sb16--box-lwh (list (+ xin back_th) y0 ti_lo) back_th back_th dz_void)
          (setq e-ld-in (entlast))
          (sb16--box-lwh (list (- (+ xin bt) back_th) y0 ti_lo) back_th back_th dz_void)
          (setq e-ld-out (entlast))
          (setq sso (ssadd e-body))
          (setq sso (ssadd e-ld-in sso))
          (setq sso (ssadd e-ld-out sso))
          (command "._UNION" sso "")
          (setq e-body (entlast))))
      (setq e-fill (sb16--fill-od-back-gap r_out xin y0 y1 t0 t1
                    (if (boundp 'sb16-fill-slice) sb16-fill-slice 0.25)))
      (if e-fill
        (progn
          (setq sso (ssadd e-body))
          (setq sso (ssadd e-fill sso))
          (command "._UNION" sso "")
          (setq e-body (entlast))))
      e-body
      )))

(defun c:SB16 (/ sock_id wall od r_out r_in sleeve_inch sleeve_len
                 bw bh bt back_th wall_t t0 t1 y0 y1 t_in_lo t_in_hi bite
                 old_os ssm e_sleeve e_brack e_final rot_y)
  (setq bite (if (boundp 'sb16-od-bite) sb16-od-bite 0.0))
  (setq
    sock_id     16.2
    wall        3.0
    od          (+ sock_id (* 2.0 wall))
    r_out       (/ od 2.0)
    r_in        (/ sock_id 2.0)
    sleeve_inch 2.0
    sleeve_len  (* sleeve_inch 25.4)   ;; sleeve length = +Y
    bw          84.25                  ;; around pipe = Z (tangent)
    bh          40.05                  ;; along sleeve = Y (centered)
    bt          18.75                  ;; radial +X from OD
    back_th     2.0
    wall_t      2.0
  )
  (setq old_os (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "._UNDO" "_Begin")
  (command "._UCS" "_W")

  (sb16--hollow-sleeve-y r_out r_in sleeve_len)
  (setq e_sleeve (entlast))
  (setq t0 (- (* 0.5 bw)))
  (setq t1 (* 0.5 bw))
  ;; Center bracket along sleeve length (Y), clear of annular openings at y=0 and y=L
  (setq y0 (/ (- sleeve_len bh) 2.0))
  (setq y1 (+ y0 bh))
  (setq t_in_lo (+ t0 wall_t))
  (setq t_in_hi (- t1 wall_t))
  (setq e_final nil)
  (if (or (<= t_in_hi t_in_lo) (>= back_th bt) (< y0 0.0) (> y1 sleeve_len))
    (princ "\nSB16: check bw/bh/bt/back_th/wall_t and that bh fits sleeve length. ")
    (progn
      (setq e_brack
        (sb16--side-bracket-xplus r_out y0 y1 t0 t1 back_th bt wall_t bite))
      (if e_brack
        (progn
          (setq ssm (ssadd e_sleeve))
          (setq ssm (ssadd e_brack ssm))
          (command "._UNION" ssm "")
          (setq e_final (entlast)))
        (setq e_final e_sleeve))))
  (if (not e_final) (setq e_final e_sleeve))
  (setq rot_y (if (boundp 'sb16-rotate-y-deg) sb16-rotate-y-deg 90.0))
  (sb16--rotate3d-about-y e_final rot_y)

  (command "._UNDO" "_End")
  (command "._UCS" "_W")
  (setvar "OSMODE" old_os)

  (princ (strcat "\nSB16 [built +Y/+X then ROTATE3D about WCS Y by " (rtos rot_y 2 2)
                 " deg] L=" (rtos sleeve_len 2 2)
                 " mm; bracket Y " (rtos y0 2 2) ".." (rtos y1 2 2)
                 " mm; bore " (rtos sock_id 2 2) " OD " (rtos od 2 2)
                 "; od-bite " (rtos bite 2 3) ". "))
  (princ)
)

(princ "\nLoaded: automation-radio-tree/sleeve-bracket-16.lsp | SB16 | build +Y/+X then ROTATE3D about WCS Y (default 90 deg, bracket -Z) ")
(princ)
