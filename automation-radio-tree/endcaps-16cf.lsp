;;; endcaps-16cf.lsp
;;; Left/right end caps for 16 mm OD carbon-fiber tube — same ID/OD and USB clip geometry as tconnector-16cf.lsp.
;;; Clip orientation: tube axis ±X; slot opens +Z. Outer end closed with a solid plug; clip centered on cap in X and Y.
;;; AutoCAD 2012+ — CYLINDER + BOX booleans only (no COM).
;;;
;;; APPLOAD this file (with or without t-connector-16cf.lsp). Command: EC16
;;;
;;; Geometry matches c:TCF16: Socket ID 16.2 mm, wall 3 mm -> OD 22.2 mm.

(defun ec16--hollow-cap ( r_out r_in depth leg / ax ay az p2 o i sso ssi cen )
  ;; Short hollow sleeve along ±X from origin; leg "PX" = +X, "NX" = −X (same as T leg).
  (setq cen '(0.0 0.0 0.0))
  (cond
    ((= leg "PX") (setq ax depth ay 0.0 az 0.0))
    ((= leg "NX") (setq ax (- depth) ay 0.0 az 0.0))
  )
  (setq p2 (list ax ay az))
  (command "._CYLINDER" "_non" cen r_out "_Axis" "_non" p2)
  (setq o (entlast))
  (command "._CYLINDER" "_non" cen r_in "_Axis" "_non" p2)
  (setq i (entlast))
  (setq sso (ssadd o))
  (setq ssi (ssadd i))
  (command "._SUBTRACT" sso "" ssi "")
  (entlast)
)

(defun ec16--close-cap-outer ( r_out cap_depth plate_th leg / p1 p2 main plug ssm )
  ;; Solid cylinder r_out closes the bore and OD at the outer axial face (PX: +X end, NX: −X end).
  (setq main (entlast))
  (cond
    ;; Plug fills bore; outer flat face at x = ±cap_depth (axis endpoints = centers of end circles).
    ((= leg "PX")
      (setq p1 (list (- cap_depth plate_th) 0.0 0.0))
      (setq p2 (list cap_depth 0.0 0.0))
    )
    ((= leg "NX")
      (setq p1 (list (- cap_depth) 0.0 0.0))
      (setq p2 (list (+ (- cap_depth) plate_th) 0.0 0.0))
    )
  )
  (command "._CYLINDER" "_non" p1 r_out "_Axis" "_non" p2)
  (setq plug (entlast))
  (setq ssm (ssadd main))
  (ssadd plug ssm)
  (command "._UNION" ssm "")
  (entlast)
)

(defun ec16--fill-flat-to-horizontal-od ( r_out bx0 bx1 by0 by1 z_top bite n / dy i y0 y1 ym zlo ss e )
  (setq dy (/ (- by1 by0) (float n)))
  (setq ss nil)
  (setq i 0)
  (while (< i n)
    (setq y0 (+ by0 (* i dy)))
    (setq y1 (+ y0 dy))
    (setq ym (* 0.5 (+ y0 y1)))
    (setq zlo (sqrt (max 0.0 (- (* r_out r_out) (* ym ym)))))
    (setq zlo (- zlo bite))
    (if (> (- z_top zlo) 1e-4)
      (progn
        (command "._BOX" "_non" (list bx0 y0 zlo) "_non" (list bx1 y1 z_top))
        (setq e (entlast))
        (if ss (ssadd e ss) (setq ss (ssadd e)))
      )
    )
    (setq i (1+ i))
  )
  ss
)

(defun ec16--z-cap-crest ( r_out x y )
  ;; Tube axis ±X: OD crest z²+y²=r_out² → z=√(r²−y²) (no +Y branch on cap; x unused).
  (sqrt (max 0.0 (- (* r_out r_out) (* y y))))
)

(defun ec16--fill-flat-to-cap-od ( r_out bx0 bx1 by0 by1 z_top bite nx ny
                                  / dx dy ix iy x0 x1 y0 y1 z00 z01 z10 z11 zlo ss e )
  ;; Match tconnector-16cf clip/OD fill: min crest Z at cell corners + 2D grid (midpoint Y-strips can leave slivers).
  (setq dx (/ (- bx1 bx0) (float nx)))
  (setq dy (/ (- by1 by0) (float ny)))
  (setq ss nil)
  (setq ix 0)
  (while (< ix nx)
    (setq x0 (+ bx0 (* ix dx)))
    (setq x1 (+ x0 dx))
    (setq iy 0)
    (while (< iy ny)
      (setq y0 (+ by0 (* iy dy)))
      (setq y1 (+ y0 dy))
      (setq z00 (ec16--z-cap-crest r_out x0 y0))
      (setq z01 (ec16--z-cap-crest r_out x0 y1))
      (setq z10 (ec16--z-cap-crest r_out x1 y0))
      (setq z11 (ec16--z-cap-crest r_out x1 y1))
      (setq zlo (- (min z00 (min z01 (min z10 z11))) bite))
      (if (> (- z_top zlo) 1e-4)
        (progn
          (command "._BOX" "_non" (list x0 y0 zlo) "_non" (list x1 y1 z_top))
          (setq e (entlast))
          (if ss (ssadd e ss) (setq ss (ssadd e)))
        )
      )
      (setq iy (1+ iy))
    )
    (setq ix (1+ ix))
  )
  ss
)

(defun ec16--add-usb-clip ( r_out depth leg / clip_h clip_w_lr clip_fb
                            rail_th back_th gw hx_w hy_w overlap side_bite
                            bx0 bx1 by0 by1 rail_x_out_l rail_x_out_r pad_x0 pad_x1
                            z_r0 z_r1 z_back0 z_back1 x_clip
                            main eb el er clip sfill fill_u fill_bite z_fill_top
                            tab_in tab_z tab_y_half y_t0 y_t1 z_t0 etab_l etab_r
                            tab_bottom_in tab_bottom_y y_b0 y_b1 z_b0 z_b1 etab_bl etab_br
                            merged ssm )
  ;; x_clip centers clip on cap along tube axis: PX cap [0..depth], NX cap [−depth..0].
  (setq clip_h       12.75)
  (setq clip_w_lr    18.0)
  (setq clip_fb      21.0)
  (setq rail_th      2.0)
  (setq back_th      2.0)
  (setq overlap      0.55)
  (setq side_bite    0.4)
  (setq tab_in       0.6)
  (setq tab_z        1.8)
  (setq tab_y_half    1.5)
  (setq tab_bottom_in 2.0)
  (setq tab_bottom_y 2.0)

  (setq hx_w (* 0.5 clip_w_lr))
  (setq hy_w (* 0.5 clip_fb))
  (setq bx0 (- hx_w))
  (setq bx1 hx_w)
  (setq by0 (- (+ hy_w side_bite)))
  (setq by1 (+ hy_w side_bite))
  (setq gw (- clip_w_lr (* 2.0 rail_th)))
  (setq rail_x_out_l (- (- (+ (* 0.5 gw) rail_th)) overlap))
  (setq rail_x_out_r (+ (+ (* 0.5 gw) rail_th) overlap))
  (setq x_clip (if (= leg "NX") (- (* 0.5 depth)) (* 0.5 depth)))
  (setq pad_x0 (+ (min bx0 rail_x_out_l) x_clip))
  (setq pad_x1 (+ (max bx1 rail_x_out_r) x_clip))

  (setq z_r0 (- r_out overlap))
  (setq z_r1 (+ r_out clip_h))
  (setq z_back0 r_out)
  (setq z_back1 (+ r_out back_th))

  (setq main (entlast))

  (command "._BOX" "_non" (list pad_x0 by0 z_back0) "_non" (list pad_x1 by1 z_back1))
  (setq eb (entlast))
  (command "._BOX" "_non" (list (+ (- (- (+ (* 0.5 gw) rail_th)) overlap) x_clip) by0 z_r0)
            "_non" (list (+ (- (* 0.5 gw)) x_clip) by1 z_r1))
  (setq el (entlast))
  (command "._BOX" "_non" (list (+ (* 0.5 gw) x_clip) by0 z_r0)
            "_non" (list (+ (+ (+ (* 0.5 gw) rail_th) overlap) x_clip) by1 z_r1))
  (setq er (entlast))

  (setq ssm (ssadd eb))
  (ssadd el ssm)
  (ssadd er ssm)
  (command "._UNION" ssm "")
  (setq clip (entlast))

  (setq fill_bite (+ overlap 0.22))
  (setq z_fill_top (+ z_back0 0.25))
  (setq sfill (ec16--fill-flat-to-cap-od r_out pad_x0 pad_x1 by0 by1 z_fill_top fill_bite 26 26))
  (if sfill
    (progn
      (command "._UNION" sfill "")
      (setq fill_u (entlast))
      (setq ssm (ssadd clip))
      (ssadd fill_u ssm)
      (command "._UNION" ssm "")
      (setq clip (entlast))
    )
  )

  (setq y_t0 (- tab_y_half))
  (setq y_t1 tab_y_half)
  (setq z_t0 (- z_r1 tab_z))
  (command "._BOX" "_non" (list (+ (- (* 0.5 gw)) x_clip) y_t0 z_t0)
            "_non" (list (+ (+ (- (* 0.5 gw)) tab_in) x_clip) y_t1 z_r1))
  (setq etab_l (entlast))
  (command "._BOX" "_non" (list (+ (- (* 0.5 gw) tab_in) x_clip) y_t0 z_t0)
            "_non" (list (+ (* 0.5 gw) x_clip) y_t1 z_r1))
  (setq etab_r (entlast))
  (setq ssm (ssadd clip))
  (ssadd etab_l ssm)
  (ssadd etab_r ssm)
  (command "._UNION" ssm "")
  (setq clip (entlast))

  (setq z_b0 z_r0)
  (setq z_b1 z_r1)
  (setq y_b0 (- by1 tab_bottom_y))
  (setq y_b1 by1)
  (command "._BOX" "_non" (list (+ (- (* 0.5 gw)) x_clip) y_b0 z_b0)
            "_non" (list (+ (+ (- (* 0.5 gw)) tab_bottom_in) x_clip) y_b1 z_b1))
  (setq etab_bl (entlast))
  (command "._BOX" "_non" (list (+ (- (* 0.5 gw) tab_bottom_in) x_clip) y_b0 z_b0)
            "_non" (list (+ (* 0.5 gw) x_clip) y_b1 z_b1))
  (setq etab_br (entlast))
  (setq ssm (ssadd clip))
  (ssadd etab_bl ssm)
  (ssadd etab_br ssm)
  (command "._UNION" ssm "")
  (setq clip (entlast))

  (setq ssm (ssadd main))
  (ssadd clip ssm)
  (command "._UNION" ssm "")
  (setq merged (entlast))
  (princ)
)

(defun c:EC16 (/ sock_id wall od r_out r_in cap_depth plate_th sep old_os base_left)
  ;; Two caps: right extends +X from insertion, left extends −X. Outer end closed; clip centered on cap in X/Y.
  (setq
    sock_id   16.2
    wall      3.0
    od        (+ sock_id (* 2.0 wall))
    r_out     (/ od 2.0)
    r_in      (/ sock_id 2.0)
    cap_depth 25.4   ;; axial length of each cap (mm), open end @ insertion to closed outer face
    plate_th  2.0    ;; solid end plug thickness (mm)
    sep       80.0   ;; distance between cap insertion points (centerlines) along X
  )
  (setq old_os (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "._UNDO" "_Begin")
  (command "._UCS" "_W")

  ;; Right cap (tube +X end): hollow + closed outer face; clip centered axially on cap.
  (ec16--hollow-cap r_out r_in cap_depth "PX")
  (ec16--close-cap-outer r_out cap_depth plate_th "PX")
  (ec16--add-usb-clip r_out cap_depth "PX")

  ;; Left cap (tube −X end): offset in WCS X so parts do not overlap.
  (setq base_left (list (- sep) 0.0 0.0))
  (command "._UCS" "_Origin" "_non" base_left)
  (ec16--hollow-cap r_out r_in cap_depth "NX")
  (ec16--close-cap-outer r_out cap_depth plate_th "NX")
  (ec16--add-usb-clip r_out cap_depth "NX")

  (command "._UCS" "_W")
  (command "._UNDO" "_End")
  (setvar "OSMODE" old_os)

  (princ (strcat "\nEC16: right cap @0,0,0 (+X); left @ " (rtos (car base_left) 2 2) ",0,0. Bore "
                 (rtos sock_id 2 2) " mm, OD " (rtos od 2 2) " mm, length " (rtos cap_depth 2 2) " mm, end plug "
                 (rtos plate_th 2 2) " mm; clip centered on cap; +Z slot. "))
  (princ)
)

(princ "\nLoaded endcaps-16cf.lsp — run EC16 for left+right end caps (USB clip, +Z slot). ")
(princ)
