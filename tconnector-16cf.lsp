;;; tconnector-16cf.lsp
;;; T-connector for 16 mm OD carbon-fiber tubes (slip-fit socket).
;;; AutoCAD 2012+ — T-sockets use CYLINDER (Axis endpoint) + SUBTRACT + UNION
;;; in WCS only (no COM). USB clip uses BOX / booleans via commands.
;;;
;;; Before use:
;;;   - New drawing, UNITS: decimal, insertion scale = Millimeters (or draw in mm).
;;;   - APPLOAD this file.
;;;   - Command: TCF16
;;;
;;; Geometry (edit constants in c:TCF16 / tcf16--add-usb-clip if needed):
;;;   Socket ID 16.2 mm, wall 3 mm -> body OD 22.2 mm, engagement 25.4 mm (1")
;;;   Two coaxial sockets along +X and -X; one branch along +Y.
;;;
;;; Optional USB: clip on flat top (z=r_out); OD fill; side tabs + bottom retention (±X into gap, +Y strip, full Z on rails).

(defun tcf16--hollow-sock ( r_out r_in depth leg / ax ay az p2 o i sso ssi cen )
  ;; Hollow tube from origin along axis to (ax,ay,az); |axis| = depth. leg = "NX"|"PX"|"PY".
  ;; CYLINDER 2Point height: base center cen, axis from cen -> p2 (WCS; use UCS W before TCF16).
  (setq cen '(0.0 0.0 0.0))
  (cond
    ((= leg "NX") (setq ax (- depth) ay 0.0 az 0.0))
    ((= leg "PX") (setq ax depth ay 0.0 az 0.0))
    ((= leg "PY") (setq ax 0.0 ay depth az 0.0))
  )
  (setq p2 (list ax ay az))
  ;; Height/direction: Axis endpoint (same as base center would break 2Point plane).
  (command "._CYLINDER" "_non" cen r_out "_Axis" "_non" p2)
  (setq o (entlast))
  (command "._CYLINDER" "_non" cen r_in "_Axis" "_non" p2)
  (setq i (entlast))
  (setq sso (ssadd o))
  (setq ssi (ssadd i))
  (command "._SUBTRACT" sso "" ssi "")
  (entlast)
)

(defun tcf16--fill-flat-to-horizontal-od ( r_out bx0 bx1 by0 by1 z_top bite n / dy i y0 y1 ym zlo ss e )
  ;; Horizontal sockets: OD in YZ is y²+z²=r_out² → z_od(y)=sqrt(r_out²-y²). Flat clip at z=z_top leaves
  ;; air wedges off the y=0 line; thin Y-slabs from z_od−bite to z_top bridge clip back to cylindrical OD.
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

(defun tcf16--add-usb-clip ( r_out depth / clip_h clip_w_lr clip_fb
                            rail_th back_th gw hx_w hy_w overlap side_bite
                            bx0 bx1 by0 by1 rail_x_out_l rail_x_out_r pad_x0 pad_x1
                            z_r0 z_r1 z_back0 z_back1
                            main eb el er clip sfill fill_u
                            tab_in tab_z tab_y_half y_t0 y_t1 z_t0 etab_l etab_r
                            tab_bottom_in tab_bottom_y y_b0 y_b1 z_b0 z_b1 etab_bl etab_br
                            merged ssm )
  ;; Flat top z=r_out; OD fill + pad_x use full rail outer X (rails stick out past ±hx_w); side_bite extends ±Y into shell.
  (setq clip_h       18.0)   ;; +2 mm rail height (z_r1); room for features / tabs
  (setq clip_w_lr    18.0)
  (setq clip_fb      12.75)   ;; +2 mm vs prior so inward tabs don’t net-reduce Y clearance in gap
  (setq rail_th      2.0)
  (setq back_th      2.0)
  ;; Into tube shell: overlap (Z) + side_bite (Y) help rails/pad UNION; rails’ outer X exceeds ±hx_w — pad/fill span rail_x_out_*.
  (setq overlap      0.35)
  (setq side_bite    0.25)
  ;; Retention tabs: inward from inner rail faces toward gap; clip_fb widened +2 mm to compensate in Y.
  (setq tab_in       0.6)
  (setq tab_z        1.8)
  (setq tab_y_half    1.5)
  ;; Bottom retention: into gap along ±X; +Y strip at by1; full rail span in Z (z_r0..z_r1).
  (setq tab_bottom_in 2.0)
  (setq tab_bottom_y 2.0)

  (setq hx_w (* 0.5 clip_w_lr))
  (setq hy_w (* 0.5 clip_fb))
  (setq bx0 (- hx_w))
  (setq bx1 hx_w)
  (setq by0 (- (+ hy_w side_bite)))
  (setq by1 (+ hy_w side_bite))
  (setq gw (- clip_w_lr (* 2.0 rail_th)))
  ;; Outer X of rail boxes (includes overlap past inner gap) — must be covered by back pad + OD fill or gap shows at sides.
  (setq rail_x_out_l (- (- (+ (* 0.5 gw) rail_th)) overlap))
  (setq rail_x_out_r (+ (+ (* 0.5 gw) rail_th) overlap))
  (setq pad_x0 (min bx0 rail_x_out_l))
  (setq pad_x1 (max bx1 rail_x_out_r))

  (setq z_r0 (- r_out overlap))
  (setq z_r1 (+ r_out clip_h))
  (setq z_back0 r_out)
  (setq z_back1 (+ r_out back_th))

  (setq main (entlast))

  ;; Thin pad on flat crest: X matches full rail outer extent so side edges tie into T with fill below.
  (command "._BOX" "_non" (list pad_x0 by0 z_back0) "_non" (list pad_x1 by1 z_back1))
  (setq eb (entlast))
  ;; Rails ±X; slot opens +Z (opposite side from tube mate).
  (command "._BOX" "_non" (list (- (- (+ (* 0.5 gw) rail_th)) overlap) by0 z_r0)
            "_non" (list (- (* 0.5 gw)) by1 z_r1))
  (setq el (entlast))
  (command "._BOX" "_non" (list (* 0.5 gw) by0 z_r0)
            "_non" (list (+ (+ (* 0.5 gw) rail_th) overlap) by1 z_r1))
  (setq er (entlast))

  (setq ssm (ssadd eb))
  (ssadd el ssm)
  (ssadd er ssm)
  (command "._UNION" ssm "")
  (setq clip (entlast))

  ;; Bridge flat clip base to round horizontal OD (same X span as pad so rail side “wings” are filled to OD).
  (setq sfill (tcf16--fill-flat-to-horizontal-od r_out pad_x0 pad_x1 by0 by1 z_back0 overlap 28))
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

  ;; Retention tabs: side = inward ±X (narrow Y) at +Z opening; bottom = ±X into gap, +Y strip, full Z on rails.
  (setq y_t0 (- tab_y_half))
  (setq y_t1 tab_y_half)
  (setq z_t0 (- z_r1 tab_z))
  (command "._BOX" "_non" (list (- (* 0.5 gw)) y_t0 z_t0)
            "_non" (list (+ (- (* 0.5 gw)) tab_in) y_t1 z_r1))
  (setq etab_l (entlast))
  (command "._BOX" "_non" (list (- (* 0.5 gw) tab_in) y_t0 z_t0)
            "_non" (list (* 0.5 gw) y_t1 z_r1))
  (setq etab_r (entlast))
  (setq ssm (ssadd clip))
  (ssadd etab_l ssm)
  (ssadd etab_r ssm)
  (command "._UNION" ssm "")
  (setq clip (entlast))

  ;; Bottom tabs: +Y end of wall; full length along Z (same as rail boxes z_r0..z_r1).
  (setq z_b0 z_r0)
  (setq z_b1 z_r1)
  (setq y_b0 (- by1 tab_bottom_y))
  (setq y_b1 by1)
  (command "._BOX" "_non" (list (- (* 0.5 gw)) y_b0 z_b0)
            "_non" (list (+ (- (* 0.5 gw)) tab_bottom_in) y_b1 z_b1))
  (setq etab_bl (entlast))
  (command "._BOX" "_non" (list (- (* 0.5 gw) tab_bottom_in) y_b0 z_b0)
            "_non" (list (* 0.5 gw) y_b1 z_b1))
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

(defun c:TCF16 (/ depth sock_id wall od r_out r_in
                  e_negx e_posx e_posy ssu old_os ans)
  (setq
    depth   25.4   ;; 1" engagement per leg
    sock_id 16.2   ;; slip-fit bore
    wall    3.0
    od      (+ sock_id (* 2.0 wall))   ;; 22.2
    r_out   (/ od 2.0)
    r_in    (/ sock_id 2.0)
  )
  (setq old_os (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (command "._UNDO" "_Begin")

  (command "._UCS" "_W")

  ;; Three hollow legs (WCS); union once all exist.
  (setq e_negx (tcf16--hollow-sock r_out r_in depth "NX"))
  (setq e_posx (tcf16--hollow-sock r_out r_in depth "PX"))
  (setq e_posy (tcf16--hollow-sock r_out r_in depth "PY"))
  (setq ssu (ssadd e_negx))
  (ssadd e_posx ssu)
  (ssadd e_posy ssu)
  (command "._UNION" ssu "")

  (initget "Yes No")
  (setq ans (getkword "\nAdd USB U-clip on flat top of cross (open +Z)? [Yes/No] <Yes>: "))
  (if (not (eq ans "No"))
    (progn
      (tcf16--add-usb-clip r_out depth)
      (princ "\nT + USB U-clip done. ")
    )
    (princ "\nT-connector only (no USB clip). ")
  )

  (command "._UNDO" "_End")
  (command "._UCS" "_W")
  (setvar "OSMODE" old_os)

  (princ "\n16.2 mm bore, 25.4 mm legs -X, +X, +Y. ")
  (princ)
)

(princ "\nLoaded tconnector-16cf.lsp — run TCF16 (optional USB clip on flat top of cross). ")
(princ)
