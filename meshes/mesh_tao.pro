;+
;
; @keyword NO_SHIFT
;
; @keyword WHOLE_ARRAYS
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: mesh_tao.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO mesh_tao $
    , NO_SHIFT=no_shift $
    , WHOLE_ARRAYS=whole_arrays
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='mesh_tao' $
    + ', NO_SHIFT=no_shift' $
    + ', WHOLE_ARRAYS=whole_arrays'
;
 nparam = N_PARAMS()
 IF (nparam NE 0) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

; inits for TAO gridded data from Kessler and McPhaden
;
   print,' TAO grid inits.'

   jpi = 86L & jpj = 21L & jpk = 51L & key_shift = 0
   IF keyword_set(NO_SHIFT) THEN BEGIN
    key_shift = 0
   ENDIF

   jpiglo = jpi
   jpjglo = jpj
   jpkglo = jpk

   ixminmesh =0
   ixmaxmesh =jpi-1
;
   iyminmesh =0
   iymaxmesh =jpj-1
;
   izminmesh =0
   izmaxmesh =jpk-1

; 1.  Define longitudes
   glamt = [120, 122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142, 144, 146, $
            148, 150, 152, 154, 156, 158, 160, 162, 164, 166, 168, 170, 172, 174, $
            176, 178, 180, 182, 184, 186, 188, 190, 192, 194, 196, 198, 200, 202, $
            204, 206, 208, 210, 212, 214, 216, 218, 220, 222, 224, 226, 228, 230, $
            232, 234, 236, 238, 240, 242, 244, 246, 248, 250, 252, 254, 256, 258]
   glamt = [glamt, 260, 262, 264, 266, 268, 270, 272, 274, 276, 278, 280, 282, 284, 286, $
            288, 290  ]
   glamt = glamt#replicate(1, jpj)

; 2 Define latitudes
   gphit = [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10  ]
   gphit = replicate(1, jpi)#gphit

; 3 Define scale factors

   zrad=6371229.0

   ; Exact method: integration on a sphere

   e1t = abs(2*!pi*zrad*cos(gphit*!pi/180.0)/jpi)

   ytop = (shift(gphit, 0, -1)+gphit)*0.5*!pi/180.0
   ybot = (shift(gphit, 0, +1)+gphit)*0.5*!pi/180.0

   ytop[*, jpj-1] = !pi/2
   ybot[*,     0] = -!pi/2

   e2t = zrad*(ytop-ybot)

;   tvnplot, e2t, min = 410000, max = 415000

; 4. Define mask

   s_file = hom_idl+'grids/mask_tao'
   restore, s_file

; 5. vertical grid

   gdept = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150,$
            160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290,$
            300, 310, 320, 330, 340, 350, 360, 370, 380, 390, 400, 410, 420, 430,$
            440, 450, 460, 470, 480, 490, 500 ]
   gdepw = gdept

   e3t = shift(gdept, 1)-gdept
   e3t[0] = e3t[1]


   key_periodique=0


   glamu = glamt
   gphiu = gphit
   e1u = e1t
   e2u = e2t

   glamv = glamt
   gphiv = gphit
   e1v = e1t
   e2v = e2t

; used for coast plot

   glamf = 0.5*(shift(glamt, -1, 0)+glamt)
   gphif = 0.5*(shift(gphit, 0, -1)+gphit)
   glamf[jpi-1, *] = glamf[jpi-2, *] + (glamf[jpi-2, *]-glamf[jpi-3, *])
   gphif[*, jpj-1] = gphif[*, jpj-2] + (gphif[*, jpj-2]-gphif[*, jpj-3])
   e1f = e1t
   e2f = e2t

   e3w = e3t

   key_offset = [0, 0, 0]
;
; indice i pour grille j moyenne zonale
;
   diaznl_idx = 1

END
