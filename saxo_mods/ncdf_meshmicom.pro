;+
;
; @param FILENAME
;
; @keyword GLAMBOUNDARY
;
; @keyword _EXTRA
;
; @uses
; <pro>common</pro>
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: ncdf_meshmicom.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO ncdf_meshmicom, filename $
    , GLAMBOUNDARY=glamboundary $
    , _EXTRA=extra
;
   compile_opt idl2, strictarrsubs
;
@common

;
 usage='ncdf_meshmicom, filename ' $
    + ', GLAMBOUNDARY=glamboundary ' $
    + ', _EXTRA=extra'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(filename,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type filename' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

;
;---------------------------------------------------------
; set default glamboundary
;---------------------------------------------------------
   IF n_elements(glamboundary) NE 2 THEN BEGIN
    glamboundary = [20, 380]
   ENDIF

;---------------------------------------------------------
; default grid definition
;---------------------------------------------------------
; Get the lon, lat, mask for T-points and compute some scale factors
; for an hypothetic regular grid. But it transforms the grid from a
; ubase grid to a tbase grid

   initncdf, filename, xaxisname = 'plon', yaxisname = 'plat', zaxisname = 'sigma', /zindex $
    , maskname = 'pmask', /ubase2tbase, GLAMBOUNDARY = glamboundary, _EXTRA=extra

;---------------------------------------------------------
; apply real glam[uvf]
;---------------------------------------------------------
; Pay attention : the longitudes in the file are considered as
; latitudes in SAXO and vice versa for U and V.

   glamu = read_ncdf('vlon', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'U')
   glamv = read_ncdf('ulon', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'V')
   glamf = read_ncdf('qlon', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'F')
;
   glamu = temporary(glamu) MOD 360
   smaller = where(glamu LT glamboundary[0])
   if smaller[0] NE -1 THEN BEGIN
    glamu[smaller] = glamu[smaller]+360
   ENDIF
   bigger = where(glamu GE glamboundary[1])
   if bigger[0] NE -1 THEN BEGIN
    glamu[bigger] = glamu[bigger]-360
   ENDIF
   glamv = temporary(glamv) MOD 360
   smaller = where(glamv LT glamboundary[0])
   if smaller[0] NE -1 THEN BEGIN
    glamv[smaller] = glamv[smaller]+360
   ENDIF
   bigger = where(glamv GE glamboundary[1])
   if bigger[0] NE -1 THEN BEGIN
    glamv[bigger] = glamv[bigger]-360
   ENDIF
   glamf = temporary(glamf) MOD 360
   smaller = where(glamf LT glamboundary[0])
   if smaller[0] NE -1 THEN BEGIN
    glamf[smaller] = glamf[smaller]+360
   ENDIF
   bigger = where(glamf GE glamboundary[1])
   if bigger[0] NE -1 THEN BEGIN
    glamf[bigger] = glamf[bigger]-360
   ENDIF
   toosmall = where(glamu EQ glamboundary[0])
   IF toosmall[0] NE -1 THEN BEGIN
    glamu[toosmall] = glamu[toosmall] + 360
   ENDIF
   toosmall = where(glamf EQ glamboundary[0])
   IF toosmall[0] NE -1 THEN BEGIN
    glamf[toosmall] = glamf[toosmall] + 360
   ENDIF

;---------------------------------------------------------
; apply real gphi[uvf]
;---------------------------------------------------------
   gphiu = read_ncdf('vlat', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'U')
   gphiv = read_ncdf('ulat', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'V')
   gphif = read_ncdf('qlat', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'F')

;---------------------------------------------------------
; apply real masks
;---------------------------------------------------------
; same feinte for the masks as for the lon/lat ????
;   umask = read_ncdf('vmask', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'U')
;   vmask = read_ncdf('umask', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'V')
;   fmask = read_ncdf('qmask', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'F')

;---------------------------------------------------------
; apply real e1[uvf]
;---------------------------------------------------------
; Read the real scale factors and scratch the old ones.
; same feinte for the masks as for the lon/lat ????
   e1u = read_ncdf('vscaley', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'U')
   e1v = read_ncdf('uscaley', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'V')
   e1f = read_ncdf('qscalex', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'F')

;---------------------------------------------------------
; apply real e2[uvf]
;---------------------------------------------------------
   e2u = read_ncdf('vscalex', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'U')
   e2v = read_ncdf('uscalex', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'V')
   e2f = read_ncdf('qscaley', 0, 0, /timestep, file = filename, /nostruct, /cont_nofill, grid = 'F')

;---------------------------------------------------------
; set default values pour the third dimension
;---------------------------------------------------------
   gdept = findgen(jpk)
   gdepw = findgen(jpk)
   e3t = replicate(1., jpk)
   e3w = replicate(1., jpk)
;---------------------------------------------------------
; set default vargrid
;---------------------------------------------------------
  vargrid = 'T'

;---------------------------------------------------------
; key_yerverse
;---------------------------------------------------------
  key_yreverse = 1
  print, 'key_yreverse : ', key_yreverse

end
