;+
;
; @param ISIZE {in}{required}{type=integer}
;
; @param JSIZE {in}{required}{type=integer}
;
; @param TYPE {in}{required}{type=string}
;
; @keyword NO_SHIFT
;
; @keyword WHOLE_ARRAYS
;
; @keyword REVERSE_Y
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
; $Id: mesh_um.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
PRO mesh_um, isize, jsize, type $
    , NO_SHIFT=no_shift $
    , WHOLE_ARRAYS=whole_arrays $
    , REVERSE_Y=reverse_y
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='mesh_um, isize, jsize, type' $
    + ', NO_SHIFT=no_shift' $
    + ', WHOLE_ARRAYS=whole_arrays' $
    + ', REVERSE_Y=reverse_y'

;
 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(isize,/type)
 IF ((arg_type NE 3) AND (arg_type NE 2)) THEN BEGIN
   ras = report(['Incorrect arg type isize' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(jsize,/type)
 IF ((arg_type NE 3) AND (arg_type NE 2)) THEN BEGIN
   ras = report(['Incorrect arg type jsize' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(type,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type type' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

; init grid, sf, masks for regular grid (includes equator)
;

   jpi = isize
   jpj = jsize
   jpk = 1 ; value set in nc_read

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

   CASE isize OF
      96: resolution = 'N96' ;;
      288: resolution = 'N144' ;;
      ELSE: print, 'UM resolution not defined'
   ENDCASE

   IF resolution EQ 'N96' THEN BEGIN

; 1.  Define longitudes

      CASE type of
         'T': BEGIN
            glamt = 360.0*findgen(jpi)/float(jpi)
            glamt = glamt#replicate(1, jpj)
         END
         'U': BEGIN
            glamt = 360.0*findgen(jpi)/float(jpi)
            delta = 0.5*(glamt[1]-glamt[0])
            glamt = glamt + delta
            glamt = glamt#replicate(1, jpj)
         END
      ENDCASE


; 2.  Define latitudes

      deltay = 2.5 ;;
      CASE type of
         'T': BEGIN
            gphit = 90.0*findgen((jpj-1)/2)/float((jpj-1)/2)+deltay
            gphitn = -gphit
            gphit = [reverse(gphit), 0.0, gphitn]
            gphit = replicate(1, jpi)#gphit
         END
         'U': BEGIN
            gphit = 90.0*findgen((jpj)/2)/float((jpj)/2)+deltay*.5
            gphitn = -gphit
            gphit = [reverse(gphit), gphitn]
            gphit = replicate(1, jpi)#gphit
         END
      ENDCASE


; 3 Define scale factors

      zrad=6371229.0

   ; Exact method: integration on a sphere

      e1t = abs(2*!pi*zrad*cos(gphit*!pi/180.0)/jpi)

      ytop = (shift(gphit, 0, -1)+gphit)*0.5*!pi/180.0
      ybot = (shift(gphit, 0, +1)+gphit)*0.5*!pi/180.0

      ytop[*, jpj-1] = !pi/2
      ybot[*,     0] = -!pi/2

      e2t = zrad*(-ytop+ybot)

; 4. Define mask

; open mask file
      tmask = lonarr(jpi, jpj)
      IF NOT keyword_set(NOMASK) THEN BEGIN
         nummsk = 12
         s_file = hom_idl+'grids/mask_um_'+ $
          strtrim(string(jpi), 2)+'x'+strtrim(string(jpj), 2)
         openr, nummsk, s_file, /get_lun, /f77
         readu, nummsk, tmask
         tmask = reverse(1-tmask, 2)
         close, nummsk
         free_lun, nummsk
      ENDIF ELSE BEGIN
         tmask[*, *]= 1
      ENDELSE

      IF keyword_set(REVERSE_Y) THEN BEGIN
         glamt = reverse(glamt, 2)
         gphit = reverse(gphit, 2)
         tmask = reverse(tmask, 2)
         e1t = reverse(e1t, 2)
         e2t = reverse(e2t, 2)
      ENDIF

; 5. vertical grid (hPa)

      gdept = [1000]
      gdepw = gdept

      e3t = 1

; shift info

      key_shift = 0

      key_offset = [0, 0, 0]
   ENDIF

   IF resolution EQ 'N144' THEN BEGIN

     ;      read netcdf
      CASE type OF
         'T': BEGIN
            s_file = hom_idl+'grids/grids_um_N144_nofrac.nc'
            glamt = nc_get(s_file, 'umat.lon')
            gphit = nc_get(s_file, 'umat.lat')
            s_file = hom_idl+'grids/areas_um_N144_nofrac.nc'
            e1t = nc_get(s_file, 'umat.srf')
            e2t = e1t
            e2t[*] = 1.
            s_file = hom_idl+'grids/masks_um_N144_nofrac.nc'
            tmask = nc_get(s_file, 'umat.msk')
         END
         'U': BEGIN
            s_file = hom_idl+'grids/grids_um_N144_nofrac.nc'
            glamt = nc_get(s_file, 'umau.lon')
            gphit = nc_get(s_file, 'umau.lat')
            s_file = hom_idl+'grids/areas_um_N144_nofrac.nc'
            e1t = nc_get(s_file, 'umau.srf')
            e2t = e1t
            e2t[*] = 1.
            s_file = hom_idl+'grids/masks_um_N144_nofrac.nc'
            tmask = nc_get(s_file, 'umau.msk')
         END
      ENDCASE
      IF NOT keyword_set(REVERSE_Y) THEN BEGIN
         glamt = reverse(glamt, 2)
         gphit = reverse(gphit, 2)
         e1t = reverse(e1t, 2)
         tmask = reverse(tmask, 2)
      ENDIF

; 5. vertical grid (hPa)

      gdept = [1000]
      gdepw = gdept

      e3t = 1

      key_shift = 0

      key_offset = [0, 0, 0]

   ENDIF

; Shift re-init U,V,F

   IF keyword_set(NO_SHIFT) THEN BEGIN
    key_shift = 0
   ENDIF

   glamu = glamt
   gphiu = gphit
   e1u = e1t
   e2u = e2t
   umask = tmask
   glamv = glamt
   gphiv = gphit
   e1v = e1t
   e2v = e2t
   vmask = tmask

   glamf = glamt
   gphif = gphit
   e1f = e1t
   e2f = e2t
   fmask = tmask

   e3w = e3t

   key_periodique=1
;
; indice i pour grille j moyenne zonale
;
   diaznl_idx = 1

END
