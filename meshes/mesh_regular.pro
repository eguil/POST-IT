;+
;
; @param DELTAX
;
; @param DELTAY
;
; @keyword INILON
;
; @keyword EQUATOR
;
; @keyword NOMASK
;
; @keyword MASK_FILE
;
; @keyword I_INDEX
;
; @keyword DELTA_I
;
; @keyword J_INDEX
;
; @keyword DELTA_J
;
; @keyword NO_SHIFT
;
; @keyword WHOLE_ARRAYS
;
; @keyword REVERSE_Y
;
; @keyword GLAMBOUNDARY
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
; $Id: mesh_regular.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO mesh_regular, deltax, deltay $
    , INILON=inilon $
    , EQUATOR=equator $
    , NOMASK=nomask $
    , MASK_FILE=mask_file $
    , I_INDEX=i_index $
    , DELTA_I=delta_i $
    , J_INDEX=j_index $
    , DELTA_J=delta_j $
    , NO_SHIFT=no_shift $
    , WHOLE_ARRAYS=whole_arrays $
    , REVERSE_Y=reverse_y $
    , GLAMBOUNDARY=glamboundary
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='mesh_regular, deltax, deltay' $
    + ', INILON=inilon' $
    + ', EQUATOR=equator' $
    + ', NOMASK=nomask' $
    + ', MASK_FILE=mask_file' $
    + ', I_INDEX=i_index' $
    + ', DELTA_I=delta_i' $
    + ', J_INDEX=j_index' $
    + ', DELTA_J=delta_j' $
    + ', NO_SHIFT=no_shift' $
    + ', WHOLE_ARRAYS=whole_arrays' $
    + ', REVERSE_Y=reverse_y' $
    + ', GLAMBOUNDARY=glamboundary'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

; init grid, sf, masks for regular grid (includes equator)
;
   print,' Regular grid inits.',deltax, deltay

   jpi = floor(360./deltax)
   jpj = floor(180./deltay)
   IF keyword_set(EQUATOR) THEN BEGIN
    jpj = jpj + 1
   ENDIF
   jpk = 1

 ; initialisation of character variables used in the execution of computegrid
   shift_txt = ''
   periodic_txt = ''
   idx_txt = ''

; 1.  Define longitudes

   IF NOT keyword_set(inilon) THEN BEGIN
    inilon = 0.
   ENDIF

   glamt = 360.0*findgen(jpi)/float(jpi)+inilon
   glamt = glamt#replicate(1, jpj)

; 2.  Define latitudes

   IF keyword_set(EQUATOR) THEN BEGIN

      gphit = 90.0*findgen((jpj-1)/2)/float((jpj-1)/2)+deltay
      gphitn = reverse(-gphit)
      gphit = [gphitn, 0.0, gphit]

   END ELSE BEGIN

      gphit = 90.0*findgen(jpj/2)/float(jpj/2)+deltay/2.
      gphitn = reverse(gphit)
      gphit = [gphitn, -gphit]

   ENDELSE

; 3. define mask

   tmask = lonarr(jpi, jpj)
   IF NOT keyword_set(NOMASK) THEN BEGIN
      s_file = hom_idl+'grids/mask_'+mask_file+'.dat'
      print, '     Read mask from ',s_file
      restore, s_file
;      nummsk = 12
;      s_file = hom_idl+'grids/mask_regular_'+ $
;       strtrim(string(jpi), 2)+'x'+strtrim(string(jpj), 2)
;      openr, nummsk, s_file, /get_lun, /f77
;      readu, nummsk, tmask
;   tmask = reverse(1-tmask, 2)
;      close, nummsk
;      free_lun, nummsk

   ENDIF ELSE BEGIN
      tmask[*, *]= 1
      print, '     Warning, No mask read'
   ENDELSE

; 4. Reduce grid in longitude

   IF keyword_set(i_index) THEN BEGIN
      idx_txt = ',XMINMESH='+string(i_index)+',XMAXMESH='+string(i_index+delta_i-1)
      idx_txt = idx_txt+',XMINDTA='+string(i_index)+',XMAXDTA='+string(i_index+delta_i-1)
   ENDIF
   IF keyword_set(j_index) THEN BEGIN
      idx_txt = idx_txt+',YMINMESH='+string(j_index)+',YMAXMESH='+string(j_index+delta_j-1)
      idx_txt = idx_txt+',YMINDTA='+string(j_index)+',YMAXDTA='+string(j_index+delta_j-1)
   ENDIF

   IF keyword_set(REVERSE_Y) THEN BEGIN
    gphit = reverse(gphit)
   ENDIF
   gphit = replicate(1, jpi)#gphit

   masked_data = 0
   mesh_type = 'atm'
;
; definition of key_shift
;
   IF keyword_set(NO_SHIFT) THEN BEGIN
      shift_txt = ', SHIFT = 0 '
   ENDIF
   IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
      shift_txt = ', SHIFT = 0 '
      periodic_txt = ', PERIODIC = 0'
   ENDIF

; Use the computegrid routine
   cmd_grid = 'computegrid, XAXIS = glamt, YAXIS = gphit, MASK = tmask, GLAMBOUNDARY = glamboundary, /FULLCGRID'+shift_txt+periodic_txt+idx_txt
   IF debug_w THEN BEGIN
    print, cmd_grid
   ENDIF
   res = execute( cmd_grid )

   print, '   key_shift =', key_shift


   key_offset = [0, 0, 0]
;
; indice i pour grille j moyenne zonale
;
   diaznl_idx = 1

END
