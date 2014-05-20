;+
;
; @keyword I_INDEX
;
; @keyword DELTA_I
;
; @keyword J_INDEX
;
; @keyword DELTA_J
;
; @keyword K_INDEX
;
; @keyword DELTA_K
;
; @keyword ZONAL
;
; @keyword NO_SHIFT
;
; @keyword WHOLE_ARRAYS
;
; @keyword H_CONFIG
;
; @keyword V_CONFIG
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
; $Id: mesh_micom.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO mesh_micom $
    , I_INDEX=i_index $
    , DELTA_I=delta_i $
    , J_INDEX=j_index $
    , DELTA_J=delta_j $
    , K_INDEX=k_index $
    , DELTA_K=delta_k $
    , ZONAL=zonal $
    , NO_SHIFT=no_shift $
    , WHOLE_ARRAYS=whole_arrays $
    , H_CONFIG=h_config $
    , V_CONFIG=V_config
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
   print, '    Init MICOM mesh horizontal config '
;
 usage='mesh_micom' $
    + ', I_INDEX=i_index' $
    + ', DELTA_I=delta_i' $
    + ', J_INDEX=j_index' $
    + ', DELTA_J=delta_j' $
    + ', K_INDEX=k_index' $
    + ', DELTA_K=delta_k' $
    + ', ZONAL=zonal' $
    + ', NO_SHIFT=no_shift' $
    + ', WHOLE_ARRAYS=whole_arrays' $
    + ', H_CONFIG=h_config' $
    + ', V_CONFIG=V_config'
;
 nparam = N_PARAMS()
 IF (nparam NE 0) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

;   h_config = 'BCM'
;   v_config = 'NONE'

;----------------------------------------------------------
; bornes de la grille
;----------------------------------------------------------

   jpi = 163
   jpj = 150
   jpk = 0


 ;  IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
 ;     ixminmesh =0 & ixmaxmesh = jpi - 1
 ;     iyminmesh =0 & iymaxmesh = jpj - 1
 ;  ENDIF ELSE BEGIN
 ;     ixminmesh =2 & ixmaxmesh = jpi - 1
 ;     iyminmesh =0 & iymaxmesh = jpj - 1
 ;  ENDELSE

   izminmesh = jpk
   izmaxmesh = jpk

;
; init shift and periodicity

   IF keyword_set(NO_SHIFT) THEN BEGIN
    key_shift = 0
   ENDIF

   no_lon_shift = 0

   key_periodique = 1

; reduce grid in latitude ?

   IF keyword_set(J_INDEX) THEN BEGIN

      IF NOT keyword_set(DELTA_J) THEN BEGIN
       delta_j = 1
      ENDIF
      delta_j = delta_j - 1

      iyminmesh = j_index
      iymaxmesh = j_index+delta_j

      print, '      set MICOM grid to j_index in ', j_index, j_index+delta_j
;; a garder ??
      iymindta = iyminmesh
;;
   ENDIF

; reduce grid in longitude ?

   IF keyword_set(I_INDEX) THEN BEGIN

      no_lon_shift = no_lon_shift + 1

      IF NOT keyword_set(DELTA_I) THEN BEGIN
       delta_i = 1
      ENDIF
      delta_i = delta_i - 1

      ixminmesh = i_index
      ixmaxmesh = i_index+delta_i

      print, '      set MICOM grid to i_index in ', i_index, i_index+delta_i
;; a garder ??
      ixmindta = ixminmesh
;;
      key_periodique = 0
      key_offset = [0, 0, 0]
      diaznl_idx = 0
      key_shift = 0
   ENDIF

; reduce grid in depth ? later

;
;------------------------------------------------------
; lecture de la grille
;------------------------------------------------------

   key_yreverse = 0

   mesmsk = 'meshmask_MICOM.nc'
   IF whole_arrays EQ 0 THEN BEGIN
      ncdf_meshmicom, hom_idl+'grids/'+mesmsk, glamboundary = glamboundary_box, onearth = onearth
   ENDIF ELSE BEGIN
      ncdf_meshmicom, hom_idl+'grids/'+mesmsk, onearth = onearth
   ENDELSE

   IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
    print, '                         [whole array]'
   ENDIF

   IF no_lon_shift EQ 0 THEN BEGIN
      key_offset = [0, 0, 0]
   ENDIF

   IF keyword_set(NO_SHIFT) THEN BEGIN
    key_offset = [0, 0, 0]
   ENDIF

   print, '     key_shift =', key_shift
   print, '     key_offset =', key_offset

   print, 'End of Micom mesh config '

; reduce grid to zonal mean (later)


END
