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
; $Id: mesh_glosea.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO mesh_glosea $
    , I_INDEX=i_index $
    , DELTA_I=delta_i $
    , J_INDEX=j_index $
    , DELTA_J=delta_j $
    , K_INDEX=k_index $
    , DELTA_K=delta_k $
    , ZONAL=zonal $
    , NO_SHIFT=no_shift $
    , WHOLE_ARRAYS=whole_arrays
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
print, '    Init GLOSEA mesh'

 usage='mesh_glosea' $
    + ', I_INDEX=i_index' $
    + ', DELTA_I=delta_i' $
    + ', J_INDEX=j_index' $
    + ', DELTA_J=delta_j' $
    + ', K_INDEX=k_index' $
    + ', DELTA_K=delta_k' $
    + ', ZONAL=zonal' $
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

;----------------------------------------------------------
; bornes de la grille
;----------------------------------------------------------

   jpi = 288
   jpj = 192
   jpk = 40

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


; init shift and periodicity

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

      print, '      set GLOSEA grid to j_index in ', j_index, j_index+delta_j

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

      print, '      set GLOSEA grid to i_index in ', i_index, i_index+delta_i

      key_periodique = 0

      key_offset = [0, 0, 0]

      diaznl_idx = 0

      key_shift = 0

   ENDIF

; reduce grid in depth ?

   IF keyword_set(K_INDEX) THEN BEGIN

      IF NOT keyword_set(DELTA_K) THEN BEGIN
       delta_k = 1
      ENDIF
      delta_k = delta_k - 1

      izminmesh = k_index-1
      izmaxmesh = k_index-1+delta_k

      print, '      set GLOSEA grid to k_index in ', k_index-1, k_index-1+delta_k

   ENDIF
;
;------------------------------------------------------
;  Read grid
;------------------------------------------------------


; T grid
   s_file = hom_idl+'grids/glosea_t_grid'
   restore, s_file
; U grid -> to do
;   s_file = hom_idl+'grids/glosea_u_grid'
;   restore, s_file

   glamu = glamt
   gphiu = gphit
   e1u = e1t
   e2u = e2t
   umask = tmask


; Shift re-init T,U,V,F

   key_shift =-16
   IF keyword_set(NO_SHIFT) THEN BEGIN
    key_shift = 0
   ENDIF

   glamt = shift(glamt,key_shift, 0)
   gphit = shift(gphit,key_shift, 0)
   e1t = shift(e1t,key_shift, 0)
   e2t = shift(e2t,key_shift, 0)
   tmask = shift(tmask,key_shift, 0, 0)

   glamu = shift(glamu,key_shift, 0)
   gphiu = shift(gphiu,key_shift, 0)
   e1u = shift(e1u,key_shift, 0)
   e2u = shift(e2u,key_shift, 0)
   umask = shift(umask,key_shift, 0, 0)

   isign=where(glamt LE glamt[0,0])
   IF isign[0] NE -1 THEN BEGIN
    glamt[isign] = glamt[isign]+360.
   ENDIF
   isign=where(glamu LE glamu[0,0])
   IF isign[0] NE -1 THEN BEGIN
    glamu[isign] = glamu[isign]+360.
   ENDIF

   glamv = glamu
   gphiv = gphiu
   e1v = e1u
   e2v = e2u
   vmask = umask

   glamf = glamt
   gphif = gphit
   e1f = e1t
   e2f = e2t
   fmask = tmask

; reduce grid to zonal mean

   IF keyword_set(ZONAL) THEN BEGIN

      no_lon_shift = no_lon_shift + 1

      jpi = 1
      ixminmesh = 0
      ixmaxmesh = 0

      print, '   set GLOSEA grid to zonal mean '

      diaznl_idx = 0

      glamt = glamt[diaznl_idx, *]
      glamu = glamu[diaznl_idx, *]
      glamv = glamv[diaznl_idx, *]
      glamf = glamf[diaznl_idx, *]
      gphit = gphit[diaznl_idx, *]
      gphiu = gphiu[diaznl_idx, *]
      gphiv = gphiv[diaznl_idx, *]
      gphif = gphif[diaznl_idx, *]

      e1t = e1t[diaznl_idx, *]
      e2t = e2t[diaznl_idx, *]
      e1u = e1u[diaznl_idx, *]
      e2u = e2u[diaznl_idx, *]
      e1v = e1v[diaznl_idx, *]
      e2v = e2v[diaznl_idx, *]

; read offset

      key_offset = [0, 0, 0]

      key_shift = 0

   ENDIF


   IF no_lon_shift EQ 0 THEN BEGIN

; read offset

      key_offset = [0, 0, 0]
;
; indice i pour grille j moyenne zonale
;
      diaznl_idx = 0

   ENDIF

END
