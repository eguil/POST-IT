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
; $Id: mesh_orca.pro 207 2010-01-26 10:38:20Z pinsard $
;
;-
PRO mesh_orca $
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
 usage='mesh_orca' $
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

   ; temporary

   IF h_config EQ 'ORCA05' THEN BEGIN
    v_config = 'L46'
   ENDIF
;   IF h_config EQ 'ORCA_R2' THEN BEGIN
;    v_config = 'L30'
;   ENDIF

   IF NOT keyword_set(H_CONFIG) THEN BEGIN
    h_config = 'ORCA_R2'
   ENDIF
   IF NOT keyword_set(V_CONFIG) THEN BEGIN
    v_config = 'L30'
   ENDIF
   print, '    Init ORCA mesh horizontal config '+h_config
   print, '                     vertical config '+v_config

   varexpp = varexp

;----------------------------------------------------------
; bornes de la grille
;----------------------------------------------------------

   CASE h_config OF
      'ORCA05': begin
         IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
            ixminmesh =0 & ixmaxmesh =721
            iyminmesh =0 & iymaxmesh =510
         ENDIF ELSE BEGIN
            ixminmesh =2 & ixmaxmesh =720
            iyminmesh =0 & iymaxmesh =510
         ENDELSE
      END
      'ORCA_R2': begin
         IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
            ixminmesh =0 & ixmaxmesh =181
            iyminmesh =0 & iymaxmesh =148
         ENDIF ELSE BEGIN
            ixminmesh =2 & ixmaxmesh =181
            iyminmesh =0 & iymaxmesh =147
         ENDELSE
      END
      'ORCA_R4':  begin
         IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
            ixminmesh =0 & ixmaxmesh =91
            iyminmesh =0 & iymaxmesh =75
         ENDIF ELSE BEGIN
            ixminmesh =2 & ixmaxmesh =91
            iyminmesh =0 & iymaxmesh =75
         ENDELSE
      END
   ENDCASE

   CASE v_config OF
      'L300': BEGIN & izminmesh =0 & izmaxmesh =300 & END
      'L30': BEGIN & izminmesh =0 & izmaxmesh =30 & END
      'L46': BEGIN & izminmesh =0 & izmaxmesh =45 & END
         ELSE: BEGIN
            print, '   **** vertical config unknown for ORCA: ',v_config
         END
   ENDCASE
;
; init shift and periodicity


;   IF keyword_set(NO_SHIFT) THEN BEGIN
;    key_shift = 0
;   ENDIF
   IF keyword_set(NO_SHIFT) THEN BEGIN
    shift = 0
    key_shift = 0
   ENDIF
   no_lon_shift = 0

;   key_periodique = 1
   periodic = 1

; reduce grid in latitude ?

   IF keyword_set(J_INDEX) THEN BEGIN

      IF NOT keyword_set(DELTA_J) THEN BEGIN
       delta_j = 1
      ENDIF
      delta_j = delta_j - 1

      iyminmesh = j_index
      iymaxmesh = j_index+delta_j

      print, '      set ORCA grid to j_index in ', j_index, j_index+delta_j
      iymindta = iyminmesh
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

      print, '      set ORCA grid to i_index in ', i_index, i_index+delta_i

      ixmindta = ixminmesh
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

      izmindta = izminmesh

      print, '      set ORCA grid to k_index in ', k_index-1, k_index-1+delta_k

   ENDIF
;
;------------------------------------------------------
; lecture de la grille
;------------------------------------------------------

   key_yreverse = 0

   CASE h_config OF
      'ORCA05':  BEGIN
         mesmsk = 'micromeshmaskORCA05.nc'
         CASE v_config of
            'L46': mesmsk = 'meshmaskORCA05_L46.nc'
            'L30': mesmsk = 'meshmaskORCA05_L30.nc'
            ELSE: BEGIN
               print, '   **** vertical config unknown for ORCA05: ',v_config
            END
         ENDCASE
         cmd_grid = 'ncdf_meshread, sm_file, GLAMBOUNDARY = glamboundary_box, PERIODIC = periodic'
      END
      'ORCA_R2': BEGIN
         CASE v_config of
            'L30': BEGIN
               CASE orca_mask_version OF
                  'V1': mesmsk = 'meshmask.orca.2d.nc'
                  'V2': mesmsk = 'meshmask_ORCA_R2.nc'
                  'V3': mesmsk = 'meshmask_ORCA_R2_V2.nc'
               ENDCASE
               IF whole_arrays EQ 0 THEN BEGIN

                  cmd_grid = 'ncdf_meshread, sm_file, GLAMBOUNDARY = glamboundary_box, PERIODIC=periodic'
               ENDIF ELSE BEGIN
                  cmd_grid = 'ncdf_meshread, sm_file, GLAMBOUNDARY = glamboundary_box, SHIFT=shift, PERIODIC=periodic'
               ENDELSE
            END
            'L46': BEGIN
               mesmsk = 'meshmaskORCA2_L46.nc'
               cmd_grid = 'ncdf_meshread, sm_file, GLAMBOUNDARY = glamboundary_box, SHIFT=shift, PERIODIC=periodic'
            END
            'L300': BEGIN
               mesmsk = 'meshmask.orca.2d.L300.nc'
               cmd_grid = 'ncdf_meshread, sm_file, GLAMBOUNDARY = glamboundary_box, PERIODIC=periodic'
            END
            ELSE: BEGIN
               print, '   **** vertical config unknown for ORCA2: ',v_config
            END
         ENDCASE
      END
      'ORCA_R4': BEGIN
         mesmsk = 'meshmask_orca4.nc'
         cmd_grid = 'ncdf_meshread, sm_file, GLAMBOUNDARY = glamboundary_box, PERIODIC=periodic'
      END
   ENDCASE

   print, '                         mask config '+orca_mask_version
   IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
    print, '                         [whole array]'
   ENDIF
	
   sm_file = hom_idl+'grids/'+mesmsk
   res = find(sm_file)

   IF res NE 'NOT FOUND' THEN BEGIN
      res_grid = execute(cmd_grid)
   ENDIF ELSE BEGIN
      stop, 'No meshmask file found for ORCA mesh config'
   ENDELSE

; reduce grid to zonal mean

   IF keyword_set(ZONAL) THEN BEGIN

      no_lon_shift = no_lon_shift + 1

      jpi = 1
      ixminmesh = 0
      ixmaxmesh = 0

      print, '   set ORCA grid to zonal mean '

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
      CASE h_config OF
         'ORCA05': BEGIN
; read offset

            key_offset = [2, 0, 0]
;
; indice i pour grille j moyenne zonale
;
            diaznl_idx = 100-1

         END
         'ORCA_R2': BEGIN
; read offset

            key_offset = [2, 0, 0]
;
; indice i pour grille j moyenne zonale
;
            diaznl_idx = 50-1

         END
         'ORCA_R4': BEGIN
; read offset

            key_offset = [2, 0, 0]
;
; indice i pour grille j moyenne zonale
;
            diaznl_idx = 25-1

         END
      ENDCASE
   ENDIF

   varexp = varexpp

   IF keyword_set(NO_SHIFT) THEN BEGIN
    key_offset = [0, 0, 0]
   ENDIF

   print, '     key_shift =', key_shift
   IF debug_w THEN BEGIN
    print, '     vargrid, varexp at exit = ',vargrid, varexp
   ENDIF

   IF debug_w THEN BEGIN
      info = report('leaving ...')
   ENDIF

END
