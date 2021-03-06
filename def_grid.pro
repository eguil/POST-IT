;+
;
; define which grid to use and set corresponding arrays
;
; @param CMD {in}{required}{type=structure}
; only cmd.exp, cmd.out, cmd.grid and cmd.var are used
;
; @examples
;
; IDL> cmd={exp:'HH',out:'-',grid:'umath',var:'temp_1'}
; IDL> glamboundary_box=0L
; IDL> box_h=[20L,380L,-30L,30L]
; IDL> sw_diffg=1L
; IDL> data_domain='global'
; IDL> def_grid, cmd
; IDL> help,cmd,/struct

; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; get rid of spawn
;
; explication common glamboundary_box, box_h, sw_diffg, data_domain
;
; realistic example
;
; @history
;
; - fplod 20091209T094824Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: def_grid.pro 213 2010-03-12 16:53:44Z ericg $
;
;-
PRO def_grid, cmd
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
   IF debug_w THEN BEGIN
    info = report('enter ...')
    print, 'cmd =', cmd
   ENDIF

 usage='def_grid, cmd'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(cmd,/type)
 IF (arg_type NE 8) THEN BEGIN
   ras = report(['Incorrect arg type cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_struct_tags=TAG_NAMES(cmd)
 tag=WHERE(STRMATCH(arg_struct_tags, 'EXP'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag EXP cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(cmd.exp,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.exp' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'OUT'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag OUT cmd' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(cmd.out,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.out' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'GRID'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag GRID cmd' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(cmd.grid,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.grid' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'VAR'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag VAR cmd' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(cmd.var,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.grid' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 common_type=size(glamboundary_box,/type)
 IF ((common_type NE 2) AND (common_type NE 3))THEN BEGIN
   ras = report(['Incorrect common type glamboundary_box' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 common_type=size(box_h,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type box_h' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 common_nelem=size(box_h,/n_elements)
 IF (common_nelem NE 4) THEN BEGIN
   ras = report(['Incorrect common n_elements box_h' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 common_type=size(sw_diffg,/type)
 IF (common_type NE 3) THEN BEGIN
   ras = report(['Incorrect common type sw_diffg' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 common_type=size(data_domain,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type data_domain' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

;   varexp = cmd.exp

   IF cmd.out EQ 'cdf' THEN BEGIN
      no_shift = 1
      whole_arrays = 1
      ;meshlec_type = '-' ; force grid read
      test_read = 1 ; force grid read : better test ? at least for atm grid
      key_onearth = 1
      glambound = 0
  ENDIF ELSE BEGIN
      no_shift = 0
      whole_arrays = 0
      tag = ''
      key_onearth = 1
      glambound = glamboundary_box
   ENDELSE

; If tv no key_shift
   IF cmd.out EQ 'tv' THEN BEGIN
      no_shift = 1
   ENDIF

; append name of variable to file ?
   file_suff_var = ''
   IF strpos(cmd.grid, '#') NE -1 THEN BEGIN
      cmd.grid = strmid(cmd.grid, 0, strlen(cmd.grid)-1)
      @def_file_suff_var
   ENDIF

   IF debug_w THEN BEGIN
    print, '    file_suff_var = ', file_suff_var
   ENDIF

; read_grid_from_file ?

   read_grid_from_file = 0

   IF strpos(cmd.grid, '@') EQ 0 THEN BEGIN
      cmd.grid = strmid(cmd.grid, 1, strlen(cmd.grid)-1)
      read_grid_from_file = 1
      print, '    Will read grid info '+cmd.grid+' from data file'
      key_shift_map = 0
      IF strpos(cmd.var, '@@') NE -1 THEN BEGIN
         @def_macro_base_fld
         var_read_grd_file = macro_base_fld_1
      ;   stop
      ENDIF ELSE BEGIN
         var_read_grd_file = cmd.var
      ENDELSE
   ENDIF ELSE BEGIN
      var_read_grd_file = cmd.var
   ENDELSE

   IF debug_w THEN BEGIN
    print, '    cmd.grid after test read_grid_from_file: ', cmd.grid
   ENDIF

; choose grid (if not already loaded)

; tests to read
   test_box = 0
   IF box_h[0] NE box_h_prev[0] OR box_h[1] NE box_h_prev[1] THEN BEGIN
    test_box = 1
   ENDIF
   test_read = 0
   IF cmd.grid NE cmd_prev.grid OR test_box EQ 1 THEN BEGIN
      test_read = 1
      suff_domain = ''
   ENDIF ELSE BEGIN
      suff_domain = suff_domain
   ENDELSE
   IF sw_diffg EQ 1 THEN BEGIN
    test_read = 1
   ENDIF

   IF test_read EQ 1 THEN BEGIN
 ; read attributes from Defaults/Grids/grid_config.def
      file_grid_config = hom_def+'grid_config.def'
      IF debug_w THEN BEGIN
       print, '      doing: grep -i "\ '+cmd.grid+' " '+file_grid_config
      ENDIF
      spawn, 'grep -i "\ '+cmd.grid+' " '+file_grid_config, line
      line = strcompress(strtrim(line[0], 2))
      length = strlen(line)

      IF debug_w THEN BEGIN
       print, '      line from grid_config.def  ',line
      ENDIF

      IF length EQ 0 THEN BEGIN
         print, ' *** def_grid : define grid ', cmd.grid, ' in file ', file_grid_config
         stop
      ENDIF ELSE BEGIN
         argvar = strsplit(line, ' ', /EXTRACT)
         mesh_type = argvar[1]
         masked_data = argvar[2]
         mesh_type2 = argvar[3]
         vargrid = argvar[4]
         name_level = argvar[5]
      ENDELSE
      meshlec_type = mesh_type
   ENDIF

;   IF masked_data EQ 0 THEN BEGIN
;    atmos_msk = 0
;   ENDIF
   IF read_grid_from_file EQ 1 THEN BEGIN ; force use all points in computation/plot
      IF atmos_msk NE 0 THEN BEGIN 
         print, ' *** WARNING ***'
         print, ' atmos_msk (in plt_def) set to 0 (will use ALL points in computation/plot) as no mask present'
         atmos_msk = 0
      ENDIF 
   ENDIF 
; init zoom for data read

   IF data_domain EQ 'global' THEN BEGIN
      ixmindta = 0
      iymindta = 0
      izmindta = 0
   ENDIF
   IF debug_w THEN BEGIN
    print, '     varexp in def_grid before case cmd.grid:', varexp
   ENDIF

   IF read_grid_from_file EQ 1 THEN BEGIN
    GOTO, final
   ENDIF
   CASE cmd.grid OF
      't30': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_gaussian, 30, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, glamboundary = glambound
            domdef & triangles=triangule()
        ENDIF
      END
      't42': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_gaussian, 42, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, glamboundary = glambound
            domdef & triangles=triangule()
         ENDIF
      END
      't62': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_gaussian, 62, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, glamboundary = glambound
            domdef & triangles=triangule()
         ENDIF
      END
      't106': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_gaussian, 106, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, glamboundary = glambound
            domdef & triangles=triangule()
         ENDIF
      END
      'umat': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_um, 96, 73, 'T', NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, /reverse_y
            domdef & triangles=triangule()
         ENDIF
      END
      'umau': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_um, 96, 72, 'U', NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, /reverse_y
            domdef & triangles=triangule()
         ENDIF
      END
      'umath': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_um, long(288), long(217), 'T', NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays
            domdef & triangles=triangule()
         ENDIF
      END
      'umauh': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_um, long(288), long(216), 'U', NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays
            domdef & triangles=triangule()
        ENDIF
      END
      'reg1.125': BEGIN
         IF test_read EQ 1 THEN BEGIN
          read_grid_from_file = 1
         ENDIF
      END
;      'reg2.5': BEGIN
;         IF test_read EQ 1 THEN BEGIN
;           read_grid_from_file = 1
;         ENDIF
;      END
      'reg1m': BEGIN
         IF test_read EQ 1 THEN BEGIN
          read_grid_from_file = 1
         ENDIF
      END
      'reg1': BEGIN
         IF test_read EQ 1 THEN BEGIN
          read_grid_from_file = 1
         ENDIF
      END
      'reg1mtrop': BEGIN
         IF test_read EQ 1 THEN BEGIN
                                ; j in [69,110] (20.5S,20.5N)
            mesh_regular, 1., 1., inilon = -179.5, mask_file = 'hadisst1', j_index =69, delta_j = 42, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, glamboundary = glambound
            domdef & triangles=triangule()
         ENDIF
      END
      'tao': BEGIN
         IF test_read EQ 1 THEN BEGIN
            mesh_tao, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays
            domdef & triangles=triangule()
         ENDIF
      END
;       'new_grid': BEGIN
;          IF test_read EQ 1 THEN BEGIN
;             mesh_new_grid ...
;             domdef & triangles=triangule()
;          ENDIF
;       END
      ELSE: BEGIN
         ; nc_grids_list defined in plt_def.pro
         indx = where(nc_grids_list EQ cmd.grid)
         IF indx NE -1 THEN BEGIN
            IF test_read EQ 1 THEN BEGIN
               mesh_pcmdi, cmd.grid, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays
               domdef & triangles=triangule()
            ENDIF
         ENDIF ELSE BEGIN

            h_config_oce = 'ORCA_R2'
            IF strpos (cmd.grid, '05') NE -1 THEN BEGIN
             h_config_oce = 'ORCA05'
            ENDIF

            IF meshlec_type NE 'oce' OR data_dom_type NE data_domain OR h_config_oce_type NE h_config_oce OR v_config_oce_type NE v_config_oce OR orca_mask_version_type NE orca_mask_version OR test_box EQ 1 OR test_read EQ 1 THEN BEGIN
               CASE data_domain OF
                  'equator': BEGIN
                     mesh_orca, j_index=73, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, H_CONFIG = h_config_oce, V_CONFIG = v_config_oce
                     suff_domain = '_eq'
                     domdef
                  END
                  'equator_band': BEGIN
                                ; j in [64,82] (5S,5N)
                     mesh_orca, j_index=64, delta_j = 19, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, H_CONFIG = h_config_oce, V_CONFIG = v_config_oce
                     suff_domain = '_eqb'
                     domdef
                  END
                  'pacific': BEGIN
                                ; j in [49,97] (30S,30N), i in
                                ; [16,106] (110E,290), k in [1-19] for ORCA2L30
                                ; (0.,364 m)
                     CASE h_config_oce of
                        'ORCA_R2': mesh_orca, j_index=49, delta_j = 49, i_index = 16, delta_i =91, k_index = 1, delta_k = 19, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, H_CONFIG = h_config_oce, V_CONFIG = v_config_oce
                        'ORCA05': mesh_orca, j_index=183, delta_j = 130, i_index = 54, delta_i =381, k_index = 1, delta_k = 19, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, H_CONFIG = h_config_oce, V_CONFIG = v_config_oce
                     ENDCASE
                     suff_domain = '_pac'
                     domdef
                     triangles=triangule()
                  END
                  'pacific_eq': BEGIN
                                ; j in [64,82] (5S,5N), i in [16,106] (110E,290)
                     mesh_orca, j_index=64, delta_j = 19, i_index = 16, delta_i =91, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays , H_CONFIG = h_config_oce, V_CONFIG = v_config_oce
                     suff_domain = '_paceq'
                     domdef
                  END
                  'zonal': BEGIN
                     mesh_orca, /zonal, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, H_CONFIG = h_config_oce, V_CONFIG = v_config_oce
                     suff_domain = '_diaznl'
                     domdef
                  END
                  'glosea': BEGIN
                     mesh_glosea, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays
                     suff_domain = '_glosea'
                     domdef
                     triangles=triangule()
                  END
                  ELSE: BEGIN
                     IF cmd.grid EQ 'BMT' OR cmd.grid EQ 'BMU' OR cmd.grid EQ 'BMV' THEN BEGIN
                        mesh_micom, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, H_CONFIG = h_config_oce, V_CONFIG = v_config_oce
                        ;; We Need to redefine vargrid because it
                        ;; has changed when call to initncdf
                        vargrid = argvar[4]
                        suff_domain = ''
                     ENDIF ELSE BEGIN
                        mesh_orca, NO_SHIFT = no_shift, WHOLE_ARRAYS = whole_arrays, H_CONFIG = h_config_oce, V_CONFIG = v_config_oce
                        suff_domain = ''
                        domdef
                        triangles=triangule()
                     ENDELSE
                  END
               ENDCASE
               masked_data = 1
               meshlec_type = 'oce'
               mesh_type = 'oce'
               data_dom_type = data_domain
               h_config_oce_type = h_config_oce
               v_config_oce_type = v_config_oce
               orca_mask_version_type = orca_mask_version
            ENDIF
         ENDELSE
      END
   ENDCASE

   key_shift_map = key_shift

   IF read_grid_from_file EQ 1 THEN BEGIN
      ; def base field if macro
 ; needed ?     cmd.grid = strmid(cmd.grid, 0, strlen(cmd.grid)-1)
      @def_file_suff_var
      var_read_grd_file = macro_base_fld
      file_suff_var = ''
   ENDIF

final:

   box_h_prev = box_h

   IF debug_w THEN BEGIN
    print, '     cmd.grid:          ', cmd.grid
    print, '     file_suff_var:     ', file_suff_var
    print, '     var_read_grd_file: ', var_read_grd_file
    print, '     meshlec_type:      ', meshlec_type
    print, '     varexp:            ', varexp
    info = report('leaving...')
   ENDIF

END
