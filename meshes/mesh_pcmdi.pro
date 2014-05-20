;+
;
; @param MODEL {in}{required}{type=string}
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
; $Id: mesh_pcmdi.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO mesh_pcmdi, model $
    , NO_SHIFT=no_shift $
    , WHOLE_ARRAYS=whole_arrays
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='mesh_pcmdi, model' $
    + ', NO_SHIFT=no_shift' $
    + ', WHOLE_ARRAYS=whole_arrays'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(model,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type model' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

; init grid, sf, masks for grids with regular grid IPCC netCDF standard
;
   IF debug_w THEN BEGIN
    info = report('enter ...')
    print, 'model =', model
   ENDIF

   print,'   Model inits for ', model
   IF debug_w THEN BEGIN
    print, '     varexp at start of mesh_pcmdi:', varexp
   ENDIF

   varexpp = varexp

 ; use key_shiftg if grid and data file are not organized the same
 ; key_shiftg[0] = longitude shift
 ; key_shiftg[1] = -1 if latitude reversing needed
 ; key_shiftg = [0, 0]
 ; avoid using key_shiftg[0] and keyshiftg in general
 ; otherwise it gets much more complicated with initncdf
   key_shift = 0

 ; initialisation of character variables used in the execution of initncdf
 ; (and computegrid as a consequence)
   shift_txt = ''
   plain_txt = ''

   sm_file = hom_idl+'grids/grids_'+model+'.nc'
   res = find(sm_file)

   IF keyword_set(NO_SHIFT) THEN BEGIN
      shift_txt = ', SHIFT = 0'
   ENDIF
   IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
      plain_txt = ', PLAIN = 1'
      ;; Force YREVERSE = 0, ZREVERSE = 0, PERIODIC = 0, SHIFT = 0, STRIDE = [1, 1, 1] and
      ;; suppress the automatic redefinition of the domain in case of x periodicity overlap,
      ;; y periodicity overlap (ORCA type only) and mask border to 0.
   ENDIF

   IF debug_w THEN BEGIN
    print, '      key_yreverse = ', key_yreverse
   ENDIF

   IF res NE 'NOT FOUND' THEN BEGIN

      cmd_grid = 'initncdf, sm_file, GLAMBOUNDARY = glamboundary_box, FULLCGRID=1'+plain_txt+shift_txt
      res_grid = execute(cmd_grid)

      IF debug_w THEN BEGIN
       print, '      Found mask from ',sm_file
      ENDIF

      tmask = byte(read_ncdf('sftlf', 0, 0, /timestep, file = sm_file, /nostruct))

      idx = where(tmask EQ valmask)
      IF idx[0] NE -1 THEN BEGIN
       tmask[idx] = 0.
      ENDIF

      idx = where(tmask LE 50.)
      tmask[idx] = 0.
      tmask = tmask < 1
      tmask = 1-tmask

   ENDIF

   IF debug_w THEN BEGIN
    print, '      key_shift = ', key_shift
    print, '      key_yreverse = ', key_yreverse
   ENDIF

   masked_data = 0
   mesh_type = 'atm'

   key_offset = [0, 0, 0]
;
; indice i pour grille j moyenne zonale
;
   diaznl_idx = 1

   varexp = varexpp

   print,'    End of initialisation for pcmdi grid:', model
   IF debug_w THEN BEGIN
    print, '     varexp :', varexp
    info = report('leaving...')
   ENDIF

END
