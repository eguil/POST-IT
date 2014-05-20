;+
;
; Reading any netcdf file
;
; @param FILE_NAME {in}{required}{type=string}
;
; @param VAR_NAME {in}{required}{type=string}
; variable name
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @keyword BOXZOOM
;
; @keyword TIME_1
;
; @keyword TIME_2
;
; @keyword ALL_DATA
;
; @keyword NO_MEAN
;
; @returns
; structure
; -1 in case of error
;
; @examples
;
; IDL> file_name='ginette'
; IDL> var_name='temp_1'
; IDL> ncdf_db='local:./'
; IDL> vert_type='z'
; IDL> result=nc_read(file_name, var_name, ncdf_db)
; IDL> help, result,/structure
; IDL> print, result
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; <pro>ncdf_getatt</pro>
;
; @todo
;
; explication sur vert_type
;
; realistic example
;
; @history
;
; - fplod 20091210T135516Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: nc_read.pro 229 2010-06-23 16:13:47Z ericg $
;
;-
FUNCTION nc_read, file_name, var_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ALL_DATA=all_data $
         , NO_MEAN=no_mean
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
   IF debug_w THEN BEGIN
    info = report('enter ...')
   ENDIF
;
; Return to caller if errors
;++ ON_ERROR, 2
;
 usage='result=nc_read(file_name, var_name, ncdf_db '$
         + ', BOXZOOM=boxzoom' $
         + ', TIME_1=time_1' $
         + ', TIME_2=time_2' $
         + ', ALL_DATA=all_data' $
         + ', NO_MEAN=no_mean)'

 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(file_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type file_name' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(var_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type var_name' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(ncdf_db,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type ncdf_db' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type=size(vert_type,/type)
 ;IF (common_type NE 7) THEN BEGIN
 ;  ras = report(['Incorrect common type vert_type' $
 ;         + '!C' $
 ;         + 'Usage : ' + usage])
 ;  return, -1
 ;ENDIF

   IF NOT keyword_set(TIME_1) THEN BEGIN
    time_1 = 1
   ENDIF
   IF NOT keyword_set(TIME_2) THEN BEGIN
    time_2 = time_1
   ENDIF
   IF debug_w THEN BEGIN
    print, '  Before read_ncdf, key_yreverse=', key_yreverse
   ENDIF

; decide which subdomain of data to read
   IF debug_w THEN BEGIN
    print, '     keyword_set(ALL_DATA) : ', keyword_set(ALL_DATA)
   ENDIF
   IF keyword_set(ALL_DATA) THEN BEGIN
      tout = 1
   ENDIF ELSE BEGIN
      tout = 0
   ENDELSE
   CASE vert_type OF
      'z' : zindex = 0
      'level' : zindex = 1
      ELSE: zindex = 0
   ENDCASE

; local directory
   IF strpos(ncdf_db, ':') GE 1 THEN BEGIN
    directory = (strsplit(ncdf_db, ':', /EXTRACT))[1]
   ENDIF ELSE BEGIN
    directory = ncdf_db
   ENDELSE

; Find the variable's attribute
   ncdf_getatt, directory+file_name, var_name, add_offset = add_offset, scale_factor = scale_factor, $
    missing_value = missing_value, units = units, calendar = calendar, long_name = long_name

   IF debug_w THEN BEGIN 
      print, '   nc_read: netCDF file attributes:  add_offset =', add_offset,', scale_factor = ', scale_factor,', missing_value =', missing_value,', units =', units, ', calendar = ', calendar,', long_name =', long_name
   ENDIF 
      
; By default units for the Z axis are meters
   IF n_elements(gdept) GT 1 THEN BEGIN
      IF name_level NE '-' THEN BEGIN
         ncdf_getatt, directory+file_name, name_level, units = units_depth
      ENDIF ELSE BEGIN
         units_depth = 'm'
      ENDELSE
   ENDIF ELSE BEGIN
      units_depth = ''
   ENDELSE

; Check consistency between time_2 computed from param in cmdline and the max number
; of time steps found in the file.
   jpt_max = find_jptmax(file_name, IODIRECTORY = directory)
   IF time_2-time_1+1 GT jpt_max AND jpt_max NE -1 THEN BEGIN
    time_2 = time_1+jpt_max-1
   ENDIF

; Read the data with a call to read_ncdf
   IF debug_w THEN BEGIN
    print, '     '
    print, '     Reading raw data from ', file_name
    IF keyword_set(boxzoom) THEN BEGIN
     print, '     boxzoom before=', boxzoom
    ENDIF
   ENDIF
   lec_data = read_ncdf(var_name, time_1-1, time_2-1, BOXZOOM = boxzoom, FILENAME = directory+file_name, $
                        /TIMESTEP, TOUT = tout, /NOSTRUCT, /CONT_NOFILL, GRID = vargrid, $
                        ZINVAR = zinvar, ZINDEX = zindex)
   field_dim = (size(lec_data))[0]
   IF field_dim LE 0 THEN BEGIN
    print, '  Something wrong happened in read_ncdf '
    return, -1
   ENDIF

   IF debug_w THEN BEGIN
    print, '     '
    IF keyword_set(boxzoom) THEN BEGIN
     print, '    boxzoom after=', boxzoom
    ENDIF
    print, '    firstxt,firstyt,firstzt=', firstxt, firstyt, firstzt
    print, '    lastxt,lastyt,lastzt=', lastxt, lastyt, lastzt
    print, '    zinvar=', zinvar
    print, '     jpt=', jpt
    print, '     key_shift=', key_shift
    print, '     After read_ncdf, key_yreverse=',key_yreverse
    print, '     help, lec_data='
    help, lec_data
    print, '     '
   ENDIF

; Average data along vertical if needed and update some features
; needed for plt (data_direc, name_suff)
   
; if ensemble, no mean done here for now (read_ncdf does not handle ensembles yet)
; see data_read for mean and model/ensemble extraction
   IF ensbl_code NE '' THEN BEGIN
      no_mean = 1
   ENDIF 

   name_suff = ''
   data_direc = ''
   update_data, TAB = lec_data, VNAME = var_name, BOXZOOM = boxzoom, ZUNITS = units_depth, $
    ZINVAR = zinvar, SUFFIX = name_suff, D_DIREC = data_direc, $
    TIME_1 = time_1, TIME_2 = time_2, NO_MEAN = no_mean
   field_dim = (size(lec_data))[0]

; Field attributes
   field = {name: '', data: lec_data, legend: '', units: '', origin: '', dim: 0, direc: data_direc}
   field.name = var_name
   field.origin = directory+file_name
   field.legend = long_name+name_suff
   field.units = units
   field.dim = field_dim

; replace NaN with valmask
;   idx_t = where (~finite(field.data))
;   IF idx_t[0] NE -1 THEN BEGIN
;    field.data(idx_t) = valmask
;   ENDIF

; get valmask (might need valmask = float(string(valmask))
   valmask = 1.e20
   IF size(missing_value, /TYPE) EQ 4 OR size(missing_value, /TYPE) EQ 5 THEN BEGIN
      valmask = missing_value
      ; ensure valmask is positive
      IF valmask LT 0 THEN BEGIN
         print, '      *** Warning valmask is negative - changing sign: ', valmask
         idx_t = where (field.data EQ valmask)
         IF idx_t[0] NE -1 THEN BEGIN
          field.data[idx_t] = -valmask
         ENDIF
         valmask = -valmask
         idx_t=0                ; free memory
      ENDIF
   ENDIF

   IF debug_w THEN BEGIN
    print, '     valmask=',valmask
   ENDIF

; if pseudo 3d mask, read mask file and set masked points to valmask
; still to do

; min/max

   chardim = ' - dim = '
   FOR i = 1, (size(field.data))[0] DO BEGIN
      chardim = chardim+string((size(field.data))[i], format = '(I5)')
   ENDFOR

   idx_valmsk = (where (field.data EQ valmask))
   idx_novalmsk = (where (field.data NE valmask))
   minf = idx_valmsk[0] EQ -1 ? min(field.data) : min(field.data[idx_novalmsk])
   maxf = idx_valmsk[0] EQ -1 ? max(field.data) : max(field.data[idx_novalmsk])

   print, ' = ',field.legend, '    [min/max = ',minf , maxf,'  ', field.units,' - masked values = ',valmask, chardim, ']'

   IF debug_w THEN BEGIN
    info = report('leaving ...')
   ENDIF

   return, field

END
