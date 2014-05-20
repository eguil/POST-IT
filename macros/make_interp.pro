;+
;
; compute linear interpolation between 2 variables [ coef1*var1 + coef2*var2 ]
; or 3 variables [ coef1*var1 + coef2*var2 + coef3*var3 ]
; includes obviously difference or addition between 2 variables or 3 variables
;
; @param FILE_NAME {in}{required}{type=string}
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
; @keyword ZMTYP
;
; @returns
; structure
; -1 in case of error
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - fplod 20091208T102329Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * syntax of array
;
; @version
; $Id: make_interp.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_interp, file_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ALL_DATA=all_data $
         , ZMTYP=zmtyp
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg

 usage='result=make_interp(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ALL_DATA=all_data ' $
         + ', ZMTYP=zmtyp)'

 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
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

 arg_type = size(ncdf_db,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type ncdf_db' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

   interp = 0.0

   tab_var = strsplit(macro_base_fld, ',', /EXTRACT)
   IF ( debug_w ) THEN BEGIN
    print, 'macro_base_field : ', macro_base_fld
   ENDIF

   nb = n_elements(tab_var)

   IF ( nb NE 4 AND nb NE 6 ) THEN BEGIN
    STOP, 'Number of arguments must be equal to 4 or 6'
   ENDIF

   varname1 = strtrim(tab_var[0], 2)
   coef1 = float(strtrim(tab_var[1], 2))
   varname2 = strtrim(tab_var[2], 2)
   coef2 = float(strtrim(tab_var[3], 2))
   IF ( nb EQ 6 ) THEN BEGIN
      varname3 = strtrim(tab_var[4], 2)
      coef3 = float(strtrim(tab_var[5], 2))
   ENDIF

   IF ( debug_w ) THEN BEGIN
      print, 'var1 : ', varname1, 'coef1 : ', coef1
      print, 'var2 : ', varname2, 'coef2 : ', coef2
      IF nb EQ 6 THEN BEGIN
       print, 'var3 : ', varname3, 'coef3 : ', coef3
      ENDIF
   ENDIF

   var1 = nc_read(file_name, varname1, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   var2 = nc_read(file_name, varname2, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   IF nb EQ 6 THEN BEGIN
    var3 = nc_read(file_name, varname3, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   ENDIF

   interp = var1.data
   idx = WHERE( var1.data NE valmask )
   IF( idx[0] EQ -1 ) THEN BEGIN
    STOP, 'STOP. Something wrong in the data'
   ENDIF

   interp[idx] = ( nb EQ 6 ) ? coef1*var1.data[idx] + coef2*var2.data[idx] + coef3*var3.data[idx] : coef1*var1.data[idx] + coef2*var2.data[idx]

   legend = ( nb EQ 6 ) ? ' : ('+strtrim(string(coef1), 2)+')*'+varname1+'+('+ strtrim(string(coef2), 2)+')*'+varname2+'+('+ strtrim(string(coef3), 2)+')*'+varname3 : ' : ('+strtrim(string(coef1), 2)+')*'+varname1+'+('+ strtrim(string(coef2), 2)+')*'+varname2

   print, 'legend : ', legend
   varunit = ''

   field = {name: '', data: interp, legend: '', units: '', origin: '', dim: 0, direc:''}
   field.origin = var1.origin
   field.dim = var1.dim
   field.legend = legend
   field.legend = var1.direc

   return, field

END
