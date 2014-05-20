;+
;
; compute ratio between 2 or 4 variables [ a/b or (a-b)/(c-d) ]
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
; $Id: make_ratio.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_ratio, file_name, ncdf_db $
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
;
 usage='result=make_ratio(file_name, ncdf_db ' $
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

; Extracts the variables between the commas.
; Pay attention : the order is important (a,b,c,d)

  div = 0.0

  tab_var = strsplit(macro_base_fld, ',', /EXTRACT)
  IF ( debug_w ) THEN BEGIN
   print, 'size(tab_var) : ', size(tab_var)
  ENDIF

  IF ( (size(tab_var))[1] NE 2 AND (size(tab_var))[1] NE 4 ) THEN BEGIN
    print, 'Number of variables in the macro should be 2 or 4'
  ENDIF

  IF ( (size(tab_var))[1] EQ 4 ) THEN BEGIN
    a = strtrim(tab_var[0], 2)
    b = strtrim(tab_var[1], 2)
    c = strtrim(tab_var[2], 2)
    d = strtrim(tab_var[3], 2)

    print, 'a : ', a
    print, 'b : ', b
    print, 'c : ', c
    print, 'd : ', d

    var_a = nc_read(file_name, a, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
    var_b = nc_read(file_name, b, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
    var_c = nc_read(file_name, c, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
    var_d = nc_read(file_name, d, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)

    div = var_a.data
    ;;div[*, *] = valmask
    div[*, *] = -1000.0
    diff = var_c.data - var_d.data

    idx = WHERE( diff NE 0.0 )
    IF( idx[0] EQ -1 ) THEN BEGIN
     STOP, 'STOP. Division cannot be computed'
    ENDIF

    div[idx] = ( var_a.data[idx] - var_b.data[idx] )/( diff[idx] )
    legend = ' : ( '+a+' - '+b+' ) / ( '+c+' - '+d+' )'

   ENDIF

   IF ( (size(tab_var))[1] EQ 2 ) THEN BEGIN

    a = strtrim(tab_var[0], 2)
    b = strtrim(tab_var[1], 2)

    print, 'a : ', a
    print, 'b : ', b

    var_a = nc_read(file_name, a, ncdf_db, TIME_1 = time_1, TIME_2 = time_2)
    var_b = nc_read(file_name, b, ncdf_db, TIME_1 = time_1, TIME_2 = time_2)

    div = var_a.data
    div[*, *] = valmask

    idx = WHERE( var_b.data NE 0.0 )
    IF( idx[0] EQ -1 ) THEN BEGIN
     STOP, 'STOP. Division cannot be computed'
    ENDIF

    div[idx] = ( var_a.data[idx] )/( var_b.data[idx]  )
    legend = ' : '+a+' / '+b

   ENDIF

   varname = 'ratio'
   varunit = ''

   field = {name: '', data: div, legend: '', units: '', origin: '', dim: 0, direc:''}
   field.origin = var_a.origin
   field.dim = var_a.dim
   field.legend = legend
   field.direc = var_a.direc

   return, field

END
