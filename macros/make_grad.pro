;+
;
; compute gradients along x and y on grid points T, U, V, lmdzl for
; any variable
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
; @version
; $Id: make_grad.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION make_grad, file_name, ncdf_db $
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
   IF debug_w THEN BEGIN
    info = report('enter ...')
    print, '  keyword_set(ALL_DATA) : ', keyword_set(ALL_DATA)
   ENDIF

 usage='result=make_grad(file_name, ncdf_db ' $
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

;  Extract the variable and the coordinate along which we want the gradients
   var_name = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[0]
   coord_name = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[1]

;  Read the variable in the correspondent netcdf file
   var = nc_read(file_name, var_name, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)

   varname = var.name
   varunit = var.units

;  Apply the saxo routine to compute gradients
   gradients = grad (var.data, coord_name)
   print, '   The vargrid variable after computation of the gradients is ', vargrid

;  Define the outputs of the function
   field = {name: '', data: gradients, legend: '', units: '', origin: '', dim: 0, direc:''}
   field.origin = var.origin
   field.dim = var.dim
   field.direc = var.direc


   IF debug_w THEN BEGIN
    info = report('leaving...')
   ENDIF

   return, field
END
