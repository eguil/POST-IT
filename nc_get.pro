;+
;
; @param FILE_NAME {in}{required}{type=string}
;
; @param VAR_NAME {in}{required}{type=string}
; variable name
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> file_name='ginette'
; IDL> var_name='temp_1'
; IDL> result=nc_get(file_name, var_name)
; IDL> print, result
;
; @todo
; errors handling
;
; @history
;
; - fplod 20091210T131639Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: nc_get.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION nc_get, file_name, var_name
;
  compile_opt idl2, strictarrsubs
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=nc_get(file_name, var_name)
;
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

 arg_type = size(var_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type var_name' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

; ouverture fichier netCDF + contenu ?
   cdfid=ncdf_open(file_name)

; obtention champ variable

   ncdf_varget, cdfid, ncdf_varid(cdfid,var_name), lec_data

   return, lec_data
END
