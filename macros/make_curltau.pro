;+
;
; make Wind Stress curl
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
; @returns
; structure
; -1 in case of error
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T094958Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_curltau.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
;
FUNCTION make_curltau, file_name, ncdf_db $
         , BOXZOOM=boxzoom  $
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
 usage='result=make_curltau(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ALL_DATA=all_data '$
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

; Read taux and tauy
;
   ;old_boite = [lon1, lon2, lat1, lat2, prof1, prof2]
   ;domdef, /memeindice
   taux = nc_read(file_name,'ustr', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data, /MEMEINDICES)
   tauy = nc_read(file_name,'vstr', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data, /MEMEINDICES)

; compute curl

   zu = taux.data
   zv = tauy.data
   wcurl = (shift(zv, -1, 0)-zv)/e1t + (zu-shift(zu, 0, -1))/e2t

;    if NOT keyword_set(key_periodique) then begin
;       wcurl(0, *) = !values.f_nan
;       wcurl(nx-1, *) = !values.f_nan
;    endif
;    wcurl(*, 0) = !values.f_nan
;    wcurl(*, ny-1) = !values.f_nan

;   wcurl = curl(taux.data, tauy.data)
   domdef, old_boite

   field = {name: '', data: wcurl, legend: '', units: '', origin: '', dim: 0, direc:''}

   field.origin = taux.origin
   field.dim = taux.dim
   field.direc = taux.direc

   return, field
END
