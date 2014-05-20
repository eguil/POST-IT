;+
;
; make Z transport in Sv at T point
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
; $Id: make_ztran.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION make_ztran, file_name, ncdf_db $
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
 usage='result=make_ztran(file_name, ncdf_db ' $
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

; Read W
;
   file_nam = strmid(file_name, 0, strlen(file_name)-4)
   ;old_boite = [lon1, lon2, lat1, lat2, prof1, prof2]
   ;domdef
   vargrid = 'W'
   w = nc_read(file_nam+'W.nc','vovecrtz', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)

;  Transport along Z : trz = w*e1t*e2t (Sv)


   e1t3d = reform(e1t, jpi*jpj)
   e1t3d = reform(e1t3d#replicate(1, jpk), jpi, jpj, jpk)

   e2t3d = reform(e2t, jpi*jpj)
   e2t3d = reform(e2t3d#replicate(1, jpk), jpi, jpj, jpk)

   trz = w.data*e1t3d*e2t3d

   trz = trz*1.e-6

   ;domdef, old_boite

   field = {name: '', data: trz, legend: '', units: '', origin: '', dim: 0, direc: ''}

   field.origin = w.origin
   field.dim = w.dim
   field.direc = w.direc

   return, field
END
