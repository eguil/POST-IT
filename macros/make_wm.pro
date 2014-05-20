;+
;
; make Wind Stress module
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
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20091210T095242Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_wm.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_wm, file_name, ncdf_db $
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
 usage='result=make_wm(file_name, ncdf_db ' $
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

; Read taux and tauy
;
   ;old_boite = [lon1, lon2, lat1, lat2, prof1, prof2]
   ;domdef
; new name files
   file_nam = strmid(base_file_name+base_suffix, 0, strlen(base_file_name+base_suffix)-1)
   vargrid = 'U'
   taux = nc_read(file_nam+'U'+suff_domain+'.nc','sozotaux', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   vargrid = 'V'
   tauy = nc_read(file_nam+'V'+suff_domain+'.nc','sometauy', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)

   wm = norme(taux.data, tauy.data)
   ;domdef, old_boite

   field = {name: '', data: wm, legend: '', units: '', origin: '', dim: 0, direc: ''}

   field.origin = taux.origin
   field.dim = taux.dim
   field.direc = taux.direc
   posave = max( [strpos(ucrt.legend, ' averaged'), strpos(ucrt.legend, ' at')] )
   IF posave NE -1 THEN BEGIN
    field.legend = strmid(ucrt.legend, posave)
   ENDIF

   return, field
END
