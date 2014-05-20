;+
;
; make maximum bowl index
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
; $Id: make_sobwlmax.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION make_sobwlmax, file_name, ncdf_db $
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
 usage='result=make_sobwlmax(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ALL_DATA=all_data ' $
         + ', ZMTYP=zmtypi)'

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

;
;
; Read sobowlin
;

   vargrid = 'T'
   new_file = new_filename(file_name, 'T', vargrid)
   bowlin = nc_read(new_file,'sobowlin', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data )

;
; interpolate from model level to depth in metres:
;

   bowlin_m = interpol([0.,total(e3w, /cumulative)], findgen(32)+1, bowlin.data)


; interpolate from depth in metres to sigma co-ordinates by performing
; density-binning
;


;  bowlin_s = interpol(s_s, depth, bowlin_m)


   field = {name: '', data: bowlin_m, legend: '', units: '', origin: '', dim: 0, direc:''}

   field.origin = bowlin.origin
   field.dim = bowlin.dim
   field.direc = bowlin.direc


   return, field
END
