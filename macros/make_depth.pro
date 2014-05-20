;+
;
; Depth along isopycnal (computed in <propost_it>bining2</propost_it>
; or <propost_it>bining3</propost_it>)
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
; @keyword _EXTRA
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
; - fplod 20100119T094024Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_depth.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_depth, file_name, ncdf_db $
        , BOXZOOM=boxzoom $
        , TIME_1=time_1 $
        , TIME_2=time_2 $
        , ALL_DATA=all_data $
        , ZMTYP=zmtyp $
        , _EXTRA=extra
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='result=make_depth(file_name, ncdf_db ' $
        + ', BOXZOOM=boxzoom ' $
        + ', TIME_1=time_1 ' $
        + ', TIME_2=time_2 ' $
        + ', ALL_DATA=all_data ' $
        + ', ZMTYP=zmtyp ' $
        + ', _EXTRA=extra)'

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

   IF splot NE 1 THEN BEGIN
      print, '  WARNING @@vodepth/@@vosigvol requires density projection !'
      return, -1
   ENDIF

; declarations
;
   fieldr = {name: '', data: fltarr(jpi, jpj, jpk), legend: '', units: '', origin: '', dim: 0, direc:''}

   IF strpos(ncdf_db, ':') GE 1 THEN BEGIN
    directory = (strsplit(ncdf_db, ':', /EXTRACT))[1]
   ENDIF ELSE BEGIN
    directory = ncdf_db
   ENDELSE

   fieldr.origin = directory+file_name
   fieldr.dim = 3
   fieldr.direc = 'xys'

   return, fieldr
end
