;+
;
;
; @param FILENAME {in}{required}{type=string}
; filename to examin.
; Must be an NetCDF file
;
; @keyword IODIRECTORY
; @keyword _EXTRA
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> filename='ginette'
; IDL> result = find_jptmax(filename)
; IDL> print,result
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; <pro>isafile</pro>
;
; @todo
; error handling
; check keywords
;
; @history
; - fplod 20091210T075903Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: find_jptmax.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
FUNCTION find_jptmax, filename $
         , IODIRECTORY=iodirectory $
         , _EXTRA=extra
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=find_jptmax(filename ' $
       +  '   , IODIRECTORY=iodirectory' $
       + '  , _EXTRA=extra)'

 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(filename,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type filename' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

   jptmax = -1
;------------------------------------------------------------
; we find the filename and open the file
;------------------------------------------------------------
   IF keyword_set(iodirectory) THEN BEGIN
      filename_tmp = isafile(FILENAME = filename, IODIRECTORY = iodirectory, _EXTRA=extra)
   ENDIF ELSE BEGIN
      filename_tmp = isafile(FILENAME = filename, _EXTRA=extra)
   ENDELSE
   cdfid = ncdf_open(filename_tmp)
   inq = ncdf_inquire(cdfid)
;------------------------------------------------------------
; we look for the record dimension
;------------------------------------------------------------
   IF inq.recdim NE -1 THEN BEGIN
      ncdf_diminq, cdfid, inq.recdim, timedimname, jptmax
   ENDIF ELSE BEGIN
      print, $
      report('the file '+filename+' has no infinite dimension !!!')
   ENDELSE

   ncdf_close, cdfid

   return, jptmax

END
