;+
;
; make ratio std dev from 2D macro_base_fld monthly time serie (defined in
; <propost_it>macro_read</propost_it)
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
; $Id: make_ratiostd.pro 205 2013-10-21 09:46:13Z EG $
;
;-
FUNCTION make_ratiostd, file_name, ncdf_db $
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
 ENDIF
;
 usage='result=make_ratiostd(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ALL_DATA=all_data ' $
         + ', ZMTYP=zmtyp)'

 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usae])
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
; Read time serie
;
   IF debug_w THEN BEGIN
    print, 'keyword_set(ALL_DATA) : ', keyword_set(ALL_DATA)
   ENDIF
   print, '    Warning: standard deviation assuming monthly time serie of base field ',macro_base_fld

   nargs = (size(strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT)))[1]
   var_name = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[0]
   
; Define if ratiostd (typea='RT') or AC amplitude (typea='AC') or AC max-min ('MM')

   IF nargs EQ 2 THEN BEGIN 
      typea = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[1]
   ENDIF ELSE BEGIN 
      typea = 'RT'
   ENDELSE 

   mfld = nc_read(file_name, var_name, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   mfld.data = trends(mfld.data, '412', 'xyt')

; mean_sc[*,*,0] is mean annual cycle (from common)

   acfld = mean_sc[*,*,0:11]


   nxa = (size(mfld.data))[1]
   nya = (size(mfld.data))[2]

   IF debug_w THEN BEGIN
    print, '   nxa, nya', nxa, nya
   ENDIF

   stdw = fltarr(nxa, nya)
   stdw[*, *] = 0

; Sampling of data and computation of new numbers of values

   @mth_decode

   FOR imth = 0, nmth-1 DO BEGIN

      IF debug_w THEN BEGIN
       print, '   month idx/value: ', imth, strd[imth]
      ENDIF

      data = (mfld.data)[*, *, reform(idxm[imth,*], njpt)]
      nval = njpt
   ; compute interannual std dev
      std = a_timecorrelate(data, 0,/covariance)
      stdw = stdw + sqrt( nval/(nval-1)*std )

   ENDFOR
   ; compute annual std dev
   nvac = 12
   stdac = a_timecorrelate(acfld, 0,/covariance)
   stdac = sqrt( nvac/(nvac-1)*stdac )
   ;stdac = stddev(acfld,  dimension = 3,  /NaN) in IDL 8.3

   idm = where(mfld.data[*, *, 0] GE valmask/10.)

   CASE typea OF 
      'AC': BEGIN               ; AC amplitude (stddev)
         stdw = stdac
      END 
      'MM': BEGIN               ; AC max-min
         stdw = max(acfld, dimension = 3) - min(acfld, dimension = 3)
      END 
      ELSE: BEGIN ; compute ratio of interannual std dev over annual cycle stddev
         stdw = stdw/float(nmth)
         stdw = stdw/stdac
      END 
   ENDCASE 


   IF idm[0] NE -1 THEN BEGIN
    stdw[idm] = valmask
   ENDIF

   varname = varname+' '+ntxt

   IF stddev_mth NE '00' AND stddev_diff EQ 1 THEN BEGIN
      print, '    Warning: standard deviation diff between '+ntxt+' and the whole serie'
      std_tot = a_timecorrelate(mfld.data, 0,/covariance)
      std_tot = sqrt( jpt/(jpt-1)*std_tot )
      IF idm[0] NE -1 THEN BEGIN
       std_tot[idm] = valmask
      ENDIF
      idx_diff = WHERE (stdw NE valmask)
      stdw[idx_diff] = stdw[idx_diff] - std_tot[idx_diff]
   ENDIF

   field = {name: '', data: stdw, legend: '', units: '', origin: '', dim: 0, direc:''}

   field.origin = mfld.origin

   field.dim = 2

   field.legend = ' '+ntxt+' ['+cmdm.date1+'-'+cmdm.spec+']'

   IF stddev_diff EQ 1 THEN BEGIN
      field.legend = ' diff between '+ntxt+' and total TS ['+cmdm.date1+' - '+cmdm.spec+']'
   ENDIF

;  data and plot are 2D

   field.direc = 'xy'

   return, field
END
