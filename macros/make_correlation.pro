;+
;
; compute point-wise correlation of two 3D (x,y,t) fields
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
; $Id: make_correlation.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
FUNCTION make_correlation, file_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ALL_DATA=all_data
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg

   IF debug_w THEN BEGIN
    info = report('enter ...')
   ENDIF

 usage='result=make_correlation(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ALL_DATA=all_data)'

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
   var_name1 = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[0]
   var_name2 = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[1]

;  Build file_name2 if different

   IF strpos (cmd1_back.grid, '#') NE -1 THEN BEGIN
      varpos = strpos(file_name, var_name1)
      file_name2= strmid(file_name, 0, varpos)+var_name2+'.nc'
      IF debug_w THEN BEGIN
       print, '   file_name2 = ', file_name2
      ENDIF
   ENDIF ELSE BEGIN
      file_name2 = file_name
   ENDELSE

;  Read the variables in the correspondent netcdf file
   var1 = nc_read(file_name, var_name1, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
   var2 = nc_read(file_name2, var_name2, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)

   varname = var1.name+'/'+var2.name+' corr.'
   varunit = '[-1/1]'

   nxa = (size(var1.data))[1]
   nya = (size(var1.data))[2]

   pt_correl = fltarr(nxa, nya)
   pt_correl[*, *] = 0


;  Sampling of data and computation of new numbers of values

   @mth_decode

   FOR imth = 0, nmth-1 DO BEGIN

      IF debug_w THEN BEGIN
       print, '   month idx/value: ', imth, strd[imth]
      ENDIF

      data1 = (var1.data)[*, *, reform(idxm[imth,*], njpt)]
      data2 = (var2.data)[*, *, reform(idxm[imth,*], njpt)]
      nval = njpt

; Compute correlation

      pt_correl = pt_correl + c_timecorrelate(data1, data2)

   ENDFOR

   idm = where(var1.data[*, *, 0] GE valmask/10.)

   pt_correl = pt_correl/float(nmth)

   IF idm[0] NE -1 THEN BEGIN
    pt_correl[idm] = valmask
   ENDIF

   varname = varname+' '+ntxt

;  Define the outputs of the function
   field = {name: varname, data: pt_correl, legend: '', units: '', origin: '', dim: 0, direc:'xy'}
   field.origin = var1.origin
   field.dim = var1.dim

   field.legend = ' '+ntxt+' ['+cmdm.date1+'-'+cmdm.spec+']'

   IF debug_w THEN BEGIN
    info = report('leaving...')
   ENDIF

   return, field

END
