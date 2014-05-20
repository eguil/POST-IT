;+
;
; compute correlation 3D (x,y,t) array to domain-averaged field (nino3 sst,...)
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
; $Id: make_correldomain.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-

FUNCTION make_correldomain, file_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ALL_DATA=all_data
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
 usage='result=make_correldomain(file_name, ncdf_db ' $
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

   var_name = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[0]
   domain = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[1]

;  Read the variable in the correspondent netcdf file
   var = nc_read(file_name, var_name, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)


; Read domain variable

   domain_db = hom_idl+'ENSO_works/data/1d_ncdf/'

   file_domain = domain_db+domain+'_'+cmd1_back.exp+'_'+cmd1_back.date1+'_'+cmd1_back.spec+'.nc'
   IF debug_w THEN BEGIN
    print, '     file_domain: ', file_domain
   ENDIF

   var_dom = ncdf_lec(file_domain, var = domain)

   IF debug_w THEN BEGIN
    print, '     size of var_dom: ', size(var_dom)
   ENDIF

   IF (size(var_dom))[1] NE jpt THEN BEGIN
      print, '  *** mistmatch in time dimension :', (size(var_dom))[1], jpt
      print, '  for file :', file_domain
      return, -1
   ENDIF

   varname = var.name+'/'+domain+' corr.'
   varunit = '[-1/1]'

   nxa = (size(var.data))[1]
   nya = (size(var.data))[2]

   IF debug_w THEN BEGIN
    print, '   nxa, nya', nxa, nya
   ENDIF

   correl = fltarr(nxa, nya)
   correl[*, *] = 0

; Sampling of data and computation of new numbers of values

   @mth_decode

   FOR imth = 0, nmth-1 DO BEGIN

      IF debug_w THEN BEGIN
       print, '   month idx/value: ', imth, strd[imth]
      ENDIF

      data = (var.data)[*, *, reform(idxm[imth,*], njpt)]
      var_domc = (var_dom)[reform(idxm[imth,*], njpt)]
      var_domc = var_domc-mean(var_domc)
      nval = njpt

; Compute correlation
      FOR idx = 0, nxa-1 DO FOR idy = 0, nya-1 DO BEGIN

         IF data[idx, idy, 0] NE valmask THEN BEGIN
            x1i = reform(data[idx, idy, *], nval)
            x1 = x1i-mean[x1i]
            zcor = c_timecorrelate[x1, var_domc]
            correl[idx, idy]= correl[idx, idy] + zcor
         ENDIF ELSE BEGIN
            correl[idx, idy] = valmask
         ENDELSE
      ENDFOR
   ENDFOR

   idm = where(correl GE valmask/10.)

   correl = correl/float(nmth)

   IF idm[0] NE -1 THEN BEGIN
    correl[idm] = valmask
   ENDIF

   varname = varname+' '+ntxt

;  Define the outputs of the function
   field = {name: varname, data: correl, legend: '', units: '', origin: '', dim: 0, direc:'xy'}
   field.origin = var.origin
   field.dim = var.dim

   field.legend = ' '+ntxt+' ['+cmdm.date1+'-'+cmdm.spec+']'

   IF debug_w THEN BEGIN
    info = report('leaving...')
   ENDIF

   return, field

END
