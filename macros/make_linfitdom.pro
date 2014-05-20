;+
;
; compute slope of linear regression of one 3D (x,y,t) field with
; a 1D time serie
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
; $Id: make_linfitdom.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_linfitdom, file_name, ncdf_db $
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

 usage='result=make_linfitdom(file_name, ncdf_db ' $
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

   var_name1 = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[0]
   domain = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[1]

;  Read the variables in the correspondant netcdf file
   var1 = nc_read(file_name, var_name1, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
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

   varname = var1.name+'/'+domain+' linfit.'
   varunit = '[-1/1]'

   nxa = (size(var1.data))[1]
   nya = (size(var1.data))[2]

   IF debug_w THEN BEGIN
    print, '   nxa, nya', nxa, nya
   ENDIF

   pt_linfit = fltarr(nxa, nya)
   pt_err = fltarr(nxa, nya)
   pt_corr = fltarr(nxa, nya)
   pt_linfit[*, *] = 0
   pt_err[*, *] = 1.1
   pt_corr[*, *] = 0.

; Sampling of data and computation of new numbers of values
   @mth_decode

   FOR imth = 0, nmth-1 DO BEGIN

      IF debug_w THEN BEGIN
       print, '   month idx/value: ', imth, strd[imth]
      ENDIF

      data1 = (var1.data)[*, *, reform(idxm[imth,*], njpt)]
      var_domc = (var_dom)[reform(idxm[imth,*], njpt)]
      var_domc = var_domc-mean(var_domc)
      nval = njpt

; Compute linear regression

      FOR idx = 0, nxa-1 DO FOR idy = 0, nya-1 DO BEGIN

         IF data1[idx, idy, 0] NE valmask THEN BEGIN

            x1i = reform(data1[idx, idy, *], nval)
            x1 = x1i-mean[x1i]

            IF linfit_map NE '' THEN BEGIN
            ; compute regression for value above (p - default) or below (m) linfit_sep
               CASE linfit_map OF
                  'm': idt = where (var_domc LE linfit_sep)
                  ELSE: idt = where (var_domc GE linfit_sep)
               ENDCASE
               x1 = x1[idt]
               var_domc = var_domc[idt]
            ENDIF

            coeffl = linfit(var_domc, x1, CHISQ = linerrl, PROB = proberrl, SIGMA = sigmaerrl)
            correl = c_timecorrelate[var_domc,x1]
            pt_linfit[idx, idy] = pt_linfit[idx, idy]+coeffl[1]
            pt_err[idx, idy] = min[pt_err[idx, idy], proberrl]
            pt_corr[idx, idy] = pt_corr[idx, idy] + correl

            IF debug_w THEN BEGIN
               IF idx EQ 30 AND idy EQ 15 THEN BEGIN
                  print, '  idx, idy, linfit_map ', idx, idy, linfit_map
                  print, '  size(idt)',size(idt)
                  print, '  pt_linfit, correl = ',coeffl[1], correl
                  return, -1
               ENDIF
            ENDIF

         ENDIF ELSE BEGIN
            pt_linfit[idx, idy] = valmask
            pt_err[idx, idy] = 0.
            pt_corr[idx, idy] = 0.
         ENDELSE

      ENDFOR
   ENDFOR

  ; make mean of period

   idm = where(pt_linfit GE valmask/10.)
   pt_linfit = pt_linfit/float(nmth)
   pt_corr = pt_corr/float(nmth)
   IF idm[0] NE -1 THEN BEGIN
    pt_linfit[idm] = valmask
   ENDIF

  ; only take points where ABS(correlation) larger than a value

   idc = where(abs(pt_corr) LT corr_thres)
   pt_linfit[idc] = !values.f_nan
   corr_txt = '[r>'+strtrim(string(corr_thres, format = '(f5.2)'), 2)+']'

   varname = varname+' '+ntxt

   print, '    Linfitdom min/max pt_corr ', min(pt_corr), max(pt_corr)

;  Define the outputs of the function
   field = {name: varname, data: pt_linfit, legend: '', units: '', origin: '', dim: 0, direc:'xy'}
   field.origin = var1.origin
   field.dim = var1.dim

   CASE linfit_map OF
      'p': BEGIN
         print, '    Linear fit computed for anomalies ABOVE ',linfit_sep
         lintxt = ' > '+ strtrim(string(linfit_sep), 2)
      END
      'm': BEGIN
         print, '    Linear fit computed for anomalies BELOW ',linfit_sep
         lintxt = ' < '+ strtrim(string(linfit_sep), 2)
      END
      ELSE: lintxt = ''
   ENDCASE

   field.legend = lintxt+' for '+ntxt+' in ['+cmdm.date1+'-'+cmdm.spec+'] '+corr_txt+' -'

; additional computations (pac_5 nino_3 nino_4 averages) *** requires whole domain ****

   IF nxt EQ jpi AND nyt EQ jpj THEN BEGIN

      n3_linfit = moyenne(pt_linfit,'xy', boite = [210, 270, -5, 5], NAN = valmask)
      n4_linfit = moyenne(pt_linfit,'xy', boite = [160, 210, -5, 5], NAN = valmask)
      zl_linfit = moyenne(pt_linfit,'xy', boite = [130, 280, -5, 5], NAN = valmask)

      print, '    Nino 3 average of slope of linear fit = ', n3_linfit
      print, '    Nino 4 average of slope of linear fit = ', n4_linfit
      print, '    Zonal 5N/5S average of slope of linear fit = ', zl_linfit

   ENDIF

 ;  jpt=nval
 ;  meants=grossemoyenne(data2, 't')
 ;  meantsp=reform(meants,nxt*nyt)
 ;  meanlf=reform(pt_linfit,nxt*nyt)
 ;  IF min (meantsp) GE 100 THEN BEGIN
 ;   meantsp=meantsp-273.15
 ;  ENDIF
 ;  pltsc, meanlf,meantsp,-70,30,20,34,"SST "+mth[strd-1], window=2
 ;  pltsc, meanlf,meantsp,-70,30,20,34,"SST "+mth[strd-1], window=2, /noerase, /ov1d, col1d=2

   IF debug_w THEN BEGIN
    info = report('leaving...')
   ENDIF

   return, field

END
