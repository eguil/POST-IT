;+
;
; @param FLD {in}{required}{type=array of floats}
; field to work on
;
; @param TREND_INT {in}{required}{type=integer}
; 1,2,3,4,...,9
;
; @param TYPE {in}{required}{type=string}
; 't', 'xt', 'yt', 'zt' or 'xyt'
;
; @keyword SILENT
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> fld=findgen(10)
; IDL> trend_int=1
; IDL> type='t'
; IDL> field_int=1
; IDL> c_normal=1
; IDL> boot_win = [24, 36, 48]
; IDL> result=trends(fld, trend_int, type)
;% Compiled module: MOMENT.
;     Normalizing 1D time series with stddev =       3.02765
;     -> compute 1D data field time integral / lenght = 0
;   Vargrid : T
;   Leaving trends.pro
;
; IDL> print, result
;      0.00000     0.330289     0.990867      1.98173      3.30289      4.95434      6.93607      9.24810
;      11.8904      14.8630
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; FLD is also a structure elsewhere
;
; if SILENT is opposite to BAVARD, keep only one
;
; difference between silent and debug_w ?
;
; change argument name "type" because no explicit enougth. may be plt_type ?
;
; explication sur field_int, c_normal, boot_win
;
; boot_win is only use if ...
;
; @history
;
; @history
; - fplod 20100114T153814Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * fix array syntax on fld and boot_win
;
; - fplod 20091210T160756Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: trends.pro 235 2010-06-25 08:46:06Z ericg $
;
;-
FUNCTION trends, fld, trend_int, type $
         , SILENT=silent
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 IF debug_w THEN BEGIN
  info = report('enter ...')
 ENDIF

 usage='result=trends(fld, trend_int, type, SILENT=silent)'
;
 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(fld,/type)
 IF (arg_type NE 4) AND (arg_type NE 5) THEN BEGIN
   ras = report(['Incorrect arg type fld' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(trend_int,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3) AND (arg_type NE 7)) THEN BEGIN
   ras = report(['Incorrect arg type trend_int' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(type,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type type' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type=size(boot_win,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type boot_win' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF
 common_nelem=size(boot_win,/n_elements)
 IF (common_nelem NE 3) THEN BEGIN
   ras = report(['Incorrect common n_elements boot_win' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF


   trend_type = strtrim(string(trend_int), 2)

; select subset of time serie according to [sel1,sel2,freq]
; =========================================================

   month1 = 11
   month2 = 12
   sample = 0
   field_intl = field_int

   CASE trend_type OF
      '7': timeunit = 30/6
      '8': timeunit = 30
      '71': BEGIN
         trend_type = '4'+strtrim(string(6*(month2-month1+1)), 2)
         timeunit = 30/6
      END
      '81': BEGIN
         trend_type = '4'+strtrim(string(month2-month1+1), 2)
         timeunit = 30
      END
      ELSE: timeunit = 1
   ENDCASE

   delt = 30/timeunit
   sel1 = (month1-1)*delt
   sel2 = (month2)*delt-1
   freq = 360/timeunit

; build indexes to keep in [0,N-1] = serie of [sel1,sel2] mod(freq)

   IF (size(fld))[0] EQ 1 AND sample EQ 1 THEN BEGIN
     ; 1D time serie
     IF NOT keyword_set(silent) THEN BEGIN
      print, '     Extract months : ', month1, month2
     ENDIF
      Ndim = (size(fld))[1]
      idx0 = floor(findgen(sel2-sel1+1)+sel1)
      ndelt = n_elements(idx0)
      idx = idx0
      ninter = Ndim/freq
      FOR t = 1, ninter-1 DO BEGIN
         idx0 = idx0+freq
         idx = [idx, idx0]
      ENDFOR

      ; new array

      fld = fld[idx]
      jpt = n_elements(fld)

      ; change time accordingly

      time = time[idx[0]]+(findgen(jpt)-1)*360/ndelt

   ENDIF

; normalise 1d time series if c_normal=1

   IF c_normal EQ 1 AND (size(fld))[0] EQ 1 THEN BEGIN
      IF NOT keyword_set(silent) THEN BEGIN
       print, '     Normalizing 1D time series with stddev = ',sqrt((moment(fld))[1])
      ENDIF
      fld = fld/sqrt((moment(fld))[1])
   ENDIF

; modify time serie according to trend_typ
; ========================================

   CASE strmid(trend_type, 0, 1) OF
      '1': BEGIN  ; trend = remove first value
;                   ---------------------------
         IF (size(fld))[0] EQ 1 THEN BEGIN
            ; 1D time serie
            fld = fld-fld[0]
         ENDIF ELSE BEGIN
            ; hovmoeller
            fldrem = fld[*, 0]#replicate(1, (size(fld))[2])
            fld = fld-fldrem
         ENDELSE
      END
      '2': BEGIN  ; drift = remove previous value
;                   ------------------------------
         IF (size(fld))[0] EQ 1 THEN BEGIN
            ; 1D time serie
            fldrem = shift(fld, 1)
            fldrem[0]= fld[0]
            fld = fld-fldrem
         ENDIF ELSE BEGIN
            ; hovmoeller
            fldrem = shift(fld, 0, 1)
            fldrem[*, 0]= fld[*, 0]
            fld = fld-fldrem
         ENDELSE
      END
      '3': BEGIN  ; inverse trend = remove mean of <n> last values
;                   ----------------------------------------------
         IF strlen(trend_type) GT 1 THEN BEGIN
          mean_n = long(strmid(trend_type, 1, strlen(trend_type)-1))
         ENDIF ELSE BEGIN
          mean_n = 1
         ENDELSE
         IF (size(fld))[0] EQ 1 THEN BEGIN
            ; 1D time serie
            fld = mean(fld[(size(fld))[1]-mean_n:(size(fld))[1]-1])-fld
         ENDIF ELSE BEGIN
            ; hovmoeller
            fldrem = reform(fld[*, (size(fld))[2]-mean_n:(size(fld))[2]-1],(size(fld))[1],mean_n)
            fldrem = total(fldrem, 2)/float(mean_n)#replicate(1, (size(fld))[2])
            idx = where(fld EQ valmask)
            fld = fldrem-fld
            IF idx[0] NE -1 THEN BEGIN
             fld[idx] = valmask
            ENDIF
         ENDELSE
      END
      '4': BEGIN  ; anomaly = remove average running mean
;                   -------------------------------------
         IF strlen(trend_type) GT 1 THEN BEGIN
          running = long(strmid(trend_type, 1, strlen(trend_type)-1))
         ENDIF ELSE BEGIN
          running = 1
         ENDELSE

         ; check that jpt is a multiple of running

         IF (jpt MOD running) NE 0 THEN BEGIN
            print, ' *** ERROR: jpt (number of dates) is not a multiple of <n> in @t4<n>', jpt, running, jpt MOD running
            return, -1
         ENDIF

         IF (size(fld))[0] EQ 1 THEN BEGIN
            ; 1D time serie
            ; build array to remove from time serie
            IF running GE 2 THEN BEGIN
               lenght = (size(fld))[1]
               fldrem = fltarr(running)
               FOR t = 0, running-1 DO BEGIN
                  fldrem[t] = mean(fld[long(findgen(lenght/running)*running+t)])
               ENDFOR
               IF fld_flag EQ 1 THEN BEGIN
                  fldrem_t1 = fldrem
               ENDIF
               IF fld_flag EQ 2 THEN BEGIN
                fldrem_t2 = fldrem
               ENDIF
               sc_ampl = sqrt((moment(fldrem))[1])
               sc_ampl2 = max(fldrem)-min(fldrem)
               fldrem = reform(fldrem#replicate(1, long(lenght/running)), lenght)

               ; Compute mean error-bar using running bootstrap window (width = 10 months)
               temperr = stat_error(fld, 10L, /mean, niter = 999)
               err_std = sqrt( total( (temperr-mean(fld))^2 )/n_elements(temperr) )

               IF NOT keyword_set(silent) THEN BEGIN
                  print, '     1D times-serie info         : mean +- err ', (moment(fld))[0], err_std
                  print, '       Mean seasonal cycle amplitude stdev / max-min', sc_ampl, sc_ampl2
               ENDIF

               ; Compute std error-bar using running bootstrap window (width in plt_def)
               IF jpt GT boot_win[0] THEN BEGIN
                  temperr = stat_error(fld, boot_win[0])
                  err_std_1 = sqrt( total((temperr-sqrt((moment(fld - fldrem))[1]) )^2)/n_elements(temperr) )
               ENDIF ELSE BEGIN
                  err_std_1 = 0
               ENDELSE
               IF jpt GT boot_win[1] THEN BEGIN
                  temperr = stat_error(fld, boot_win[1])
                  err_std_2 = sqrt( total((temperr-sqrt((moment(fld - fldrem))[1]) )^2)/n_elements(temperr) )
               ENDIF ELSE BEGIN
                  err_std_2 = 0
               ENDELSE
               IF jpt GT boot_win[2] THEN BEGIN
                  temperr = stat_error(fld, boot_win[2])
                  err_std_3 = sqrt( total((temperr-sqrt((moment(fld - fldrem))[1]) )^2)/n_elements(temperr) )
               ENDIF ELSE BEGIN
                  err_std_3 = 0
               ENDELSE

               IF NOT keyword_set(silent) THEN BEGIN
                print, '     1D times-serie anomaly info : var/stddev +- error (24,36,48)', (moment(fld - fldrem))[1], sqrt((moment(fld - fldrem))[1]), err_std_1, err_std_2, err_std_3
               ENDIF
               stddev_txt = strtrim(string(sqrt((moment(fld - fldrem))[1])), 2)
               IF NOT keyword_set(silent) THEN BEGIN
                print, ' '
               ENDIF
            ENDIF ELSE BEGIN
               fldrem = mean(fld)
            ENDELSE
            idx = where(fld EQ valmask)
            fld = fld - fldrem
            ; keep mean seasonal cycle in common
            mean_sc = fldrem

            IF idx[0] NE -1 THEN BEGIN
               fld[idx] = valmask
            ENDIF

; compute time lag correlation
            IF ioverchk EQ 2 AND c_correl EQ 1 THEN BEGIN
               IF (size(fld))[1] EQ (size(fld_prev_t))[1] THEN BEGIN
                  lag_array = findgen(2*lag_correl+1)-lag_correl
                  lag_correlation = C_CORRELATE(fld, fld_prev_t, lag_array)
                  print, '     1D time series lag correlation = ', lag_correlation
                  indx = where (lag_correlation EQ max(lag_correlation))
                  IF indx[0] NE -1 THEN BEGIN
                     print, '        Lag of max correlation =', lag_array[indx],lag_correlation[indx]
                     print, '        Lag correlation limit [-N,N] =', lag_correl
                  ENDIF
               ENDIF
            ENDIF

;
; apply running mean stdev
;
            IF run_stddev GT 0 THEN BEGIN
               print, '     Compute running std dev, window = ',run_stddev
               print, ' '
               full = 0
               nyears = lenght/running
               bootfoo = run_stddev/running
               stat = spectra(fld, nyears, tukey, run_stddev/running, full, bootfoo)
               stdarr = fld
               stdarr[*] = !VALUES.F_NAN
               size_data = (size(stat.run_std))[1]
               stdarr[run_stddev/2:(run_stddev/2)+size_data-1]= stat.run_std
               fld = stdarr
            ENDIF

            fld_prev_t = fld

         ENDIF ELSE BEGIN
            ; hovmoeller (2D and 3D)
            ; build array to remove from time serie
            IF NOT keyword_set(silent) THEN BEGIN
             print, '       [Anomaly = remove average running mean (hovmoeller)]'
            ENDIF
            CASE type OF
               'xt': BEGIN
                  nx = (size(fld))[1]
                  lenght = (size(fld))[2]
                  fldrem = reform(fld, nx, running, lenght/running)
                  fldrem = total(fldrem, 3)/(lenght/running)
                  fldrem = fldrem[*]
                  fldrem = reform(fldrem#replicate(1, long(lenght/running)),nx, lenght)
               END
               'yt': BEGIN
                  ny = (size(fld))[1]
                  lenght = (size(fld))[2]
                  fldrem = reform(fld, ny, running, lenght/running)
                  fldrem = total(fldrem, 3)/(lenght/running)
                  fldrem = fldrem[*]
                  fldrem = reform(fldrem#replicate(1, long(lenght/running)),ny, lenght)
               END
               'zt': BEGIN
                  nz = (size(fld))[1]
                  lenght = (size(fld))[2]
                  fldrem = reform(fld, nz, running, lenght/running)
                  fldrem = total(fldrem, 3)/(lenght/running)
                  fldrem = fldrem[*]
                  fldrem = reform(fldrem#replicate(1, long(lenght/running)),nz, lenght)
               END
               'xyt': BEGIN
                  nx = (size(fld))[1]
                  ny = (size(fld))[2]
                  lenght = (size(fld))[3]
                  fldrem = reform(fld, nx*ny, running, lenght/running)
                  fldrem = total(fldrem, 3)/(lenght/running)
                  fldrem = fldrem[*]
                  fldrem = reform(fldrem#replicate(1, long(lenght/running)),nx*ny, lenght)
                  fldrem = reform(fldrem, nx, ny, lenght)
               END
               ELSE: BEGIN
                  print, 'The type '+type+' is not handled in trends to build the seasonal cycle fldrem. Please change your cmd.plt'
                  return, -1
                     END
            ENDCASE

             idx = where(fld EQ valmask)
             fld = fld - fldrem
             ; keep mean seasonal cycle in common
             mean_sc = fldrem
             IF idx[0] NE -1 THEN BEGIN
              fld[idx] = valmask
             ENDIF
         ENDELSE

      END
      '5': BEGIN  ; Apply digital filter
;                   --------------------
         IF strlen(trend_type) EQ 1 THEN BEGIN
            print, '   *** specify filter width as <time_int>_<lower>_<upper>'
            return, -1
         ENDIF
         IF (size(fld))[0] EQ 1 THEN BEGIN ; 1D time serie
            print, '     1D times-serie info         : mean ', (moment(fld))[0]
            print, '     1D times-serie anomaly info : var/stddev ', (moment(fld))[1], sqrt((moment(fld))[1])
            print, ' '
            print, '   *** filter on 1D data not ready'

         ENDIF ELSE BEGIN
            print, '   *** filter on 2D data not ready'
            return, -1
         ENDELSE

;   filter = digital_filter(0.15, 0.66666, 50, 15)
;                           fraction of niquisk freq (upper, lower
;                           limit indays))
;   output_1d = convol(input_1d, filter)
      END
      '6': field_intl = 1; time integral done after
      ELSE:
   ENDCASE

; compute field time integral if required
   IF n_elements(field_int) EQ 0 THEN BEGIN
    field_int = 0
   ENDIF
   IF field_intl EQ 1 THEN BEGIN
      ; running integral ?
      IF strlen(trend_type) GT 1 THEN BEGIN
       int_win = long(strmid(trend_type, 1, strlen(trend_type)-1))
      ENDIF ELSE BEGIN
       int_win = 0
      ENDELSE
      CASE (size(fld))[0] OF
         1: BEGIN
            print, '     -> compute 1D data field time integral / lenght = ', strtrim(string(int_win), 2)
            fldint = fld
            lenght = (size(fld))[1]
            IF int_win EQ 0 THEN BEGIN
               ; integral from start
               FOR t = 0, lenght-1 DO BEGIN
                  fldint[t] = total(fld[0:t])
               ENDFOR
            ENDIF ELSE BEGIN
               ; running integral
               FOR t = 0, int_win-1 DO BEGIN
                  fldint[t] = total(fld[0:t])
               ENDFOR
               FOR t = int_win,lenght-1 DO BEGIN
                  fldint[t] = total(fld[t-int_win+1:t])
               ENDFOR
            ENDELSE
            fld = fldint
         END
         2: BEGIN
            print, '     -> compute 2D data field time integral / lenght = ', strtrim(string(int_win), 2)
            CASE type OF
               'yt': BEGIN
                  ny = (size(fld))[1]
                  lenght = (size(fld))[2]
                  FOR i = 0, ny-1 DO BEGIN
                     zwrk = fld[i, *]
                     zint = zwrk
                     IF int_win EQ 0 THEN BEGIN
                        ; integral from start
                        FOR t = 0, lenght-1 DO BEGIN
                           zint[t] = total(zwrk[0:t])
                        ENDFOR
                     ENDIF ELSE BEGIN
                        ; running integral
                        FOR t = 0, int_win-1 DO BEGIN
                           zint[t] = total(zwrk[0:t])
                        ENDFOR
                        FOR t = int_win,lenght-1 DO BEGIN
                           zint[t] = total(zwrk[t-int_win+1:t])
                        ENDFOR
                     ENDELSE
                     fld[i, *] = zint
                  ENDFOR
               END
               'xt': BEGIN
                  nx = (size(fld))[1]
                  lenght = (size(fld))[2]
                  FOR i = 0, nx-1 DO BEGIN
                     zwrk = fld[i, *]
                     zint = zwrk
                     IF int_win EQ 0 THEN BEGIN
                        ; integral from start
                        FOR t = 0, lenght-1 DO BEGIN
                           zint[t] = total(zwrk[0:t])
                        ENDFOR
                     ENDIF ELSE BEGIN
                        ; running integral
                        FOR t = 0, int_win-1 DO BEGIN
                           zint[t] = total(zwrk[0:t])
                        ENDFOR
                        FOR t = int_win,lenght-1 DO BEGIN
                           zint[t] = total(zwrk[t-int_win+1:t])
                        ENDFOR
                     ENDELSE
                     fld[i, *] = zint
                  ENDFOR
               END
               ELSE: BEGIN
                  print, '     -> 2D data field type ', type, ' not ready'
               END
            ENDCASE
         END
      ENDCASE
   ENDIF

; if write_data then write to ascii/cdf file

   IF n_elements(write_data) EQ 0 THEN BEGIN
    write_data = 0
   ENDIF
   IF write_data NE 0 THEN BEGIN
      IF write_data GE 1 THEN BEGIN
         ; ascii
         scale = 1.
         get_lun, nuldat
         IF field.origin EQ 'div' THEN BEGIN
          name_exp = strjoin(strsplit(cmd_wrk.exp, '/', /EXTRACT), '-', /SINGLE)
         ENDIF ELSE BEGIN
          name_exp = cmd_wrk.exp
         ENDELSE
         filename = name_exp+'_'+ cmd_wrk.var+'_'+cmd_wrk.date1+'_'+cmd_wrk.spec+'_'+cmd_wrk.plt+'.asc'
         openw, nuldat, asciidir+filename
         print, '     -> writing 1D data to ', asciidir+filename
         print, ' '
         printf, nuldat, fld*scale, format = '(f10.4)'
         free_lun, nuldat
         close, nuldat
      ENDIF ELSE BEGIN
         ; netCDF (tcdf case)
         flddesc = field
         flddesc.data = fld
         cdf_description = nc_build(cmd_wrk, flddesc, type, vargrid)
         fldcdf = {data:fld, units:field.units, short_name:field.legend+'_'+legbox, long_name:field.legend+' ('+legbox+')', missing_value:valmask, direc:type}
         file_out = cmd_wrk.var+'_'+strtrim(string(FORMAT = '(I2.2)', ABS(write_data)), 2)+'.nc'
         pltcmd ='nc_put, fldcdf, file_out, ncdf_db = hom_idl'+cdf_description
         printf, nulhis, strcompress(pltcmd)
         res = execute(pltcmd)
      ENDELSE
   ENDIF

 IF debug_w THEN BEGIN
  print, '   Vargrid : ', vargrid
  info = report('leaving ...')
 ENDIF

 return, fld

END
