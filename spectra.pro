;+
;
; Program to extract data, compute anomaly, do time series analysis.
; Computes detrended data using a linear regression and
; produces autocovariances and correlograms.
; Then computes spectrum with and without
; the Tukey window for various values of m.
;
; @param DATIN {in}{required}{type=array of floats}
; 1D input data (monthly time serie)
;
; @param NYEARS {in}{required}{type=integer}
; number of years in data
;
; @param TUKEY {in}{required}{type=integer}
; tukey window in years (20 as a first guess for 100 years)
;
; @param RUNNING {in}{required}{type=integer}
; running window in years for standard deviation variations (15 first guess)
;
; @param FULL {in}{required}{type=integer}
; 1 = no anomaly, 0: anomaly wrt SC
;
; @param BOOTWIN {in}{required}{type=integer}
; bootstrap window for error bar calculation (in months)
;
; @returns
; if called with
; field=spectra(datin, nyears, tukey, running, full, bootwin)
; then field.std is the standard deviation
; field.spectw the normalized spectrum, etc...
;
; structure { std: standard deviation
;                          mean: mean of serie
;                          sc: mean seasonal cycle
;                          sc_range: range of mean SC
;                          sc_std: seasonal cycle STD
;                          anom: anomaly time serie (non-normalized)
;                          spec: spectrum
;                          spectw: normalized spectrum with Tukey window
;                          time: spectra time array
;                          fmax: frequency of maximum power
;                          run_std: running standard deviation
;                          run_sc: running seasonal cycle
;                          anom_stdsc: anomaly time serie using run_sc
;                          time_std: associated time axis
;                          err_std: error bar on std dev + new mean
;                        }
;
; @uses
; <propost_it>stat_error</propost_it>
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - Adapted by Eric Guilyardi from a fortran program by Julia Slingo
; (c) CGAM January 2002
;
; @version
; $Id: spectra.pro 237 2010-06-30 08:56:48Z ericg $
;
;-
FUNCTION spectra, datin, nyears, tukey, running, full, bootwin
;
  compile_opt idl2, strictarrsubs

 usage='result=spectra(datin, nyears, tukey, running, full, bootwin)

 nparam = N_PARAMS()
 IF (nparam LT 6) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(datin,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type datin' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(tukey,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type tukey' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(running,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type running' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(full,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type full' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(bootwin,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type bootwin' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

;
; 0. Initializations
; ------------------

   ntime = nyears*12            ; size of time serie
   lfreq = ntime/2              ; number of wavelenghts
   ntim1=ntime-1

   period = fltarr(lfreq)       ; frequency axis
   amean = fltarr(12)           ; mean seasonal cycle
   ameansc = fltarr(12)           ; mean seasonal cycle (work array)
   datad = fltarr(ntime)        ; anomaly data normalized
   anom = fltarr(ntime)         ; anomaly data (non-normalized)
   stdsc = fltarr(12)           ; Seasonal cycle of STD DEV
   weight = fltarr(ntime+1)
   cx = fltarr(ntim1+1)
   rx = fltarr(ntim1+1)
   powerux = fltarr(lfreq)      ; spectra
   powerwx = fltarr(lfreq)      ; spectra with Tukey window

   run_std = fltarr(ntime-(running*12)+1)              ; time serie of STD DEV
   sc_run = fltarr(ntime-(running*12)+1)              ; time serie of running SC
   anom_stdsc = fltarr(ntime-(running*12)+1)
   time_std = (findgen(ntime-(running*12)+1)+(running*12)/2)/12.

   err_std = fltarr(2)

   pi = 3.141592654
   twopin=2.0*pi/float(ntime)
   pirm=pi/float(ntime)

   period = (float(ntime)/(float(findgen(lfreq))+1.))/12.

;
; 1. Compute monthly means and anomalies
; --------------------------------------
;
; mean of time serie
;
   smean = mean(datin)
; mean seasonal cycle
;
   FOR t = 0, 12-1 DO BEGIN
      amean[t] = mean(datin[long(findgen(ntime/12)*12+t)])
   ENDFOR
;
; range of mean SC
;
   sc_range = max(amean)-min(amean)
;
; interannual anomaly
;
   CASE full OF
      '0': datad = datin - reform(amean#replicate(1, long(ntime/12)), ntime)
      '1': datad = datin
   ENDCASE

   anom = datin - reform(amean#replicate(1, long(ntime/12)), ntime)

   xmean = mean(datad)
;
; standard deviation
;
   sdev = sqrt((moment(datad))[1])

; error bar on std

   temperr = stat_error(anom, bootwin)
   ; err= +- sqrt(sum(temperr(i)-sdev)**2)/N)
   err_std[0] = sqrt( total((temperr-sdev)^2)/n_elements(temperr) )
;   err_std[0] = sqrt((moment(temperr))[1])
   err_std[1] = mean(temperr)

;
; standard deviation and SC in running window
;
   FOR t = 0, ntime-(running*12) DO BEGIN
      run_std[t] = sqrt((moment(datad[t:t+(running*12)-1]))[1])
;      dattmp = datin[t:t+(running*12)-1]
;      ntsc = running*12
;      FOR tsc = 0, 12-1 DO BEGIN
;         ameansc[tsc] = mean(dattmp(long(findgen(ntsc/12)*12+tsc)))
;      ENDFOR
;      sc_run[t] = sqrt((moment(ameansc))[1])
;      sc_run[t] = max(ameansc)-min(ameansc)
;      sc_run[t] = mean(dattmp(long(findgen(running/12)*12+ (t MOD 12))))
   ENDFOR

   ; anomaly time serie wrt time varying SC

;      anom_stdsc = datin - [sc_run[0]#replicate(1, long(running*12/2)), sc_run, sc_run(ntime-(running*12))#replicate(1, long(running*12/2-1))]

;   run_std = smooth(run_std, running*12)
;   run_std[0:(running*12)/2] = 0.
;   run_std[ntime-(running*12)/2, ntime-1] = 0.
;
; standard deviation SC
;
   FOR t = 0, 12-1 DO BEGIN
      stdsc[t] = sqrt((moment(datin[long(findgen(ntime/12)*12+t)]))[1])
   ENDFOR
;
; Normalize data by standard deviation
;
   datad = datad-xmean
   datad = datad/sdev

;
; 2. Spectra
; ----------
;
; Compute autocovariance coefficients
;
   FOR k = 0, ntim1 DO BEGIN
      cx[k] = (total(datad[0:ntime-k-1]*(shift(datad, -k))[0:ntime-k-1]))/float(ntime)
   ENDFOR
;
; Compute autocorrelations for plotting
;
   rx = cx/cx[0]
;
;  Compute the spectrum with no windowing
;
   FOR i=0,lfreq-2 DO BEGIN
      omega=twopin*float(i+1)
      powerux[i] = (cx[0]+total((2.0*shift(cx, -1)*cos(omega*float(findgen(ntim1)+1)))[0:ntim1-1]))/pi
   ENDFOR
;
; Compute the spectrum with windowing
;
   mm=tukey*12
   pirm=pi/float(mm)
;
; compute the weights for the Tukey window
;
   weight=0.5*(1.0+cos(pirm*findgen(mm+1)))
;
; Compute windowed spectrum
;
   FOR i=0,lfreq-2 DO BEGIN
      omega=twopin*float(i+1)
      powerwx[i] = (weight[0]*cx[0]+total((2.0*shift(weight, -1)*shift(cx, -1)*cos(omega*float(findgen(mm)+1)))[0:mm-1]))/pi
   ENDFOR

   pmax = max(powerwx)

;
; Find max amplitude freq
;
   fmax = max(powerwx/pmax)
   index = where (powerwx/pmax EQ fmax)
   fmax = period[index]

;
; 3. Organise output
; ------------------
;

   spectra = {std:sdev, mean:smean, sc:amean, sc_range:sc_range, sc_std:stdsc, anom:anom, spec:powerux, spectw:powerwx/pmax, time:period, fmax:fmax, run_std:run_std, sc_run:sc_run, anom_stdsc:anom_stdsc, time_std:time_std, err_std:err_std}

   return, spectra

END
