;+
;
; Program to estimate statistical error of either one time series (std)
; or significance of std dev difference between two time series
;
; Errors are estimated using a moving block bootstrap (of length
; 'window') to account for serial correlations
;
; @param DATA {in}{required}
; 1d array
;
; @param WINDOW {in}{required}{type=integer}
; window size
;
; @keyword DATA2
;
; @keyword NITER {in}{type=integer}
; number of iteration (999 by default to have 1% precision)
;
; @keyword BAVARD
;
; @keyword MEAN
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> data=findgen(10)
; IDL> window=1
; IDL> result=stat_error(data, window)
; IDL> help,result
; IDL> print,result
;
; @history
; - fplod 20091210T154941Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;   * syntax of array
;
; - (c) 2005, E. Guilyardi - thanks to Pascal Terray for theory
;
; @version
; $Id: stat_error.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION stat_error, data, window $
         , DATA2=data2 $
         , NITER=niter $
         , BAVARD=bavard $
         , MEAN=mean
;
  compile_opt idl2, strictarrsubs
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result= stat_error(data, window ' $
         + ', DATA2=data2 ' $
         + ', NITER=niter ' $
         + ', BAVARD=bavard' $
         + ', MEAN=mean)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
;
 arg_type = size(data,/type)
 IF (arg_type NE 4) AND (arg_type NE 5) THEN BEGIN
   ras = report(['Incorrect arg type data' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(window,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type window' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF


; 0. Inits
; ---------

   niter1 = 999
   IF keyword_set(niter) THEN BEGIN
    niter1 = niter
   ENDIF

   errstats = fltarr(niter1)

; define array: either [data] or [data, data2]


   IF keyword_set(data2) THEN BEGIN
      ; 2 arrays
      array = [data, data2]
      nblocks = floor(n_elements(data2)/window)
      IF n_elements(data2) NE nblocks*window THEN BEGIN
         print, ' ERROR: window must divide data2 size: ', n_elements(data2), window
         return, -1
      ENDIF
      sizetest = n_elements(data2)
   ENDIF ELSE BEGIN
      ; 1 array
      array = data
      nblocks = floor(n_elements(data)/window)
;      IF n_elements(data) NE nblocks*window THEN BEGIN
;         print, ' ERROR: window must divide data size: ', n_elements(data), window
;         return, -1
;      ENDIF
      sizetest = n_elements(data)
   ENDELSE

   size = n_elements(array)
   nblockst = floor(size/window)

   IF keyword_set(bavard) THEN BEGIN

      print, '        Error stats on array size/min/max: ', size, min(array), max(array)
      print, '        using moving window of ', window, ' with N iter =', niter1
      print, '        total number of blocks', nblockst
      print, '        number of blocks and size of test array', nblocks, sizetest

   ENDIF

   it = 0
   seed = 0

   idx0 = findgen(window)


   WHILE it LE niter1-1 DO BEGIN
;
; 1. Randomly define blocks
; -------------------------
      ib = 0
      rand = fix(nblockst*randomu(seed, nblocks))

      testarr = array[(rand[0]*window)+idx0]

      WHILE ib LE nblocks - 2 DO BEGIN

         testarr = [testarr, array[(rand[ib]*window)+idx0]]
         ib = ib + 1
      ENDWHILE
;
; 2. Compute stats
; ----------------
      IF keyword_set(mean) THEN BEGIN
         errstats[it] = mean(testarr)
      ENDIF ELSE BEGIN
;         errstats[it] = sqrt((moment(testarr))[1])
         errstats[it] = sqrt(total(testarr^2)/n_elements(testarr))
      ENDELSE
; method 1 keep mean i.e = sqrt(sum(testarr(i)**2)/N)
; method 2      errstats[it] = sqrt((moment(testarr))[1])

      it = it + 1
   ENDWHILE

;
; 3. Organise output
; ------------------
;

   return, errstats

END
