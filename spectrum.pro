;+
;
; From a time serie of length T=tinc*N
; Return power spectrum and array of periods
;
; @param TS {in}{required}{type=array of floats}
;
; @param TINC {in}{required}{type=float}

; @returns
; power spectrum and array of periods
;
; @examples
;
; IDL> ts=findgen(10)
; IDL> tinc=1.
; IDL> result=spectrum(ts, tinc)
; IDL> print, result
;  1.00000e+11      4.50000
;      10.0000      1.61803
;      5.00000     0.850651
;      3.33333     0.618034
;      2.50000     0.525731
;
; @uses
; <proidl>FFT</proidl>
;
; @history
;
; - fplod 20091210T153430Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: spectrum.pro 202 2010-01-25 10:37:45Z pinsard $
;
;-
FUNCTION spectrum, ts, tinc
;
  compile_opt idl2, strictarrsubs
;
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=spectrum(ts, tinc)
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(ts,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type ts' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(tinc,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type tinc' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

  N=n_elements(ts)
  N21 = N/2
  ps=ABS(FFT(ts, -1))
  ps=ps[0:N21-1]

  period=tinc*N/(findgen(N21)+0.0000000000001)
  period[0]=99999999999.

  ret=findgen(2,N21)
  ret[1,*]=ps
  ret[0,*]=period

  return, ret
end
