;+
;
; compute alpha=-1/rho (d rho / d T)
;
; @categories
; Calculation
;
; @param T {in}{required}{type=float}
; potential temperature in deg celsius
;
; @param S {in}{required}{type=float}
; salinity in psu
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> s=40.
; IDL> t=40.
; IDL> result=alpha(t,s)
; IDL> print, result
;  0.000402648
;
; @uses
; <propost_it>eos</propost_it>
;
; @todo
;
; @history
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - fplod 20091208T172115Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * remove unused p
;
; @version
; $Id: alpha.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION alpha, t, s
;
  compile_opt idl2, strictarrsubs
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=alpha(t, s)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(t,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type t' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(s,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type s' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

   dt = 0.05
   siga = eos(t, s)-1000.
   sigb = eos(t+dt, s)-1000.

   al = -0.001*(sigb-siga)/dt/(1.+1.e-3*siga)

   return, al

END
