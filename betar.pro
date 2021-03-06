;+
;
; compute beta= 1/rho (d rho / d S)
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
; IDL> result=betar(t,s)
; IDL> print, result
;  0.000728829
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
;   * remove unused p
;
; @version
; $Id: betar.pro 182 2009-12-09 15:50:58Z pinsard $
;
;-
FUNCTION betar, t, s
;
  compile_opt idl2, strictarrsubs
;
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=betar(t, s)'
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

   ds = 0.01
   siga = eos(t, s)-1000.
   sigb = eos(t, s+ds)-1000.

   be = 0.001*(sigb-siga)/ds/(1.+1.e-3*siga)

   return, be

END
