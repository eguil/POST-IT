;+
;
; potential density (p=0)
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
; potential density (p=0)
; -1 in case of error

; @examples
;
; IDL> s=40
; IDL> t=40
; IDL> rho=eos(t,s)
; IDL> print, rho
;      1021.68
;
; @todo
; why rho in example does not agree to <propost_it>make_eos</propost_it>
; reference ?
;
; double emploi avec <pro>rhon</pro> ?
;
; @history
;
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - fplod 20091208T112404Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * add example
;
; @version
; $Id: eos.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION eos, t, s
;
  compile_opt idl2, strictarrsubs
;
 usage='result=eos(t, s)'

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

   sr=sqrt(abs(s))
   r1=((((6.536332E-9*t-1.120083E-6)*t+1.001685E-4)*t $
        -9.095290E-3)*t+6.793952E-2)*t+999.842594
   r2=(((5.3875E-9*t-8.2467E-7)*t+7.6438E-5)*t-4.0899E-3)*t+8.24493E-1
   r3=(-1.6546E-6*t+1.0227E-4)*t-5.72466E-3
   rhopn = ( ( 4.8314E-4*s + r3*sr +r2)*s +r1)

   return, rhopn
END
