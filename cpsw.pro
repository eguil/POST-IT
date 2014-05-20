;+
;
; Specific heat of sea water (J/KG C)
;
; @categories
; Calculation
;
;
; @param T {in}{required}{type=float}
; potential temperature in deg celsius
;
; @param S {in}{required}{type=float}
; salinity in psu
;
; @param P {in}{required}{type=float}
; pressure
;
; @returns
; Specific heat of sea water (J/KG C)
;
; -1 in case of error
;
; @examples
;
; IDL> s=40.
; IDL> t=40.
; IDL> p=500.
; IDL> result=cpsw(t,s,p)
; IDL> print, result
;      3904.01
;
; @todo
;
; @history
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: cpsw.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION cpsw, t, s, p
;
  compile_opt idl2, strictarrsubs
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=cpsw(t, s, p)'
;
 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
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
 arg_type = size(p,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type p' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

      CP1 = 0.
      CP2 = 0.
      SR=SQRT(ABS(S))
; SPECIFIC HEAT CP0 FOR P=0 (MILLERO ET AL. 1973)
      A = (-1.38E-3*T+0.10727)*T-7.644
      B = (5.35E-5*T-4.08E-3)*T+0.177
      C = (((2.093236E-5*T-2.654387E-3)*T+0.1412855)*T-3.720283)*T+4217.4
      CP0 = (B*SR + A) * S + C
; CP1 PRESSURE AND TEMPERATURE TERMS FOR S = 0
      A = (((1.7168E-8*T+2.0357E-6)*T-3.13885E-4)*T+1.45747E-2)*T-0.49592
      B = (((2.2956E-11*T-4.0027E-9)*T+2.87533E-7)*T-1.08645E-5)*T+2.4931E-4
      C = ((6.136E-13*T-6.5637E-11)*T+2.6380E-9)*T-5.422E-8
      CP1 = ((C*P+B)*P+A)*P
; CP2 PRESSURE AND TEMPERATURE TERMS FOR S > 0
      A = (((-2.9179E-10*T+2.5941E-8)*T+9.802E-7)*T-1.28315E-4)*T+4.9247E-3
      B = (3.122E-8*T-1.517E-6)*T-1.2331E-4
      A = (A+B*SR)*S
      B = ((1.8448E-11*T-2.3905E-9)*T+1.17054E-7)*T-2.9558E-6
      B = (B+9.971E-8*SR)*S
      C = (3.513E-13*T-1.7682E-11)*T+5.540E-10
      C = (C-1.4300E-12*T*SR)*S
      CP2 = ((C*P+B)*P+A)*P
; SPECIFIC HEAT RETURN
      cp = CP0 + CP1 + CP2
   return, cp
END
