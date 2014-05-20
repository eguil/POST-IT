;+
;
; @param X {in}{required}{type=1D array of floats}
; independent variable
;
; @param A {in}{required}{type=array of 3 floats}
; coefficients
;
; @param F {out}{type=float}
; dependent variable
;
; @param PDER {out}{type=float}
; partial derivatives
;
; @examples
;
; IDL> x=fltarr(10)
; IDL> a=[1., 2., 3.]
; IDL> gfunct,x,a,f,pder
; IDL> print,f,pder
;       4.00000      4.00000      4.00000      4.00000      4.00000      4.00000      4.00000      4.00000
;       4.00000      4.00000
;       1.00000      1.00000      1.00000      1.00000      1.00000      1.00000      1.00000      1.00000
;       1.00000      1.00000
;       0.00000      0.00000      0.00000      0.00000      0.00000      0.00000      0.00000      0.00000
;       0.00000      0.00000
;       1.00000      1.00000      1.00000      1.00000      1.00000      1.00000      1.00000      1.00000
;       1.00000      1.00000
;
; @history
;
; - fplod 20091210T092547Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters

; @version
; $Id: gfunct.pro 186 2009-12-10 16:31:06Z pinsard $
;
;-
PRO gfunct $
    , x $
    , a $
    , f $
    , pder
;
  compile_opt idl2, strictarrsubs
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='gfunct,x,a,f,pder'
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(x,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type x' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 arg_dim=size(x,/n_dimensions)
 IF (arg_dim LT 1) THEN BEGIN
   ras = report(['Incorrect arg dimension x' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(a,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type a' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 arg_dim=size(a,/n_elements)
 IF (arg_dim NE 3) THEN BEGIN
   ras = report(['Incorrect arg dimension a' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF


F = A[0] * EXP(A[1] * X) + A[2]
pder = [[EXP(A[1] * X)], [A[0] * X * EXP(A[1] * X)], [replicate(1.0, N_ELEMENTS(X))]]

END
