;+
;
; Compute the numerator of the skewness expression
;
; @categories
; Statistics
;
; @param X {in}{required}{type=array of floats}
; An array which last dimension is the time dimension so
; size n.
;
; @param NT
;
; @keyword DOUBLE
; If set to a non-zero value, computations are done in
; double precision arithmetic.
;
; @keyword NAN
;
; @returns
; -1 in case of error
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - fplod 20091118T085621Z
;
;   * extract from skewness_4d.pro
;
; 24/2/2000 Sebastien Masson (smasson\@lodyc.jussieu.fr)
;
; Based on the  a_timecorrelate procedure of IDL
; INTRODUCTION TO STATISTICAL TIME SERIES
; Wayne A. Fuller
; ISBN 0-471-28715-6
;
; @version
; $Id: skewness_num.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION skewness_num, x, nt $
         , DOUBLE=double $
         , NAN=nan
;
  compile_opt idl2, strictarrsubs
;
 usage='result=skewness_num(x, nt ' $
         + ', DOUBLE=double ' $
         + ', NAN=nan)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(x,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type x ' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(nt,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type nt' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

  timedim = size(x, /n_dimensions)
  xmean = NAN ? TOTAL(x, timedim, DOUBLE = double, NAN = nan) / TOTAL(FINITE(x), timedim) : $
   TOTAL(x, timedim, DOUBLE = double) / nt
  one = double ? 1.0d : 1.0
  xmean = xmean[*]#replicate(one, nt)
  res = TOTAL( (x-xmean)^3, timedim, DOUBLE = double, NAN = nan)

  RETURN, res

END
