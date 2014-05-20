;+
;
; Compute the skewness from 1d to 4d vectors
;
; Same function as SKEWNESS but accept array (until 4
; dimension) for input and perform  the skewness
; along the time dimension which must be the last
; one of the input array.
;
; @categories
; Statistics
;
; @param X {in}{required}{type=array of floats}
; An array which last dimension is the time dimension so
; size n.
;
; @keyword DOUBLE
; If set to a non-zero value, computations are done in
; double precision arithmetic.
;
; @keyword NVAL
;
; @returns
; -1 in case of error
;
; @uses
; <pro>common</pro>
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - fplod 20091118T085847Z
;
;   * externalize skewness_num
;
; 24/2/2000 Sebastien Masson (smasson\@lodyc.jussieu.fr)
;
; Based on the  a_timecorrelate procedure of IDL
; INTRODUCTION TO STATISTICAL TIME SERIES
; Wayne A. Fuller
; ISBN 0-471-28715-6
;
;
; @version
; $Id: skewness_4d.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION skewness_4d, x $
         , DOUBLE = double $
         , NVAL = nval
;
   compile_opt idl2, strictarrsubs
;
@common
;
   ON_ERROR, 2

 usage='result=skewness_4d( x '$
         + ', DOUBLE = double ' $
         + ', NVAL = nval)'

 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
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

   xdim = SIZE(x, /dimensions)
   xndim = SIZE(x, /n_dimensions)
   nt = xdim[xndim-1]

; Keyword NAN activated if needed
; Keyword NVAL not compulsory.

   NAN = ( (WHERE(FINITE(x) EQ 0 ))[0] NE -1 ) ? 1 : 0
;We can retrieve the matrix of real lenghts of time-series
   ntreal = ( (WHERE(FINITE(x) EQ 0 ))[0] NE -1 ) ?  TOTAL(FINITE(x), xndim) : nt

   IF ARG_PRESENT(NVAL) THEN BEGIN
    nval = ntreal
   ENDIF

; Check length.
   IF (WHERE(ntreal LE 1))[0] NE -1 THEN BEGIN
    MESSAGE, "Matrix of length of time-series must contain 2 or more elements"
   ENDIF

; If the DOUBLE keyword is not set then the internal precision and
; result are identical to the type of input.
   type = SIZE(x, /TYPE)
   useDouble = (N_ELEMENTS(Double) eq 1) ? KEYWORD_SET(Double) : $
    (type eq 5)

; Type of outputs according to the type of data input
   case xndim of
      1: Skew = useDouble ? DBLARR(1) : FLTARR(1)
      2: Skew = useDouble ? DBLARR(xdim[0]) : FLTARR(xdim[0])
      3: Skew = useDouble ? DBLARR(xdim[0], xdim[1]) : FLTARR(xdim[0], xdim[1])
      4: Skew = useDouble ? DBLARR(xdim[0], xdim[1], xdim[2]) : FLTARR(xdim[0], xdim[1], xdim[2])
   endcase
; Compute standard deviation
; ntreal might be a matrix
   std = a_timecorrelate(x, 0, /covariance)
   std = sqrt(std)
   zero = where(std EQ 0)

   if zero[0] NE -1 THEN BEGIN
    STOP, $
    'Cannot compute skewness since there are zeros in the matrix of standard deviations !'
   ENDIF

; Problem with high masked values (x^3 makes NAN when x is high)
; Threshold put on the values of the tab
   idx_std = WHERE (std GT 1.0e+10)
   x = x < 1.0e+10
   std = std < 1.0e+10

; Compute skewness
   Skew = Skewness_Num(x, nt, Double = useDouble, NAN = nan) / (ntreal*std^3)
   IF idx_std[0] NE -1 THEN BEGIN
    Skew[idx_std] = valmask
   ENDIF

   return, useDouble ? Skew : FLOAT(Skew)

END
