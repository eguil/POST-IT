;+
;
; make work
;
; @param FILE_NAME {in}{required}{type=string}
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @keyword TIME_1
;
; @keyword TIME_2
;
; @keyword ZMTYP
;
; @returns
; structure
; -1 in case of error

; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_work.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION make_work, file_name, ncdf_db $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ZMTYP=zmtyp
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='result=make_work(file_name, ncdf_db ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ZMTYP=zmtyp)'

 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(file_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type file_name' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(ncdf_db,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type ncdf_db' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

; ellipse param

; test 1
;   x0 = 145
;   y0 = 10
;   a1 = 25
;   b1 = 5
;   delta_r = 3

; test 2
;   x0 = 150
;   y0 = 10
;   a1 = 22
;   b1 = 5
;   delta_r = 10

   x0 = 150
   y0 = 10
   a1 = 22
   b1 = 5
   delta_r = 5


   a2 = a1+delta_r/cos(y0*acos(0)*2/180.)
   b2 = b1+delta_r

   work = glamt
   work[*, *] = 0.
   alpha = glamt
   alpha[*, *] = 0.

   A = ((glamu-x0)^2)/(a1^2) + ((gphiu-y0)^2)/b1^2
   A0 = ((glamu-x0)^2)/(a2^2) + ((gphiu-10)^2)/b2^2

   idx = where(A LE 1.)
   work[idx] = 1.
   idx = where((A0-A) NE 0)
   alpha[idx] = (A0[idx]-1)*A[idx]/(A0[idx]-A[idx])
   idx = where(A GT 1 AND A0 LE 1)
   work[idx] = alpha[idx]

   valmask = 1.e20

   field = {name: '', data: work, legend: '', units: '', origin: '', dim: 0, direc:''}

   field.origin = 'make_work'
   field.dim = 2
   field.direc = 'xy'

   return, field
END
