;+
;
; @categories
; Calendar
;
; @param MONTH {in}{required}{type=integer}
;
; @returns
; month 2 characters strings
; -1 in case of error
;
; @todo
; why not use <pro>monthname</pro> instead ?
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: trans_month.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION trans_month, month
;
  compile_opt idl2, strictarrsubs
;
 usage='result=trans_month(month)'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
   ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(month,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type month' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF


   CASE month OF
      01: mn = 'JAN'
      02: mn = 'FEB'
      03: mn = 'MAR'
      04: mn = 'APR'
      05: mn = 'MAY'
      06: mn = 'JUN'
      07: mn = 'JUL'
      08: mn = 'AUG'
      09: mn = 'SEP'
      10: mn = 'OCT'
      11: mn = 'NOV'
      12: mn = 'DEC'
      ELSE:
   ENDCASE

   return, mn
END
