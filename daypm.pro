;+
;
; define max number of days in month
;
; @categories
; Calendar
;
; @param month_i {in}{required}{type=integer}
; Number of the desired month (1 = January, ..., 12 = December).
;
; @param year_i {in}{required}{type=integer}
; year
;
; @returns
; max number of days in month/year
; -1 in case of error
;
; @examples
;
;   IDL> calendar_type=1
;   IDL> month_i=2
;   IDL> year_i=2000
;   IDL> result=daypm(month_i, year_i)
;   IDL> print, result
;          29
;
;   IDL> calendar_type=0
;   IDL> month_i=2
;   IDL> year_i=2000
;   IDL> result=daypm(month_i, year_i)
;   IDL> print, result
;          28
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
;
; should it be replaced by <pro>daysinmonth</pro> ? may be a difference
; between CALENDAR_TYPE possible values and key_caltype possible values ...
;
; @history
;
; - fplod 20091208T170905Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: daypm.pro 224 2010-05-28 13:30:44Z ericg $
;
;-
FUNCTION daypm, month_i, year_i
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=daypm(month_i, year_i)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    mn = -1
    return, mn
 ENDIF

 arg_type = size(month_i,/type)
 IF (arg_type NE 2 AND arg_type NE 3) THEN BEGIN
   ras = report(['Incorrect arg type month_i' $
          + '!C' $
          + 'Usage : ' + usage])
    mn = -1
    return, mn
 ENDIF
 arg_type = size(year_i,/type)
 IF (arg_type NE 2 AND arg_type NE 3) THEN BEGIN
   ras = report(['Incorrect arg type year_i' $
          + '!C' $
          + 'Usage : ' + usage])
    mn = -1
    return, mn
 ENDIF

 common_type=size(calendar_type,/type)
 IF (common_type NE 2) THEN BEGIN
   ras = report(['Incorrect common type calendar_type' $
          + '!C' $
          + 'Usage : ' + usage])
    mn = -1
    return, mn
 ENDIF

   IF calendar_type LE 1 THEN BEGIN
      IF calendar_type EQ 0 THEN BEGIN
         feb = 28
      ENDIF ELSE BEGIN
         leap = year_i MOD 4
         IF leap EQ 0 THEN BEGIN
          feb = 29
         ENDIF ELSE BEGIN
          feb = 28
         ENDELSE
      ENDELSE
      CASE month_i OF
         1: mn = 31
         2: mn = feb
         3: mn = 31
         4: mn = 30
         5: mn = 31
         6: mn = 30
         7: mn = 31
         8: mn = 31
         9: mn = 30
         10: mn = 31
         11: mn = 30
         12: mn = 31
         ELSE:
      ENDCASE

   ENDIF ELSE BEGIN
    mn = calendar_type
   ENDELSE

   return, mn
END
