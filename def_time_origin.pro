;+
;
; compute date format : 0010-JAN-01 00:00:00
;
; @param TIMEAVE {in}{required}{type=string}
;
; @param DATE1 {in}{required}{type=string}
; [yy..yyy][mm][dd]-[sssss] if <n>s
; [yy..yyy][mm][dd] if <n>d
; [yy..yyy][mm]     if <n>m
; [yy..yyy]         if <n>y
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> timeave='1m'
; IDL> date1='199201'
; IDL> result=def_time_origin(timeave, date1)
; IDL> print, result
; 1992-JAN-01 00:00:00
;
; @todo
; error handling
; what are count and time ? usefull ?
;
; @history
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: def_time_origin.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION def_time_origin, timeave, date1
;
  compile_opt idl2, strictarrsubs
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result= def_time_origin(timeave, date1)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(timeave,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type timeave' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(date1,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type date1' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

   len1 = strlen(date1)

   mean_type = strmid(timeave, strlen(timeave)-1,1)
   CASE mean_type OF
      'y':BEGIN                 ; year case
         year1 = long(date1)
         time_origin = string(FORMAT = '(I4)', year1)+'-JAN-01 00:00:00'
      END
      'm': BEGIN                ; month case
         CASE strmid(timeave, strlen(timeave)-2,1) OF
            'm': BEGIN          ; mean month
               month1 = long(strmid(date1, 0, 2))
               time_origin = '0000-JAN-01 00:00:00'
            END
            ELSE: BEGIN         ; month / year
               month1 = long(strmid(date1, len1-2, 2))
               year1 = long(strmid(date1, 0, len1-2))
               time_origin = string(FORMAT = '(I4.4)', year1)+'-'+trans_month(month1)+'-01 00:00:00'
            END
         ENDCASE
      END
      'd': BEGIN                ; day case
         day1 = strmid(date1, len1-2, 2)
         month1 = long(strmid(date1, len1-4, 2))
         year1 = long(strmid(date1, 0, len1-4))
         time_origin = string(FORMAT = '(I4)', year1)+'-'+trans_month(month1)+'-'+day1+' 00:00:00'
      END
      's': BEGIN                ; seconds case
         sec1 = long(strmid(date1, len1-5, 5))
         day1 = long(strmid(date1, len1-8, 2))
         month1 = long(strmid(date1, len1-10, 2))
         year1 = long(strmid(date1, 0, len1-10))
         print, '   **** def_time_origin not ready for timeave = ', timeave
      END
      ELSE: BEGIN
         print, '   **** def_time_origin not ready for timeave = ', timeave
         count = -1
         time = 0
      END
   ENDCASE

   return, time_origin

END
