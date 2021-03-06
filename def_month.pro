;+
;
; translate month number in string
;
; @categories
; Calendar
;
; @param TIMAVE {in}{required}{type=string}
;
; @param DATE {in}{required}{type=string}
;
; @examples
;
; IDL> date='186001'
; IDL> timave='1m\@t412'
; IDL> mn = def_month(timave, date)
; IDL> print, mn
; January
;
; IDL> timave='1mm'
; IDL> date='01_1860-1959'
; IDL> mn = def_month(timave, date)
; IDL> print, mn
; January
;
; @returns
; month name or season code
; '???' or '??????' if decoding problem
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; be more explicit when returns ??? or ??????
;
; add explanation on season code
;
; @history
;
; - fplod 20091208T165219Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: def_month.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION def_month, timave, date
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=def_month(timave, date)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    mn = '???'
    return, mn
 ENDIF
 arg_type = size(timave,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type timave' $
          + '!C' $
          + 'Usage : ' + usage])
    mn = '???'
    return, mn
 ENDIF
 arg_type = size(date,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type date' $
          + '!C' $
          + 'Usage : ' + usage])
    mn = '???'
    return, mn
 ENDIF

   IF strpos(date, '_') GT -1 THEN BEGIN
    date = strmid(date, 0, strpos(date, '_'))
   ENDIF
   CASE strmid(timave, 0, 2) OF
      '1m': BEGIN
         CASE strmid(date, strlen(date)-2, 2) OF
            '01': mn = 'January'
            '02': mn = 'February'
            '03': mn = 'March'
            '04': mn = 'April'
            '05': mn = 'May'
            '06': mn = 'June'
            '07': mn = 'July'
            '08': mn = 'August'
            '09': mn = 'September'
            '10': mn = 'October'
            '11': mn = 'November'
            '12': mn = 'December'
            ELSE: mn = '???'
         ENDCASE
      END
      '3m': BEGIN
         IF strpos(timave, '3mm') GT -1 THEN BEGIN
            CASE time_array[0] OF
               1: BEGIN
                  CASE strmid(date, strlen(date)-2, 2) OF
                     '01': mn = 'JFM'
                     '02': mn = 'AMJ'
                     '03': mn = 'JAS'
                     '04': mn = 'OND'
                     ELSE: mn = '???'
                  ENDCASE
               END
               -9: BEGIN
                  CASE strmid(date, strlen(date)-2, 2) OF
                     '01': mn = 'MAM'
                     '02': mn = 'JJA'
                     '03': mn = 'SON'
                     '04': mn = 'DJF'
                     ELSE: mn = '???'
                  ENDCASE
               END
               ELSE: BEGIN      ; = 4 or 11
                  CASE strmid(date, strlen(date)-2, 2) OF
                     '01': mn = 'DJF'
                     '02': mn = 'MAM'
                     '03': mn = 'JJA'
                     '04': mn = 'SON'
                     ELSE: mn = '???'
                  ENDCASE
               END
            ENDCASE

            ENDIF ELSE BEGIN
               CASE time_array[0] OF
                  -11: BEGIN
                     CASE strmid(date, strlen(date)-2, 2) OF
                        '01': mn = 'JFM'
                        '04': mn = 'AMJ'
                        '07': mn = 'JAS'
                        '10': mn = 'OND'
                        ELSE: mn = '???'
                     ENDCASE
                  END
                  ELSE: BEGIN
                     CASE strmid(date, strlen(date)-2, 2) OF
                        '01': mn = 'DJF'
                        '04': mn = 'MAM'
                        '07': mn = 'JJA'
                        '10': mn = 'SON'
                        ELSE: mn = '???'
                     ENDCASE
                  END
               ENDCASE

            ENDELSE
      END
      '6m': BEGIN
         IF strpos(timave, '6mm') GT -1 THEN BEGIN
            CASE strmid(date, strlen(date)-2, 2) OF
               '01': mn = 'JFMAMJ'
               '02': mn = 'JASOND'
               ELSE: mn = '??????'
            ENDCASE
         ENDIF
      END
      ELSE:
   ENDCASE

   return, mn
END
