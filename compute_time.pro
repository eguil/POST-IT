;+
;
; compute number + array of time intervals in units of timeave between date1
; and date2
;
; @param TIMEAVE {in}{required}{type=string}
;
; @param DATE1 {in}{required}{type=string}
; [yy..yyy][mm][dd]-[sssss] if <n>s
; [yy..yyy][mm][dd] if <n>d
; [yy..yyy][mm]     if <n>m
; [yy..yyy]         if <n>y
;
; @param DATE2 {in}{required}{type=string}
; [yy..yyy][mm][dd]-[sssss] if <n>s
; [yy..yyy][mm][dd] if <n>d
; [yy..yyy][mm]     if <n>m
; [yy..yyy]         if <n>y
;
; @keyword NEXT
; if /NEXT, compute next date after date1, using date2 units of timeave
;
; @returns
; structure of a number + array of time intervals in units of timeave between
; date1 and date2.
;
; -1 in case of error
;
; @examples
;
; IDL> calendar_type='0'
; IDL> timeave = '1mm'
; IDL> date1 = '01_1860-1959'
; IDL> date2 = '12_1860-1959'
; IDL> nb_cycles=1
; IDL> result = compute_time(timeave, date1, date2)
; IDL> help,result,/structure
; ** Structure <8481304>, 2 tags, length=52, data length=52, refs=1:
;   COUNT           LONG                12
;   SCALE           LONG      Array[12]
;
; IDL> print, result
; {          12     1721424     1721454     1721484     1721514     1721544     1721574     1721604     1721634
;     1721664     1721694     1721724     1721754
; }
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; <pro>julday</pro>
;
; @todo
; add info and example on calendar_type and nb_cycles
;
; @history
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: compute_time.pro 229 2010-06-23 16:13:47Z ericg $
;
;-
FUNCTION compute_time, timeave, date1, date2 $
         , NEXT=next
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=compute_time(timeave, date1, date2,NEXT=next)'
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
 arg_type = size(date2,/type)
 IF (arg_type NE 7) AND (arg_type NE 3) THEN BEGIN
   ras = report(['Incorrect arg type date2' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 common_type=size(nb_cycles,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type nb_cycles' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

   len1 = strlen(date1)
   IF NOT keyword_set(NEXT) THEN BEGIN

      len2 = strlen(date2)
      mean_type = strmid(timeave, strlen(timeave)-1,1)
      CASE mean_type OF
         'y':BEGIN              ; year case
            delta_time = long(strmid(timeave, 0, strlen(timeave)-1))
            year1 = long(date1)
            year2 = long(date2)
            count = (year2-year1+1)*delta_time
            time = julday(replicate(1, count),replicate(15, count),year1+findgen(count))
         END
         'm': BEGIN              ; month case
            CASE strmid(timeave, strlen(timeave)-2,1) OF
               'm': BEGIN   ; mean month
                  delta_time = long(strmid(timeave, 0, strlen(timeave)-2))
                  month1 = long(strmid(date1, 0, 2))
                  month2 = long(strmid(date2, 0, 2))
                  count = (month2-month1+1)
;                  count = (month2-month1+1)/delta_time
                  ; time scale for hov
                  time = julday(month1+lindgen(nb_cycles*count), replicate(15,nb_cycles*count), $
                                replicate(1,nb_cycles*count))
               END
               ELSE: BEGIN  ; month / year
                  delta_time = long(strmid(timeave, 0, strlen(timeave)-1))
                  month1 = long(strmid(date1, len1-2, 2))
                  year1 = long(strmid(date1, 0, len1-2))
                  month2 = long(strmid(date2, len2-2, 2))
                  year2 = long(strmid(date2, 0, len2-2))
                  count = 0
                  im = month1
                  iy = year1
                  idate = long(month1+100*year1)
                  WHILE idate LE date2 DO BEGIN
                     idate = long(im+100*iy)
                     count = count + 1
                     im = im + delta_time
                     IF im EQ 13 THEN BEGIN
                        iy = iy+1
                        im = 1
                     ENDIF
                  ENDWHILE
                  count = count - 1
                 ; time scale for hov
                  time = julday(month1+lindgen(count), replicate(15, count), replicate(year1, count))
               END
            ENDCASE
        END
         'd': BEGIN              ; day case
            delta_time = long(strmid(timeave, 0, strlen(timeave)-1))
            day1 = long(strmid(date1, len1-2, 2))
            month1 = long(strmid(date1, len1-4, 2))
            year1 = long(strmid(date1, 0, len1-4))
            day2 = long(strmid(date2, len2-2, 2))
            month2 = long(strmid(date2, len2-4, 2))
            year2 = long(strmid(date2, 0, len2-4))
            count = 0
            id = day1
            im = month1
            iy = year1
            idate = long(day1+100*month1+10000*year1)
            WHILE idate LE date2 DO BEGIN
               idate = long(id+100*im+10000*iy)
               count = count + 1
               id = id + delta_time
               idmax = daypm(im, iy)
               IF id GT idmax THEN BEGIN
                  im = im + 1
                  id = id - idmax
               ENDIF
               IF im EQ 13 THEN BEGIN
                  iy = iy+1
                  im = 1
               ENDIF
            ENDWHILE
            count = count - 1
            ; time scale for hov
            time = julday(replicate(month1, count), day1+lindgen(count), replicate(year1, count))
         END
         's': BEGIN              ; seconds case
            delta_time = long(strmid(timeave, 0, strlen(timeave)-1))
            sec1 = long(strmid(date1, len1-5, 5))
            day1 = long(strmid(date1, len1-8, 2))
            month1 = long(strmid(date1, len1-10, 2))
            year1 = long(strmid(date1, 0, len1-10))
            sec2 = long(strmid(date2, len1-5, 5))
            day2 = long(strmid(date2, len1-8, 2))
            month2 = long(strmid(date2, len2-10, 2))
            year2 = long(strmid(date2, 0, len2-10))
            count = 0
            is = sec1
            id = day1
            im = month1
            iy = year1
            WHILE is LE sec2 AND id LE day2 AND im LE month2 $
             AND iy LE year2 DO BEGIN
               count = count + 1
               is = is + delta_time
               IF is GT 86400 THEN BEGIN
                  id = id + 1
                  is = is - 86400
               ENDIF
               idmax = daypm(im, iy)
               IF id EQ idmax THEN BEGIN
                  im = im + 1
                  id = 1
               ENDIF
               IF im EQ 13 THEN BEGIN
                  iy = iy+1
                  im = 1
               ENDIF
            ENDWHILE
                                ; time scale for hov
            IF calendar_type LE 1 THEN BEGIN
               print, '  to correct... see compute_time.pro  '
;           use double type for hours - yyyymmdd is noon, yyyymmdd.00 is 0h
               stop
               time = sec1/86400.+lindgen(count)*1/86400.*delta_time+ $
                julday(month1,1,year1)
            ENDIF ELSE BEGIN
               time = sec1/86400.+lindgen(count)*1/86400.*delta_time+ $
                julday(month1,1,year1, ndayspm = calendar_type)
            ENDELSE
         END
         ELSE: BEGIN
            print, '   **** compute_time not ready for timeave = ', timeave
            count = -1
            time = 0
         END
      ENDCASE
      IF date1 EQ date2 THEN BEGIN
       count = 1
      ENDIF
      timearr = {count: count, scale: time}
      return, timearr

   ENDIF ELSE BEGIN
      ; next date after date1, using date2 units of timeave
      CASE strmid(timeave, strlen(timeave)-1, 1) OF
         'd': BEGIN
            month1 = long(strmid(date1, len1-10, 2))
            year1 = long(strmid(date1, 0, len1-10))
            dmax = daypm(month1, year1)
            IF debug_w THEN BEGIN 
               print, '   dmax, date1 in compute_time /NEXT: ', dmax,'  ',  date1
            ENDIF 
            ndays = long(strmid(timeave, 0, strlen(timeave)-1))
            day1 = long(strmid(date1, len1-2, 2))
            month1 = long(strmid(date1, len1-4, 2))
            year1 = long(strmid(date1, 0, len1-4))
            iadd = 1
            day2 = day1
            month2 = month1
            year2 = year1
            WHILE iadd LE date2 DO BEGIN
               day2 = day2+1
               IF day2 GT dmax THEN BEGIN
                  month2 = month2 + 1
                  day2 = 1
                  IF month2 GT 12 THEN BEGIN
                     year2 = year2+1
                     month2 = 1
                  ENDIF
               ENDIF
               iadd = iadd + 1
            ENDWHILE
            IF year2 GE 10 THEN BEGIN
               nextdate = string(year2)
            ENDIF ELSE BEGIN
               nextdate = string(format = '(I2.2)',year2)
            ENDELSE
            nextdate = nextdate+string(format = '(I2.2)', month2)+string(format = '(I2.2)', day2)
         END
         'm': BEGIN
            month1 = long(strmid(date1, len1-2, 2))
            year1 = long(strmid(date1, 0, len1-2))
            iadd = 1
            month2 = month1
            year2 = year1
            WHILE iadd LE date2 DO BEGIN
               month2 = month2 + 1
               IF month2 GT 12 THEN BEGIN
                  year2 = year2+1
                  month2 = 1
               ENDIF
               iadd = iadd + 1
            ENDWHILE 
            IF year2 GE 10 THEN BEGIN
               nextdate = string(year2)
            ENDIF ELSE BEGIN
               nextdate = string(format = '(I2.2)',year2)
            ENDELSE
            nextdate = nextdate+string(format = '(I2.2)', month2)
         END 
         ELSE: BEGIN
            print, '   **** next time not ready for timeave = ', timeave
            nextdate = -1
         END
      ENDCASE
      return, nextdate
   ENDELSE

END
