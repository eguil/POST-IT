;+
;
; @categories
; Calendar
;
; @version
; $Id: mth_decode.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-

      mth = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'DJF', 'MAM', 'JJA', 'SON']
      CASE STRMID(stddev_mth, 0, 2) OF
         '01': strd = [1]
         '02': strd = [2]
         '03': strd = [3]
         '04': strd = [4]
         '05': strd = [5]
         '06': strd = [6]
         '07': strd = [7]
         '08': strd = [8]
         '09': strd = [9]
         '10': strd = [10]
         '11': strd = [11]
         '12': strd = [12]
         ELSE: CASE STRMID(stddev_mth, 0, 3) OF
         'DJF': BEGIN & strd = [1, 2, 12] & ntxt = 'DJF' & END
         'MAM': BEGIN & strd = [3, 4, 5] & ntxt = 'MAM' & END
         'JJA': BEGIN & strd = [6, 7, 8] & ntxt = 'JJA' & END
         'SON': BEGIN & strd = [9, 10, 11] & ntxt = 'SON' & END
            ELSE: CASE STRMID(stddev_mth, 0, 6) OF
               'JASOND' : BEGIN & strd = [7, 8, 9, 10, 11, 12] & ntxt = 'JASOND' & END
               'JFMAMJ' : BEGIN & strd = [1, 2, 3, 4, 5, 6] & ntxt = 'JFMAMJ' & END
            ELSE: BEGIN & strd = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] & ntxt = 'Annual' & END
            ENDCASE
         ENDCASE
      ENDCASE

      nmth = (size(strd))[1]
      njpt = n_elements((lindgen(jpt))[0:*:12])
      IF debug_w THEN BEGIN
       print, '   nmth,njpt', nmth, njpt
      ENDIF

      idxm = lonarr(nmth, njpt)

      FOR imth = 0, nmth-1 DO BEGIN
         idxm[imth, *]= (lindgen(jpt))[strd[imth]-1:*:12]
      ENDFOR

      IF nmth EQ 1 THEN BEGIN
       ntxt = mth[strd-1]
      ENDIF

      print, '    Warning: monthly computation and average for following period: ', ntxt
