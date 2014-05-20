;+
;
; read GMT palette into IDL color/intervals system
;
; --> common color2 used by <pro>label_gmt</pro>
;
; This palette is loaded using <proidl>TVLCT</proidl>
;
; @param VAR_NAME {in}{required}{type=string}
;
; @param C_ANOT_STR {out}{type=string}
;
; @param FMT {out}{type=string}
;
; @param FOUND {out}{type=integer}
; set to 0 if palette file not found
;
; @param READPAL {in}{required}{type=integer}
;
; @examples
;
; IDL> pal_type = 'col'
; IDL> field={origin:'ginette'}
; IDL> var_name='sosstsst'
; IDL> readpal=2
; IDL> lec_pal_gmt,var_name, c_anot_str, fmt, found, readpal
;     Reading palette : /usr/lodyc/incas/fplod/post_it_ws/config/palettes/palet_frequency_pwr.cpt
;
; To see changes
;
; IDL> print, c_anot_str, fmt, found, readpal
; *****    0.    0.    1.    3.    5.    7.    9.   11.   13.   15.   17.   19.   21.   23.   25.   27.   29.
;  31.   33.
; (f5.0)           1       2
;
; IDL> pal_type = 'col'
; IDL> field=create_struct('origin','ginette')
; IDL> var_name='sosstsst'
; IDL> readpal=3
; IDL> lec_pal_gmt,var_name, c_anot_str, fmt, found, readpal
;     Reading palette : /usr/lodyc/incas/fplod/post_it_ws/config/palettes/palet_DIVISION.cpt
;
; To see changes
;
; IDL> print, c_anot_str, fmt, found, readpal
;******    0.0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8    0.9    1.0    1.1    1.2    1.3
;   1.4    1.5    1.6    1.7    1.8    1.9    2.0   10.0
;(f6.1)           1       3
;
; IDL> pal_type = 'col'
; IDL> field=create_struct('origin','ginette')
; IDL> fldatt=create_struct('assos','sosstsst')
; IDL> var_name='sosstsst'
; IDL> readpal=1
; IDL> lec_pal_gmt,var_name, c_anot_str, fmt, found, readpal
;      Reading palette : /usr/lodyc/incas/fplod/post_it_ws/config/palettes/palet_SOSSTSST.cpt
;
; To see changes
;
; IDL> print, c_anot_str, fmt, found, readpal
;*****   -2.    1.    2.    3.    4.    5.    6.    7.    8.    9.   10.   11.   12.   13.   14.   15.   16.
;  17.   18.   19.   20.   21.   22.   23.   24.   25.   26.   27.   28.   29.   30.   31.   32.
;(f5.0)           1       1
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; get rid of spawn
;
; est-il bien normal d'avoir des etoiles dans c_anot_str ?!
;
; ++ autres exemples avec pal_type = 'col'
; ++ autres exemples avec readpal=1
;
; ++ more realistic examples
;
; test si field.origin defined
;
; @history
;
; - fplod 20100114T144253Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * fix typo
;
; - fplod 20091210T093834Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: lec_pal_gmt.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
PRO lec_pal_gmt, var_name, c_anot_str, fmt, found, readpal
;
 compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='lec_pal_gmt, var_name, c_anot_str, fmt, found, readpal'
;
 nparam = N_PARAMS()
 IF (nparam LT 5) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(var_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type var_name' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(readpal,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type readpal' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type = size(pal_type,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type pal_type' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

   IF n_elements(idx_pal) EQ 0 THEN BEGIN
    idx_pal = 0
   ENDIF

   found = 1
   IF pal_type NE '2dom' THEN BEGIN
      ; read palette
      ; build palette name
      CASE pal_type OF
         'bw': col = '_BW'
         'col': col = ''
      ENDCASE
      CASE field.origin OF
         'diff': col = col+'d'
         ELSE:
      ENDCASE

      CASE readpal OF
         1: palname = 'palet_'+strupcase(fldatt.assos)+col+'.cpt'
         2: palname = 'palet_frequency_pwr'+col+'.cpt'
         3: palname = 'palet_DIVISION.cpt'
         ELSE: palname = 'palet_'+strupcase(fldatt.assos)+col+'.cpt'
      ENDCASE
      ; if exist, read

      file_cpt = hom_def+'palettes/'+palname
      spawn, 'ls '+file_cpt, line
      line = strcompress(strtrim(line[0], 2))
      length = strlen(line)

      IF length EQ 0 THEN BEGIN
         print, '     No palette found for ', var_name
         print, '     (', file_cpt, ')'
         found = 0
      ENDIF ELSE BEGIN

         ; decode palette
         print, '     Reading palette : ', file_cpt
         print, ' '
         spawn, 'awk -f '+hom_idl+'procs/awk_palette '+file_cpt, line
         line = strcompress(strtrim(line, 2))
         length = strlen(line)
         ncont = n_elements(line)
         read_array = fltarr(4, ncont)
         i = 0
         FOR i = 0, ncont-1 DO BEGIN
            read_array[0, i] = strsplit(line[i], ' ', /EXTRACT)
         ENDFOR
         levels_gmt = fltarr(ncont)
         levels_gmt = [-999999.0, reform(read_array[0, *], ncont)]
         ; build IDL palette in lct=<nn>
         red = reform(long(read_array[1, *]), ncont)
         green = reform(long(read_array[2, *]), ncont)
         blue = reform(long(read_array[3, *]), ncont)

         ; compute format fmt (uses tag 'piso' in color palet file)

         spawn, 'grep piso '+file_cpt, line
         IF n_elements(line) NE 0 AND (strpos(line, 'piso'))[0] NE -1 THEN BEGIN
            iso = abs(float(line[0]))
            IF iso EQ 0.1 THEN BEGIN
               fmt = '(f6.1)'
            ENDIF ELSE IF iso EQ .2 THEN BEGIN
               fmt = '(f6.1)'
            ENDIF ELSE IF iso EQ .05 THEN BEGIN
               fmt = '(f6.2)'
            ENDIF ELSE IF iso EQ .02 THEN BEGIN
               fmt = '(f6.2)'
            ENDIF ELSE IF iso LT .1 THEN BEGIN
               fmt = '(f6.3)'
            ENDIF ELSE IF iso LE .25 THEN BEGIN
               fmt = '(f6.3)'
            ENDIF ELSE IF iso LT 1 THEN BEGIN
               fmt = '(f5.1)'
            ENDIF ELSE BEGIN
               fmt = '(f5.0)'
            ENDELSE
            IF long(iso) NE 0 THEN BEGIN
               IF iso/long(iso) NE 1 THEN BEGIN
                fmt = '(f5.1)'
               ENDIF
            ENDIF
         ENDIF ELSE BEGIN
            print, ' *** WARNING !!!! piso missing in file_cpt'
         ENDELSE

         IF idx_pal EQ 0  THEN BEGIN
            ; add black as first color
            red = [0, red]
            green = [0, green]
            blue = [0, blue]
         ENDIF
         IF debug_w THEN BEGIN 
            print, '   idx_pal in lec_pal_gmt: ', idx_pal
         ENDIF 

         tvlct, red, green, blue, idx_pal

         ncont_gmt = ncont+1
         coul_gmt = findgen(ncont_gmt)+idx_pal
         max_gmt = levels_gmt[ncont_gmt-1]

         IF (idx_pal + ncont) GT 100 THEN BEGIN
            print, '     **** Warning idx_pal gt 100 : palette no updated'
         ENDIF ELSE BEGIN
;            idx_pal = idx_pal + ncont + 1
         ENDELSE

         ; annotation string
         c_anot_str = string(levels_gmt, format = fmt)

         IF debug_w THEN BEGIN 
            print, ' >> In lec_pal_gmt: '
            print, '    idx_pal    : ', idx_pal
            print, '    red        : ', red
            print, '    green      : ', green 
            print, '    blue       : ', blue
            print, '    ncont_gmt  : ', ncont_gmt
            print, '    coul_gmt   : ', coul_gmt
            print, '    levels_gmt : ', levels_gmt
         ENDIF 

         found = 1
      ENDELSE
   ENDIF


END
