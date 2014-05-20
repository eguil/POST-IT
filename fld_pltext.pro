;+
;
; Define field plot attributes min/max
;
; @param VAR_NAME {in}{required}{type=string}
; variable name
;
; @param PLTTYP {in}{required}{type=string}
; plot type
;
; @param DIMPLOT {in}{required}{type=integer}
; dimension of plot (1,2)
;
; @param HOTYP {in}{required}{type=string}
;
; @returns
; outputs structure
;
; -1 in case of error
;
; @examples
;
; IDL> var_name='temp_1'
; IDL> hotyp='-'
; IDL> plttyp='pltt'
; IDL> dimplot=1
; IDL> free_1d_minmax='yes'
; IDL> result=fld_pltext(var_name, plttyp, dimplot, hotyp)
; IDL> help, result,/structure
; ** Structure <84797cc>, 10 tags, length=56, data length=56, refs=1:
;    NAME            STRING    'temp_1'
;    ASSOS           STRING    'temp'
;    MIN             FLOAT          -2.00000
;    MAX             FLOAT           30.0000
;    HOMIN           FLOAT           3.68000
;    HOMAX           FLOAT           3.71000
;    MIN1D           FLOAT               NaN
;    MAX1D           FLOAT               NaN
;    DMAX            FLOAT           3.00000
;    SPEC            FLOAT               NaN
;
; IDL> print, result
; { temp_1 temp     -2.00000      30.0000      3.68000      3.71000          NaN          NaN      3.00000
;           NaN}
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; get rid of spawn
;
; explication sur common free_1d_minmax
;
; @history
;
; - fplod 20091210T080930Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - EG 4/10/99
;
; @version
; $Id: fld_pltext.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
FUNCTION fld_pltext, var_name, plttyp, dimplot, hotyp
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=fld_pltext(var_name, plttyp, dimplot, hotyp)
;
 nparam = N_PARAMS()
 IF (nparam LT 4) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(var_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type var_name' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(plttyp,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type plttyp' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(dimplot,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type dimplot' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(hotyp,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type hotyp' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type=size(free_1d_minmax,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type free_1d_minmax' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

; define variable plot attributes (min/max)

   fldextr = {name:'', assos:'', min: 0.0, max: 0.0, homin:0.0, homax:0.0, min1d:0.0, max1d:0.0, dmax:0.0, spec:0.0}

; name of defaults files

   file_mmx = 'fld_glo_mmx.def'

; min/max
   print,' '
   print,'    Reading minmax of ',var_name,' from : ', file_mmx

   spawn, 'grep -i "\ '+var_name+' " '+hom_def+file_mmx, line
   line = strcompress(strtrim(line[0], 2))
   length = strlen(line)

   IF length EQ 0 THEN BEGIN
      print, '     *** no min & max found for field ', var_name, $
       ' in file fld_glo_mmx.def'
         fldextr.min = min(field.data[where(field.data NE valmask)])
         fldextr.max = max(field.data[where(field.data NE valmask)])
         fldextr.homin = fldextr.min
         fldextr.homax = fldextr.max
         fldextr.min1d = fldextr.min
         fldextr.max1d = fldextr.max
         fldextr.dmax = max([abs(fldextr.min), abs(fldextr.max)])
         fldextr.assos = var_name
         fldextr.spec = 1.
   ENDIF ELSE BEGIN
      argvar = strsplit(line, ' ', /EXTRACT)
      IF strmid(argvar[1], 0, 1) EQ '@' THEN BEGIN
         ; get info from other variable
         friend_name = strmid(argvar[1], 1, strlen(argvar[1])-1)
         print, '      -> linked to ', friend_name
         fldextr = fld_pltext(friend_name, plttyp, dimplot, hotyp)
         fldextr.assos = friend_name
      ENDIF ELSE BEGIN
         fldextr.min = float(argvar[1])
         fldextr.max = float(argvar[2])
         fldextr.homin = !VALUES.F_NAN
         fldextr.homax = !VALUES.F_NAN
         fldextr.min1d = !VALUES.F_NAN
         fldextr.max1d = !VALUES.F_NAN
         fldextr.dmax = !VALUES.F_NAN
         fldextr.spec = !VALUES.F_NAN
         IF debug_w THEN BEGIN
          print, '   Number of args for fldext:', n_elements(argvar)
         ENDIF
         IF n_elements(argvar) GE 5 THEN BEGIN
            fldextr.homin = float(argvar[3])
            fldextr.homax = float(argvar[4])
         ENDIF
         IF n_elements(argvar) GE 7 THEN BEGIN
            fldextr.min1d = float(argvar[5])
            fldextr.max1d = float(argvar[6])
         ENDIF
         IF n_elements(argvar) GE 9 THEN BEGIN
            fldextr.spec = float(argvar[8])
         ENDIF

         IF free_1d_minmax EQ 'yes' THEN BEGIN
            IF hotyp EQ 't' THEN BEGIN
               fldextr.homin = !VALUES.F_NAN
               fldextr.homax = !VALUES.F_NAN
            ENDIF
            fldextr.min1d = !VALUES.F_NAN
            fldextr.max1d = !VALUES.F_NAN
         ENDIF
         IF n_elements(argvar) GE 8 THEN BEGIN
            fldextr.dmax = float(argvar[7])
         ENDIF
         tempvar = SIZE(TEMPORARY(argvar))
         fldextr.assos = var_name
      ENDELSE
   ENDELSE

   fldextr.name = var_name

   return, fldextr
END
