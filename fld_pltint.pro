;+
;
; Define field plot attributes min/max/int/mult
;
; @param VAR_NAME {in}{required}{type=string}
; variable name
;
; @param PLTTYP {in}{in}{required}{type=string}
; plot type
;
; @param DIMPLOT {in}{in}{required}{type=integer}
; dimension of plot (1,2)
;
; @param HOTYP {in}{required}{type=string}
;
; @returns
; outputs structure
; define variable plot attributes (contour intervals)
;
; @examples
;
; IDL> var_name='temp_1'
; IDL> hotyp='-'
; IDL> plttyp='pltt'
; IDL> dimplot=1
; IDL> field={origin:'div'}
; IDL> result=fld_pltint(var_name, plttyp, dimplot, hotyp)
; IDL> help, result,/structure
; ** Structure <8476b7c>, 6 tags, length=40, data length=40, refs=1:
;    NAME            STRING    'temp_1'
;    INT             FLOAT           1.00000
;    MULT            FLOAT           1.00000
;    ADD             FLOAT           0.00000
;    UNIT            STRING    ''
;    MID             FLOAT           26.0000
;
; IDL> print, result
; { temp_1      1.00000      1.00000      0.00000       26.0000}
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; get rid of spawn
;
; explication sur common field.origin
;
; @history
; - fplod 20091210T080930Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - EG 25/2/99
;
; @version
; $Id: fld_pltint.pro 229 2010-06-23 16:13:47Z ericg $
;
;-
FUNCTION fld_pltint, var_name, plttyp, dimplot, hotyp
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=fld_pltint(var_name, plttyp, dimplot, hotyp)
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

   fldintr = {name:'', int: 0.0, mult: 0.0, add: 0.0, unit: '', mid: 0.0}

; name of defaults files

   CASE plttyp of
      'plt': file_iso = 'fld_glo_iso.def'
      'pltz': file_iso = 'fld_glo_iso.def'
      'pltt': file_iso = 'fld_glo_iso.def'
      ELSE: file_iso = 'fld_glo_iso.def'
   ENDCASE
   IF field.origin EQ 'diff' THEN BEGIN
    file_iso = 'fld_zoo_iso.def'
   ENDIF

   IF debug_w THEN BEGIN
    print, ' field.origin in fld_pltint: ', field.origin
   ENDIF

   print,' '
   print,'    Reading iso/mult of ',var_name,' from : ', file_iso

   spawn, 'grep -i "\ '+var_name+' " '+hom_def+file_iso, line
   line = strcompress(strtrim(line[0], 2))
   length = strlen(line)

   IF length EQ 0 THEN BEGIN

      print, '    *** field_pltint : no iso/mult for field ', var_name, $
       ' in file ', file_iso
      fldintr.int = !VALUES.F_NAN
      fldintr.mult = 1.0
      fldintr.add = 0.0
      fldintr.unit = ''
      fldintr.mid = 0.0

   ENDIF ELSE BEGIN
      argvar = strsplit(line, ' ', /EXTRACT)
      IF strmid(argvar[1], 0, 1) EQ '@' THEN BEGIN
                                ; get info from other variable
         friend_name = strmid(argvar[1], 1, strlen(argvar[1])-1)
         print, '      -> linked to ', friend_name
         fldintr = fld_pltint(friend_name, plttyp, dimplot, hotyp)
      ENDIF ELSE BEGIN
         fldintr.int = float(argvar[3])
                                ; separation for iso =0 if difference
                                ; plot
         CASE field.origin OF
            'diff': BEGIN
               fldintr.mid = 0.
               fldatt.max = fldatt.dmax
               fldatt.min = -fldatt.dmax
               fldatt.homax = fldatt.max
               fldatt.homin = fldatt.min
               fldatt.max1d = fldatt.max
               fldatt.min1d = fldatt.min
            END
            ELSE: fldintr.mid = float(argvar[5])
         ENDCASE
         IF size(argvar, /N_ELEMENTS) eq 8 OR field.origin EQ 'div' THEN BEGIN ; OR field.origin EQ 'diff'
            fldintr.mult = 1.0
            fldintr.add = 0.0
            fldintr.unit = ''
         ENDIF ELSE BEGIN
            IF field.origin EQ 'diff' AND float(argvar[8]) EQ 1. THEN BEGIN
               fldintr.mult = 1.0
               fldintr.add = 0.0
               fldintr.unit = ''
            ENDIF ELSE BEGIN 
               fldintr.mult = float(argvar[8])
               fldintr.add = float(argvar[9])
               fldintr.unit = argvar[10]
            ENDELSE 
         ENDELSE
         tempvar = SIZE(TEMPORARY(argvar))
      ENDELSE
   ENDELSE

   fldintr.name = var_name

   return, fldintr
END
