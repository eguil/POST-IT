;+
;
; find hovmoeller box in Defaults file
;
; @param PLT {in}{required}{type=string}
;
; @param DIMPLOT {in}{required}{type=integer}
;
; @param BOXDEF {out}{type=string}
;
; @param STRIDE {out}{type=integer}
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> plt='xy'
; IDL> dimplot=1
; IDL> box_h=[0, 0, 0, 0]
; IDL> result=def_box(plt, dimplot, boxdef, stride)
; IDL> print, result, boxdef, stride
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; get rid of spawn
;
; boxdef out ? est-ce bien normal dans une fonction
; stride out ? est-ce bien normal dans une fonction
;
; explication sur box_h et def_stride
;
; @history
; - fplod 20100114T134125Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * boxdef and stride are output variables
;
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: def_box.pro 219 2010-05-21 22:09:09Z ericg $
;
;-
FUNCTION def_box, plt, dimplot, boxdef, stride
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 IF debug_w THEN BEGIN
  info = report('enter ...')
 ENDIF
;
 usage='result=def_box(plt, dimplot, boxdef, stride)'
;
 nparam = N_PARAMS()
 IF (nparam LT 4) THEN BEGIN
   ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF
 arg_type = size(plt,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type plt' $
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

 common_type=size(def_stride,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type def_stride' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type=size(box_h,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type box_h' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF
 common_nelem=size(box_h,/n_elements)
 IF (common_nelem NE 4) THEN BEGIN
   ras = report(['Incorrect common n_elements box_h' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

   plt_test = plt
   IF strpos(plt, '@') GT 1 THEN BEGIN
      plt_test = strmid(plt, 0, strpos(plt, '@'))
   ENDIF
   CASE strmid(plt_test, 0, 2) OF
      'xy': boxdef = strmid(plt_test, 3, strlen(plt_test)-3)
      'z_': boxdef = strmid(plt_test, 2, strlen(plt_test)-2)
      'y_': boxdef = strmid(plt_test, 2, strlen(plt_test)-2)
      'x_': boxdef = strmid(plt_test, 2, strlen(plt_test)-2)
      'zt': boxdef = strmid(plt_test, 3, strlen(plt_test)-3)
      ELSE: boxdef = strmid(plt_test, dimplot+1, strlen(plt_test)-(dimplot+1))
   ENDCASE

   force_all_data_read = 1

   IF debug_w THEN BEGIN
    print, '      boxdef stage 1 = ',boxdef
   ENDIF

   ; define box from pseudo-3Dmask

   pseudo_3d_msk = ''

   IF strmid(boxdef, 0, 1) EQ '#' THEN BEGIN
      pseudo_3d_msk = strmid(boxdef, 1, strlen(boxdef)-1)
      boxdef = 'global'

   ENDIF

   IF boxdef NE 'global' AND boxdef NE '' THEN BEGIN

      file_b = hom_def+'domain_boxes.def'

      spawn, 'grep -i " '+boxdef+' " '+file_b, line
      line = strcompress(strtrim(line[0], 2))
      length = strlen(line)

      IF length EQ 0 THEN BEGIN

         print, '  *** Box ', boxdef, ' not found in file '
         print, file_b
         return, -1

      ENDIF ELSE BEGIN
         argvar = strsplit(line, ' ', /EXTRACT)
         box = argvar[1]
         box = float(strsplit(argvar[1], '/', /EXTRACT))
         stride = long(argvar[2])
      ENDELSE

   ENDIF ELSE BEGIN
      box = box_h
      stride = def_stride
   ENDELSE

   IF debug_w THEN BEGIN
    print, '      boxdef, box final = ', boxdef, ' ', box
    info = report('leaving ...')
   ENDIF

   return, box
END
