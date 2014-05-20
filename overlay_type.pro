;+
;
; overlay color line types COL1D=col1d,STY1D=sty1d
;
; @param IOVER {in}{required}{type=integer}
;
; @param DIMPLOT {in}{required}{type=integer}
;
; @keyword FORCE to force ov1d=1
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> dimplot=1
; IDL> iover=1
; IDL> index_over=1
; IDL> pal_type = 'col'
; IDL> line_color = [1,1,2,4,2,4,7,8]
; IDL> line_thick = [2,2,2,2,2,2,2,2]
; IDL> line_style = [1,3,1,1,3,3,3,3]
; IDL> result=overlay_type(iover, dimplot)
; IDL> print,result
; ,ov1d=0,COLOR=       4, thick=       2, linestyle=           0
;
; @uses
; <propost_it>com_eg</propost_it>
;
; @todo
; explication index_over, pal_type, line_color, line_thick, line_style
;
; @history
;
; - fplod 20091210T144051Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;   * syntax of array
;
; @version
; $Id: overlay_type.pro 247 2010-08-05 14:44:44Z ericg $
;
;-
FUNCTION overlay_type, iover, dimplot, FORCE = force, NOADD = noadd
;
  compile_opt idl2, strictarrsubs
;
@com_eg

;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=overlay_type(iover, dimplot)'
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(iover,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type iover' $
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

 common_type = size(index_over,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type index_over' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type = size(pal_type,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type pal_type' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type = size(line_color,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type line_color' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type = size(line_thick,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type line_thick' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type = size(line_style,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type line_style' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

; overlay switch

   overc = ',ov1d='+strtrim(string((iover-1) < 1), 2)

   IF iover EQ 1 THEN BEGIN
    overc = ',ov1d=0'
   ENDIF

   IF keyword_set(force) THEN BEGIN
    overc = ',ov1d=1'
   ENDIF

   IF keyword_set(noadd) THEN BEGIN
      noadd = 1
   ENDIF ELSE BEGIN 
      noadd = 0
   ENDELSE 
      
     
; overlay color

   colov = ''

   IF dimplot EQ 1 AND noadd NE 1 THEN BEGIN
    index_over = index_over + 1
   ENDIF
   IF dimplot EQ 2 THEN BEGIN
    index_over = 1
   ENDIF

   IF index_over EQ 1 AND dimplot EQ 1 THEN BEGIN
;      index_over = 2
;      make palette
      red =   [0, 255,   0,   0, 255, 255, 0  ]
      green = [0,   0, 255,   0,   0, 255, 255]
      blue =  [0,   0,   0, 255, 255,   0, 255]
      red = [0, red, red, red, red, red, red, red ]
      green = [0, green, green, green, green, green, green, green]
      blue = [0, blue, blue, blue, blue, blue, blue, blue ]
      IF debug_w THEN BEGIN 
         print, ' -> Defining RGB color palette in overlay_type.pro'
      ENDIF 
      tvlct, red, green, blue
      ENDIF

   IF debug_w THEN BEGIN 
      print, '   iover, index_over = ', iover, index_over
   ENDIF 

   IF pal_type EQ 'col' AND index_over GE 1 THEN BEGIN
    colov = ',COLOR='+string(line_color[index_over-1])
   ENDIF

; overlay thickness

   line_thick_txt = ', thick='+string(line_thick[index_over-1])

; overlay style

   line_style_txt = ', linestyle='+string(line_style[index_over-1]-1)

; result

   IF dimplot EQ 1 THEN BEGIN
    overc = overc+colov+line_thick_txt+line_style_txt
   ENDIF
;   IF dimplot EQ 2 THEN BEGIN
;    overc = overc+line_thick_txt+line_style_txt
;   ENDIF
   IF dimplot EQ 2 THEN BEGIN
      overc = overc+line_style_txt
  ENDIF

   IF debug_w THEN BEGIN 
      print, '   overc in overlay_type = ', overc
   ENDIF 

   return, overc
END
