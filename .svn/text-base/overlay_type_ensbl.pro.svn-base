;+
;
; overlay color line types for ensembles COL1D=col1d,STY1D=sty1d
;
; @param IOVER {in}{required}{type=integer}
;
; @param IENS {in}{required}{type=integer}
;
; @keyword MEMBER for individual member (line thickness = )
;
; @keyword MEAN for ensemble mean (line thickness = )
;
; @returns
; -1 in case of error
;
; @examples <TODO>
;
; IDL> iover=1
; IDL> iens=1
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
; $Id: overlay_type.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION overlay_type_ensbl, iover, iens, MEMBER = member, MEAN = mean
;
  compile_opt idl2, strictarrsubs
;
@com_eg

;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=overlay_type(iover, iens)'
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

 arg_type = size(iens,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type iens' $
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

   IF iens EQ 0 THEN BEGIN 
      overc = ',ov1d='+strtrim(string((iover-1) < 1), 2)
   ENDIF ELSE BEGIN 
      overc = ',ov1d=1'
   ENDELSE 
        
; overlay color

   colov = ''

   IF iover EQ 1 AND iens EQ 0 THEN BEGIN
 ;     make palette
      red =   [0, 255,   0,   0, 255, 255, 0  ]
      green = [0,   0, 255,   0,   0, 255, 255]
      blue =  [0,   0,   0, 255, 255,   0, 255]
      
      red = [0, red, red, red, red, red, red, red ]
      green = [0, green, green, green, green, green, green, green]
      blue = [0, blue, blue, blue, blue, blue, blue, blue ]
      tvlct, red, green, blue
   ENDIF

   IF pal_type EQ 'col' THEN BEGIN
      CASE ensbl_code OF
         'mame': BEGIN 
            colov = ',COLOR='+string(line_color[index_over+iens-1])
         END
         ELSE: BEGIN 
            IF ensbl_color_member EQ 0 THEN BEGIN 
               colov = ',COLOR='+string(line_color[index_over+iover-2])
            ENDIF ELSE BEGIN
               colov = ',COLOR='+string(line_color[index_over+iens-1])
            ENDELSE 
         END 
      ENDCASE 
   ENDIF

;   thickness amd style read from plt_map

   IF ensbl_color_member EQ 0 THEN BEGIN 
      IF keyword_set(MEAN) OR ensbl_code EQ 'mame' THEN BEGIN 
         line_thick_txt = ', thick='+string(ensbl_thick_mean)
         line_style_txt = ', linestyle='+string(ensbl_style_mean-1)
      ENDIF ELSE BEGIN 
         line_thick_txt = ', thick='+string(ensbl_thick_member)
         line_style_txt = ', linestyle='+string(ensbl_style_member-1)
      ENDELSE 
   ENDIF ELSE BEGIN 
      line_thick_txt = ', thick='+string(line_thick[index_over+iens-1])
      line_style_txt = ', linestyle='+string(line_style[index_over+iens-1]-1)
   ENDELSE 

; result

   overc = overc+colov+line_thick_txt+line_style_txt

   IF debug_w THEN BEGIN 
      print, '   iover, iens, index_over = ', iover, iens, index_over
      print, '   overc in overlay_type_ensbl = ', overc
   ENDIF 

   return, overc
END
