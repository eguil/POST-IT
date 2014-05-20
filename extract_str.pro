;+
;
; extract sub-string of line that is between char1 and char2
;
; @categories
; String
;
; @param LINE {in}{required}{type=string}
; string to be exam
;
; @param CHAR1 {in}{required}{type=string}
; first character of the substring to
; be extracted from LINE
;
; @param CHAR2 {in}{required}{type=string}
; last character of the substring to
; be extracted from LINE
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> line='A1234B678'
; IDL> char1='A'
; IDL> char2='B'
; IDL> result=extract_str(line, char1, char2)
; IDL> print, result
; 1234
;
; @history
;
; - fplod 20091208T165219Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: extract_str.pro 235 2010-06-25 08:46:06Z ericg $
;
;-
FUNCTION extract_str, line, char1, char2
;
  compile_opt idl2, strictarrsubs
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=extract_str(line, char1, char2)'
;
 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    mn = '???'
    return, mn
 ENDIF
;
 arg_type = size(line,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type line' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF
;
 arg_type = size(char1,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type char1' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(char2,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type char2' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF


   pos_char1 = strpos(line,char1)
   pos_char2 = strpos(line,char2)
   lenght_to_extract = pos_char2-pos_char1-1

   IF lenght_to_extract LE 0 THEN BEGIN
      print, 'No sub-string in ', line, ' between in : ', char1, char2
      return, -1
   ENDIF

   return, STRMID(line, pos_char1+1, lenght_to_extract)
END
