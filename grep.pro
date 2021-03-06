;+
;
; grep
;
; @param COMMAND {in}{required}{type=string}
;
; @param SEPARATOR {in}{required}{type=string}
;
; @param INDEX {in}{required}{type=integer}
;
; @returns
; -1 in case of error
;
; @todo
; get rid of spawn
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: grep.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION grep, command, separator, index
;
  compile_opt idl2, strictarrsubs
;
 usage='result=grep(command, separator, index)

 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(command,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type command' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(separator,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type separator' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(index,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type index' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

  spawn, command, line
   line = strcompress(strtrim(line[0]))
   argvar = strsplit(line, separator, /EXTRACT)

return, argvar[index]

END
