;+
;
; grep + sed
;
; @param COMMAND1 {in}{required}{type={string}
;
;
; @param COMMAND2 {in}{required}{type={string}
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
; $Id: grepsed.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-

FUNCTION grepsed, command1,command2
;
  compile_opt idl2, strictarrsubs
;
 usage='result=grepsed(command1,command2)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(command1,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type command1' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(command2,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type command2' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

   spawn, command1+'|'+command2, line

return, line[0]

END
