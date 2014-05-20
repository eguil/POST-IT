;+
;
; decode command line of post-it
;
; @param CMDLINE {in}{required}{type=array of strings}
; array of command lines
;
; @param INDEX {in}{required}{type=integer}
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> cmdline=['temp_1 0 HH1 umath xy 1m 199201 - 1 1 v']
; IDL> index=0
; IDL> trend_typ='0'
; IDL> result = decode_cmd(cmdline, index)
; IDL> help,result,/structure
;   VAR             STRING    'temp_1'
;   ON              LONG                 0
;   EXP             STRING    'HH1'
;   GRID            STRING    'umath'
;   PLT             STRING    'xy'
;   TIMAVE          STRING    '1m'
;   DATE1           STRING    '199201'
;   SPEC            STRING    '-'
;   DISP            STRING    '1'
;   PROJ            STRING    '1'
;   OUT             STRING    'v'
;   VAR2            STRING    '-'
;   TREND           STRING    '0'
; IDL> print, result
;{ temp_1           0 HH1 umath xy 1m 199201 - 1 1 v - 0}
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; error handling
;
; explication sur common trend_typ
;
; @history
; - fplod 20100115T091459Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * enhanced check of cmdline content
;
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: decode_cmd.pro 219 2010-05-21 22:09:09Z ericg $
;
;-
FUNCTION decode_cmd, cmdline, index
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=decode_cmd(cmdline, index)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(cmdline,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmdline' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_dim =  size(cmdline,/n_elements)
 IF (arg_dim LT 1) THEN BEGIN
  ras = report(['Incorrect arg dimension cmdline' $
         + '!C' $
         + 'Usage : ' + usage])
  return, -1
 ENDIF

 arg_type = size(index,/type)
 IF ((arg_type NE 3) AND (arg_type NE 2)) THEN BEGIN
   ras = report(['Incorrect arg type index' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 IF (index GE N_ELEMENTS(cmdline)) THEN BEGIN
   ras = report(['index too big' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type=size(trend_typ,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type trend_typ' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 cmd = {var:'', on: 0, exp:'', grid:'',plt:'', timave:'', date1:'', spec:'',$
          disp:'', proj:'', out:'', var2:'', trend:''}

 line = strcompress(strtrim(cmdline[index], 2))
 argvar = strsplit(line, ' ', /EXTRACT)
 cmd_dim =  size(argvar,/n_elements)
 IF (cmd_dim LT 11) THEN BEGIN
  ras = report(['Incorrect number of fields in cmdline[' $
         + strtrim(string(index),2) + ']' $
         + '!C' $
         + 'cmdline[' + strtrim(string(index),2) + '] : ' $
         + cmdline[index] ])
  return, -1
 ENDIF
   cmd.var    = argvar[0]
   cmd.on     = argvar[1]
   cmd.exp    = argvar[2]
   cmd.grid   = argvar[3]
   cmd.plt    = argvar[4]
   cmd.timave = argvar[5]
   cmd.date1  = argvar[6]
   cmd.spec   = argvar[7]
   cmd.disp   = argvar[8]
   cmd.proj   = argvar[9]
   cmd.out    = argvar[10]

   cmd.var2 = '-'

; trend type

   cmd.trend = trend_typ
   IF strpos(cmd.timave, '@') GT 1 THEN BEGIN
      letter = strmid(cmd.timave, strpos(cmd.timave, '@')+1,1)
      CASE letter OF
         't': BEGIN
            cmd.trend = strmid(cmd.timave, strpos(cmd.timave, '@t')+2, strlen(cmd.timave)-strpos(cmd.timave, '@t')-2)
            cmd.timave = strmid(cmd.timave,0, strpos(cmd.timave, '@t'))
         END
         ELSE: BEGIN
            print, ' **** unknown letter after @ in cmd.timave (try t):', letter
         END
      ENDCASE
   ENDIF
; Lead time in date1/spec in case of ensembles
   IF strpos(cmd.date1, 'LT') GT 1 THEN BEGIN
      ensbl_lt = long(strmid(cmd.date1, strpos(cmd.date1, 'LT')+2, strlen(cmd.date1)))
      cmd.date1 = strmid(cmd.date1, 0, strpos(cmd.date1, 'LT'))
   ENDIF ELSE BEGIN 
      ensbl_lt = 0
   ENDELSE 
   IF strpos(cmd.spec, 'LT') GT 1 THEN BEGIN
      ensbl_lt2 = long(strmid(cmd.spec, strpos(cmd.spec, 'LT')+2, strlen(cmd.spec)))
      cmd.spec = strmid(cmd.spec, 0, strpos(cmd.spec, 'LT'))
   ENDIF ELSE BEGIN 
      ensbl_lt2 = 0
   ENDELSE 

   return, cmd
END
