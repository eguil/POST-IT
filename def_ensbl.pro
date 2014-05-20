;+
;
; define properties of ensemble - uses array ensemble_base_description defined in post-it.pro
;<EXPID>_ens = <timeunit>/<inidate>/<lastdate>/<number of models>/<number of members>/<lenght of runs in timeunit>
; @param EXPID {in}{required}{type=string}
;
; @returns
; -1 in case of error
; @examples
;
; IDL> ensemble_base_decription=
; IDL> expid=
; IDL> result=def_ensbl(expid)
; IDL> print, result
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; check commons
;
; examples
; @history
;
;
;   * check parameters
;
; @version
;
;-
FUNCTION def_ensbl, expid
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=def_ensbl(expid)'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(expid,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type expid' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

   nbase = n_elements(ensemble_base_description)-1
   ibase = 0

; loop on ensemble definition

   WHILE ibase LE nbase DO BEGIN
      argvar = strsplit((ensemble_base_description[ibase])[0], ' ', /EXTRACT)
      IF strmid(argvar[0], 0, strlen(argvar[0])-4) EQ expid THEN BEGIN
         ensbldef = (strsplit((ensemble_base_description[ibase])[0], ' ', /EXTRACT))[2]
         ensblmods = (strsplit((ensemble_base_description[ibase])[0], ' ', /EXTRACT))[3]
         GOTO, fin
      ENDIF
      ibase = ibase + 1
   ENDWHILE

   print, '  ENSEMBLE definition missing for ', expid
   return, -1

fin:
   ; define each property and put in structure ensbl_def
   argdef = strsplit(ensbldef, '/', /EXTRACT)
   names_arr = strarr(long(argdef[3]))
   ensbl_def = {timeunit:'', inidate:'', lastdate:'', models: 0, members: 0, length:0, model_names:names_arr}

   ensbl_def.timeunit = argdef[0]
   ensbl_def.inidate = argdef[1]
   ensbl_def.lastdate = argdef[2]
   ensbl_def.models = argdef[3]
   ensbl_def.members = argdef[4]
   ensbl_def.length = argdef[5]
   ensbl_def.model_names = strsplit(ensblmods, '/', /EXTRACT)

   IF debug_w THEN BEGIN
      print, '    ensbl.def structure in def_ensbl.pro:', ensbl_def
   ENDIF 

   return, 0

END
