;+
;
; find data base of expid - uses array data_bases
;
; @param EXPID {in}{required}{type=string}
;
; @returns
; -1 in case of error
; @examples
;
; IDL> data_bases=
; IDL> spec_bases=
; IDL> expid=
; IDL> result=def_dbase(expid)
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
; - fplod 20091209T094824Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: def_dbase.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION def_dbase, expid
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=def_dbase(expid)'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    datebase = NaN
    return, database
 ENDIF
 arg_type = size(expid,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type expid' $
          + '!C' $
          + 'Usage : ' + usage])
    datebase = NaN
    return, database
 ENDIF

; special suffix ?

   nbases = n_elements(spec_bases)-1
   ibase = 0

; loop on spec database definition

   WHILE ibase LE nbases - 1 DO BEGIN
      argvar = strsplit((spec_bases[ibase])[0], ' ', /EXTRACT)
      IF strpos(expid, argvar[0]) NE -1 THEN BEGIN
         database = (strsplit((spec_bases[ibase])[0], ' ', /EXTRACT))[2]
         GOTO, fin
      ENDIF
      ibase = ibase + 1
   ENDWHILE

   nbase = n_elements(data_bases)-1
   ibase = 1

; loop on database definition

   WHILE ibase LE nbase - 1 DO BEGIN
      argvar = strsplit((data_bases[ibase])[0], ' ', /EXTRACT)
      IF strmid(argvar[0], 0, strlen(argvar[0])-3) EQ expid THEN BEGIN
         database = (strsplit((data_bases[ibase])[0], ' ', /EXTRACT))[2]
         GOTO, fin
      ENDIF
      ibase = ibase + 1
   ENDWHILE

; default data base : data_bases[0]

   ibase = 0
   database = (strsplit((data_bases[ibase])[0], ' ', /EXTRACT))[2]

fin:
   ; check if common variable path used

   ind1 = strpos(database, '<')
   IF ind1 NE -1 THEN BEGIN
      database_split = strsplit(database, '<>', /EXTRACT)
      path_base = database_split[1]
      command = 'path_var = '+path_base
      res = execute(command)
      database_split[1] = path_var
      database = strjoin(database_split)
      database = strsed(database, '//', '/')
   ENDIF

   return, database

END
