;+
;
; define file name following
; <expid>_<timeave>_<date1>_<date2>[_<spec>]_<grid> convention
;
; @param CMD {in}{required}{type=structure}
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @param FILE_NAME {out}{type=string}
;
; @param DELTA_T1 {out}{type=integer}
;
; @examples
;
; IDL> cmd={timave:'1m',date1:'199201',plt:'xy',spec:'-',exp:'HH1'}
; IDL> ncdf_db = 'local:./'
; IDL> mesh_type2=''
; IDL> suff_domain=''
; IDL> file_suff_var=''
; IDL> def_file_name, cmd, ncdf_db, file_name, delta_t1
; IDL> print,file_name,delta_t1
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
;
; explication sur mesh_type2,  suff_domain, file_suff_var
;
; realistic example
;
; @history
;
; - fplod 20100114T132123Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * delta_t1 is an output parameter
;
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: def_file_name.pro 216 2010-04-01 13:17:02Z ericg $
;
;-
PRO def_file_name, cmd, ncdf_db, file_name, delta_t1
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
    print, '       cmd at top of proc = ', cmd
   ENDIF

 usage='def_file_name, cmd, ncdf_db, file_name, delta_t1'
;
 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(cmd,/type)
 IF (arg_type NE 8) THEN BEGIN
   ras = report(['Incorrect arg type cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_struct_tags=TAG_NAMES(cmd)

 tag=WHERE(STRMATCH(arg_struct_tags, 'TIMAVE'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag TIMAVE cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(cmd.timave,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.timave' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'DATE1'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag DATE1 cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(cmd.date1,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.date1' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'PLT'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag PLT cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(cmd.plt,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.plt' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'SPEC'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag SPEC cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(cmd.spec,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.spec' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'EXP'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag EXP cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(cmd.exp,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.exp' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(ncdf_db,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type ncdf_db' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 common_type=size(mesh_type2,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type mesh_type2' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 common_type=size(suff_domain,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type suff_domain' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 common_type=size(file_suff_var,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type file_suff_var' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

; define date1 and date2

   timavef = cmd.timave
   date1 = cmd.date1

   suffix = '_'+mesh_type2+suff_domain+file_suff_var

   base_suffix = '_'+mesh_type2

   f_suffix = suffix

   IF debug_w THEN BEGIN
    print, '     suffix before search_time_file = ', suffix
   ENDIF

   CASE strmid(cmd.plt, 0, 2) OF
      'xt': search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef
      'yt': search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef
      'zt': search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef
      't_': search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef
      ELSE: BEGIN
         CASE strmid(cmd.plt, 0, 3) OF
             'xyt': search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef
             ELSE: BEGIN
                 cmdspec_b = cmd.spec
                 cmd.spec = cmd.date1
                 search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef
                 cmd.spec = cmdspec_b
             END
         ENDCASE
      END
   ENDCASE
   IF debug_w THEN BEGIN
    print, '     suffix after search_time_file = ', suffix
    print, '     date1, date2 = ', date1, ' ', date2
   ENDIF

   IF date1 EQ '???' THEN BEGIN
      print, ' *** File not found: check dates in post_it.pro line !'
      stop
   ENDIF

   file_name = cmd.exp+'_'+timavef+'_'+date1+'_'+date2+suffix+'.nc'
   base_file_name = cmd.exp+'_'+timavef+'_'+date1+'_'+date2

   IF debug_w THEN BEGIN
    print, '     vargrid = ', vargrid
    print, '     file_name = ',file_name
    info = report('leaving ...')
   ENDIF

END
