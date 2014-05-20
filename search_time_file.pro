;+
;
; look for time serie file with date min =< cmd.date1 and date max => cmd.spec
;
; @param CMD {in}{required}{type=structure}
; command line of window
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @param SUFFIX {in}{required}{type=string}
;
; @param DELTA_T1 {out}{type=integer}
;
; @param DATE1 {out}{type=string}
; [yy..yyy][mm][dd]-[sssss] if <n>s
; [yy..yyy][mm][dd] if <n>d
; [yy..yyy][mm]     if <n>m
; [yy..yyy]         if <n>y
;
; @param DATE2 {out}{type=string}
; [yy..yyy][mm][dd]-[sssss] if <n>s
; [yy..yyy][mm][dd] if <n>d
; [yy..yyy][mm]     if <n>m
; [yy..yyy]         if <n>y
;
; @param TIMAVEF {out}{type=string}
;
; output : data file date1 and date2
;
; @examples
;
; IDL> cmd={timave:'1m',date1:'199201',spec:'-',exp:'HH',var:'temp_1'}
; IDL> ncdf_db='local:./'
; IDL> suffix=''
; IDL> search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef
; IDL> print,  date1, date2,delta_t1,timavef
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; get rid of spawn
;
; error handling
; realistic examples
;
; @history
;
; - fplod 20091210T150954Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: search_time_file.pro 230 2010-06-24 10:46:53Z ericg $
;
;-
PRO search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2

   IF debug_w THEN BEGIN
    info = report('enter ...')
    print, '       cmd at top of proc = ', cmd
    print, '       suffix at top of proc = ', suffix
   ENDIF
; 
 usage='search_time_file, cmd, ncdf_db, suffix, date1, date2, delta_t1, timavef'
;
 nparam = N_PARAMS()
 IF (nparam LT 7) THEN BEGIN
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

 tag=WHERE(STRMATCH(arg_struct_tags, 'VAR'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag VAR cmd' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(cmd.var,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.grid' $
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

 arg_type = size(suffix,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type suffix' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

   timavef = cmd.timave
   date1 = cmd.date1
   date2 = cmd.spec

   found = 1
   delta_t1 = 0
   suff_var = ''

   IF strpos(cmd.timave, 'mm') NE -1 THEN BEGIN
    meanm = 'yes'
   ENDIF ELSE BEGIN
    meanm = 'no'
   ENDELSE

   CASE meanm OF

      'no': BEGIN ; No mean month case

         file_name_root = cmd.exp+'_'+cmd.timave+'_'

         ; local or remote DB
         machine = (strsplit(ncdf_db, ':', /EXTRACT))[0]
         CASE machine OF
            'local': BEGIN
               ; field name added (1 variable)
               variab = '_'+cmd.var
               print, '    Looking for '+(strsplit(ncdf_db, ':', /EXTRACT))[1]+file_name_root+'*'+suffix+'.nc ...'
               spawn, 'ls '+(strsplit(ncdf_db, ':', /EXTRACT))[1]+file_name_root+'*'+suffix+'.nc', line2
               line = line2
            END
            ELSE: BEGIN
               spawn, 'grep -i " '+machine+' " '+hom_def+'remote/remote.def', rem_line
               nrem = n_elements(rem_line)
               IF nrem EQ 1 THEN BEGIN
                  rem_line = strcompress(strtrim(rem_line[0], 2))
                  argvar = strsplit(rem_line, ' ', /EXTRACT)
                  remote_name = argvar[1]
                  remote_login = argvar[2]
                  print, '    rsh '+remote_login+'@'+remote_name+' ls '+$
                   (strsplit(ncdf_db, ':', /EXTRACT))[1]+file_name_root+'*'+suffix+'.nc'
                  print, ''
                  spawn, 'rsh '+remote_login+'@'+remote_name+' ls '+$
                   (strsplit(ncdf_db, ':', /EXTRACT))[1]+file_name_root+'*'+suffix+'.nc', line
               ENDIF ELSE BEGIN
                  print, ' *** search_file : '+machine+' unknown in config/remote'
               ENDELSE
            END
         ENDCASE

         nfiles = n_elements(line)

         print, '     -> ', strtrim(string(nfiles), 2), ' file(s) found for date1 '+date1+' and date2 '+date2+':'
         print, '      ', line
         print, ' '

         ; identify right file
         ; if ensemble, date1 is set by cmd.date1

         il = 0
         found = 0
         delta_1 = 0L
         WHILE il LE nfiles-1 AND found EQ 0 DO BEGIN
            file = line[il]
              ; remove root directory if exists
            IF strpos(file, '/') NE -1 THEN BEGIN
               argvar = strsplit(file, '/', /EXTRACT)
               n = n_elements(argvar)
               file = argvar[n-1]
            ENDIF ELSE BEGIN
               print, '  *** File missing or'
               print, '  *** Wrong definition for database: ', file
               stop
            ENDELSE
            IF debug_w THEN BEGIN
               print, '    file = ', file
            ENDIF
            argvar = strsplit(file, '_', /EXTRACT)
            dat1 = argvar[2]
            dat2 = argvar[3]
            
            IF debug_w THEN BEGIN
               print, '    dat1, date1, dat2, date2 ', dat1,' ', date1,' ', dat2,' ', date2
            ENDIF
            
            IF ensbl_code NE '' AND toggle_ensbl_diff EQ 0 THEN BEGIN 
               IF  ensbl_lt NE 0 THEN BEGIN ; ensemble case
                  IF (long(dat1) EQ long(date1)) THEN BEGIN ; only look for initial date
                     found = 1
                     delta_t1 = ensbl_lt-1 ; (index set by lead time <date1>LT<n>)
                     date1 = dat1
                     date2 = dat2
                     cmd.date1 = date1
                     cmd.spec = date2
                     var_file = strpos(file, cmd.var) 
                  ENDIF
               ENDIF 
            ENDIF ELSE BEGIN ; general case 
                   
               IF strpos(dat1, '_') EQ -1 AND strpos(dat2, '_') EQ -1 AND $
                strpos(dat1, '-') EQ -1 AND strpos(dat2, '-') EQ -1 THEN BEGIN
                  IF (long(dat1) LE long(date1) AND long(dat2) GE long(date2)) THEN BEGIN
                     IF debug_w THEN BEGIN  
                        print, ' il, file found: ',  il,  file
                     ENDIF 
                     found = 1
                     delta_t2 = compute_time(cmd.timave, dat1, date1)
                     delta_t1 = delta_t2.count-1
                     date1 = dat1
                     date2 = dat2
                     var_file = strpos(file, cmd.var) 
                  ENDIF   
               ENDIF ELSE BEGIN
                  found = 1
                  nfiles = 1
                  delta_t1 = 0
               ENDELSE
            ENDELSE 
            
            il = il+1
         ENDWHILE
      END
      'yes': BEGIN ; Mean month case
         CASE strmid(cmd.timave, strlen(cmd.timave)-2, 1) OF
            'm': BEGIN          ; mean month case
               nmonths = long(strmid(cmd.timave, 0,strlen(cmd.timave)-2))
               date1 = '01'
               ndate = string(long(12/nmonths), format = '(I2.2)')
               date2 = ndate+'_'+strmid(cmd.date1, 3, strlen(cmd.date1)-3)
               timavef = strmid(cmd.timave, 0,strlen(cmd.timave)-2)+'mm'
            END
            ELSE:
         ENDCASE
         delta_t1 = 0
         nfiles = 1
         found = 1
      END
      ELSE:
   ENDCASE

   IF nfiles EQ 0 OR found EQ 0 THEN BEGIN
    date1 = '???'
    date2 = '???'
   ENDIF

END
