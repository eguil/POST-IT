;+
;
; get remote file
;
; @param FILE_NAME {in}{required}{type=string}
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @examples
;
; IDL> file_name='ginette'
; IDL> ncdf_db='local:./'
; IDL> data_bases=['local:./'] ++
; IDL> get_file,file_name, ncdf_db
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; get rid of spawn
;
; explication common DATA_BASES
;
; realistic examples
;
; @history
; - fplod 20091210T091413Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: get_file.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
PRO get_file, file_name, ncdf_db
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='get_file, file_name, ncdf_db'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(file_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type file_name' $
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

 common_type=size(data_bases,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type data_bases' $
          + '!C' $
          + 'Usage : ' + usage])
  stop
 ENDIF

   str = strsplit(ncdf_db, ':', /EXTRACT)
   machine = str[0]
   path_remote = str[1]

   IF machine NE 'local' THEN BEGIN

      print, '    Data base = '+ncdf_db
      print, ' '

      ; check if file is in default directory

      local_dir = (strsplit(data_bases[0], ':', /EXTRACT))[1]

      list = findfile(local_dir+file_name, count = nb_file)

      IF nb_file EQ 0 THEN BEGIN

         print, '    '+machine+'.get '+file_name+' '+path_remote+' '+local_dir
         spawn, hom_def+'remote/'+machine+'.get '+file_name+' '+path_remote+' '+local_dir, result
         print, result

      ENDIF ELSE BEGIN
         print, '    File '+file_name+' already in ',local_dir & print, ''
      ENDELSE

      ncdf_db = data_bases[0]

   ENDIF

END
