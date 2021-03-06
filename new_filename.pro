;+
;
; change grid_oldgrid by grid_newgrid
;
; @param FILE_IN {in} {required} {type=string}
;
; @param OLDGRID {in}
;
; @param NEWGRID {in} {required} {type=string}
;
; @returns
; file out
; -1 in case of error
;
; @examples
;
; IDL> file_in='grids_ncpt62.nc'
; IDL> newgrid='ginette'
; IDL> result= new_filename(file_in, '', newgrid)
; IDL> print, result
; gridginette_ncpt62.nc
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
;
; do we really need common and com_eg ?
;
; oldgrid unused
;
; parametrizisation of prefixe ( now 'grids_') used to determine
; grid localization in file name.
;
; @history
;
; - fplod 20091210T140929Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: new_filename.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION new_filename, file_in, oldgrid, newgrid
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=new_filename(file_in, oldgrid, newgrid)
 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(file_in,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type file_in' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(newgrid,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type newgrid' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

   file_nam = strmid(file_in, 0,strpos(file_in, 'grid_')+5 )
   file_out = file_nam+newgrid+strmid(file_in, strpos(file_in, 'grid_')+6, strlen(file_in)-1)

   return, file_out

END
