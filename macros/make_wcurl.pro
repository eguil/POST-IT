;+
;
; make curl of vector field
;
;
; @param FILE_NAME {in}{required}{type=string}
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @keyword BOXZOOM
;
; @keyword TIME_1
;
; @keyword TIME_2
;
; @keyword ALL_DATA
;
; @keyword ZMTYP
;
; @returns
; structure
; -1 in case of error

; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_wcurl.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
FUNCTION make_wcurl, file_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ALL_DATA=all_data $
         , ZMTYP=zmtyp
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
   IF debug_w THEN BEGIN
    info = report('enter ...')
   ENDIF

 usage='result=make_wcurl(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ALL_DATA=all_data ' $
         + ', ZMTYP=zmtyp)'

 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(file_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type file_name' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(ncdf_db,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type ncdf_db' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

   var_name1 = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[0]
   var_name2 = (strsplit(macro_base_fld, ',', ESCAPE = ' ', /EXTRACT))[1]

;  Build file_name2 if different

   IF strpos (cmd1_back.grid, '#') NE -1 THEN BEGIN
      varpos = strpos(file_name, var_name1)
      file_name2= strmid(file_name, 0, varpos)+var_name2+'.nc'
      IF debug_w THEN BEGIN
       print, '   file_name2 = ', file_name2
      ENDIF
   ENDIF ELSE BEGIN
      file_name2 = file_name
   ENDELSE

;
;  Read the variables in the correspondent netcdf file
   var_x = nc_read(file_name, var_name1, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
   var_y = nc_read(file_name2, var_name2, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)

; TO DO: ORCA grid C case
;   vargrid = 'U'
;   new_file = new_filename(file_name, 'T', vargrid)
;   taux = nc_read(new_file,'sozotaux', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
;   vargrid = 'V'
;   new_file = new_filename(file_name, 'T', vargrid)
;   tauy = nc_read(new_file,'sometauy', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)

;   for ORCA C grid vcurl = curl(var_x, var_y)

   grille,mask,glam,gphi,gdep,nx,ny,nz,premierx,premiery,premierz,dernierx,derniery,dernierz

   zu = var_x.data
   zv = var_y.data


   IF jpt EQ 1 THEN BEGIN
      wcurl = (shift(zv, -1, 0)-zv)/e1t[premierx:dernierx, premiery:derniery] + (zu-shift(zu, 0, -1))/e2t[premierx:dernierx, premiery:derniery]
   ENDIF ELSE BEGIN
      e1t3d = reform(reform(e1t[premierx:dernierx, premiery:derniery], nx*ny)#replicate(1, jpt), nx, ny, jpt)
      e2t3d = reform(reform(e2t[premierx:dernierx, premiery:derniery], nx*ny)#replicate(1, jpt), nx, ny, jpt)
      wcurl = (shift(zv, -1, 0, 0)-zv)/e1t3d + (zu-shift(zu, 0, -1, 0))/e2t3d
   ENDELSE

   field = {name: '', data: wcurl, legend: '', units: '', origin: '', dim: 0, direc:''}

   field.origin = var_x.origin
   field.dim = var_x.dim
   field.direc = var_x.direc

   return, field
END
