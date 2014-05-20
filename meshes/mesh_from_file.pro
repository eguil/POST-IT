;+
;
; @param MODEL {in}{required}{type=string}
;
; @param FILE_NAME {in}{required}{type=string}
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @param VAR_NAME {in}{required}{type=string}
; variable name
;
; @keyword ALL_DATA
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: mesh_from_file.pro 234 2010-06-24 16:22:36Z ericg $
;
;-
PRO mesh_from_file, model, file_name, ncdf_db, var_name $
    , ALL_DATA=all_data
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
   IF debug_w THEN BEGIN
    info = report('enter ...')
    IF keyword_set(all_data) THEN BEGIN
     print, '        All_data = ', all_data
    ENDIF
   ENDIF

 usage='mesh_from_file, model, file_name, ncdf_db, var_name' $
    + ', ALL_DATA=all_data'
;
 nparam = N_PARAMS()
 IF (nparam LT 4) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(model,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type model' $
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

 arg_type = size(var_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type var_name' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
;
; init grid, sf from file_name
; masks made in data_read
;
   IF strpos(ncdf_db, ':') GE 1 THEN BEGIN
    directory = (strsplit(ncdf_db, ':', /EXTRACT))[1]
   ENDIF ELSE BEGIN
    directory = ncdf_db
   ENDELSE

   s_file = directory+file_name

   print,'    Model inits for ', model, ' from file: ', s_file

   sm_file = hom_idl+'grids/grids_'+model+'.nc'
   res = find(sm_file)
   varexpp = varexp
   IF res NE 'NOT FOUND' THEN BEGIN

      IF keyword_set(ALL_DATA) THEN BEGIN
         initncdf, s_file, GLAMBOUNDARY = glamboundary_box, PLAIN = 1, fullcgrid = 1
      ENDIF ELSE BEGIN
         initncdf, s_file, GLAMBOUNDARY = glamboundary_box, fullcgrid = 1
      ENDELSE

      print, '    Found mask from ',sm_file

      tmask = byte(read_ncdf('sftlf', 0, 0, /timestep, file = sm_file, /nostruct))

      idx = where(tmask EQ valmask)

      IF idx[0] NE -1 THEN BEGIN
       tmask[idx] = 0.
      ENDIF
      idx = where(tmask LE 50.)
      tmask[idx] = 0.
      tmask = tmask < 1
      tmask = 1-tmask
      triangles=triangule()

   ENDIF ELSE BEGIN
      CASE var_name OF
         '@@voenergy': BEGIN
            CASE cmd1_back.grid OF
               'T': var_local = 'votemper'
               'T05': var_local = 'votemper'
               'T05#': var_local = 'votemper'
               ELSE: var_local = 'so'
            ENDCASE
            END
         ELSE: var_local = var_read_grd_file
      ENDCASE
      IF strpos(var_local, '(next)') NE -1 THEN BEGIN
         idx = strpos(var_local, '=')
         var_local = strmid(var_local, 0, idx)
         sl_pos = strpos(var_local, '/')
         IF sl_pos NE -1 THEN BEGIN
          var_local = strmid(var_local, 0, sl_pos)
         ENDIF
      ENDIF

      IF debug_w THEN BEGIN
       print, '   var_local = ', var_local
      ENDIF

      ; find valmask
      cdfidl=ncdf_open(s_file)
      contient=ncdf_inquire(cdfidl)
      varidl = ncdf_varid(cdfidl, var_local)
      varcontient=ncdf_varinq(cdfidl, var_local)
      valmask = 1.e20
      FOR i = 0, varcontient.natts-1 DO BEGIN
         att_txt = ncdf_attname(cdfidl, varidl, i)
         IF att_txt EQ 'missing_value' OR att_txt EQ 'mask value' OR att_txt EQ '_FillValue' THEN BEGIN
            ncdf_attget, cdfidl, varidl, att_txt, valmask
            IF debug_w THEN BEGIN
             print, '     valmask found = ',valmask, ' in attribute ', att_txt
            ENDIF
         ENDIF
      ENDFOR
      ncdf_close, cdfidl
      ; build grid
      CASE mesh_type of
         'atm':  BEGIN
            IF debug_w THEN BEGIN 
               print, '   zaxisname = ', name_level
            ENDIF 
            IF keyword_set(ALL_DATA) THEN BEGIN
               initncdf, s_file, GLAMBOUNDARY = glamboundary_box, ZAXISNAME = name_level, PLAIN = 1, fullcgrid = 1
            ENDIF ELSE BEGIN
               initncdf, s_file, GLAMBOUNDARY = glamboundary_box, ZAXISNAME = name_level, fullcgrid = 1
            ENDELSE
         END
         ELSE: BEGIN
            IF debug_w THEN BEGIN 
               print, '   zaxisname = ', name_level
            ENDIF 
            IF keyword_set(ALL_DATA) THEN BEGIN
               initncdf, s_file, USEASMASK = var_local, missing_value = valmask, GLAMBOUNDARY = glamboundary_box, ZAXISNAME = name_level, PLAIN = 1, fullcgrid = 1
            ENDIF ELSE BEGIN
               initncdf, s_file, USEASMASK = var_local, missing_value = valmask, GLAMBOUNDARY = glamboundary_box, ZAXISNAME = name_level, fullcgrid = 1
           ENDELSE
         END
      ENDCASE
      IF debug_w THEN BEGIN
       print, '     Grid info after initncdf :'
       print, '     glamt :', size(glamt)
       print, '     gphit :', size(gphit)
       print, '     gdept :', size(gdept)
      ENDIF

   ENDELSE

   varexp = varexpp

   key_offset = [0, 0, 0]

; indice i pour grille j moyenne zonale
;
   diaznl_idx = 1
   IF debug_w THEN BEGIN
    print, '     model, vargrid, varexp = ',model, ' ', vargrid,' ', varexp
    info = report('leaving...')
   ENDIF

END
