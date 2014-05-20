;+
;
; Reading Data
;
; @param CMD {in}{required}{type=structure}
; only cmd.plt is used
;
; @param HOTYP {in}{required}{type=string}
;
; @param PLTTYP {in}{required}{type=string}
;
; @param DIMPLOT {in}{required}{type=integer}
;
; @param IOVER {in}{required}
;
; @keyword ALL_DATA
;
; @keyword USE_LARGE_DOMAIN
;
; @keyword _EXTRA
;
; @returns
; -1 in case of error
;
; @examples
;
; IDL> cmd={exp:'HH',timave:'1m',date1:'199201',plt:'xy',spec:'-',grid:'umath',var:'temp_1'}
; IDL> hotyp='-'
; IDL> plttyp='pltt'
; IDL> dimplot=1
; IDL> data_bases=['local:./'] ++
; IDL> result=data_read(cmd, hotyp, plttyp, dimplot, iover )
; IDL> print, result

; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
; <propost_it>zoom_indices_debug</propost_it>
;
; <propost_it>def_dbase</propost_it>
;
; @todo
;
; use of iover ?
; init of data_bases in example
;
; @history
;
; - fplod 20091209T094824Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: data_read.pro 246 2010-07-12 14:03:17Z ericg $
;
;-
FUNCTION data_read, cmd, hotyp, plttyp, dimplot, iover $
         , ALL_DATA=all_data $
         , USE_LARGE_DOMAIN=use_large_domain $
         , _EXTRA=extra
;
  compile_opt idl2, strictarrsubs
;
@com_eg
@common
;
; Return to caller if errors
 ON_ERROR, 2
;
   IF debug_w THEN BEGIN
    info = report('enter ...')
    print, '       cmd at top of data_read = ', cmd
   ENDIF

 usage='result=data_read( cmd, hotyp, plttyp, dimplot, iover ' $
         + ', ALL_DATA=all_data' $
         + ', USE_LARGE_DOMAIN=use_large_domain' $
         + ', _EXTRA=extra)'

;
 nparam = N_PARAMS()
 IF (nparam LT 4) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(cmd,/type)
 IF (arg_type NE 8) THEN BEGIN
   ras = report(['Incorrect arg type cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_struct_tags=TAG_NAMES(cmd)

 tag=WHERE(STRMATCH(arg_struct_tags, 'TIMAVE'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag TIMAVE cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(cmd.timave,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.timave' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'DATE1'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag DATE1 cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(cmd.date1,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.date1' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'PLT'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag PLT cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(cmd.plt,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.plt' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'SPEC'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag SPEC cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(cmd.spec,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.spec' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'GRID'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag GRID cmd' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF
 arg_type = size(cmd.grid,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.grid' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'VAR'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag VAR cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(cmd.grid,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.var' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'EXP'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag EXP cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(cmd.exp,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.exp' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(hotyp,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type hotyp' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(plttyp,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type plttyp' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(dimplot,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type dimplot' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 common_type=size(data_bases,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type data_bases' $
          + '!C' $
          + 'Usage : ' + usage])
  return, -1
 ENDIF

 common_type=size(box_h,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type box_h' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF
 common_nelem=size(box_h,/n_elements)
 IF (common_nelem NE 4) THEN BEGIN
   ras = report(['Incorrect common n_elements box_h' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

; force data read

   read = 1

IF read EQ 1 THEN BEGIN

   cmd_prev = cmd

; difference from cmd.exp ? division from exp ?

   diff_from_exp = 0
   div_from_exp = 0

   IF strpos(cmd.exp, '-') NE -1 THEN BEGIN
      argvar = strsplit(cmd.exp,'-', /EXTRACT)
   ENDIF ELSE IF strpos(cmd.exp, '/') NE -1 THEN BEGIN
      argvar = strsplit(cmd.exp,'/', /EXTRACT)
   ENDIF ELSE BEGIN
      argvar = cmd.exp
   ENDELSE

   IF n_elements(argvar) EQ 2 THEN BEGIN
      IF strpos(cmd.exp, '-') NE -1 THEN BEGIN
         diff_from_exp = 1
         exp_init = cmd.exp
         cmd.exp = argvar[0]
      ENDIF
      IF strpos(cmd.exp, '/') NE -1 THEN BEGIN
         div_from_exp = 1
         exp_init = cmd.exp
         cmd.exp = argvar[0]
      ENDIF
   ENDIF
   varexp = cmd.exp

; define data base

   ncdf_db = def_dbase(cmd.exp)

   IF debug_w THEN BEGIN
    print, '       diff/div_from_exp  in data_read = ', diff_from_exp, div_from_exp
    print, '       varexp defined in data_read = ', varexp
   ENDIF

; define file name

   def_file_name, cmd, ncdf_db, file_name, delta_t1

; get file if remote

   get_file, file_name, ncdf_db

; read grid from file if require (@<grid> option)

   IF read_grid_from_file EQ 1 THEN BEGIN
      mesh_from_file, cmd.grid, file_name, ncdf_db, cmd.var, ALL_DATA = all_data
      key_shift_map = key_shift
      file_name2 = file_name
      ncdf_db2 = ncdf_db
   ENDIF

; define horizontal domain and vertical domain if needed (used in
; read_ncdf and update_data)

   IF keyword_set(use_large_domain) THEN BEGIN
      box_plot = box_h
   ENDIF ELSE BEGIN
      box_plot = def_box(cmd.plt, dimplot, legbox, time_stride)
   ENDELSE
   small_domain = cmd.plt
   CASE n_elements(box_plot) OF
      4 : IF vert_switch GE 1 THEN BEGIN
           box_plot = [box_plot, vert_mean]
          ENDIF
      6 : IF vert_switch GE 1 THEN BEGIN
           box_plot = [box_plot[0:3], vert_mean]
          ENDIF ELSE BEGIN
           box_plot = box_plot[0:3]
          ENDELSE
   ENDCASE

; Exceptions : need to read all the vertical data (and not a subset)
; for the pltz case. Other cases ?
   IF plttyp EQ 'pltz' THEN BEGIN
    box_plot = box_plot[0:3]
   ENDIF

; define timesteps time1 and time2 and time array

   IF hotyp NE '-' THEN BEGIN
      IF ensbl_code NE '' AND remove_1mm EQ 0 THEN BEGIN ; ensembles case : use lead time to compute date2 (spec)
         ldate2 = compute_time(cmd.timave, cmd.date1, long(ensbl_lt2-ensbl_LT), /NEXT)
      ENDIF ELSE BEGIN ; general case
         ldate2 = cmd.spec
      ENDELSE    
      IF debug_w THEN BEGIN 
         print, '   date1, date2 before time array computation in data_read: ', cmd.date1, ldate2
      ENDIF 
      time1 = delta_t1+1
      timearr = compute_time(cmd.timave, cmd.date1, ldate2)
      time2 = timearr.count+delta_t1
      time = timearr.scale
   ENDIF ELSE BEGIN
      IF strpos(cmd.timave, 'mm') NE -1 THEN BEGIN
       time1 = delta_t1+long(strmid(cmd.date1, 0, 2))
      ENDIF ELSE BEGIN
       time1 = delta_t1+1
      ENDELSE
      time2 = time1
      time = time1
   ENDELSE

; save time in common fld_att

   time1_r = time1
   time2_r = time2

; call specific read routine

   CASE STRMID(cmd.var, 0, 2) OF
      '@@': BEGIN
         IF debug_w THEN BEGIN 
            print, '=======> Macro case in data_read'
         ENDIF 
         IF debug_w THEN BEGIN
          print, 'keyword_set(all_data) : ', keyword_set(all_data)
         ENDIF
         fldr = macro_read(file_name, cmd.var, ncdf_db, BOXZOOM = box_plot, TIME_1 = time1, TIME_2 = time2, ALL_DATA = all_data, _EXTRA=extra)
         IF stddev_diff EQ 1 THEN BEGIN
          fldr.origin = 'diff'
         ENDIF
      END
      ELSE: BEGIN
         CASE plttyp OF
            'yfx': BEGIN        ; scatter plot y=f(x)
               IF debug_w THEN BEGIN 
                  print, '=======> yfx case in data_read'
               ENDIF 
               ; read 2 fields
               idx = strpos(cmd.var, '=f(')
               @two_var_read
               ; custom stucture for scatter plot
               fldr = {name: var1, data: fldr1.data, legend: fldr1.legend, $
                       units:  fldr1.units, origin: fldr1.origin, dim: fldr1.dim, $
                       name2: var2, data2: fldr2.data, legend2: fldr2.legend, $
                       units2:  fldr2.units, origin2: fldr2.origin, dim2: fldr2.dim}
               ; diff not possible yet
               IF diff_from_exp EQ 1 OR STRMID(cmd.spec, 0, 2) EQ 'd:' THEN BEGIN
                  diff_from_exp = 0
                  cmd.spec = '-'
                  print, '   data_read : difference in scatter plot not ready'
               ENDIF
               ; re-organise cmd.var
               cmd.var = var1
               cmd.var2 = var2

            END
            'ybinx': BEGIN
               IF debug_w THEN BEGIN 
                  print, '======> ybinx case in data_read'
               ENDIF 
               ; check how many fields to read
               sl_pos = strpos(cmd.var, '/')
               var3_ybinx = ""
               IF sl_pos NE -1 THEN BEGIN
                  eq_pos = strpos(cmd.var, '=')
                  str_len = strlen(cmd.var)
                  var1 = strmid(cmd.var, 0, sl_pos)
                  var3_ybinx = strmid(cmd.var, sl_pos+1, eq_pos-sl_pos-1)
                  cmd.var = var1+strmid(cmd.var, eq_pos, str_len)
               ENDIF
               ; read 2 fields
               idx = strpos(cmd.var, '=bin(')
               @two_var_read

               ; if two grids different then stop for now (best to interpolate on same grid)

  ;             IF grid2_test_ybinx NE cmd.grid THEN BEGIN
  ;                print, '  *** WARNING !!!! ybinx not ready for different grids'
  ;                print, '  --> use interpolation to have fields on same grid'
  ;                return, -1
  ;             ENDIF

               ; if third field needed, then read
               IF var3_ybinx NE "" THEN BEGIN
                  IF strpos (cmd_wrk.grid, '#') NE -1 THEN BEGIN
                     varpos = strpos(file_name, var1)
                     file_name3= strmid(file_name, 0, varpos)+var3_ybinx+'.nc'
                     IF debug_w THEN BEGIN
                      print, '   file_name3 = ', file_name3
                     ENDIF
                  ENDIF ELSE BEGIN
                     file_name3 = file_name
                  ENDELSE
                  box_plot = def_box(small_domain, dimplot, legbox, time_stride)
                  fldr3 = nc_read(file_name3, var3_ybinx, ncdf_db, BOXZOOM = box_plot, $
                                  TIME_1 = time1, TIME_2 = time2, ALL_DATA = all_data, _EXTRA=extra)
                  datyp3 = datyp
                  vargrid3 = vargrid1
                  ; custom stucture for bin plot
                  fldr = {name: var1, data: fldr1.data, legend: fldr1.legend, $
                          units:  fldr1.units, origin: fldr1.origin, dim: fldr1.dim, $
                          name2: var2, data2: fldr2.data, legend2: fldr2.legend, $
                          units2:  fldr2.units, origin2: fldr2.origin, dim2: fldr2.dim, $
                          name3: var3_ybinx, data3: fldr3.data, legend3: fldr3.legend, $
                          units3:  fldr3.units, origin3: fldr3.origin, dim3: fldr3.dim}
               ENDIF ELSE BEGIN
                  ; custom stucture for bin plot
                  fldr = {name: var1, data: fldr1.data, legend: fldr1.legend, $
                          units:  fldr1.units, origin: fldr1.origin, dim: fldr1.dim, $
                          name2: var2, data2: fldr2.data, legend2: fldr2.legend, $
                          units2:  fldr2.units, origin2: fldr2.origin, dim2: fldr2.dim}
               ENDELSE

               ; re-organise cmd.var
               cmd.var = var1
               cmd.var2 = var2
            END
            ELSE: BEGIN
               IF vecplot EQ 1 THEN BEGIN
                  IF debug_w THEN BEGIN 
                     print, '======> Vector case in data_read'
                  ENDIF 
                  ; vectors case
                  idx = strpos(cmd.var, ',')
                  var1 = strmid(cmd.var, 1, idx-1)
                  var2 = strmid(cmd.var, idx+1, strlen(cmd.var)-idx-2)
;                  print, var1, var2
;old version                  file_nam = strmid(file_name, 0, strlen(file_name)-4)
                  file_nam = strmid(base_file_name+base_suffix, 0, strlen(base_file_name+base_suffix)-1)
                  vargrid = strmid(cmd.grid, 0, 1)
                  CASE STRMID(var1, 0, 2) OF
                     '@@': fldr1 = macro_read(file_name, var1, ncdf_db, BOXZOOM = box_plot, $
                                              TIME_1 = time1, TIME_2 = time2, _EXTRA=extra)
                     ELSE: fldr1 = nc_read(file_nam+strmid(cmd.grid, 0, 1)+suff_domain+'.nc', var1, ncdf_db, $
                                           BOXZOOM = boxplot, TIME_1 = time1, TIME_2 = time2, $
                                           ALL_DATA = all_data, _EXTRA=extra)
                  ENDCASE
                  vargrid = strmid(cmd.grid, 1, 1)
                  CASE STRMID(var2, 0, 2) OF
                     '@@': fldr2 = macro_read(file_name, var2, ncdf_db, BOXZOOM = box_plot, $
                                              TIME_1 = time1, TIME_2 = time2, _EXTRA=extra)
                     ELSE: fldr2 = nc_read(file_nam+strmid(cmd.grid, 1, 1)+suff_domain+'.nc', var2, ncdf_db, $
                                           BOXZOOM = box_plot, TIME_1 = time1, TIME_2 = time2, $
                                           ALL_DATA = all_data, _EXTRA=extra)
                  ENDCASE
                                ; consistency check
                  IF fldr1.dim NE fldr2.dim THEN BEGIN
                     print, '  *** dimension inconsistency in vector plot : ', cmd.var
                     return, -1
                  ENDIF
                ; custom stucture for vector plot
                  fldr = {name: var1, data: fldr1.data, legend: fldr1.legend, $
                          units:  fldr1.units, origin: fldr1.origin, dim: fldr1.dim, $
                          name2: var2, data2: fldr2.data, legend2: fldr2.legend, $
                          units2:  fldr2.units, origin2: fldr2.origin, dim2: fldr2.dim}
                  IF diff_from_exp EQ 1 OR STRMID(cmd.spec, 0, 2) EQ 'd:' THEN BEGIN
                     diff_from_exp = 0
                     cmd.spec = '-'
                     print, '   data_read : difference in vector plot not ready'
                  ENDIF
                                ; re-organise cmd.var
                  cmd.var = var1
                  cmd.var2 = var2


               ENDIF ELSE BEGIN
                  ; general case
                  IF debug_w THEN BEGIN 
                     print, '======> General case in data_read'
                  ENDIF 
                  fldr = nc_read(file_name, cmd.var, ncdf_db, BOXZOOM = box_plot, $
                                  TIME_1 = time1, TIME_2 = time2, ALL_DATA = all_data, _EXTRA=extra)
                  ;
                  ; perform mean if hovmoeller diagram is required
                  ; It is absolutely necessary for the division case with
                  ; a hovmoeller diagram (before performing the division)
                  IF plttyp EQ 'pltt' AND div_from_exp EQ 1 THEN BEGIN
                     mask_z, fldr.data, cmd, boite_pltt, dimplot, legz
                     z2d = checkfield(fldr.data, plttyp, TYPE = hotyp, BOXZOOM = boite_pltt, DIREC = direc, _EXTRA=extra)
                     fldr = { name : fldr.name, data : z2d, legend : fldr.legend, units : fldr.units, $
                               origin : fldr.origin, dim : size(z2d, /N_DIMENSIONS), direc : hotyp}
                     print, 'Averaging made before the call to SAXO plt routines !!!!'
                  ENDIF
               ENDELSE
            END
         ENDCASE
      END
   ENDCASE

; Redefinition of time because it is updated in read_ncdf
; Useful for pltt routine

   IF hotyp NE '-' THEN BEGIN
      time = timearr.scale
   ENDIF ELSE BEGIN
      time = time1
   ENDELSE
   
; Extract ensemble member(s) as requested (could be done in nc_read via boxzoom: less memory used)

   @ensbl_data_extract

; density projection
   IF cmd.grid EQ "grden" THEN BEGIN 
      bin_read = 1 
   ENDIF ELSE BEGIN 
      bin_read = 0 
   ENDELSE   

   IF splot EQ 1 THEN BEGIN
      
      IF bin_read EQ 1 THEN BEGIN 
         print,  ' no bining as field already bined'
      ENDIF ELSE BEGIN 
         IF cmd.timave EQ '1y' AND 1 THEN BEGIN
            print, 'data_read:  Constructing annual mean from monthly means...'
            cmd.timave = '1m'
            year_s = cmd.date1
            cumulative = 0.
            FOR i = 1, 12 DO BEGIN
               IF i EQ 1  THEN BEGIN
                  cmd.date1 = year_s + '01'
               ENDIF
               IF i EQ 2  THEN BEGIN
                  cmd.date1 = year_s + '02'
               ENDIF
               IF i EQ 3  THEN BEGIN
                  cmd.date1 = year_s + '03'
               ENDIF
               IF i EQ 4  THEN BEGIN
                  cmd.date1 = year_s + '04'
               ENDIF
               IF i EQ 5  THEN BEGIN
                  cmd.date1 = year_s + '05'
               ENDIF
               IF i EQ 6  THEN BEGIN
                  cmd.date1 = year_s + '06'
               ENDIF
               IF i EQ 7  THEN BEGIN
                  cmd.date1 = year_s + '07'
               ENDIF
               IF i EQ 8  THEN BEGIN
                  cmd.date1 = year_s + '08'
               ENDIF
               IF i EQ 9  THEN BEGIN
                  cmd.date1 = year_s + '09'
               ENDIF
               IF i EQ 10 THEN BEGIN
                  cmd.date1 = year_s + '10'
               ENDIF
               IF i EQ 11 THEN BEGIN
                  cmd.date1 = year_s + '11'
               ENDIF
               IF i EQ 12 THEN BEGIN
                  cmd.date1 = year_s + '12'
               ENDIF
               print, '   Date1 = ', cmd.date1
               sfild = fldr
               bin_sigma, cmd, sfild, BOXZOOM = box_plot, ALL_DATA = all_data ;;;;;;;;;;;;;; prob
               cumulative = cumulative + sfild.data
            ENDFOR
            fldr = sfild
            fldr.data = cumulative/12.
            cmd.timave = '1y'
            cmd.date1 = year_s
         ENDIF ELSE BEGIN
            IF cmd.timave EQ '1m' AND strmid(cmd.plt, 0, 2) EQ 'st' AND really_1m_st EQ 1 THEN BEGIN
               sfild = fldr
               timedim = (size(sfild.data))[(size(sfild.data))[0]]
               fldrd = fltarr((size(sfild.data))[1],(size(sfild.data))[2], (sig_max - sig_min)/sig_del + 1, timedim)
               FOR i = 0, timedim-1 DO BEGIN
                  print, 'data_read: Performing monthly bining. Indices: first, last, current = 0,', timedim-1, i
                  sfild2 = {name: sfild.name, data: sfild.data[*, *, *, i], legend: sfild.legend, units: sfild.units, origin: sfild.origin, dim: sfild.dim-1}
                  bin_sigma, cmd, sfild2, BOXZOOM = box_plot, ALL_DATA = all_data
                  fldrd[*, *, *, i]= sfild2.data
               ENDFOR
               fldr = {name: sfild.name, data:fldrd, legend: sfild.legend, units: sfild.units, origin: sfild.origin, dim: sfild.dim}
            ENDIF ELSE BEGIN
               sfild = fldr
               ;;bin_sigma, cmd, sfild, BOXZOOM = box_plot, ALL_DATA = all_data
               bin_sigma, cmd, sfild, ALL_DATA = all_data
               fldr = sfild
               grille,mask,glam,gphi,gdep,nx,ny,nz,firstx,firsty,firstz,lastx,lasty,lastz
            ENDELSE
         ENDELSE
      ENDELSE 
   ENDIF

; modify data info if needed (actual data modification done in; trends.pro called by pltt.pro)

   CASE plttyp OF
      'pltt': BEGIN
         IF run_stddev EQ 0 THEN BEGIN
            CASE strmid(cmd.trend, 0, 1) OF
               '1': fldr.origin = 'diff'
               '2': fldr.origin = 'diff'
               '3': fldr.origin = 'diff'
               '4': fldr.origin = 'diff'
               ELSE:
            ENDCASE
         ENDIF
      END
      ELSE:
   ENDCASE

   IF n_elements(fldr.data) EQ 1 THEN BEGIN
    return, fldr
   ENDIF

; Difference 2 cases : from cmd.exp or from cmd.spec

   IF debug_w THEN BEGIN 
      print, '   Testing difference cases in data_read...'
   ENDIF 

   IF diff_from_exp EQ 1 THEN BEGIN
      remove_1mm = 0

      cmd.exp = argvar[1]
      print, '    remove : ', cmd.exp

      ; if remove 1mm adapt cmd.timave, cmd.date1 and cmd.spec
      IF strpos(cmd.exp, '1mm') NE -1 THEN BEGIN 
         remove_1mm = 1
         rdat1 = cmd.date1
         rdat2 = cmd.spec
         rtime = time
         rcmd = cmd
         argexp = strsplit(cmd.exp, '_', /EXTRACT)
         cmd.exp = argexp[0]
         argmm = strsplit(argexp[2], '/', /EXTRACT)
         mmdates = argmm[0]+'-'+argmm[1]
         cmd.timave = '1mm'
         cmd.date1 = '01_'+mmdates
         cmd.spec = '12_'+mmdates
      ENDIF

      ; read field2
      field2 = data_read(cmd, hotyp, plttyp, dimplot, iover, ALL_DATA = all_data, _EXTRA=extra)
      IF n_elements(field2.data) EQ 1 THEN BEGIN
       return, field2
      ENDIF

      ; perform difference

      IF remove_1mm EQ 1 THEN BEGIN ; remove 1mm
         IF debug_w THEN BEGIN 
            print, '   Remove 1mm of ', cmd.exp
         ENDIF 
         IF hotyp NE '-' THEN BEGIN 
            ; from 1mm file (field2) build time serie corresponding to time array 
            time = rtime
            cmd.timave = rcmd.timave
            cmd.date1 = rcmd.date1
            cmd.spec = rcmd.spec
            jpt = n_elements(time) 
            ; extract months indices of time array
            caldat, time, months 
            CASE (size(field2.data))[0] OF 
               2: BEGIN 
                  diffr = field2.data[*, months-1]
               END 
               3: BEGIN 
                  diffr = field2.data[*, *, months-1]
               END 
               4: BEGIN 
                  diffr = field2.data[*, *, *, months-1]
               END
            ENDCASE 
         ENDIF ELSE BEGIN
            ; one date case
         ENDELSE 
      ENDIF ELSE BEGIN ; general case
         diffr = field2.data
      ENDELSE 
      ; take care of mask
      IF (where(fldr.data EQ valmask))[0] NE -1 THEN BEGIN
         diffr[where(fldr.data EQ valmask)] = valmask
      ENDIF
      ; take care of ensemble dimension of field 1
         ; adapt dimension of array to remove (tabd) to ensbl case
      IF ensbl_code NE '' AND ensbl_dim GT 1 THEN BEGIN 
         CASE (size(fldr.data))[0]OF
            3: BEGIN 
               diffr = reform((reform(diffr, nxt*nyt, /overwrite))#replicate(1, ensbl_dim), nxt, nyt, ensbl_dim)
            END 
            4: BEGIN 
               diffr = reform((reform(diffr, nxt*nyt*jpt, /overwrite))#replicate(1, ensbl_dim), nxt, nyt, jpt, ensbl_dim)
               diffr = transpose(diffr, [0, 1, 3, 2])
            END 
         ENDCASE 
      ENDIF 
      ; perform difference
      fldr.data = fldr.data - diffr

      undefine,  diff
      fldr.origin = 'diff'
      cmd.exp = exp_init
      varexp = cmd.exp

   ENDIF ELSE IF div_from_exp EQ 1 THEN BEGIN  ; Division : 1 case

      cmd.exp = argvar[1]
      print, '    divide by : ', cmd.exp

      div = fldr.data

      ; read field2
      field2 = data_read(cmd, hotyp, plttyp, dimplot, iover, ALL_DATA = all_data, _EXTRA=extra)
      IF n_elements(field2.data) EQ 1 THEN BEGIN
       return, field2
      ENDIF

      IF plttyp EQ 'pltt' THEN BEGIN
         mask_z, field2.data, cmd, boite_pltt, dimplot, legz
         z2d = checkfield(field2.data, plttyp, TYPE = hotyp, BOXZOOM = boite_pltt, DIREC = direc, _EXTRA=extra)
         field2 = { name : field2.name, data : z2d, legend : field2.legend, units : field2.units, $
                   origin : field2.origin, dim : size(z2d, /N_DIMENSIONS), direc : hotyp}
         print, 'Averaging made before the call to SAXO plt routines !!!!'
      ENDIF

      ; perform division
      IF (where(field2.data EQ 0.0))[0] NE -1 THEN BEGIN
         print, 'Be careful some data are null. They will be masked ! '
         idx0 = where(field2.data EQ 0.0)
         field2.data[idx0] = valmask
         fldr.data[idx0] = valmask
         div[idx0] = valmask
      ENDIF

      idx = where(field2.data NE valmask)
      div[idx] = fldr.data[idx] / field2.data[idx]

      fldr.data = div
      undefine,  div

      fldr.origin = 'div'
      cmd.exp = exp_init
      varexp = cmd.exp

   ENDIF ELSE BEGIN

; If difference, decode second field, read and perform difference

      IF STRMID(cmd.spec, 0, 2) EQ 'd:' THEN BEGIN

         cmd2 = cmd
         cmdl = STRMID(cmd.spec, 2, strlen(cmd.spec)-2)

         print, '    remove : ', cmdl

         ; decode field2
         argvar = strsplit(cmdl, '/', /EXTRACT)

         cmd2.exp = argvar[0]
         cmd2.timave = argvar[1]
         cmd2.date1 = argvar[2]
         cmd2.spec = '-'

         IF n_elements(argvar) EQ 4 THEN BEGIN
          cmd2.spec = argvar[3]
         ENDIF

         ; read field2
         field2 = data_read(cmd2, hotyp, plttyp, dimplot, iover, ALL_DATA = all_data, _EXTRA=extra)
         IF n_elements(field2.data) EQ 1 THEN BEGIN
          return, field2
         ENDIF

         ; perform difference
         diff = fldr.data-field2.data
         IF (where(fldr.data EQ valmask))[0] NE -1 THEN BEGIN
          diff[where(fldr.data EQ valmask)] = valmask
         ENDIF
         fldr.data = diff
         undefine, diff
         fldr.origin = 'diff'
         cmd.exp = cmd.exp+' - '+cmd2.exp+ ' ('+cmd2.timave+' '+cmd2.date1+')'
         varexp = cmd.exp

      ENDIF

   ENDELSE

ENDIF ELSE BEGIN

   print, ''
   print, '   Data already read (previous field)'
   print, ''

ENDELSE

;@zoom_indices_debug

; Pb with varexp which is always updated in read_ncdf
; For Seb, varexp is the name of the file. For Eric, varexp is the name of the experiment
   varexp = cmd.exp

   IF debug_w THEN BEGIN
    print, '       varexp= ', varexp
    info = report('leaving ...')
   ENDIF

return, fldr

END
