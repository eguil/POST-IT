;+
;
; @version
; $Id: two_var_read.pro 215 2010-03-31 15:58:02Z ericg $
;
;-
               var1 = strmid(cmd.var, 0, idx)
               fldr1 = nc_read(file_name, var1, ncdf_db, BOXZOOM = box_plot, $
                               TIME_1 = time1, TIME_2 = time2, ALL_DATA = all_data, _EXTRA=extra)
               file_name1 = file_name
               ncdf_db1 = ncdf_db
               CASE cmd.grid OF
                  'U': vargrid1 = 'U'
                  'V': vargrid1 = 'V'
                  ELSE: vargrid1 = 'T'
               ENDCASE
               stddev_mth = '00'
               use_large_domain = 0
               ; read second variable

               IF debug_w THEN BEGIN
                print, 'Reading second variable in data_read/two_var_read'
               ENDIF

               IF strpos(cmd.var, '(next)') EQ -1 THEN BEGIN  ; type 1 y=f(x) on 1 line

                  var2 = strmid(cmd.var, idx+3, strlen(cmd.var)-idx-4)
                  ; check months to analyse
                  pos_underscore = strpos(var2,'_')
                  IF pos_underscore GE 0 THEN BEGIN
                     stddev_mth = STRMID(var2, pos_underscore + 1, 6)
                     var2 = STRMID(var2, 0, pos_underscore)
                  ENDIF
                  fldr2 = nc_read(file_name, var2, ncdf_db, BOXZOOM = box_plot, $
                                  TIME_1 = time1, TIME_2 = time2, ALL_DATA = all_data, _EXTRA=extra)
                  datyp2 = datyp
                  cmd2 = cmd
                  sw_diffg = 0
                  vargrid2 = vargrid1
               ENDIF ELSE  BEGIN  ; type 2 y=f(next) or y=bin(x) on 2 lines

  ;Exception if large_domain = 1
                  IF debug_w THEN BEGIN
                   print, 'type 2 y=f(next) or y=bin(x) on 2 lines'
                  ENDIF
                  IF large_domain EQ 1 AND plttyp EQ 'ybinx' THEN BEGIN
                     use_large_domain = 1
                  ENDIF

                  cmd2 = decode_cmd(cmdline_main, idx_main+1)
                  ; check months to analyse
                  pos_underscore = strpos(cmd2.var,'_')
                  IF pos_underscore GE 0 THEN BEGIN
                     stddev_mth = STRMID(cmd2.var, pos_underscore + 1, 6)
                     cmd2.var = STRMID(cmd2.var, 0, pos_underscore)
                  ENDIF
                  cmd2.timave = cmd.timave
                  cmd2.date1 = cmd.date1
                  cmd2.spec = cmd.spec
                  datyp2 = def_dptyp(cmd2)
                  cmd2_back = cmd2
                  ; other grid ?
                  grid2_test = strsplit(strsplit(cmd2.grid,'@',/EXTRACT),'#',/EXTRACT)
                  grid2_test = cmd2.grid
                                ; force new grid to update suffix in def_file_name (BUG to correct)
                  grid2_test = 'toto'
                  sw_diffg = 0
                  IF grid2_test NE cmd.grid THEN BEGIN
                     sw_diffg = 1
                     jptb = jpt
                     IF debug_w THEN BEGIN
                      print, '    (in data_read) cmd2.grid <> cmd.grid ', cmd2.grid, cmd.grid
                      print, '    exec def_grid, cmd2_back in data_read : '
                     ENDIF
                     grid2_full = cmd2_back.grid
                     def_grid, cmd2_back
                     jpt = jptb
                  ENDIF

                  fldr2 = data_read(cmd2, datyp2.hotyp, datyp2.plttyp, 1, 0, ALL_DATA = all_data, USE_LARGE_DOMAIN = use_large_domain, _EXTRA=extra)
                  var2 = cmd2.var
                  print, '   Variable 2 ', var2, ' read on vargrid ', vargrid
                  jptb = jpt
                  if debug_w THEN BEGIN
                   print, '   yfx grids different ? / sw_diffg = ', sw_diffg
                  ENDIF

                  IF sw_diffg EQ 1 THEN BEGIN
                     IF debug_w THEN BEGIN
                      print, '   exec def_grid, cmd1_back in data_read : '
                     ENDIF
                     grid1_full = cmd1_back.grid
                     def_grid, cmd1_back
                     IF read_grid_from_file EQ 1 THEN BEGIN
                        mesh_from_file, cmd1_back.grid, file_name1, ncdf_db1, cmd1_back.var
                        key_shift_map = key_shift
                     ENDIF
                  ENDIF
                  jpt = jptb
                  CASE cmd2.grid OF
                     'U': vargrid2 = 'U'
                     'V': vargrid2 = 'V'
                     ELSE: vargrid2 = 'T'
                  ENDCASE
               ENDELSE
               ; consistency check
               IF fldr1.dim NE fldr2.dim THEN BEGIN
                  print, '  *** dimension inconsistency in y=f(x) : ', cmd.var
                  print, fldr1.dim, fldr2.dim
                  return, -1
               ENDIF
