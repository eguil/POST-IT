;+
;
; Scatter plot y=f(x)
; called by <propost_it>plt_map</propost_it>
;
; @history
; - fplod 20100114T144659Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * fix array syntax on fld and fld2
;
; @version
; $Id: yfx.pro 236 2010-06-25 09:12:43Z ericg $
;
;-
               t = tmask
               boxx = ''
               boxy = ''
               add_txt = ''
               coeffm_txt = ''
            ; arrange/average data if needed
              ; space and time plots fld
               IF strpos(cmd.plt, 'xy_') NE -1 THEN BEGIN
                  mask_z, fld, cmd, boite_plt, dimplot, boxx
                  boxx = ' ['+boxx+']'
                  print, '   Averaging (space) '+cmd.var+' in'+boxx
                  fld = checkfield(fld, 'plt', type = 'xy', boite = boite_plt)
               ENDIF
               IF datyp.hotyp NE '-' THEN BEGIN
                  mask_z, fld, cmd, boite_plt1d, dimplot, boxx
                  boxx = ' ['+boxx+']'
                  vargrid = vargrid1
                  IF debug_w THEN BEGIN
                   print, '   yfx: domdef and vargrid : ', boite_plt1d[0:3], vargrid
                  ENDIF
                  domdef, boite_plt1d[0:3]
                  print, '   Averaging (time serie plot) cmd.var '+cmd.var+' in'+boxx
                  IF debug_w THEN BEGIN
                   print, '    fld size', size(fld)
                  ENDIF
                  fld = checkfield(fld, 'plt1d', type = datyp.hotyp, boite = boite_plt1d)
                  IF cmd.trend GT 0 THEN BEGIN
                     fld_flag = 1
                     fld = trends(fld, cmd.trend, datyp.hotyp)
                     add_txt = trd_txt
                  ENDIF
               ENDIF
              ; space and time plots fld2
               IF debug_w THEN BEGIN
                print, '   cmd1_back = ', cmd1_back
                print, '   cmd2_back = ', cmd2_back
                IF sw_diffg THEN BEGIN
                 print, '   grid1_full = ', grid1_full
                 print, '   grid2_full = ', grid2_full
                ENDIF
               ENDIF
               IF sw_diffg EQ 1 THEN BEGIN
                  jptb = jpt
                  IF debug_w THEN BEGIN
                   print, '  calling def_grid, cmd2_back in plt_map/yfx '
                  ENDIF
                  cmd2_back.grid = grid2_full
                  def_grid, cmd2_back
                  IF read_grid_from_file EQ 1 THEN BEGIN
                     mesh_from_file, cmd2_back.grid, file_name2, ncdf_db2, cmd2_back.var
                     key_shift_map = key_shift
                  ENDIF
                  jpt = jptb
               ENDIF
               IF strpos(cmd2.plt, 'xy_') NE -1 THEN BEGIN
                  mask_z, fld2, cmd2_back, boite_plt2, dimplot, boxy
                  boxy = ' ['+boxy+']'
                  print, '   Averaging (space) '+cmd2_back.var+' in'+boxy
                  fld2 = checkfield(fld2, 'plt', type = 'xy', boite = boite_plt2)
               ENDIF
               IF datyp2.hotyp NE '-' THEN BEGIN
                  run_stddev = 0
                  IF strpos(cmd2.plt, '@r') GE 1 THEN BEGIN
                     run_stddev = strmid(cmd2.plt, strpos(cmd2.plt, '@r')+2, strlen(cmd2.plt))
                     cmd2.plt = strmid(cmd2.plt, 0, strpos(cmd2.plt, '@r'))
                  ENDIF
                  mask_z, fld2, cmd2_back, boite_plt1d2, dimplot, boxy
                  boxy = ' ['+boxy+']'
                  vargrid = vargrid2
                  IF debug_w THEN BEGIN
                   print, '    domdef and vargrid : ', boite_plt1d2[0:3], vargrid
                  ENDIF
                  domdef, boite_plt1d2[0:3]
                  print, '   Averaging  (time serie plot) cmd2.var '+cmd2.var+' in'+boxy
                  IF debug_w THEN BEGIN
                   print, '    fld2 size', size(fld2)
                  ENDIF
                  fld2 = checkfield(fld2, 'plt1d', type = datyp2.hotyp, direc = 'xy', boite = boite_plt1d2, /timearray)
                  IF cmd2.trend GT 0 THEN BEGIN
                     fld_flag = 2
                     fld2 = trends(fld2, cmd2.trend, datyp2.hotyp)
                     add_txt = trd_txt
                  ENDIF
               ENDIF
               IF sw_diffg EQ 1 THEN BEGIN
                  sw_diffg = 0
                  jptb = jpt
                  IF debug_w THEN BEGIN
                   print, ' calling def_grid, cmd1_back in plt_map/yfx '
                  ENDIF
                  cmd1_back.grid = grid1_full
                  def_grid, cmd1_back
                  IF read_grid_from_file EQ 1 THEN BEGIN
                     mesh_from_file, cmd1_back.grid, file_name1, ncdf_db1, cmd1_back.var
                     key_shift_map = key_shift
                  ENDIF
                  jpt = jptb
               ENDIF

               ; Compute linear fit coeeficients
               fmt_linfit = '(F9.4)'
               ;  whole serie
               coeff = linfit(fld2, fld, CHISQ = linerr, PROB = proberr, SIGMA = sigmaerr)
               print, '    Linfit coef (a+bx) = ', coeff[0], coeff[1], ' errbar = ',sigmaerr
               ; text for plot info in legend_overlay
               coeff_txt = 'Linfit = '+strtrim(string(coeff[1], format = fmt_linfit), 2)
               IF proberr NE 1.0 THEN BEGIN
                print, '    WARNING: proba = ',proberr
               ENDIF
               ; divide serie in two domains (on x axis) seperated by linfit_sep (plt_def)
               ; do only if 1m@t412 (if full serie, then separate by season)

               IF linfit_sep NE -99999 AND cmd.trend EQ 412 THEN BEGIN

                  idx_low = where (fld2 LE linfit_sep)
                  idx_up = where (fld2 GE linfit_sep)
                  coefflu_txt = ' ['

                  IF idx_low[0] NE -1 THEN BEGIN
                     coeffl = linfit(fld2[idx_low], fld[idx_low], CHISQ = linerrl, PROB = proberrl, SIGMA = sigmaerrl)
                     print, '    Linfit coef below ',strtrim(string(linfit_sep), 2),' = ', coeffl[0], coeffl[1], ' errbar = ',sigmaerrl
                     IF proberrl NE 1.0 THEN BEGIN
                      print, '    *** WARNING: proba lower = ',proberrl
                     ENDIF
                     coefflu_txt = coefflu_txt+strtrim(string(coeffl[1], format = fmt_linfit), 2)+'<'+strtrim(string(linfit_sep, format = '(F4.2)'), 2)
                  ENDIF

                  IF idx_up[0] NE -1 THEN BEGIN
                     coeffu = linfit(fld2[idx_up], fld[idx_up], CHISQ = linerru, PROB = proberru, SIGMA = sigmaerru)
                     print, '    Linfit coef above ',strtrim(string(linfit_sep), 2),' = ', coeffu[0], coeffu[1], ' errbar = ',sigmaerru
                     IF proberru NE 1.0 THEN BEGIN
                      print, '    *** WARNING: proba upper = ',proberru
                     ENDIF
                  coefflu_txt = coefflu_txt+'>'+strtrim(string(coeffu[1], format = fmt_linfit), 2)
                  ENDIF
                  coefflu_txt = coefflu_txt+']'
               ENDIF ELSE BEGIN
                  coefflu_txt = ''
               ENDELSE

               ; test lag_linfit
               cp1 = linfit(shift(fld2, 1), fld, CHISQ = linerrp1, PROB = proberrp1, SIGMA = sigmaerrp1)
               cp2 = linfit(shift(fld2, 2), fld, CHISQ = linerrp2, PROB = proberrp2, SIGMA = sigmaerrp2)
               cp3 = linfit(shift(fld2, 3), fld, CHISQ = linerrp3, PROB = proberrp3, SIGMA = sigmaerrp3)
               cm1 = linfit(shift(fld2, -1), fld, CHISQ = linerrm1, PROB = proberrm1, SIGMA = sigmaerrm1)
               cm2 = linfit(shift(fld2, -2), fld, CHISQ = linerrm2, PROB = proberrm2, SIGMA = sigmaerrm2)
               cm3 = linfit(shift(fld2, -3), fld, CHISQ = linerrm3, PROB = proberrm3, SIGMA = sigmaerrm3)
               print, '   '
               print, '    -3 Linfit coef (a+bx) = ', cm3[0], cm3[1], ' errbar = ',sigmaerrm3
               print, '    -2 Linfit coef (a+bx) = ', cm2[0], cm2[1], ' errbar = ',sigmaerrm2
               print, '    -1 Linfit coef (a+bx) = ', cm1[0], cm1[1], ' errbar = ',sigmaerrm1
               print, '    +1 Linfit coef (a+bx) = ', cp1[0], cp1[1], ' errbar = ',sigmaerrp1
               print, '    +2 Linfit coef (a+bx) = ', cp2[0], cp2[1], ' errbar = ',sigmaerrp2
               print, '    +3 Linfit coef (a+bx) = ', cp3[0], cp3[1], ' errbar = ',sigmaerrp3

            ; plot domain
               boxyfx = def_box(cmd.plt, dimplot, legz, time_stride)
            ; define line color and type
               overc = overlay_type(iover, dimplot)
               vardate = date_txt
               varname = varlegend
               varunit = '    '+cmd.var+boxx+' =f('+cmd.var2+boxy+')   '+add_txt
               pltcmdstd = 'pltsc,fld,fld2,minc,maxc,minc2,maxc2,varlegend2, boite=boxyfx'+overc+com_strplt
               IF hotyp EQ 't' THEN BEGIN ; time scatter plot
                  IF mean_sc_only EQ 0 OR mean_sc_only EQ 1 OR mean_sc_only EQ 4 THEN BEGIN
                    ; number of time colors
                     IF strpos(symbol_families, 'x') NE -1 THEN BEGIN
                        coding = strsplit(symbol_families, 'x', /EXTRACT)
                        ncolors = long(coding[0])
                        ntimes = long(coding[1])
                     ENDIF ELSE BEGIN
                        ncolors = long(symbol_families)
                        ntimes = 1
                     ENDELSE
                     IF ncolors GE 2 THEN BEGIN ; time color coding
                        ic = 0
                        jpto = jpt
                        jpt = jpt/ncolors
                        lincoef = fltarr(ncolors)
                        linerro = fltarr(ncolors)
                        linprob = fltarr(ncolors)
                        linerr0 = fltarr(ncolors)
                        coeffm_txt = ' ['
                        WHILE ic LE ncolors-1 DO BEGIN
                           idx0 = (floor(findgen(jpto/(ncolors*ntimes)))*ncolors*ntimes)+ic*(ntimes)
                           idx = idx0
                           jl = 1
                           WHILE jl LE ntimes-1 DO BEGIN
                              idx = [idx, idx0+jl]
                              jl = jl+1
                           ENDWHILE
                           fldp = fld[idx]
                           fldp2 = fld2[idx]
;                           coeff = linfit(fldp2(where (fldp2(*) GT 0.)), fldp(where (fldp2(*) GT 0.)), CHISQ = errlin, PROB = prblin)
                           coeff = linfit(fldp2, fldp, CHISQ = errlin, PROB = prblin, SIGMA = sigmaerr)
                           linerro[ic] = sigmaerr[1]
                           linprob[ic] = prblin
                           lincoef[ic] = coeff[1]
                           linerr0[ic] = sigmaerr[0]
                           print, '    Period '+strtrim(string(ic+1), 2)+' Linfit coef =', coeff[0],coeff[1], ' errbar = ',sigmaerr
                           coeffm_txt = coeffm_txt+strtrim(string(coeff[1], format = fmt_linfit), 2)+'/'
                           valmask = ABS(valmask)
                           IF mean_sc_only EQ 0 OR mean_sc_only EQ 1 THEN BEGIN
                              pltcmd = 'pltsc,fldp,fldp2,minc,maxc,minc2,maxc2,varlegend2, boite=boxyfx'+com_strplt+',ov1d='+string(ic)+',COLOR='+string(symbol_color[ic])+', STY1D='+string(symbol_style[ic]-1)+', SYMSIZE='+string(symbol_size)
                              printf, nulhis, strcompress(pltcmd)
                              IF debug_w THEN BEGIN
                               print, '   ',pltcmd
                              ENDIF
                              res = execute(pltcmd)
                           ENDIF
                           IF mean_sc_only EQ 4 AND ic EQ ncolors-1 THEN BEGIN
                              print, '    Linfit slope + error (July-Dec)=', mean(lincoef[5:10]), mean(linerro[5:10])
                              print, '    Linfit slope + error (Jan-June)=', mean([lincoef[0:4], lincoef[11]]), mean([linerro[0:4], linerro[11]])
                              vardate = 'toto'
                              jpt = ncolors
                              time = lindgen(12)*30*1+ julday(1,1,01, ndayspm = calendar_type)
                              hotyp = 't'
                              ; field specific min/max and ytitle
                              @../config/yfx_sc_config.pro
                              coeff_txt = "" & coefflu_txt = "" & coeffm_txt = ""
                              print, '    Mult, min, max set in plt_map ',mult_coeff, minmax
                              pltcmd = 'pltt,lincoef*'+string(mult_coeff)+', '''+hotyp+''''+minmax+com_strplt+',ov1d=0,COLOR=1, thick=4, STY1D=0'

                              printf, nulhis, strcompress(pltcmd)
                              IF debug_w THEN BEGIN
                               print, '   ', pltcmd
                              ENDIF
                              res = execute(pltcmd)
                              pltcmd = 'pltt,(lincoef-linerro)*'+string(mult_coeff)+', '''+hotyp+''''+minmax+com_strplt+',ov1d=1,COLOR=1, thick=1, STY1D=0'
                              printf, nulhis, strcompress(pltcmd)
                              IF debug_w THEN BEGIN
                               print, '   ', pltcmd
                              ENDIF
                              res = execute(pltcmd)
                              pltcmd = 'pltt,(lincoef+linerro)*'+string(mult_coeff)+', '''+hotyp+''''+minmax+com_strplt+',ov1d=1,COLOR=1, thick=1, STY1D=0'
                              printf, nulhis, strcompress(pltcmd)
                              IF debug_w THEN BEGIN
                               print, '   ',pltcmd
                              ENDIF
                              res = execute(pltcmd)
                              pltcmd = 'pltt,linerr0, '''+hotyp+''''+minmax+com_strplt+',ov1d=1,COLOR=2, thick=1, STY1D=0'
                              printf, nulhis, strcompress(pltcmd)
                              IF debug_w THEN BEGIN
                               print, '   ', pltcmd
                              ENDIF
                              res = execute(pltcmd)
                           ENDIF
                           ic = ic + 1
                        ENDWHILE
                        coeffm_txt = coeffm_txt+']'
                     ENDIF ELSE BEGIN
                        ; one color
                        IF debug_w THEN BEGIN
                         print, '   ',pltcmdstd
                        ENDIF
                        res = execute(pltcmdstd[0])
                     ENDELSE
                  ENDIF
               ENDIF ELSE BEGIN
                  ; standard scatter plot
                  IF debug_w THEN BEGIN
                   print, '   ',pltcmdstd
                  ENDIF
                  res = execute(pltcmdstd[0])
               ENDELSE
               IF cmd.trend EQ 412 THEN BEGIN
                coeffm_txt = ''
               ENDIF
               ; Add slope value
               @legend_overlay

               ; add mean SC in 4x3 plots

               IF symbol_families EQ '4x3' THEN BEGIN

                  IF cmd.trend EQ '412' THEN BEGIN
                     fldrem_t1 = fldrem_t1-mean(fldrem_t1)
                     fldrem_t2 = fldrem_t2-mean(fldrem_t2)
                  ENDIF ELSE BEGIN
                     running = 12L
                     lenght = (size(fld))[1]
                     fldrem_t1 = fltarr(running)
                     fldrem_t2 = fltarr(running)
                     FOR t1 = 0, running-1 DO BEGIN
                        fldrem_t1[t1] = mean(fld[long(findgen(lenght/running)*running+t1)])
                        fldrem_t2[t1] = mean(fld2[long(findgen(lenght/running)*running+t1)])
                     ENDFOR
                  ENDELSE

                  print, '    Seasonal cycle var/stddev fld1 ', (moment(fldrem_t1))[1], sqrt((moment(fldrem_t1))[1])
                  print, '    Seasonal cycle var/stddev fld2 ', (moment(fldrem_t2))[1], sqrt((moment(fldrem_t2))[1])

                  fldrem_t1 = [fldrem_t1, fldrem_t1[0]]
                  fldrem_t2 = [fldrem_t2, fldrem_t2[0]]
                  sw_ov1d = mean_sc_only
                  IF mean_sc_only EQ 3 AND cmd.trend EQ '412' THEN BEGIN
                     ; SC of std dev
                     stat = spectra(fld, jpt/12, 20, 15, 0)
                     stat2 = spectra(fld2, jpt/12, 20, 15, 0)
                     print, '     Stddev of monthly stddevs fld', sqrt((moment(stat.sc_std-(moment(stat.sc_std))[0]))[1])
                     print, '     Stddev of monthly stddevs fld2', sqrt((moment(stat2.sc_std-(moment(stat2.sc_std))[0]))[1])
                     stdsc = [stat.sc_std, stat.sc_std[0]]
                     stdsc2 = [stat2.sc_std, stat2.sc_std[0]]
                     pltcmd = 'pltsc,stdsc,stdsc2,0. ,maxc,0.,maxc2,varlegend2, boite=boxyfx'+com_strplt+',ov1d=1-min(1,sw_ov1d), STY1D=-1, THICKN=3, SYMSIZE='+string(symbol_size)+', FRACTION=fraction'
                     printf, nulhis, strcompress(pltcmd)
                     IF debug_w THEN BEGIN
                      print, pltcmd
                     ENDIF
                     res = execute(pltcmd[0])
                     pltcmd = 'xyouts,stdsc2-(maxc2)/(.3*win[0]*win[1]),stdsc,string(long(findgen(12))+1),charsize=1.3,alignment=0.5'
                     printf, nulhis, strcompress(pltcmd)
                     IF debug_w THEN BEGIN
                      print, pltcmd
                     ENDIF
                     res = execute(pltcmd[0])
                  ENDIF ELSE BEGIN
                     ov1d_val = 0
                     IF mean_sc_only EQ 1 THEN BEGIN
                      ov1d_val = 1
                     ENDIF
                     pltcmd = 'pltsc,fldrem_t1,fldrem_t2,minc,maxc,minc2,maxc2,varlegend2, boite=boxyfx'+com_strplt+',ov1d=ov1d_val, STY1D=-1, THICKN=5, SYMSIZE='+string(symbol_size)+', FRACTION=fraction'
                     printf, nulhis, strcompress(pltcmd)
                     IF debug_w THEN BEGIN
                      print, pltcmd
                     ENDIF
                     IF mean_sc_only NE 0 THEN BEGIN
                      res = execute(pltcmd[0])
                     ENDIF
                     IF mean_sc_only EQ 2 THEN BEGIN
                                ; add month info win[0]*win[1]
                        pltcmd = 'xyouts,fldrem_t2-(maxc2-minc2)/(0.5*win[0]*win[1]),fldrem_t1,string(long(findgen(12))+1),charsize=1.3,alignment=0.5'
                        printf, nulhis, strcompress(pltcmd)
                        IF debug_w THEN BEGIN
                         print, pltcmd
                        ENDIF
                        res = execute(pltcmd[0])
                     ENDIF
                  ENDELSE
               ENDIF

               ; overplot sigma contours in T-S plane
               IF (cmd.var EQ 'sosstsst' and cmd.var2 EQ 'sosaline') OR (cmd.var EQ 'votemper' and cmd.var2 EQ 'vosaline') THEN BEGIN
                  IF noerase EQ 0 THEN BEGIN
                     min_t = minc-2.0
                     max_t = maxc+2.0
                     min_s = minc2-2.0
                     max_s = maxc2+2.0
                     delta_t = 0.5
                     delta_s = 0.05
                     t_vec = findgen((max_t-min_t)/delta_t+1)*delta_t+min_t
                     s_vec = findgen((max_s-min_s)/delta_s+1)*delta_s+min_s
                     t_ = t     ; since t is already defined
                     t = t_vec
                     FOR i = 1, (max_s-min_s)/delta_s DO t = [[t], [t_vec]]
                     s = s_vec
                     FOR i = 1, (max_t-min_t)/delta_t DO s = [[s], [s_vec]]
                     s = transpose(s)
                     sr=sqrt(abs(s))
                     r1=((((6.536332E-9*t-1.120083E-6)*t+1.001685E-4)*t $
                          -9.095290E-3)*t+6.793952E-2)*t+999.842594
                     r2=(((5.3875E-9*t-8.2467E-7)*t+7.6438E-5)*t-4.0899E-3)*t+8.24493E-1
                     r3=(-1.6546E-6*t+1.0227E-4)*t-5.72466E-3
                     rhopn = ( ( 4.8314E-4*s + r3*sr +r2)*s +r1) -1000.0
                     min_sig = round(min(rhopn)+0.5)-1.
                     max_sig = round(max(rhopn)-0.5)+1.
                     delta_sig = 1. ; define contour interval here!
                     levels = findgen((max_sig-min_sig)/delta_sig+1)*delta_sig+min_sig
                     labels = levels
                     labels[*] = 1
                     contour, rhopn, s, t, /overplot, levels = levels, c_labels = labels, c_linestyle = 0
                     t = t_
                  ENDIF
               ENDIF

               printf, nulhis, 'boite_yfx=',boxyfx
               printf, nulhis, strcompress(pltcmd)

               tmask = t
               ; force read grid for next window
               cmd_prev.grid = 'toto'

