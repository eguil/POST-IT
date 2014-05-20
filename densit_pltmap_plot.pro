;+
;
; time series of rms deviation on a sigma surface (Paul William)
;
; @history
; - fplod 20100114T144659Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * fix array syntax on fld
;
; @version
; $Id: densit_pltmap_plot.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-

                  sizefld=size(fld)
                  fld=fld[*,*,round((sig_surface-sig_min)/sig_del),*]
                  fld = reform(fld, sizefld[1], sizefld[2], sizefld[4])
                  fld2 = fltarr(sizefld[4])
                  ;FOR count = 0, sizefld[4]-1 DO fld2[count] = mean(fld[*,*,count], /nan)
                  FOR count = 0, sizefld[4]-1 DO fld2[count] = stddev(fld[*,*,count], /nan)
                  fld = fld2
                  mask_z, fld, cmd, boite_plt1d, dimplot, legz
                  niveau = 1
                  sin = ''
                  take_log = ',take_log=0'
                  pltcmd = 'plt1d,{a:fld, d:'''+date_txt+''', n:'''+varlegend+' Stddev '+legz+legendsurf+''', u:'''+field.units+''', g:'''+vargrid+'''},'''+plt1dtyp+''''+mincmd+maxcmd+intcmd+', boite=boite_plt1d'+overc+sin+com_strplt+take_log
                  IF cmd.timave EQ '1y' AND cmd.exp NE 'SQ2' THEN BEGIN ; curve-fitting and over-plot
                     time_years = FLOAT(INDGEN(sizefld(4)))
                     weights = replicate(1.0, sizefld(4))
                     params = [fld[0],-0.1,0.] ; initial guess
                     fldfit = CURVEFIT(time_years, fld, weights, params, error, FUNCTION_NAME='gfunct')
                     PRINT, 'Best fit: sigma = ', params[0], '*exp(', params[1], 't) + ', params[2]
                     PRINT, 'errors: ', error
                     PRINT, 'e-folding time (years) = ', -1.0/params[1]
                     look = ', linestyle=0,xthick=2,ythick=2,zthick=2'
                     com_strplt = ',petit = ['+string(win[0])+','+string(win[1])+','+string(win[2])+']'+filltxt+nocoltxt+', LANDSCAPE = '+string(landscape)+', NOCOLORBAR = '+string(nocolorbar)+', NOERASE = '+string(noerase)+look+labstr+c_anot+colbar+cal_typ+',cont_thick=2'+type_yz+window_number+tit_str+xlogax
                     pltcmd = 'plt1d,{a:fld, d:'''+date_txt+''', n:'''+varlegend+' Stddev (tau ='+strcompress(-1.0/params[1])+'yr) '+legz+legendsurf+''', u:'''+field.units+''', g:'''+vargrid+'''},'''+plt1dtyp+''''+mincmd+maxcmd+intcmd+', boite=boite_plt1d'+overc+sin+com_strplt+take_log
                     res = execute(pltcmd)
                     noerase = 1
                     look = ', linestyle=2,xthick=2,ythick=2,zthick=2'
                     com_strplt = ',petit = ['+string(win[0])+','+string(win[1])+','+string(win[2])+']'+filltxt+nocoltxt+', LANDSCAPE = '+string(landscape)+', NOCOLORBAR = '+string(nocolorbar)+', NOERASE = '+string(noerase)+look+labstr+c_anot+colbar+cal_typ+',cont_thick=2'+type_yz+window_number+tit_str+xlogax
                     pltcmd = 'plt1d,{a:fldfit, d:'''+date_txt+''', n:'''+varlegend+' Stddev (tau ='+strcompress(-1.0/params[1])+'yr) '+legz+legendsurf+''', u:'''+field.units+''', g:'''+vargrid+'''},'''+plt1dtyp+''''+mincmd+maxcmd+intcmd+', boite=boite_plt1d'+overc+sin+com_strplt+take_log
                  ENDIF
