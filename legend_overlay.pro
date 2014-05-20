;+
;
; @todo
; should be a function
;
; @version
; $Id: legend_overlay.pro 246 2010-07-12 14:03:17Z ericg $
;
;-
; print legend (line + text) in pltt type 't' (called from plt_map)
                                ; domain_y = minc,maxc /
                                ; domaint=time[0], time[jptmax-1]
IF debug_w THEN BEGIN
   print, ' in legend_overlay, iover=', iover
ENDIF
;
; decode legend format from command line (see def_work cmdi.proj)
;
leg_pos = strmid(leg_format, 0, 2); (UL=-1, UR, LR, LL)
IF strpos(leg_format, '_') NE -1 THEN BEGIN
   txt_format = strmid(leg_format, 3, strlen(leg_format))
ENDIF ELSE BEGIN
   txt_format = default_txt_format
ENDELSE

IF leg_format NE 'N' THEN BEGIN 
IF txt_format NE 'N' THEN BEGIN 

; when there is more than one seasonal cycle
   jptmax = jpt * nb_cycles
   IF debug_w THEN BEGIN
      print, '  leg_format, leg_pos: ', leg_format, leg_pos
      print, '  jpt, jptmax, nb_cycles: ', jpt, jptmax, nb_cycles
   ENDIF
   index = iover
   nmax = nover
   
   IF ensbl_code NE '' THEN BEGIN 
      CASE ensbl_code OF 
         'meme': BEGIN 
         END 
         'mame': BEGIN 
            cmd.exp = ensbl_def.model_names[iens]
            index = iens+1
            nmax = jpe
         END 
         ELSE: BEGIN 
            argn = strsplit(ensbl_code, 'm', /EXTRACT)
            imod = long(argn[0])
            cmd.exp = ensbl_def.model_names[imod-1]
         END
      ENDCASE 
   ENDIF 

; legend line text
   leg_txt = ''
   IF strpos(txt_format, 'E') NE -1 THEN BEGIN ; experiment
      leg_txt = leg_txt+cmd.exp+' '
   ENDIF
   IF strpos(txt_format, 'V') NE -1 THEN BEGIN ; variable
      leg_txt = leg_txt+cmd.var+' '
   ENDIF
   IF strpos(txt_format, 'L') NE -1 THEN BEGIN ; legend
      leg_txt = leg_txt+field.legend+' '
   ENDIF
   IF strpos(txt_format, 'U') NE -1 THEN BEGIN ; unit
      leg_txt = leg_txt+'('+field.units+') '
   ENDIF
   IF strpos(txt_format, 'T') NE -1 THEN BEGIN ; date 1
      leg_txt = leg_txt+cmd.date1+' '
   ENDIF
   IF strpos(txt_format, 'B') NE -1 THEN BEGIN ; box
      leg_txt = leg_txt+legz+' '
   ENDIF
   IF strpos(txt_format, 'S') NE -1 THEN BEGIN ; std dev txt
      leg_txt = leg_txt+' std='+stddev_txt+' '
   ENDIF
   IF strpos(txt_format, 'M') NE -1 THEN BEGIN ; member if ensemble
      leg_txt = leg_txt+ensbl_legend_member
   ENDIF
   
   CASE plttyp OF
      'yfx': leg_txt = coeff_txt+coefflu_txt+coeffm_txt
      ELSE:
   END
   
; keep length of leg_txt fixed to iover=0 value

   IF iover EQ 1 THEN BEGIN 
      leg_txt_length = strlen(leg_txt)
   ENDIF 
   

   IF debug_w THEN BEGIN
      print, ' iover, leg_pos, leg_txt + length = ', iover, ' ', leg_pos, ' ', leg_txt, strlen(leg_txt)
   ENDIF
; px_n = percentage of window size function of number of windows
; initial percentage px_1r is function of strlen(leg_txt)
; n_leg_max = max number of lines in legend box

   CASE nwin_tot OF
      1: BEGIN
         px_1r = float(93-leg_txt_length)/100
         px_2r = px_1r+0.05 & px_1l = 0.03 & px_2l = 0.08
         n_leg_max = 20 & txt_ft_size = 1.4 & y_scale_txt = 0.998
         delta_txt_add = 0.05
      END
      2: BEGIN
         px_1r = float(84-leg_txt_length)/100
         px_2r = px_1r+0.1 & px_1l = 0.03 & px_2l = 0.13
         n_leg_max = 20 & txt_ft_size = 1.4 & y_scale_txt = 0.995
         delta_txt_add = 0.07
      END
      3: BEGIN
         px_1r = float(84-leg_txt_length)/100
         px_2r = px_1r+0.1 & px_1l = 0.03 & px_2l = 0.13
         n_leg_max = 15 & txt_ft_size = 1.2 & y_scale_txt = 0.995
         delta_txt_add = 0.1
      END
      ELSE: BEGIN
         px_1r = float(75-leg_txt_length)/100
         px_2r = px_1r+0.1 & px_1l = 0.03 & px_2l = 0.13
         n_leg_max = 15 & txt_ft_size = 1.1 & y_scale_txt = 0.995
         delta_txt_add = 0.1
      END
   ENDCASE

; size between end of line and text (% of window size)
   delta_txt = 0.01
   px_tr = px_2r+delta_txt
   px_tl = px_2l+delta_txt
   
   IF plttyp EQ 'yfx' THEN BEGIN
      px_tl = 0.04
   ENDIF
   
   IF debug_w THEN BEGIN
      print, ' nwin_tot, n_leg_max = ', nwin_tot, n_leg_max
   ENDIF

; find size of window
   yrange = !y.crange
   xrange = !x.crange

   delta = (yrange[1]-yrange[0])/(float(n_leg_max)+2.)
; position of legend line (x_leg_1 to 2,y_leg) and text x_leg_t,; y_leg_t
   IF strmid(leg_pos, 0, 1) EQ 'L' THEN BEGIN
 ; (L)ower
      y_leg = yrange[0]+(nmax-float(index)+0.5)*delta
   ENDIF ELSE BEGIN
 ; (U)pper
      y_leg = yrange[1]-(float(index))*delta
   ENDELSE
; special case of plt1d,'z'

   IF plttyp EQ 'plt1d' AND plt1dtyp EQ 'z' THEN BEGIN
      ymin = boite_plt1d[4]
      ymax = boite_plt1d[5]
      y_dim = boite_plt1d[5]-boite_plt1d[4]
      delta = y_dim/(float(n_leg_max)+2.)
      IF strmid(leg_pos, 0, 1) EQ 'L' THEN BEGIN
                                ; (L)ower
         y_leg = ymax-(nover-float(iover)+0.5)*delta
      ENDIF ELSE BEGIN
                                ; (U)pper
         y_leg = ymin+(float(iover)+0.5)*delta
      ENDELSE
   ENDIF
   y_leg_t = y_leg*y_scale_txt
; case pltt plt1d
   IF debug_w THEN BEGIN
      print, '   size(time) ', size(time)
      print, '   minc/maxc =', minc, maxc
   ENDIF

   CASE plttyp OF
      'pltt': BEGIN
         IF strmid(leg_pos, 1, 1) EQ 'R' THEN BEGIN
                                ; (R)ight
            x_leg_1 = xrange[0]+px_1r*(xrange[1]-xrange[0]) 
            x_leg_2 = xrange[0]+px_2r*(xrange[1]-xrange[0])
            x_leg_t = xrange[0]+px_tr*(xrange[1]-xrange[0])
         ENDIF ELSE BEGIN
                                ; (L)eft
            x_leg_1 = xrange[0]+px_1l*(xrange[1]-xrange[0]) 
            x_leg_2 = xrange[0]+px_2l*(xrange[1]-xrange[0])
            x_leg_t = xrange[0]+px_tl*(xrange[1]-xrange[0])
         ENDELSE
      END
      'plt1d': BEGIN
         CASE plt1dtyp OF
            'x': BEGIN
               xmin = boite_plt1d[0]
               x_dim = boite_plt1d[1]-boite_plt1d[0]
            END
            'y': BEGIN
               xmin = boite_plt1d[2]
               x_dim = boite_plt1d[3]-boite_plt1d[2]
            END
            'z': BEGIN
               xmin = minc
               x_dim = maxc-minc
            END
            ELSE:
         ENDCASE
         IF strmid(leg_pos, 1, 1) EQ 'R' THEN BEGIN
            ; (R)ight
            x_leg_1 = xmin+px_1r*x_dim
            x_leg_2 = xmin+px_2r*x_dim
            x_leg_t = xmin+px_tr*x_dim
         ENDIF ELSE BEGIN
            ; (L)eft
            x_leg_1 = xmin+px_1l*x_dim
            x_leg_2 = xmin+px_2l*x_dim
            x_leg_t = xmin+px_tl*x_dim
         ENDELSE
      END
      'yfx': BEGIN
         x_leg_1 = -1.e10
         x_leg_2 = -1.e10
         IF debug_w THEN BEGIN
            print, 'minc2/maxc2 =', minc2, maxc2
         ENDIF
         xmin = minc2
         x_dim = maxc2-minc2
         IF strmid(leg_pos, 1, 1) EQ 'R' THEN BEGIN
       ; (R)ight
            x_leg_t = xmin+px_tr*x_dim
         ENDIF ELSE BEGIN
       ; (L)eft
            x_leg_t = xmin+px_tl*x_dim
         ENDELSE
      END 
      'ybinx': BEGIN
         x_leg_1 = -1.e10
         x_leg_2 = -1.e10
         IF debug_w THEN BEGIN
            print, 'minc2/maxc2 =', minc2, maxc2
         ENDIF
         xmin = minc2
         x_dim = maxc2-minc2
         IF strmid(leg_pos, 1, 1) EQ 'R' THEN BEGIN
       ; (R)ight
            x_leg_t = xmin+px_tr*x_dim
         ENDIF ELSE BEGIN
       ; (L)eft
            x_leg_t = xmin+px_tl*x_dim
         ENDELSE
      END
   ENDCASE


   leg = [y_leg, y_leg]

; draw line
   overc_l = strmid(overc,7, strlen(overc)-1)
   CASE plttyp OF
      'pltt': BEGIN
         time_prevl = time
         time = [x_leg_1, x_leg_2]
         jpt_o = jpt
         jpt = 2
         pltcmd = 'pltt, leg, '+''''+'t'+''''+overc_l+ ',ov1d=1'+',/noerase'
         printf, nulhis, strcompress(pltcmd)
         res = execute(pltcmd)
         IF debug_w THEN BEGIN 
            print, pltcmd
         ENDIF 
         time = time_prevl
         jpt = jpt_o
      END
      'plt1d': BEGIN
         x_leg = [x_leg_1, x_leg_2]
         pltcmd = 'oplot,x_leg,leg'+colov+line_thick_txt+', LINESTYLE='+string(line_style[iover-1]-1)
         printf, nulhis, strcompress(pltcmd)
         res = execute(pltcmd)
         IF debug_w THEN BEGIN 
            print, pltcmd
         ENDIF 
      END
      ELSE:
   ENDCASE
   IF debug_w THEN BEGIN 
      print, '   overc_l, leg, x_leg_1, x_leg_2 = ', overc_l, leg, x_leg_1, x_leg_2
      print, '   x_leg_t, y_leg_t, leg_txt = ', x_leg_t, y_leg_t, leg_txt
   ENDIF 
   
; draw text
   xyouts, x_leg_t, y_leg_t, leg_txt, charsize = txt_ft_size, color = string(line_color[iover-1]), charthick = 1., font = 1.

ENDIF 
ENDIF 
