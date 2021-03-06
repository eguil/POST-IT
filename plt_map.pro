;+
;
; make window plot
;
; @param CMD {in}{required}{type=structure}
; command line of window
;
; @param IPLOT {in}{required}{type=integer}
;
; @param WIN {in}
; position of window (petit[,,,])
;
; @param IOVER {in}{required}{type=integer}
; overlay index
;
; @param LANDSCAPE {in}
;
; @examples
;
; IDL> cmd={plt:'xy',var:''}
; IDL> iplot=1
; IDL> win=[0,0,0]
; IDL> iover=1
; IDL> plt_map, cmd, iplot, win, iover, landscape
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; <propost_it>data_read</propost_it>
;
; @todo
; finish check parameters
;
; @history
; - fplod 20100114T144659Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * fix array syntax on fld, pltcmd and time
;
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters (not finish)
;
; - EG 19-2-99
;
; @version
; $Id: plt_map.pro 246 2010-07-12 14:03:17Z ericg $
;
;-
PRO plt_map, cmd, iplot, win, iover, landscape
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
;
 IF debug_w THEN BEGIN
  info = report('enter ...')
 ENDIF
;
 usage='plt_map, cmd, iplot, win, iover, landscape'
;
 nparam = N_PARAMS()
 IF (nparam LT 4) THEN BEGIN
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
; ++ check cmd tags (name, type)

 arg_type = size(iplot,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type iplot' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
;
 arg_type = size(iover,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type iover' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type=size(nb_cycles,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type nb_cycles' $
          + '!C' $
          + 'Usage : ' + usage])
  stop
 ENDIF

;
; empty window case for on=2
;
   ioverchk = iover

   cmd_wrk = cmd

   IF cmd.on EQ 2 THEN BEGIN
    GOTO, finish
   ENDIF
;
; incompatible options of plt_def and post_it
;
   trend_typp = cmd.trend
;
; =============
;  1.read data
; =============
;
; type of plot

; time series of rms deviation on a sigma surface?
   st_rms = 0
   IF strpos(cmd.plt, 'st') NE -1 AND strpos(cmd.var, '@s') GT 4 THEN BEGIN
      st_rms = 1
      sig_surface = strmid(cmd.var, strpos(cmd.var, '@s')+2, strlen(cmd.var))
      cmd.var = strmid(cmd.var, 0, strpos(cmd.var, '@s'))
      legendsurf = '  on sigma='+sig_surface
   ENDIF

   CASE cmd.plt OF
      'x': cmd.plt = cmd.plt+'_global'
      'y': cmd.plt = cmd.plt+'_global'
      'z': cmd.plt = cmd.plt+'_global'
      's': cmd.plt = cmd.plt+'_global'
      'yz': cmd.plt = cmd.plt+'_global'
      ELSE:
   ENDCASE

; Definition of plttyp, hotyp, dimplot
   datyp = def_dptyp(cmd)

   plttyp = datyp.plttyp
   hotyp = datyp.hotyp
   dimplot = datyp.dimplot
   pltztyp = datyp.pltztyp
   plt1dtyp = datyp.plt1dtyp
   splot = datyp.splot

   hotypchk = hotyp

; stat computations:

   CASE cmd.plt OF
   'xyt': BEGIN
           plttyp = 'plt'
           hotyp = 'xyt'
           dimplot = 2
          END
       ELSE:
   ENDCASE

; xy plot on density surface (cmd.var = <var>@s<sig_surf>
   legsurf = ''
   sig_surf = 0.
   IF  strpos(cmd.var, '@s') GT 4 THEN BEGIN
      splot = 1
      sig_surf = strmid(cmd.var, strpos(cmd.var, '@s')+2, strlen(cmd.var))
      cmd.var = strmid(cmd.var, 0, strpos(cmd.var, '@s'))
      legsurf = '  on sigma='+sig_surf
   ENDIF

; contour of coasts

   CASE splot OF
      1: c_cont = 0
      0: c_cont = (!d.n_colors-1) < 255
   ENDCASE

; scatter plot y=f(x)

   IF strpos(cmd.var, '=f(') GE 1 THEN BEGIN
      CASE cmd.plt OF
         'xy': cmd.plt = cmd.plt+'_global'
         ELSE:
      ENDCASE
      plttyp = 'yfx'
      dimplot = 1
   ENDIF

; binning plot y=bin(x)

   IF strpos(cmd.var, '=bin(') GE 1 THEN BEGIN
      CASE cmd.plt OF
         'xy': cmd.plt = cmd.plt+'_global'
         ELSE:
      ENDCASE
      plttyp = 'ybinx'
      dimplot = 1
   ENDIF

   IF cmd.grid EQ "@grden" THEN BEGIN 
      bin_read = 1 ; read already bined file
   ENDIF ELSE BEGIN 
      bin_read = 0 
   ENDELSE   

; vector plot

   IF strpos(cmd.var, '[') EQ 0 THEN BEGIN
      vecplot = 1
      ; sampling of vector
      IF vector_sample GE 2 THEN BEGIN
         vect_sample = ',UNVECTSUR=strtrim(string(vector_sample),2)'
      ENDIF ELSE BEGIN
         vect_sample = ''
      ENDELSE
   ENDIF ELSE BEGIN
      vecplot = 0
   ENDELSE

; spectrum

   spectplt = 0
   IF strpos(cmd.plt, '@s') GE 1 THEN BEGIN
      IF hotyp EQ 't' THEN BEGIN
       spectplt = 1
      ENDIF ELSE BEGIN
       print, '    *** spectrum [@s] applies to 1D time serie only (t_<box>)'
       stop
      ENDELSE
      cmd.plt = strmid(cmd.plt, 0, strpos(cmd.plt, '@s'))
   ENDIF

; wavelet

   waveplt = 0
   IF strpos(cmd.plt, '@w') GE 1 THEN BEGIN
      IF hotyp EQ 't' THEN BEGIN
       waveplt = 1
      ENDIF ELSE BEGIN
       print, '    *** wavelet [@w] applies to 1D time serie only (t_<box>)'
       stop
      ENDELSE
      cmd.plt = strmid(cmd.plt, 0, strpos(cmd.plt, '@w'))
   ENDIF

; running std dev

   run_stddev = 0
   IF strpos(cmd.plt, '@r') GE 1 THEN BEGIN
      IF hotyp NE 't' THEN BEGIN
         print, '    *** running [@r<win>] std dev applies to 1D time serie only (t_<box> & 1m@t412)'
         stop
      ENDIF
      run_stddev = strmid(cmd.plt, strpos(cmd.plt, '@r')+2, strlen(cmd.plt))
      cmd.plt = strmid(cmd.plt, 0, strpos(cmd.plt, '@r'))
   ENDIF


; define grids
; read if new type + triangule
;
   cmd1_back = cmd
   sw_diffg = 0
   def_grid, cmd
   IF debug_w THEN BEGIN
    print, '   after def_grid in plt_map: cmd.grid/varexp/vargrid ',cmd.grid,' ', varexp, ' ', vargrid
   ENDIF

; vertical average if 3D field
   vert_switch = 0
   IF plttyp EQ 'plt' AND vert_type ne '0' THEN BEGIN
    vert_switch = 1
   ENDIF
   IF plttyp EQ 'pltt' AND vert_type ne '0' THEN BEGIN
    vert_switch = 1
   ENDIF
   IF plttyp EQ 'plt1d' AND vert_type ne '0' THEN BEGIN
    vert_switch = 1
   ENDIF
   IF plt1dtyp EQ 'z' THEN BEGIN
    vert_switch = 0
   ENDIF

; read all data if netcdf output

   IF debug_w THEN BEGIN
    print, '    force_all_data_read in pltmap: ', force_all_data_read
   ENDIF

   IF cmd.out NE 'cdf' THEN BEGIN
      all_data_str = ''
   ENDIF ELSE BEGIN
      all_data_str = ',/ALL_DATA'
   ENDELSE

; Define ensemble simulation <cmd.exp>#<ensemble_codes>

   toggle_ensbl_diff = 0
   ensbl_diff = 0
   ensbl_code_diff = ''
   IF strpos(cmd.exp, '#') NE -1 THEN BEGIN
      ensbl_code = strmid(cmd.exp, strpos(cmd.exp, '#')+1, strlen(cmd.exp))
      exp_ens = strmid(cmd.exp, 0, strpos(cmd.exp, '#'))
      IF strpos(ensbl_code, '-') NE -1 THEN BEGIN
         ; difference
         argvar = strsplit(ensbl_code,'-', /EXTRACT)
         ensbl_diff = 1
         ensbl_diff_other = 0
         IF strmid(argvar[1],0,1) NE 'm' THEN BEGIN ; other simulation
            ensbl_diff_other = 1
            cmd.exp = exp_ens+'-'+argvar[1]
            ensbl_code = argvar[0]
         ENDIF ELSE BEGIN ; remove other member or ensemble mean (deal with it in ensbl_data_extract.pro) 
            cmd.exp = exp_ens
            ensbl_code_diff = argvar[1]
            ensbl_code = argvar[0]
         ENDELSE 
      ENDIF ELSE BEGIN
         cmd.exp = exp_ens
      ENDELSE  
      ; define ensemble properties
      result=def_ensbl(exp_ens)
      IF result EQ -1 THEN BEGIN 
         stop
      ENDIF 
   ENDIF ELSE BEGIN
      ensbl_code = ''
   ENDELSE 

; ===========
;  Read data
; ===========

   really_1m_st = 1

   IF cmd.timave EQ '1y' AND strmid(cmd.plt, 0, 2) EQ 'st' AND bin_read EQ 0 THEN BEGIN
      @densit_pltmap_read
   ENDIF ELSE BEGIN
      pltcmd = 'field = data_read(cmd,'''+hotyp+''','''+plttyp+''','+string(dimplot)+','+string(iover)+all_data_str+', ZMTYP = '''+cmd.plt+''')'
      printf, nulhis, strcompress(pltcmd)
      printf, nulhis, ' '
      IF execute(pltcmd) EQ 0 THEN BEGIN
       stop
      ENDIF
   ENDELSE

; data specific treatments
   IF cmd.var EQ 'sla' THEN BEGIN
    field.origin = 'diff'
   ENDIF
   IF cmd.var EQ 'sosstsst' AND cmd.exp EQ 'HadISST1' THEN BEGIN
      ; limit data to -1.8 C
      idx = where(field.data EQ valmask)
      field.data = field.data > (-1.8)
      IF idx[0] NE -1 THEN BEGIN
       field.data[idx] = valmask
      ENDIF
   ENDIF

;help, field,/structure
   IF n_elements(field.data) EQ 1 THEN BEGIN
    stop
   ENDIF

; change cmd.var if macro
   CASE STRMID(cmd.var, 0, 2) OF
      '@@': BEGIN
         cmd.var = field.name
         cmd.var = STRMID(cmd.var, 2, 15)
         IF debug_w THEN BEGIN
          print, 'cmd.var : ', cmd.var
         ENDIF
      END
      ELSE:
   ENDCASE
   IF vecplot EQ 1 THEN BEGIN
      ; vectors case
      CASE STRMID(cmd.var2, 0, 2) OF
         '@@': BEGIN
            cmd.var2 = STRMID(cmd.var2, 2, 15)
         END
         ELSE:
      ENDCASE
   ENDIF

; spectum plot
   IF spectplt EQ 1 THEN BEGIN
    plttyp = 'spec'
   ENDIF
; wavelet plot
   IF waveplt EQ 1 THEN BEGIN
    plttyp = 'wavelet'
   ENDIF

   xlogax = ''

; density pojection plot (splot=1)
   IF splot EQ 1 THEN BEGIN
      ; keep grid attributes
      izminmesh_b = izminmesh
      izmaxmesh_b = izmaxmesh
      jpk_b = jpk
      jpkglo_b = jpkglo
      gdept_b = gdept
      gdepw_b = gdepw
      e3t_b = e3t
      e3w_b = e3w
      tmask_b = tmask
;      IF cmd.var EQ 'vosigvol' THEN BEGIN
;       xlogax = ',xlog = 1'
;      ENDIF
;  modify vertical grid -> sigma
      IF strmid(cmd.var, 0, 2) EQ '@@' THEN BEGIN  
         n_sig = (sig_max - sig_min)/sig_del + 1
         z_sig = sig_min+findgen(n_sig)*sig_del
         izminmesh = 0
         izmaxmesh = n_sig-1
         jpk = long(izmaxmesh-izminmesh+1)
         jpkglo = jpk
         gdept = z_sig
         gdepw = gdept
         e3t = shift(gdept, 1)-gdept
         e3t[0] = e3t[1]
         e3w = e3t
      ENDIF 
;ELSE BEGIN 
;         izminmesh = 0
;         izmaxmesh = (size(gdept))[1] - 1
;      ENDELSE 
      prof1 = sig_min
      prof2 = sig_max
;      grille,mask,glam,gphi,gdep,nx,ny,nz,premierx,premiery,premierz,dernierx,derniery,dernierz    ; added PDW 12/5/04
;      tmask = mask  ; added PDW 12/5/04
      tmask = reform(reform(tmask[*, *, 0], jpi*jpj)#replicate(1, jpk), jpi, jpj, jpk)
      domdef
   END

   IF debug_w THEN BEGIN
    print, '   cmd after data_read in plt_map = ', cmd
   ENDIF
;
; ======================
;  2. window attributes
; ======================
;

; find field attributes
; ---------------------

   fldatt = {name:'', assos:'', min: 0.0, max: 0.0, int: 0.0, mult: 0.0, add: 0.0, unit: '', mid: 0.0, homin:0.0, homax:0.0, min1d:0.0, max1d:0.0, dmax:0.0, spec:0.0}

; which variable to look for

   CASE plttyp OF
      'ybinx': BEGIN
         ; if var3 exists then var = var1/var3
         IF var3_ybinx NE "" THEN BEGIN
          var_att = cmd.var+"/"+var3_ybinx
         ENDIF ELSE BEGIN
          var_att = cmd.var
         ENDELSE
      END
      ELSE: var_att = cmd.var
   ENDCASE

   IF debug_w THEN BEGIN
    print, '    Looking for min/max/iso attributes for variable :', var_att
   ENDIF

; extremum
   fldext = fld_pltext(var_att, plttyp, dimplot, hotyp)

   fldatt.name = fldext.name
   fldatt.min = fldext.min
   fldatt.max = fldext.max
   fldatt.homin = fldext.homin
   fldatt.homax = fldext.homax
   fldatt.min1d = fldext.min1d
   fldatt.max1d = fldext.max1d
   fldatt.dmax = fldext.dmax
   fldatt.spec = fldext.spec
   fldatt.assos = fldext.assos

; interval + change of unit
   fldint = fld_pltint(var_att, plttyp, dimplot, hotyp)

;   help, fldint, /struct

   fldatt.int = fldint.int
   fldatt.mult = fldint.mult
   fldatt.add = fldint.add
   fldatt.unit = fldint.unit
   fldatt.mid = fldint.mid

   IF debug_w THEN BEGIN
    print, 'plt_map 1 :', fldatt.mult, fldatt.add, fldatt.min, fldatt.max
   ENDIF

   IF fldatt.mult NE 1. OR fldatt.add NE 0. THEN BEGIN
    field.units = fldatt.unit
   ENDIF
   fld = field.data*fldatt.mult + fldatt.add
   IF (where(field.data EQ valmask))[0] NE -1 THEN BEGIN
    fld[where(field.data EQ valmask)] = valmask
   ENDIF

;  field 2 if needed

   IF  plttyp EQ 'yfx' OR plttyp EQ 'ybinx' OR  vecplot EQ 1 THEN BEGIN
      fldatt2 = fldatt

      fldext2 = fld_pltext(cmd.var2, plttyp, dimplot, hotyp)

      fldatt2.name = fldext2.name
      fldatt2.min = fldext2.min
      fldatt2.max = fldext2.max
      fldatt2.homin = fldext2.homin
      fldatt2.homax = fldext2.homax
      fldatt2.min1d = fldext2.min1d
      fldatt2.max1d = fldext2.max1d
      fldatt2.dmax = fldext2.dmax

; interval + change of unit
      fldint2 = fld_pltint(cmd.var2, plttyp, dimplot, hotyp)

      fldatt2.int = fldint2.int
      fldatt2.mult = fldint2.mult
      fldatt2.add = fldint2.add
      fldatt2.unit = fldint2.unit
      fldatt2.mid = fldint2.mid

      IF fldatt2.mult NE 1. OR fldatt2.add NE 0. THEN BEGIN
       field.units2 = fldatt2.unit
      ENDIF
      fld2 = field.data2*fldatt2.mult + fldatt2.add
      IF (where(field.data2 EQ valmask))[0] NE -1 THEN BEGIN
       fld2[where(field.data2 EQ valmask)] = valmask
      ENDIF

   ENDIF

;  field 3 if needed

   IF  plttyp EQ 'ybinx' AND var3_ybinx NE "" THEN BEGIN
      fldatt3 = fldatt

      fldext3 = fld_pltext(var3_ybinx, plttyp, dimplot, hotyp)

      fldatt3.name = fldext3.name
      fldatt3.min = fldext3.min
      fldatt3.max = fldext3.max
      fldatt3.homin = fldext3.homin
      fldatt3.homax = fldext3.homax
      fldatt3.min1d = fldext3.min1d
      fldatt3.max1d = fldext3.max1d
      fldatt3.dmax = fldext3.dmax

; interval + change of unit
      fldint3 = fld_pltint(var3_ybinx, plttyp, dimplot, hotyp)

      fldatt3.int = fldint3.int
      fldatt3.mult = fldint3.mult
      fldatt3.add = fldint3.add
      fldatt3.unit = fldint3.unit
      fldatt3.mid = fldint3.mid

      IF fldatt3.mult NE 1. OR fldatt3.add NE 0. THEN BEGIN
       field.units3 = fldatt3.unit
      ENDIF
      fld3 = field.data3*fldatt3.mult + fldatt3.add
      IF (where(field.data3 EQ valmask))[0] NE -1 THEN BEGIN
       fld3[where(field.data3 EQ valmask)] = valmask
      ENDIF

   ENDIF

; If running std dev change min/max
   IF run_stddev GT 0 THEN BEGIN
      fldatt.homin = 0.
      fldatt.homax = fldext.dmax
   ENDIF

; legend text
; -----------

;   IF plttyp EQ "pltt" THEN BEGIN
      CASE strmid(cmd.trend, 0, 1) OF
         '1': trd_txt = ' trend'
         '2': trd_txt = ' drift'
         '3': trd_txt = ' inverse trend'
         '4': trd_txt = ' anomaly'
         '5': trd_txt = ' filter'
         '7': trd_txt = ' month sel'
         ELSE: trd_txt = ''
      ENDCASE
   IF field_int EQ 1 AND dimplot EQ 1 THEN BEGIN
      trd_txt = trd_txt+' integral'
   ENDIF

   varlegend = field.legend+trd_txt+' ('+field.units+')'

   CASE plttyp OF
      'yfx': varlegend2 = field.legend2+trd_txt+' ('+field.units2+')'
      'ybinx': BEGIN
         IF strmid(cmd2.trend, 0, 1) EQ '4' THEN BEGIN
          varlegend = field.legend2+trd_txt+' ('+field.units2+')'
         ENDIF ELSE BEGIN
          varlegend = field.legend2+' ('+field.units2+')'
         ENDELSE
         varlegend2 = field.legend+trd_txt+' ('+field.units+')'
      END
      ELSE: BEGIN
         IF run_stddev GT 0 THEN BEGIN
          trd_txt = trd_txt+' running Std Dev ['+string(run_stddev, format = '(I3)')+']'
         ENDIF
      END
   ENDCASE

; ensemble specification

   IF ensbl_code NE '' THEN BEGIN 
      varlegend = varlegend+' '+ensbl_legend
   ENDIF 

; date text

   CASE strmid(cmd.timave, 0, 2) OF
      '1m': BEGIN
         mn = def_month(cmd.timave, cmd.date1)
         IF strmid(cmd.timave, 0, 3) EQ '1mm' THEN BEGIN
          date_txt = mn+' ('+strmid(cmd.date1, 3, strlen(cmd.date1)-3)+')'
         ENDIF ELSE BEGIN
          date_txt = mn+' '+strmid(cmd.date1, 0, strlen(cmd.date1)-2)
         ENDELSE
      END
      '3m': BEGIN
         mn = def_month(cmd.timave, cmd.date1)
         IF strmid(cmd.timave, 0, 3) EQ '3mm' THEN BEGIN
          date_txt = mn+' ('+strmid(cmd.date1, 3, strlen(cmd.date1)-3)+')'
         ENDIF ELSE BEGIN
          date_txt = mn+' '+strmid(cmd.date1, 0, strlen(cmd.date1)-2)
         ENDELSE
      END
      '6m': BEGIN
         mn = def_month(cmd.timave, cmd.date1)
         IF strmid(cmd.timave, 0, 3) EQ '6mm' THEN BEGIN
          date_txt = mn+' ('+strmid(cmd.date1, 3, strlen(cmd.date1)-3)+')'
         ENDIF ELSE BEGIN
          date_txt = mn+' '+strmid(cmd.date1, 0, strlen(cmd.date1)-2)
         ENDELSE
      END
      ELSE:date_txt = cmd.timave+' '+cmd.date1
   ENDCASE

   CASE plttyp OF
      'pltt':BEGIN
         date_txt = cmd.timave
         IF strmid(cmd.timave, 1, 2) EQ 'mm' THEN BEGIN
          date_txt = cmd.timave+' ('+strmid(cmd.date1, 3, strlen(cmd.date1)-3)+')'
         ENDIF
      END
      'yfx': BEGIN
         IF hotyp NE '-' THEN BEGIN
          date_txt = cmd.timave+' '+cmd.date1+' '+cmd.spec
         ENDIF
      END
      'ybinx': BEGIN
         IF hotyp NE '-' THEN BEGIN
          date_txt = cmd.timave+' '+cmd.date1+' '+cmd.spec
         ENDIF
      END
      'wavelet': BEGIN
         date_txt = cmd.timave+' Wavelet'
         IF strmid(cmd.timave, 1, 2) EQ 'mm' THEN BEGIN
          date_txt = cmd.timave+' Wavelet ('+strmid(cmd.date1, 3, strlen(cmd.date1)-3)+')'
         ENDIF
      END
      ELSE:
   ENDCASE

   IF ensbl_code NE '' THEN BEGIN 
      CASE plttyp OF
         'plt': BEGIN 
            date_txt =  'Start: '+date_txt+'+LT'+strtrim(string(ensbl_lt), 2)
         END 
         ELSE: 
      ENDCASE 
   ENDIF 

; min/max/int

; defined if fraction of x/y.range is added to plot domain

;   IF free_1d_minmax EQ 'no' THEN BEGIN
;    fraction = 0.
;   ENDIF ELSE BEGIN
;    fraction = 1.0
;   ENDELSE
   fraction = 0.

   CASE dimplot OF
      1: BEGIN
         CASE plttyp OF
         'pltt': BEGIN & minc = fldatt.homin & maxc = fldatt.homax & END
         'spec': BEGIN & minc = 0. & maxc = fldatt.spec & END
         'wavelet': BEGIN & minc = fldatt.homin & maxc = fldatt.homax & END
         'plt1d': BEGIN & minc = fldatt.min1d & maxc = fldatt.max1d & END
         'yfx': BEGIN ; y=f(x) scatter plot
            IF long(strmid(cmd.trend, 0, 1)) GT 0 THEN BEGIN
               IF run_stddev EQ 0 THEN BEGIN
                  minc = -fldatt.dmax & maxc = fldatt.dmax
                  minc2 = -fldatt2.dmax & maxc2 = fldatt2.dmax
               ENDIF ELSE BEGIN
                  minc = 0. & maxc = fldatt.dmax
                  minc2 = 0. & maxc2 = fldatt2.dmax
               ENDELSE
            ENDIF ELSE BEGIN
               minc = fldatt.min1d & maxc = fldatt.max1d
               minc2 = fldatt2.min1d & maxc2 = fldatt2.max1d
            ENDELSE
         END
         'ybinx': BEGIN ; y=bin(x) binning plot
            IF long(strmid(cmd.trend, 0, 1)) GT 0 THEN BEGIN
                  minc = -fldatt.dmax & maxc = fldatt.dmax
                  minc2 = fldatt2.min1d & maxc2 = fldatt2.max1d
            ENDIF ELSE BEGIN
               minc = fldatt.min1d & maxc = fldatt.max1d
               minc2 = fldatt2.min1d & maxc2 = fldatt2.max1d
            ENDELSE
            IF large_domain EQ 1 THEN BEGIN
               minc2 = -fldatt2.dmax & maxc2 = fldatt2.dmax
            ENDIF
            IF debug_w THEN BEGIN
             print, '   minc/maxc for 1d fld1/fld2', minc, maxc, minc2, maxc2
            ENDIF
         END
         ELSE: BEGIN & minc = 0. &  maxc = 0. & END
         ENDCASE
         fldatt.int = !VALUES.F_NAN
      END
      2: BEGIN
         CASE plttyp OF
         'pltt': BEGIN & minc = fldatt.homin & maxc = fldatt.homax & END
         ELSE: BEGIN & minc = fldatt.min & maxc = fldatt.max & END
         ENDCASE
      END
   ENDCASE

   IF cmd.var NE fld_prev OR cmd.trend NE '0' THEN BEGIN
; field.origin EQ 'diff' OR
      print, ''
      CASE dimplot OF
         2: BEGIN
            print, '    '+cmd.var, ' plot attributes ('+plttyp+') : min/max/int=', $
             minc, maxc, fldatt.int
            IF ( fldatt.mult NE 1.0 OR  fldatt.add NE 0.0 ) THEN BEGIN
             print, '     --> Modify field using : ', 'mult/add=' , $
             fldatt.mult, fldatt.add, '    to obtain :    ', fldatt.unit
            ENDIF
         END
         1: BEGIN
            print, '    '+cmd.var, ' plot attributes ('+plttyp+') : min/max=',minc, maxc
            IF ( fldatt.mult NE 1.0 OR  fldatt.add NE 0.0 ) THEN BEGIN
             print, '     --> Modify field using : ', 'mult/add=' , $
             fldatt.mult, fldatt.add, '    to obtain :    ', fldatt.unit
            ENDIF
         END
         0: BEGIN
            print, '    '+cmd.var, ' plot attributes ('+plttyp+') : min/max=',minc, maxc
            IF ( fldatt.mult NE 1.0 OR  fldatt.add NE 0.0 ) THEN BEGIN
             print, '     --> Modify field using : ', 'mult/add=' , $
             fldatt.mult, fldatt.add, '    to obtain :    ', fldatt.unit
            ENDIF
            print, '    '+cmd.var2, ' plot attributes ('+plttyp+') : min/max=',minc2, maxc2
            IF ( fldatt2.mult NE 1.0 OR  fldatt2.add NE 0.0 ) THEN BEGIN
             print, '     --> Modify field using : ', 'mult/add=' , $
             fldatt2.mult, fldatt2.add, '    to obtain :    ', fldatt2.unit
            ENDIF
         END
         ELSE:
      ENDCASE
      print, ''

   ENDIF
   IF ( fldatt.mult EQ -1.0) THEN BEGIN
      temp_m = minc
      minc = -maxc
      maxc = -temp_m
   ENDIF
   IF finite(fldatt.int) EQ 0 THEN BEGIN
      intcmd = ''
      colbarfor = ''
      fmt = '(f6.1)'
   ENDIF ELSE BEGIN
      intcmd = ',int = '+string(fldatt.int)
      IF fldatt.int LT 0.1 THEN BEGIN
         fmt = '(f6.2)'
      ENDIF ELSE IF fldatt.int LT 1 THEN BEGIN
         fmt = '(f5.1)'
      ENDIF ELSE BEGIN
         fmt = '(f5.0)'
      ENDELSE
      IF long(fldatt.int) NE 0 THEN BEGIN
         IF fldatt.int/long(fldatt.int) NE 1 THEN BEGIN
          fmt = '(f5.1)'
         ENDIF
      ENDIF
      colbarfor = ', format = '''+fmt+''''
   ENDELSE

;  if window > 1 or overlay > 1 noerase

   IF win[2] NE 1 OR iover GT 1 THEN BEGIN
      noerase = 1
   ENDIF ELSE BEGIN
      noerase = 0
   ENDELSE

; choose window number

   CASE multi_win OF
      1: window_number = ', window='+string(iplot)
      ELSE: window_number = ''
   ENDCASE

; color label control

   labstr = ''
   IF iover EQ 1 THEN BEGIN
      nofill = 1-shading
   ENDIF ELSE BEGIN
      nofill = 1
   ENDELSE

   nocoltxt = ''
   IF nofill EQ 1 THEN BEGIN
    nocoltxt = ',NOCOULEUR=1'
   ENDIF

   IF dimplot NE 2 THEN BEGIN
    nofill = 1
   ENDIF

   nocolorbar = 0
   IF col_palette EQ 'no' THEN BEGIN
    nocolorbar = 1
   ENDIF
   IF pal_type EQ '2dom' THEN BEGIN
    nocolorbar = 1
   ENDIF
   IF iover GT 1 THEN BEGIN
    nocolorbar = 1
   ENDIF
   IF dimplot NE 2 THEN BEGIN
    nocolorbar = 1
   ENDIF
   IF cmd.out NE 'cdf' THEN BEGIN
      readpal = 0
      IF nofill EQ 0 AND dimplot GT 1 THEN BEGIN
       readpal = 1
      ENDIF
      IF waveplt EQ 1 THEN BEGIN
       readpal = 2
      ENDIF
      IF field.origin EQ 'div' AND plttyp NE 'pltt' THEN BEGIN
       readpal = 3
      ENDIF
      ;; Necessary for correlation plots (overlay of 2D fields)
      IF dimplot GT 1 AND nover GT 1 AND iover GT 1 THEN BEGIN
       readpal = 1
      ENDIF
      IF readpal GE 1 THEN BEGIN
         lec_pal_gmt, cmd.var, c_anot_str, fmt, found, readpal
         colbarfor = ', format = '''+fmt+''''
         IF found EQ 1 THEN BEGIN
            labstr = ',label=3'
            ; if mincmd/naxcmd not defined then use one from palette
            ; if define take min of 2 mins and max of 2 maxs
            min_palette = min(levels_gmt)
            max_palette = max(levels_gmt)
            IF finite(minc) NE 0 THEN BEGIN
             minc = min([minc, min_palette])
            ENDIF ELSE BEGIN
             minc = min_palette
            ENDELSE
            IF finite(maxc) NE 0 THEN BEGIN
             maxc = max([maxc, max_palette])
            ENDIF ELSE BEGIN
             maxc = max_palette
            ENDELSE
            print, '    '+cmd.var, ' plot attributes ('+plttyp+') modified via color palette: min/max=', $
    minc, maxc
         ENDIF
      ENDIF
   ENDIF
   colbar = colbarfor

   mincmd = ''
   maxcmd = ''

   IF finite(minc) NE 0 THEN BEGIN
      mincmd = ','+string(minc)
      maxcmd = ','+string(maxc)
   ENDIF

; contour annotation + other color bar controls

   IF n_elements(c_anot_str) EQ 0 THEN BEGIN
      c_anot = ''
   ENDIF ELSE BEGIN
      c_anot = ',c_annotation=c_anot_str'
      ; sampling of colorbar annotation
      IF col_palette EQ 'yes' THEN BEGIN
         sampling = win[0]*win[1]
;         idx_cb = lindgen((ncont_gmt-2)/sampling)*sampling+1
         idx_cb0 = lindgen(ncont_gmt-1)+1
         idx_cb = lindgen(ncont_gmt-2)+2
         idx_cb1 = lindgen(ncont_gmt-2)+1
         colbar = colbar+',discret=coul_gmt[idx_cb1],div=n_elements(idx_cb0)-1,cb_label=levels_gmt[idx_cb0]'
;         c_anot = ',c_annotation=levels_gmt[idx_cb0]'
      ENDIF
   ENDELSE
;
; vertical grid type
;
   IF debug_w THEN BEGIN
    print, '    splot=', splot
   ENDIF
   CASE mesh_type OF
      'oce': BEGIN
         IF splot EQ 1 THEN BEGIN
            type_yz = ',type_yz =''sigma'''
            zoom_txt = ', zoom='+string(sig_max)
         ENDIF ELSE BEGIN
            type_yz = ',type_yz=''m'''
            zoom_txt = ', zoom='+string(zoom_z)
         ENDELSE
         END
      'atm': BEGIN & type_yz = ',type_yz=''Pa''' & zoom_txt = ', zoom='+string(pres_max) & END
      ELSE:type_yz = ''
   ENDCASE

; continents color, thickness
;   c_cont = 100
; continent fill
; real continent drawn
   strcont =''
   IF mesh_type NE 'oce' THEN BEGIN
      IF cont_fill EQ 0 THEN BEGIN
       strcont = ',/CONT_NOFILL, nite=0'
      ENDIF
      IF cont_real GE 1 THEN BEGIN
       strcont = strcont+', REALCONT = '+string(strtrim(cont_real, 2))+', COAST_THICK = 2'
      ENDIF
   ENDIF

; calendar type

   cal_typ = ''
   IF calendar_type GT 1 THEN BEGIN
    cal_typ = ',ndayspm=calendar_type'
   ENDIF

; titles options
; By default, SAXO puts a title and a subtitle for the first plot
; Then, if you overlay another field, title and subtitle are set to ''
   CASE title_type OF
      "T": tit_str = ',subtitle='''','+marge_option
      "S": tit_str = ',title='''','+marge_option
      "TS": tit_str = ','+marge_option
      "off":tit_str = ',title='''',subtitle='''','+marge_option
   ENDCASE

; fill_space option

   filltxt = ''
   IF fill_space EQ 1 THEN BEGIN
    filltxt = ',/rempli'
   ENDIF

; axis charsize option (x/ychartxt read from plt_def)

; common command string to plots

   com_strplt = ',petit = ['+string(win[0])+','+string(win[1])+','+string(win[2])+']'+filltxt+nocoltxt+', LANDSCAPE = '+string(landscape)+', NOCOLORBAR = '+string(nocolorbar)+', NOERASE = '+string(noerase)+look+labstr+c_anot+colbar+cal_typ+',cont_thick=2'+type_yz+window_number+tit_str+xlogax+', xcharsize='+string(xchartxt)+', ycharsize='+string(ychartxt)

; add contour_options not overlay

   IF iover EQ 1 THEN BEGIN
    com_strplt = com_strplt+contour_options
   ENDIF

   IF debug_w THEN BEGIN
    print, '   Cmd before making output in plt_map = ', cmd
    print, '   We are in plt_map: '
   ENDIF

;   @zoom_indices_debug

; ============================
;  3. make output (data/plot)
; ============================
;

   CASE cmd.out OF
      'data': write_data = long((iplot-1)*10+win[2])  ;  write 1D data ascii in trends.pro
      'tcdf': write_data = -long((iplot-1)*10+win[2])  ;  write 1D data netcdf in trends.pro
      'cdf': write_data = 0
      ELSE: write_data = 0
   ENDCASE

;  make output

   CASE cmd.out OF
      'tv': BEGIN ; $$$$$$$$$$$$$$$$$$$$$$ tvnplot $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         erase

         CASE (size(fld))[0] OF
            2: fldtv = fld
            3: BEGIN
               niveau = xquestion('3d data : which level ?', '1', /chkwidget)
               fldtv = fld[*, *, niveau]
            END
            4: BEGIN
               niveau = xquestion('3d data : which level ?', '1', /chkwidget)
               timeplot = xquestion('4d data : which date ?', '1', /chkwidget)
               fldtv = fld[*, *, niveau, timeplot]
            END
            ELSE:
         ENDCASE

         CASE cmd.grid OF
;         'T': tvnplot, fldtv*tmask
            'T': tvplus, fldtv, min = fldatt.min, max = fldatt.max
            'U': tvplus, fldtv, min = fldatt.min, max = fldatt.max
            'V': tvplus, fldtv, min = fldatt.min, max = fldatt.max
            'W': tvplus, fldtv, min = fldatt.min, max = fldatt.max
            ELSE: tvplus, fldtv, min = fldatt.min, max = fldatt.max
         ENDCASE
      END

      'cdf': BEGIN  ; $$$$$$$$$$$$$$$$$$$$$$   netCDF output $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

         ; build structures containing grids
         cdf_description = nc_build(cmd, field, field.direc, vargrid)
         ; build netCDF file (here if whole data)
         IF hotyp EQ '-' OR hotyp EQ 'xyt' THEN BEGIN
            fldcdf = {data:field.data, units:field.units, short_name:cmd.var, long_name:field.legend, missing_value:valmask, direc:field.direc}
            file_out = cmd.var+'_'+strtrim(string(FORMAT = '(I2.2)', iplot), 2)+'.nc'
            pltcmd ='nc_put, fldcdf, file_out, ncdf_db = hom_idl'+cdf_description
            printf, nulhis, strcompress(pltcmd)
            res = execute(pltcmd)
         ENDIF
       END
       'tcdf': BEGIN ; $$$$$$$$$$$$$$$$$$$$$$   1D netCDF output $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
          mask_z, fld, cmd, boite_plt1d, dimplot
          fld = checkfield(fld, 'pltt', type = datyp.hotyp, boite = boite_plt1d)
          IF debug_w THEN BEGIN
           print, 'size(fld)', size(fld)
           print, 'boite_plt1d', boite_plt1d
          ENDIF
          IF cmd.trend GT 0 THEN BEGIN
             fld = trends(fld, cmd.trend, datyp.hotyp)
             add_txt = trd_txt
          ENDIF ELSE BEGIN
             print, 'You need to have a trend to make 1D netcdf output'
          ENDELSE
       END
       ELSE: BEGIN ; $$$$$$$$$$$$$$$$$$$$$$$$$$   make plot   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

          IF debug_w THEN BEGIN
             print, '   Plot type (plttyp) = ', plttyp
          ENDIF
         CASE plttyp OF
            'plt': BEGIN
         ;
         ; Map plot
         ; --------
         ;
               print, '     '+cmd.var+' data to plot min and max :            ', $
                min(fld[where (fld LE valmask/10)], /nan), max(fld[where (fld LE valmask/10)], /nan)
               print, ''

               map_cmd = ''

               niveau = 1

               ; compute mean and rms of field
               ; zmean = moyenne(fld, 'xy', boite = box_plt, /NAN)
               ; zrms = moyenne(fld, 'xy', boite = box_plt, /RMS, /NAN)
               ;filleg = ' [region mean/rms='+string(strtrim(zmean, 2))+'/'+string(strtrim(zrms, 2))+']'
               filleg = ''

               IF vecplot EQ 0 THEN BEGIN
                  ; eddy field ? (if so, remove zonal mean)
                  IF strpos(cmd.plt, '@z') GT 1 THEN BEGIN
                     fldzm = moyenne(fld, 'x', boite=[20,380,-90,90], /NAN)
                     fldzm = replicate(1, (size(fld))[1])#fldzm
                     fld = fld-fldzm
                     edd_txt = ' Eddy '
                  ENDIF ELSE BEGIN
                     edd_txt = ''
                  ENDELSE

                  IF hotyp EQ 'xyt' THEN BEGIN
                   date_txt = 'monthly'
                  ENDIF
                  pltcmd = 'plt,{a:fld, d:'''+date_txt+''', n:'''+edd_txt+varlegend+' '+legbox+legsurf+''', u:'''+field.units+filleg+''', g:'''+vargrid+'''}'+mincmd+maxcmd+intcmd+com_strplt+strcont+map_cmd
                  ;; For 2D plots with overlays (correlation and proba for instance)
                  ;; Scratch the titles and define contours style for the second plot
                  IF nover GT 1 AND iover GT 1 THEN BEGIN
                     contour_style = ',c_thick=2,c_linestyle=2,c_charthick=2'
                     CASE title_type OF
                        'T': pltcmd = pltcmd+',title='''''+contour_style
                        'S': pltcmd = pltcmd+',subtitle='''''+contour_style
                        'TS': pltcmd = pltcmd+',title='''',subtitle='''''+contour_style
                        'off': pltcmd = pltcmd+contour_style
                     END
                  ENDIF
               ENDIF ELSE BEGIN

                  fld = {a:fld, g:strmid(cmd.grid, 0, 1)}
                  fld2 = {a:fld2, g:strmid(cmd.grid, 1, 1)}
                  fldv = {data1: fld, data2: fld2}
                  vargrid = strmid(cmd.grid, 2, 1)

                  pltcmd = 'ajoutvect,fldv'+vect_sample+com_strplt+strcont+map_cmd

               ENDELSE

               IF debug_w THEN BEGIN
                print, strcompress(pltcmd)
               ENDIF
               printf, nulhis, strcompress(pltcmd)
               res = execute(pltcmd[0])

               ; draw nino boxes
               IF nino_plot GE 1 THEN BEGIN
                  xn4 = [160, 210, 210, 160, 160]
                  yn4 = [5, 5, -5, -5, 5]
                  plot, xn4, yn4, /noerase
                  xn3 = [210, 270, 270, 210, 210]
                  yn3 = [5, 5, -5, -5, 5]
                  plot, xn3, yn3, /noerase
                  IF nino_plot GE 2 THEN BEGIN
                     xn1 = [270, 280, 280, 270, 270]
                     yn1 = [-5, -5, -10, -10, -5]
                     plot, xn1, yn1, /noerase
                     xn2 = [270, 280, 280, 270, 270]
                     yn2 = [0, 0, -5, -5, 0]
                     plot, xn2, yn2, /noerase
                  ENDIF
               ENDIF

            END
            'pltz': BEGIN
         ;
         ; Vertical section/mean plot
         ; --------------------------
         ;
               g = gphit
               t = tmask

            ; if already 1D, reform fld
               fld = reform(fld)

            ; mask fld
               mask_z, fld, cmd, boite_pltz, dimplot, legz
               printf, nulhis, ' boite_pltz = ', boite_pltz

               print, '     '+cmd.var+' data to plot min and max :            ', $
                min(fld[where (fld NE valmask)], /NAN), max(fld[where (fld NE valmask)], /NAN)
               print, ''
               IF vecplot EQ 0 THEN BEGIN

                  IF cmd.var EQ 'vozonbsf' THEN BEGIN
                   xindstr = ', /xindex'
                  ENDIF ELSE BEGIN
                   xindstr = ''
                  ENDELSE

                  pltcmd = 'pltz,{a:fld, d:'''+date_txt+''', n:'''+varlegend+'   '+legz+''', u:'''+field.units+''', g:'''+vargrid+'''}'+mincmd+maxcmd+intcmd+',/'+pltztyp+', boite=boite_pltz'+zoom_txt+com_strplt+xindstr


               ENDIF ELSE BEGIN

                  fld = {a:fld, g:strmid(cmd.grid, 0, 1)}
                  fld2 = {a:fld2, g:strmid(cmd.grid, 1, 1)}
                  fldv = {data1: fld, data2: fld2}
                  vargrid = strmid(cmd.grid, 2, 1)

                  pltcmd = 'ajoutvectz,fldv'+com_strplt+strcont+',type='''+strmid(cmd.plt, 0, 2)+''', boite=boite_pltz'

               ENDELSE

               IF debug_w THEN BEGIN
                print, pltcmd
               ENDIF
               printf, nulhis, strcompress(pltcmd)
               res = execute(pltcmd[0])


               ; overlay bowl if sig_bowl=1
               IF sig_bowl EQ 1 THEN BEGIN
                  ; define line color, thickness and type
                  iover = 2
                  overc = overlay_type(iover, dimplot)
                  ; type of latitude axis
                  IF strmid(cmd.plt, 0, 1) EQ 'y' AND lat_axis EQ 'sin' THEN BEGIN
                   sin = ',/sin'
                  ENDIF ELSE BEGIN
                   sin = ''
                  ENDELSE
                  plt1dtyp = strmid(pltztyp, 0, 1)
                  noerase = 1
                  com_strplt = ', NOERASE = '+string(noerase)
                  pltcmd = 'plt1d,field.bowl,/'+plt1dtyp+', boite=boite_pltz'+overc+sin+com_strplt
                  IF debug_w THEN BEGIN
                   print, pltcmd
                  ENDIF

                  res = execute(pltcmd[0])
               ENDIF

               gphit = g
               tmask = t

            END
            'plt1d': BEGIN
         ;
         ; 1D, x, y, z plot
         ; ----------------
         ;
               g = gphit
               t = tmask
            ; if already 1D, reform fld
               fld = reform(fld)
               
            ; mask fld
               mask_z, fld, cmd, boite_plt1d, dimplot, legz

               IF debug_w THEN BEGIN
                print, '  iover, boite_plt1d in plt1d:', iover, boite_plt1d
               ENDIF
               niveau = 1
            ; define line color, thickness and type
               overc = overlay_type(iover, dimplot)
            ; type of latitude axis
               IF strmid(cmd.plt, 0, 1) EQ 'y' AND lat_axis EQ 'sin' THEN BEGIN
                sin = ',/sin'
               ENDIF ELSE BEGIN
                sin = ''
               ENDELSE
               take_log = ',take_log=0'
               IF cmd.var EQ 'vosigvol' AND n_elements(strsplit(cmd.exp,'-', /EXTRACT)) EQ 1 THEN BEGIN ; don't take log if difference plot
                  take_log = ',take_log=1'
               ENDIF
               ; if ensemble loop on each member
                  ; if ensbl_mean=0 plot just members
                  ; if ensbl_mean=1 plot members + ensemble mean
                  ; if ensbl_mean=2 plot min/max envelop + ensemble mean
               IF ensbl_code NE '' THEN BEGIN 
                  iens = 0
                  jpe = jpk
                  IF ensbl_diff_other EQ 1 AND ensbl_dim GT 1 THEN BEGIN
                     jpe = (size(fld))[3]
                  ENDIF 
                  IF debug_w THEN BEGIN 
                     print, '  jpe in plt_map/plt1d:', jpe
                  ENDIF 
                  WHILE iens LE jpe -1 DO BEGIN 
                     IF jpe GT 1 THEN BEGIN 
                        flde = reform(fld[*, *, iens, *], nxt, nyt, jpt)
                     ENDIF ELSE BEGIN 
                        flde = fld
                     ENDELSE 
                     overc = overlay_type_ensbl(iover, iens, /member)
                     IF ensbl_mean GT 0 THEN BEGIN 
                        IF iens EQ 0 THEN BEGIN 
                           fldmean = flde
                        ENDIF ELSE BEGIN
                           fldmean = fldmean+flde
                        ENDELSE 
                     ENDIF 
                     IF ensbl_mean LE 1 OR dimplot EQ 2 THEN BEGIN ; plot each member
                        pltcmd = 'plt1d,{a:flde, d:'''+date_txt+''', n:'''+varlegend+'   '+legz+''', u:'''+field.units+''', g:'''+vargrid+'''},'''+plt1dtyp+''''+mincmd+maxcmd+intcmd+', boite=boite_plt1d'+overc+sin+com_strplt+take_log
                        printf, nulhis, strcompress(pltcmd)
                        IF debug_w THEN BEGIN
                           print, pltcmd
                        ENDIF
                        res = execute(pltcmd[0])
                     ENDIF 
                     IF ensbl_code EQ 'mame' AND iover EQ jpe-1 THEN BEGIN 
                        nb_cycles = 1
                        @legend_overlay
                     ENDIF 
                     iens = iens + 1
                  ENDWHILE 
                  ; compute and draw ensemble_mean (if ensbl_mean eq 1 and dimplot ne 2) 
                  IF dimplot EQ 1 THEN BEGIN 
                     IF ensbl_mean GE 1 AND ensbl_code NE 'mame' THEN BEGIN 
                        fldmean = fldmean/float(jpe)
                        IF ensbl_mean EQ 2 THEN BEGIN 
                           overc = overlay_type_ensbl(iover, 0, /mean)
                        ENDIF ELSE BEGIN 
                           overc = overlay_type_ensbl(iover, iens, /mean)
                        ENDELSE 
                        pltcmd = 'plt1d,{a:fldmean, d:'''+date_txt+''', n:'''+varlegend+'   '+legz+''', u:'''+field.units+''', g:'''+vargrid+'''},'''+plt1dtyp+''''+mincmd+maxcmd+intcmd+', boite=boite_plt1d'+overc+sin+com_strplt+take_log
                        printf, nulhis, strcompress(pltcmd)
                        IF debug_w THEN BEGIN
                           print, pltcmd
                        ENDIF
                        res = execute(pltcmd[0])
                       ; increment line properties index
                        index_over = index_over + 1
                     ENDIF 
                                ; compute and draw min/max envelop
                     IF ensbl_mean EQ 2 AND ensbl_code NE 'mame' THEN BEGIN 
                        fldetmp = grossemoyenne(fld, 'xy', /NAN)
                        fldemin = min(fldetmp, DIMENSION = 1)
                        fldemax = max(fldetmp, DIMENSION = 1)
                        overc = overlay_type_ensbl(iover, iens, /member)
                        pltcmd = 'plt1d,{a:fldemin, n:'''+date_txt+'  '+varlegend+'   '+legbox+''', u:'''+field.units+filleg+''', g:'''+vargrid+'''}, '''+hotyp+''''+com_strplt+overc
                        printf, nulhis, strcompress(pltcmd)
                        IF debug_w THEN BEGIN
                           print, pltcmd
                        ENDIF
                        res = execute(pltcmd[0])
                        pltcmd = 'plt1d,{a:fldemax, n:'''+date_txt+'  '+varlegend+'   '+legbox+''', u:'''+field.units+filleg+''', g:'''+vargrid+'''}, '''+hotyp+''''+com_strplt+overc
                        printf, nulhis, strcompress(pltcmd)
                        IF debug_w THEN BEGIN
                           print, pltcmd
                        ENDIF
                        res = execute(pltcmd[0])
                     ENDIF 
                  ENDIF 
                  ; increment line properties index
                  index_over = index_over + jpe - 1
                  IF debug_w GE 2 THEN BEGIN 
                     ready = ''
                     read,'<Return> for next curve ', ready
                  ENDIF 
               ENDIF ELSE BEGIN ; General pltt case
                  print, '     '+cmd.var+' data to plot min and max :            ', $
                   min(fld[where (fld NE valmask)]), max(fld[where (fld NE valmask)])
                  pltcmd = 'plt1d,{a:fld, d:'''+date_txt+''', n:'''+varlegend+'   '+legz+''', u:'''+field.units+''', g:'''+vargrid+'''},'''+plt1dtyp+''''+mincmd+maxcmd+intcmd+', boite=boite_plt1d'+overc+sin+com_strplt+take_log
                  printf, nulhis, 'boite_plt1d=',boite_plt1d
                  printf, nulhis, strcompress(pltcmd)
                  IF debug_w THEN BEGIN
                     print, pltcmd
                  ENDIF
                  res = execute(pltcmd[0])
                  
               ENDELSE 
               @legend_overlay
               ; draw zero lines
               xn = [0, 0]
               yn = [-1.e10, 1.e10]
               plot, xn, yn, /noerase
               plot, yn, xn, /noerase
               gphit = g
               tmask = t

            END
            'pltt': BEGIN
            ;
            ; hovmoeller
            ; -----------
            ;
               g = gphit
               t = tmask

            ; mask fld
	    mask_z, fld, cmd, boite_pltt, dimplot, legz
            ; define line color, thickness and type
            overc = overlay_type(iover, dimplot)
            ; define additional text for pltt
            @add_txt_pltt

            ; General case: compute mean and time rms if hoytp='t'
            ; Ensemble case : tbd
               IF hotyp EQ 't' THEN BEGIN
                  IF ensbl_code EQ '' THEN BEGIN 
                     IF field.dim EQ 3 THEN BEGIN
                        zmeant = grossemoyenne(fld, 'xy', boite = boite_pltt, /NAN)
                     ENDIF ELSE BEGIN
                        zmeant = grossemoyenne(fld, 'xyz', boite = boite_pltt, /NAN)
                     ENDELSE
                     zmean = total(zmeant)/float(jpt)
                     zrms = sqrt(total(zmeant^2))/float(jpt)
                     filleg = filleg+' ['+cmd.exp+' time mean='+string(strtrim(zmean, 2))+'/ time rms of '+legbox+' ave.='+string(strtrim(zrms, 2))+']'
                     print, '   '+cmd.exp+'  time mean=  '+string(strtrim(zmean, 2))+' / time rms of '+legbox+' ave.='+string(strtrim(zrms, 2))+''
                  ENDIF ELSE BEGIN 
                     filleg = ' stats on ensembles tbd'
                     print,' stats on ensembles tbd' 
                  ENDELSE 
               ENDIF 
            ; repeat for seasonal mean
               CASE cmd.timave OF
                  '1mm': rep_txt = ',repeat_c=nb_cycles,DATE_FORMAT="%M"'
                  ELSE: rep_txt = ''
               ENDCASE

               print, '     '+cmd.var+' data to plot min and max :            ', $
                min(fld[where (fld NE valmask)]), max(fld[where (fld NE valmask)])
               print, ''
               vardate = 'toto'
               grille, mask, glam, gphi, gdep, nx, ny,nz


               ; if common_time = 1, then impose common time array (from first one)

               IF common_time EQ 1 THEN BEGIN
                  IF iover EQ 1 THEN BEGIN
                   time_prev = time
                  ENDIF ELSE BEGIN
                   time = time_prev
                  ENDELSE
               ENDIF
               printf, nulhis, 'boite_pltt=',boite_pltt

               IF debug_w THEN BEGIN 
                  print, '          Calendar type = ', key_caltype
               ENDIF 

               ; if time_domain NE 'auto' then impose time_domain
               IF time_domain[0] NE 'auto' THEN BEGIN 
                  datdom1 = time_domain[0]+'01'
                  datdom2 = time_domain[1]+'31'
                  datdomtxt = ','+datdom1+','+datdom2
               ENDIF ELSE BEGIN 
                  datdomtxt = ''
               ENDELSE 

               IF st_rms EQ 1 THEN BEGIN
                  ; time series of rms deviation on a sigma surface
                  @densit_pltmap_plot
               ENDIF ELSE BEGIN
                  ; if ensemble loop on each member
                  ; if ensbl_mean=0 plot just members
                  ; if ensbl_mean=1 plot members + ensemble mean
                  ; if ensbl_mean=2 plot min/max envelop + ensemble mean
                  IF ensbl_code NE '' THEN BEGIN 
                     iens = 0
                     jpe = jpk
                     IF ensbl_diff_other EQ 1 AND ensbl_dim GT 1 THEN BEGIN
                        jpe = (size(fld))[3]
                     ENDIF 
                     IF debug_w THEN BEGIN 
                        print, ' jpe in plt_map/pltt:', jpe
                     ENDIF 
                     WHILE iens LE jpe -1 DO BEGIN 
                        IF jpe GT 1 THEN BEGIN 
                           flde = reform(fld[*, *, iens, *], nxt, nyt, jpt)
                        ENDIF ELSE BEGIN 
                           flde = fld
                        ENDELSE 
                        IF dimplot EQ 1 THEN BEGIN 
                           overc = overlay_type_ensbl(iover, iens, /member)
                        ENDIF ELSE BEGIN 
                           overc = ""
                        ENDELSE 
                        IF ensbl_mean GT 0 THEN BEGIN 
                           IF iens EQ 0 THEN BEGIN 
                              fldmean = flde
                           ENDIF ELSE BEGIN
                              fldmean = fldmean+flde
                           ENDELSE 
                        ENDIF 
                        IF ensbl_mean LE 1 OR dimplot EQ 2 THEN BEGIN ; plot each member
                           pltcmd = 'pltt,{a:flde, n:'''+date_txt+'  '+varlegend+'   '+legbox+''', u:'''+field.units+filleg+''', g:'''+vargrid+'''}, '''+hotyp+''''+mincmd+maxcmd+datdomtxt+intcmd+',boite=boite_pltt'+com_strplt+overc+filtxt+',TREND_TYPE='+cmd.trend+rep_txt 
                           printf, nulhis, strcompress(pltcmd)
                           IF debug_w THEN BEGIN
                              print, pltcmd
                           ENDIF
                           res = execute(pltcmd[0])
                        ENDIF 
                        IF ensbl_code EQ 'mame' AND iover EQ jpe-1 THEN BEGIN 
                           nb_cycles = 1
                           @legend_overlay
                        ENDIF 
                        iens = iens + 1
                     ENDWHILE 
                     ; compute and draw ensemble_mean (if ensbl_mean eq 1 and dimplot ne 2) 
                     IF dimplot EQ 1 THEN BEGIN 
                        IF ensbl_mean GE 1 AND ensbl_code NE 'mame' THEN BEGIN 
                           fldmean = fldmean/float(jpe)
                           IF ensbl_mean EQ 2 THEN BEGIN 
                              overc = overlay_type_ensbl(iover, 0, /mean)
                           ENDIF ELSE BEGIN 
                              overc = overlay_type_ensbl(iover, iens, /mean)
                           ENDELSE 
                           pltcmd = 'pltt,{a:fldmean, n:'''+date_txt+'  '+varlegend+'   '+legbox+''', u:'''+field.units+filleg+''', g:'''+vargrid+'''}, '''+hotyp+''''+mincmd+maxcmd+datdomtxt+intcmd+',boite=boite_pltt'+com_strplt+overc+filtxt+',TREND_TYPE='+cmd.trend+rep_txt
                           printf, nulhis, strcompress(pltcmd)
                           IF debug_w THEN BEGIN
                              print, pltcmd
                           ENDIF
                           res = execute(pltcmd[0])
                           index_over = index_over + 1
                        ENDIF 
                                ; compute and draw min/max envelop
                        IF ensbl_mean EQ 2 AND ensbl_code NE 'mame' THEN BEGIN 
                           fldetmp = grossemoyenne(fld, 'xy', /NAN)
                           fldemin = min(fldetmp, DIMENSION = 1)
                           fldemax = max(fldetmp, DIMENSION = 1)
                           overc = overlay_type_ensbl(iover, iens, /member)
                           pltcmd = 'plt1d,{a:fldemin, n:'''+date_txt+'  '+varlegend+'   '+legbox+''', u:'''+field.units+filleg+''', g:'''+vargrid+'''}, '''+hotyp+''''+com_strplt+overc
                           printf, nulhis, strcompress(pltcmd)
                           IF debug_w THEN BEGIN
                              print, pltcmd
                           ENDIF
                           res = execute(pltcmd[0])
                           pltcmd = 'plt1d,{a:fldemax, n:'''+date_txt+'  '+varlegend+'   '+legbox+''', u:'''+field.units+filleg+''', g:'''+vargrid+'''}, '''+hotyp+''''+com_strplt+overc
                           printf, nulhis, strcompress(pltcmd)
                           IF debug_w THEN BEGIN
                              print, pltcmd
                           ENDIF
                           res = execute(pltcmd[0])
                        ENDIF 
                     ENDIF 
                     ; increment line properties index
                     index_over = index_over + jpe - 1
                     IF debug_w GE 2 THEN BEGIN 
                        ready = ''
                        read,'<Return> for next curve ', ready
                     ENDIF 
                  ENDIF ELSE BEGIN ; General pltt case
                     ; time/space filter ?
                     IF strpos(cmd.plt, '@f') GT 1 THEN BEGIN
                        filter = long(strmid(cmd.plt, strpos(cmd.plt, '@f')+3, strlen(cmd.plt)-strpos(cmd.plt, '@f')-3))
                        fildirec = strmid(cmd.plt, strpos(cmd.plt, '@f')+2, 1)
                        IF fildirec EQ 't' THEN BEGIN
                     ;      IF iover EQ 0 THEN BEGIN 
                        ;      xtitle = xtitle+' (filter '+strtrim(string(filter), 2)+' warning : discrete filter)'
                      ;     ENDIF 
;                           stop
;                           help, fld
                           IF (size(fld))[0] EQ 3 THEN BEGIN 
                              fld = smooth(fld, [1, 1, filter])
                           ENDIF ELSE BEGIN 
                              fld = smooth(fld, [1, 1, 1, filter])
                           ENDELSE 
                           fld[0:filter/2-1] = 0.
                           fld[(size(fld))[1]-filter/2-1:(size(fld))[1]-1] = 0.
                        ENDIF
                     ENDIF
                     pltcmd = 'pltt,{a:fld, n:'''+date_txt+'  '+varlegend+'   '+legbox+''', u:'''+field.units+filleg+''', g:'''+vargrid+'''}, '''+hotyp+''''+mincmd+maxcmd+intcmd+datdomtxt+',boite=boite_pltt'+com_strplt+overc+filtxt+',TREND_TYPE='+cmd.trend+rep_txt; +',/integration'
                     printf, nulhis, strcompress(pltcmd)
                     IF debug_w THEN BEGIN
                        print, pltcmd
                     ENDIF
;                     stop
                     res = execute(pltcmd[0])
                  ENDELSE 
               ENDELSE

               IF iover EQ 4 THEN BEGIN
                print, 'There might be a pb with the legends !'
               ENDIF
               ; legend if plot=t
               IF ensbl_code NE 'mame' THEN BEGIN 
                  IF hotyp EQ 't' THEN BEGIN
                                ; positions of the legend depend on nb_cycles
                                ; it is different from 1 only with 1mm case
                     IF cmd.timave NE '1mm' THEN BEGIN
                        nb_cycles = 1
                     ENDIF
                     @legend_overlay
                  ENDIF
               ENDIF 
               gphit = g
               tmask = t
               ; draw zero lines
               xn = [0, 0]
               yn = [-1.e10, 1.e10]
               plot, xn, yn, /noerase
               plot, yn, xn, /noerase
            END

            'yfx': BEGIN
               @yfx
            END
            'ybinx': BEGIN
               @ybinx
            END
            'spec': BEGIN
         ;
         ; Spectrum plot y=fft(x)
         ; ---------------------
         ;
               t = tmask
            ; plot domain
               mask_z, fld, cmd, boite_pltspec, dimplot, legspec
            ; define line color and type
               overc = overlay_type(iover, dimplot)
               vardate = date_txt
               varname = varlegend
               varunit = cmd.exp+'   '+cmd.timave+' '+cmd.date1+'-'+cmd.spec+'  '+varlegend+'  '+legspec

            ; make mean in box
               fld = checkfield(fld, 'pltt', type = 't', boite = boite_pltspec, _EXTRA=extra)
            ; apply trends
               IF long(cmd.trend) GT 0 THEN BEGIN
                fld = trends(fld, long(cmd.trend), 't')
               ENDIF

            ; make spectrum
               time_interval = time[1]-time[0]

               print, '     Max plot spectrum in plt_def : days/month/year ', max_spec, max_spec/30, max_spec/360
               print, '      lenght of time serie (years) = ', long(time[jpt-1]-time[0]+time_interval)/360
               print, '      spectrum window (years)/chunks = ', spec_win/360, long(time[jpt-1]-time[0]+time_interval)/spec_win
               print, ' '

            ; number of time windows�
               nt_win = long((time[jpt-1]-time[0]+time_interval)/spec_win)
               idx_win_size = long(spec_win/time_interval)
               IF idx_win_size*nt_win NE jpt THEN BEGIN
                  print, '   *** Warning : spec_win must divide lenght of time serie ', idx_win_size, jpt
                  stop
               ENDIF
            ; mean of idx_win_size chunks
               spect = spectrum(fld[0:idx_win_size-1], time_interval)
               FOR it = 2, nt_win DO BEGIN
                  idx1 = idx_win_size*(it-1)
                  idx2 = idx1 + idx_win_size - 1
                  specc = spectrum(fld[idx1:idx2], time_interval)
                  spect = spect + specc
               ENDFOR
               spect = spect/nt_win
            ; build new time array
               idx = where (spect[0, *] LE max_spec)
               jpt = n_elements(idx)
               fld = reverse(reform(spect[1, idx], jpt))
               time = findgen(jpt)
               time = reverse(reform(spect[0, idx], jpt))
               IF max(time) GT 20*360 THEN BEGIN
                  time = time/360
                  xtitle = 'Years'
               ENDIF ELSE IF max(time) GT 5*360 THEN BEGIN
                  time = time/30
                  xtitle = 'Months'
               ENDIF ELSE xtitle = 'Days'
            ; time/space filter ?
               IF strpos(cmd.plt, '@f') GT 1 THEN BEGIN
                  filter = long(strmid(cmd.plt, strpos(cmd.plt, '@f')+3, strlen(cmd.plt)-strpos(cmd.plt, '@f')-3))
                  fildirec = strmid(cmd.plt, strpos(cmd.plt, '@f')+2, 1)
                  IF fildirec EQ 't' THEN BEGIN
                     xtitle = xtitle+' (filter '+strtrim(string(filter), 2)+' warning : discrete filter)'
                     fld = smooth(fld, filter)
                     fld[0:filter/2-1] = 0.
                     fld[(size(fld))[1]-filter/2-1:(size(fld))[1]-1] = 0.
                  ENDIF
               ENDIF

            ; rms with previous curve

               IF iover EQ 1 THEN BEGIN
                  spec_prev = fld
               ENDIF ELSE BEGIN
                  time_int = time-shift(time, 1)
                  time_int[0] = 0.
                  divi = total(time_int^2)
                  rms_spec = sqrt( total( ((fld-spec_prev)*time_int)^2) / (divi > 0.))
                  print, '       RMS spectra difference with previous spectra =', rms_spec
               ENDELSE


            ; plot
               ytitle = 'Power spectrum (window='+strtrim(string(spec_win/360), 2)+'y)'
               pltcmd = 'splot,time,fld,xstyle=1,ystyle=1,title=varunit,xtitle=xtitle,ytitle=ytitle'+overc+com_strplt+',yrange=[0,'+string(fldatt.spec)+']'
               printf, nulhis, 'boite_pltspec=',boite_pltspec
               printf, nulhis, strcompress(pltcmd)
               IF debug_w THEN BEGIN
                print, pltcmd
               ENDIF
               res = execute(pltcmd[0])

               tmask = t

            END
            'wavelet': BEGIN
            ;
            ; Wavelet plot
            ; ------------
            ;
               t = tmask
            ; plot domain
               mask_z, fld, cmd, boite_pltspec, dimplot, legspec
            ; define line color and type
               vardate = date_txt
               varname = varlegend + ' '+date_txt
               varunit = cmd.exp+'   '+cmd.timave+' '+cmd.date1+'-'+cmd.spec+'  '+varlegend+'  '+legspec
            ; make mean in box
               fld = checkfield(fld, 'pltt', type = 't', boite = boite_pltspec, _EXTRA=extra)
            ; apply trends
               IF long(cmd.trend) GT 0 THEN BEGIN
                fld = trends(fld, long(cmd.trend), 't')
               ENDIF

            ; make spectrum
               time_interval = time[1]-time[0]

               print, '     Max plot spectrum in plt_def : days/month/year ', max_spec, max_spec/30, max_spec/360
               print, '      lenght of time serie (years) = ', long(time[jpt-1]-time[0]+time_interval)/360
               print, ' '
            ; make wavelet
               wave = wavelet(fld,time_interval,period=period,coi=coi,/pad,signif=signif)
               power = abs(wave)^2
               nscale = n_elements(period)
               period = period/365 ; to have period in years
            ; compute significance
               signif = rebin(transpose(signif),jpt,nscale)
            ; plot wavelet
               mincmd = ','+string(min(power))
               maxcmd = ','+string(max(power))
               boite_pltspec = [0, 0, min(period), max(period)]
;               domdef, boite_pltspec
               lat1r = lat1
               lat2r = lat2
               lat1 = 0
               lat2 = max_spec/360
               key_onearth = 0
               pltcmd = 'plttg,power,period'+mincmd+maxcmd+intcmd+',boite=boite_pltspec,reverse_y=1,nocontour=1'+com_strplt
               printf, nulhis, 'boite_pltspec=',boite_pltspec
               printf, nulhis, strcompress(pltcmd)
               IF debug_w THEN BEGIN
                print, pltcmd
               ENDIF
               res = execute(pltcmd[0])
               contour,abs(wave)^2/signif,time,period, /overplot,level=1.0,c_annot='95%'
               plot,time,coi/365, noclip = 0, /noerase
               tmask = t
               lat1 = lat1r
               lat2 = lat2r
               key_onearth = 1
            END
            ELSE: BEGIN
               print, ' unknown projection plot ', cmd.plt, ' / plot type = ', plttyp

            END
         ENDCASE

      END

   ENDCASE

   fld_prev = cmd.var
;
; reset incompatible options of plt_def

   cmd.trend = trend_typp

;
; reset vertical grid after density bining

   IF splot EQ 1 THEN BEGIN

      izminmesh = izminmesh_b
      izmaxmesh = izmaxmesh_b
      jpk = jpk_b
      jpkglo = jpkglo_b
      gdept = gdept_b
      gdepw = gdepw_b
      e3t = e3t_b
      e3w = e3w_b
      tmask = tmask_b

   ENDIF
finish:
   IF debug_w THEN BEGIN
    info = report('leaving ...')
   ENDIF
;
END
