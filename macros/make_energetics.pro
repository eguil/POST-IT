;+
;
; make energetics computations
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
; <pro>grossemoyenne</pro>
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_energetics.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
FUNCTION make_energetics, file_name, ncdf_db $
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
    print, 'cmd_wrk.grid =', cmd_wrk.grid
   ENDIF
;
 usage='result=make_energetics(file_name, ncdf_db ' $
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

CASE cmd_wrk.grid OF
   'T': source_model = 'opa'
   'T#': source_model = 'opa#'
   'T05': source_model = 'opa'
   'T05#': source_model = 'opa#'
   ELSE: source_model = 'ipcc'
ENDCASE

;; full vertical domain
;; imposes vert_type = '0' in plt_def
vert_switch = 0
IF debug_w THEN BEGIN
   print, 'base_file_name:', base_file_name
   print, 'file_name:', file_name
ENDIF
;
; Read T, S, U, V, W, taux, tauy
;
   print, '    Data size to read (M)= ', nxt*nyt*jpt*(5*nzt + 2)*1.e-6

CASE source_model OF
   'opa': BEGIN
      tn = nc_read(file_name,'votemper', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      sn = nc_read(file_name,'vosaline', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      IF data_domain EQ 'pacific' THEN BEGIN
         file_namu = strmid(file_name, 0, strlen(file_name)-8)+'U_pac.nc'
         file_namv = strmid(file_name, 0, strlen(file_name)-8)+'V_pac.nc'
         file_namw = strmid(file_name, 0, strlen(file_name)-8)+'W_pac.nc'
      ENDIF ELSE BEGIN
         file_namu = strmid(file_name, 0, strlen(file_name)-4)+'U.nc'
         file_namv = strmid(file_name, 0, strlen(file_name)-4)+'V.nc'
         file_namw = strmid(file_name, 0, strlen(file_name)-4)+'W.nc'
      ENDELSE
      un = nc_read(file_namu,'vozocrtx', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      vn = nc_read(file_namv,'vomecrty', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      wn = nc_read(file_namw,'vovecrtz', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      tauxn = nc_read(file_namu,'sozotaux', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      tauyn = nc_read(file_namv,'sometauy', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      var_temp = 'votemper'
      file_temp = file_name
   END
   'opa#': BEGIN
      tn = nc_read(file_name,'votemper', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      IF data_domain EQ 'pacific' THEN BEGIN
         file_nams = strmid(file_name, 0, strlen(file_name)-17)+'T_pac_vosaline.nc'
         file_namu = strmid(file_name, 0, strlen(file_name)-17)+'U_pac_vozocrtx.nc'
         file_namv = strmid(file_name, 0, strlen(file_name)-17)+'V_pac_vomecrty.nc'
         file_namw = strmid(file_name, 0, strlen(file_name)-17)+'W_pac_vovecrtz.nc'
         file_namsu = strmid(file_name, 0, strlen(file_name)-17)+'U_pac_sozotaux.nc'
         file_namsv = strmid(file_name, 0, strlen(file_name)-17)+'V_pac_sometauy.nc'
      ENDIF ELSE BEGIN
         file_nams = strmid(file_name, 0, strlen(file_name)-13)+'T_vosaline.nc'
         file_namu = strmid(file_name, 0, strlen(file_name)-13)+'U_vozocrtx.nc'
         file_namv = strmid(file_name, 0, strlen(file_name)-13)+'V_vomecrty.nc'
         file_namw = strmid(file_name, 0, strlen(file_name)-13)+'W_vovecrtz.nc'
         file_namsu = strmid(file_name, 0, strlen(file_name)-13)+'U_sozotaux.nc'
         file_namsv = strmid(file_name, 0, strlen(file_name)-13)+'V_sometauy.nc'
      ENDELSE
      sn = nc_read(file_nams,'vosaline', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      un = nc_read(file_namu,'vozocrtx', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      vn = nc_read(file_namv,'vomecrty', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      wn = nc_read(file_namw,'vovecrtz', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      tauxn = nc_read(file_namsu,'sozotaux', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      tauyn = nc_read(file_namsv,'sometauy', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      var_temp = 'votemper'
      file_temp = file_name
      source_model = 'opa'
   END
   'ipcc': BEGIN
      base_file_name_grd = base_file_name+base_suffix
      tn = nc_read(base_file_name_grd+'_thetao.nc','thetao', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      sn = nc_read(base_file_name_grd+'_so.nc','so', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      un = nc_read(base_file_name_grd+'_uo.nc','uo', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      vn = nc_read(base_file_name_grd+'_vo.nc','vo', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      wn = nc_read(base_file_name_grd+'_wo.nc','wo', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      tauxn = nc_read(base_file_name_grd+'_tauu.nc','tauu', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      tauyn = nc_read(base_file_name_grd+'_tauv.nc','tauv', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
      var_temp = 'thetao'
      file_temp = base_file_name_grd+'_thetao.nc'
   END
ENDCASE

t_unit = tn.units

; assign and free memory

   t = tn.data & tn = 0
   s = sn.data & sn = 0
   u = un.data & un = 0
   v = vn.data & vn = 0
   w = wn.data & wn = 0
   tx = tauxn.data & tauxn = 0
   ty = tauyn.data & tauyn = 0

   rg = 9.81

   print, '     read ok'

; rearrange data depending on source

CASE source_model OF
   'opa': BEGIN
      ; build mask
      IF valmask EQ 0. THEN BEGIN
         tmaskr = abs(s) gt 1.e-6
         valmask = 1.e20
         idxw = where (tmaskr EQ 0)
         IF idxw[0] NE -1 THEN BEGIN
          w[idxw] = valmask
         ENDIF
         idxt = where (tmaskr EQ 0)
         IF idxt[0] NE -1 THEN BEGIN
          t[idxt] = 0.
         ENDIF
         IF idxt[0] NE -1 THEN BEGIN
          s[idxt] = 0.
         ENDIF
         tmaskr = 0 ; free memory
      ENDIF ELSE BEGIN
         idxt = where (t EQ valmask)
         IF idxt[0] NE -1 THEN BEGIN
          t[idxt] = 0.
         ENDIF
         IF idxt[0] NE -1 THEN BEGIN
          s[idxt] = 0.
         ENDIF
      ENDELSE

      ; transform W fields onto T grid
      maskw = w LT valmask/10.
      w_T = 0.5*( w*maskw + shift(w, 0, 0, -1, 0)*shift(maskw, 0, 0, -1, 0) )
      w_T[*, *, (size(w))[3]-1, *] = w_T[*, *, (size(w))[3]-2, *]
      w = 0
      maskw = 0  ; free memory

   END
   'ipcc': BEGIN
      idx_t = where (t GT valmask/10)
      ; special case CCCMA
      IF strpos(cmd_wrk.exp, 'CCCMA') NE -1 THEN BEGIN
         ; transform W fields onto T grid
         maskw = w LT valmask/10.
         w_T = 0.5*( w*maskw + shift(w, 0, 0, -1, 0)*shift(maskw, 0, 0, -1, 0) )
         w_T[*, *, (size(w))[3]-1, *] = w_T[*, *, (size(w))[3]-2, *]
         w = 0
         w_T[idx_t] = valmask
         maskw = 0 ; free memory
      ENDIF ELSE BEGIN
         w_T = w
      ENDELSE

      idx_2d = where (u[*, *, 0, 0] GT valmask/10.)
      IF idx_2d[0] NE -1 THEN BEGIN
       tx[idx_2d] = valmask
      ENDIF
      idx_2d = where (v[*, *, 0, 0] GT valmask/10.)
      IF idx_2d[0] NE -1 THEN BEGIN
       ty[idx_2d] = valmask
      ENDIF
      idx = where (t LT valmask/10.)
      IF t_unit NE "C" THEN BEGIN
         IF idx[0] NE -1 THEN BEGIN
          t[idx] = t[idx]-273.15
         ENDIF
      ENDIF
      idx1 = n_elements(idx_t)
      idx_w = where (w_T GT valmask/10)
      idx2 = n_elements(idx_w)
      IF idx1 NE idx2 THEN BEGIN
         print, ' *** WARNING wo and thetao do not have same masks !', idx1, idx2, min(idx_t -idx_w), max(idx_t- idx_w)
         print, ' *** Setting wo to zero on all masked points'
         w_T[idx_w] = 0.
      ENDIF ELSE BEGIN
         print, '       check these are 0 (W,T): ', min(idx_t -idx_w), max(idx_t- idx_w)
      ENDELSE
      w = 0 & idx = 0 & idx_t = 0 & idx_w = 0 ; free memory
      idxt=where(t GT valmask/10.)
      idxs=where(s GT valmask/10.)
      IF source_model EQ 'ipcc' THEN BEGIN
       print, '       check these are 0 (T,S): ', min(idxt -idxs), max(idxt-idxs)
      ENDIF
      IF idxt[0] NE -1 THEN BEGIN
       t[idxt]=0.
      ENDIF
      IF idxs[0] NE -1 THEN BEGIN
       s[idxs]=0.
      ENDIF
      idxs = 0                  ; free memory

   END
ENDCASE


; compute potential density rho

   print, '     compute rho...'


   sr=sqrt(abs(s))
   r1=((((6.536332E-9*t-1.120083E-6)*t+1.001685E-4)*t $
        -9.095290E-3)*t+6.793952E-2)*t+999.842594
   r2=(((5.3875E-9*t-8.2467E-7)*t+7.6438E-5)*t-4.0899E-3)*t+8.24493E-1
   r3=(-1.6546E-6*t+1.0227E-4)*t-5.72466E-3
   rhop = ( ( 4.8314E-4*s + r3*sr +r2)*s +r1)
   print, '      rhop min/max', min(rhop), max(rhop)
   IF idxt[0] NE -1 THEN BEGIN
    rhop[idxt] = valmask
   ENDIF
   sr = 0 & r1 = 0 &  r2 = 0 &  r3 = 0 & t = 0 & s = 0 ; free memory

; compute mean profiles on T grid

   vargrid = 'T'
   IF valmask NE 0 THEN BEGIN
      rho_s = grossemoyenne(rhop, 'xyt', boite = zbox, NAN = valmask)
   ENDIF ELSE BEGIN
      rho_s = grossemoyenne(rhop, 'xyt', boite = zbox)
   ENDELSE

   rho_s4d = replicate(1, nxt*nyt)#rho_s
   rho_s4d = reform(rho_s4d[*]#replicate(1, jpt), nxt, nyt, nzt, jpt, /overwrite)


; compute mean stability = d(rho_s)/dz (on W grid)

   rho_diff = (rho_s-shift(rho_s,-1))/shift(e3w, -1)
   rho_diff = shift(temporary(rho_diff), 1)
   rho_diff[0] = 0.

; transform onto T grid

   rho_diff_T = 0.5*(rho_diff+shift(rho_diff, -1))
   rho_diff_T[(size(rho_diff))[1]-1] = rho_diff[(size(rho_diff))[1]-2]

   stab_inv = ABS(1./rho_diff_T)

   rho_diff_T = 0 ; free memory

; remove first 2 levels (MXL too unstable)

   stab_inv[0:1] = 0.

; test: remove only top level

;   stab_inv[0:0] = 0.

; compute [(rho-rho_s)**2]/stability

   stab_inv = replicate(1, nxt*nyt)#stab_inv
   stab_inv = reform(stab_inv[*]#replicate(1, jpt), nxt, nyt, nzt, jpt, /overwrite)

   int_val2 = ((rhop-rho_s4d)^2)*stab_inv
   IF idxt[0] NE -1 THEN BEGIN
    int_val2[idxt] = 0.
   ENDIF

   print, '     compute APE...'

   ape = 0.5*rg*grossemoyenne(int_val2, 'xyz', /integration)

   int_val2 = 0 ; free memory

   ape_wr = ape
   ape = ape*1.e-18
;
; compute buoyancy forcing bfx = int[(rho-rho_s).w]dxdydz
;
   print, '     compute BF...'

   int_val = (rhop-rho_s4d)*(w_T)
   IF idxt[0] NE -1 THEN BEGIN
    int_val[idxt] = 0.
   ENDIF

; remove first 2 levels (MXL too unstable)
   int_val[*, *, 0:1, *] = 0.

   bfx = rg*grossemoyenne(int_val, 'xyz', /integration)

   int_val = 0 ; free memory

   bfx_wr = bfx
   bfx_b = bfx
   bfx = bfx*1.e-11

; compute wind work = int(tau.um)dx.dy where um=u(over 30 m)

   print, '     compute WW...'

   umean=grossemoyenne(u,'z',boite=[0,30])
   vmean=grossemoyenne(v,'z',boite=[0,30])
;   umean=grossemoyenne(u,'z',boite=[0,10])
;   vmean=grossemoyenne(v,'z',boite=[0,10])

   idx = where(tx GT valmask/10.)
   idy = where(ty GT valmask/10.)
   idxu = where(umean GT valmask/10.)
   idyv = where(vmean GT valmask/10.)

   IF idx[0] NE -1 THEN BEGIN
    tx[idx] = 0.
   ENDIF
   IF idy[0] NE -1 THEN BEGIN
    ty[idy] = 0.
   ENDIF
   IF idxu[0] NE -1 THEN BEGIN
    umean[idxu] = 0.
   ENDIF
   IF idyv[0] NE -1 THEN BEGIN
    vmean[idyv] = 0.
   ENDIF

   dot_prodx = tx*umean
   dot_prody = ty*vmean

   wwx = grossemoyenne(dot_prodx, 'xy', /integration)
   wwy = grossemoyenne(dot_prody, 'xy', /integration)
   ww = wwx + wwy

   ww_wr = ww
   ww_b = ww
   ww = ww*1.e-11
   wwx = wwx*1.e-11
   wwy = wwy*1.e-11

; compute forcing efficiency: stddev(B)/stddev(W)

   bfx_1mm = trends(bfx_b, 412, 't')
   bfx_sc = mean_sc
   ww_1mm = trends(ww_b, 412, 't')
   ww_sc = mean_sc
   efficiency = sqrt((moment(bfx_1mm))[1])/sqrt((moment(ww_1mm))[1])
   efficiency_sc = sqrt((moment(bfx_sc[0:11]))[1])/sqrt((moment(ww_sc[0:11]))[1])

; plotting stuff

   ps = 0

   red = [0, 255,   0,   0, 0, 255]
   green = [0,   0, 255,   0, 0,   0]
   blue = [0,   0,   0, 255, 0, 255]
   red = [0, red, red, red, red, red, red, red ]
   green = [0, green, green, green, green, green, green, green]
   blue = [0, blue, blue, blue, blue, blue, blue, blue ]
   tvlct, red, green, blue

   IF cmd_wrk.out EQ 'ps' THEN BEGIN
    ps = 1
   ENDIF

   IF ps EQ 1 THEN BEGIN
    openps
   ENDIF

   pltt, ape, 't', petit = [2, 4, 1], landscape = 1, /rempli, /BASICMARGES, title = 'APE (full) '+cmd_wrk.exp, window=3
   pltt, ww, 't', petit = [2, 4, 2], min = -2, max = 5, /noerase, /rempli, /BASICMARGES, title = 'Wind work (full) '+cmd_wrk.exp, window=3
   pltt, bfx, 't', petit = [2, 4, 8], min = -2, max = 5, color = 4, /noerase, /rempli, /BASICMARGES, title = 'B (full)', window=3

   ape_1mm = trends(ape, 412, 't')
   pltt, ape_1mm, 't', petit = [2, 4, 3], /noerase, /rempli, /BASICMARGES, title = 'APE (inter)', window=3
   jpt_b = jpt
   jpt = 24
   pltt, mean_sc[0:23], 't', petit = [2, 4, 5], /noerase, /rempli, /BASICMARGES, title = 'APE (seasonal cycle x 2)', window=3

   jpt = jpt_b

   ww_1mm = trends(ww, 412, 't')
   tmp = mean_sc
   wwx_1mm = trends(wwx, 412, 't')

   pltt, ww_1mm, 't', petit = [2, 4, 4], color = 2, /noerase, /rempli, /BASICMARGES, title = 'Interannual W (red) B (blue) [efficiency = '+string(strcompress(efficiency))+']', window=3
   pltt, bfx_1mm*1.e-11, 't', petit = [2, 4, 4], /ov1d, color = 4, thick = 2, /noerase, /rempli, /BASICMARGES, window=3

   jpt_b = jpt
   jpt = 24
   pltt, tmp[0:23], 't', petit = [2, 4, 6], min = -1, max = 3.5, /noerase, /rempli, /BASICMARGES, title = 'Seasonal Cycle x 2 (W total: black, B: blue, Wx/y: red/green)', window=3
   pltt, mean_sc[0:23], 't', petit = [2, 4, 6], /ov1d, color = 2, thick = 2, /noerase, /rempli, /BASICMARGES, window=3
   wwy_1mm = trends(wwy, 412, 't')
   pltt, mean_sc[0:23], 't', petit = [2, 4, 6], /ov1d, color = 3, thick = 2, /noerase, /rempli, /BASICMARGES, window=3
   pltt, bfx_sc[0:23]*1.e-11, 't', petit = [2, 4, 6], /ov1d, color = 4, thick = 1, /noerase, /rempli, /BASICMARGES
   jpt = jpt_b

; compute and plot sst in nino 3

   maxdepth = 10
   IF gdept[0] GT maxdepth THEN BEGIN
    maxdepth = gdept[0]
   ENDIF

   ;domdef, [210., 280., -5., 5., 0., maxdepth]
   bte = [210., 280., -5., 5., 0., maxdepth]
   tn = nc_read(file_temp,var_temp, ncdf_db, BOXZOOM = bte, TIME_1 = time_1, TIME_2 = time_2)
   st = tn.data
   sst = grossemoyenne(st, 'xyz')
   sst_wr = sst
   sst = trends(sst, 412, 't')

   pltt, sst, 't', petit = [2, 4, 7], /noerase, /rempli, /BASICMARGES, title = 'Nino3 SSTA'

   correlation = C_CORRELATE(ape, sst, [0])

   IF ps EQ 1 THEN BEGIN
      closeps
      printps
   ENDIF

; write to ascii file

   get_lun, nuldat
   filename = cmd_wrk.exp+'_'+cmd_wrk.date1+'_'+cmd_wrk.spec+'_'+cmd_wrk.plt+'_sst.asc'
   openw, nuldat, asciidir+filename
   print, '     -> writing nino 3 sst data to ', asciidir+filename & print, ' '
   printf, nuldat, sst_wr, format = '(f8.3)'
   free_lun, nuldat & close, nuldat

   get_lun, nuldat
   filename = cmd_wrk.exp+'_'+cmd_wrk.date1+'_'+cmd_wrk.spec+'_'+cmd_wrk.plt+'_ape.asc'
   openw, nuldat, asciidir+filename
   print, '     -> writing ape data to ', asciidir+filename & print, ' '
   printf, nuldat, ape_wr, format = '(g10.4)'
   free_lun, nuldat & close, nuldat

   get_lun, nuldat
   filename = cmd_wrk.exp+'_'+cmd_wrk.date1+'_'+cmd_wrk.spec+'_'+cmd_wrk.plt+'_ww.asc'
   openw, nuldat, asciidir+filename
   print, '     -> writing ww data to ', asciidir+filename & print, ' '
   print, '         min/max of ww, ww_1mm = ', min(ww), max(ww), min(ww_1mm), max(ww_1mm)
   printf, nuldat, ww_wr, format = '(g10.4)'
   free_lun, nuldat & close, nuldat

   get_lun, nuldat
   filename = cmd_wrk.exp+'_'+cmd_wrk.date1+'_'+cmd_wrk.spec+'_'+cmd_wrk.plt+'_bf.asc'
   openw, nuldat, asciidir+filename
   print, '     -> writing bf data to ', asciidir+filename & print, ' '
   print, '         min/max of bfx, bfx_1mm= ', min(bfx), max(bfx), min(bfx_1mm), max(bfx_1mm)
   printf, nuldat, bfx_wr, format = '(g10.4)'
   free_lun, nuldat & close, nuldat

; check that d(APE)/dt ~ ww

   dapedt = (ape-shift(ape, 1))/(86400.*30.)
;   pltt, dapedt-(ww*1.e11),'t',petit=[1,2,1],/rempli,/portrait
;   pltt, dapedt/(ww*1.e11),'t',petit=[1,2,2],/rempli,/portrait,/noerase
   print, ' d(APE)/dt / wind work correlation', C_CORRELATE(dapedt, ww, [0])
   print, ' APE/nino3 sst correlation=', correlation
   print, ' B/W efficiency (interannual) = ', efficiency
   print, ' B/W efficiency (SC) = ', efficiency_sc

   field = {name: '', data: rhop, legend: '', units: '', origin: '', direc: '', dim: 0}

   field.origin = tn.origin
   field.dim = tn.dim - 1

   field.direc = 'xyzt'

   return, field
END
