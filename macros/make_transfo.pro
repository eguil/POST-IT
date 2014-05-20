;+
;
; compute transformation
;
; @param FILE_NAME {in}{required}{type=string}
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @param TODO {in}
;
; input denflx = density flux
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
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T094532Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - EG january 2001 (inspired from denflux.f)
;
; @version
; $Id: make_transfo.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_transfo, file_name, ncdf_db, todo $
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
 usage='result=make_transfo(file_name, ncdf_db, todo ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ALL_DATA=all_data ' $
         + ', ZMTYP=zmtyp)'

 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
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

 arg_type = size(todo,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type todo' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

;
; Read sst (C), sss (PSU), Q [W/m2], E-P [kg/m2/s]=[mm/s]
;
   sst = nc_read(file_name,'sosstsst', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   sss = nc_read(file_name,'sosaline', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   hef = nc_read(file_name,'sohefldo', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   emp = nc_read(file_name,'sowaflup', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   sst = sst.data
   sss = sss.data
   hef = hef.data
   emp = emp.data

   idxt=where(sst eq valmask)
   idxs=where(sss eq valmask)
   IF idxt[0] NE -1 THEN BEGIN
    sst[idxt]=0.
   ENDIF
   IF idxs[0] NE -1 THEN BEGIN
    sss[idxs]=0.
   ENDIF

; Compute potential density (sigma_0)

   rho0 = eos[sst, sss]-1000.
   IF idxs[0] NE -1 THEN BEGIN
    rho0[idxt] = valmask
   ENDIF

; Define density grid

   n_sig = (sig_max - sig_min)/sig_del + 1
   z_sig = sig_min+findgen(n_sig)*sig_del

; other inits

   sizef = size(sst)
   jpi = sizef[1]
   jpj = sizef[2]
   jpt = 1
   IF sizef[0] EQ 3 THEN BEGIN
    jpt = sizef[3]
   ENDIF

   dt = 1./float(jpt)            ; integration delta time
   drho = sig_del          ; delta rho
   dsig = sig_del
   P = 0.                        ; pressure = 0 (potential density)

   t_area = 0.
   t_heat = 0.
   t_wafl = 0.
   fluxbar = 0.
   fluxsal = 0.

; Output arrays

   fheat = fltarr(jpi, jpj, jpt)
   fwafl = fltarr(jpi, jpj, jpt)
   dia = fltarr(n_sig+1)
   diawf = fltarr(n_sig+1)
   diat = fltarr( n_sig+1, jpt)
   diawft = fltarr(n_sig+1, jpt)
   hist = fltarr(n_sig+1, jpt)
   histwf = fltarr(n_sig+1, jpt)
   area = fltarr(n_sig+1, jpt)
   nval = lonarr(n_sig+1, jpt)

; Work arrays

   timwrk = fltarr(jpt)

   trf_bin = fltarr(n_sig+1)
   trf_bin[*] = 0.
;
;  Loop on 2D domain (and time as '*' dimension)
;
   FOR ji = 0, (jpi-1) DO BEGIN
      FOR jj = 0, (jpj-1) DO BEGIN

         IF rho0[ji, jj, 0] NE valmask THEN BEGIN
;
;        input fields
            t = sst[ji, jj, *]
            s = sss[ji, jj, *]
            hf = hef[ji, jj, *]
            ep = emp[ji, jj, *]
            sig = rho0[ji, jj, *]

;        compte alpha, beta, Cp
            al = alpha[t, s, P]
            be = betar[t, s, P]
            Cp = Cpsw[t, s, P]

;        find density bin
            ib = ROUND((sig-sig_min)/dsig)

            ib = ib > 0
            ib = ib < n_sig

;        compute area and integral of fluxes

            dA = e1t[ji, jj]*e2t[ji, jj]
        ; conv = m2 -> 1.e6 km2
            conv = 1.e-6*1.e-6
            t_area = t_area + dA*conv
        ; conv = W -> PW
            conv = 1.e-15
            t_heat = t_heat + total(hf)*dt*dA*conv
        ; conv = mm -> m and m3/s to Sv
            conv = 1.e-3*1.e-6
            t_wafl = t_wafl + total(ep)*dt*dA*conv

;        tally number of point in each bin

            FOR jt = 0, (jpt-1) DO BEGIN
               area[ib[jt], jt] = area[ib[jt], jt] + dA
               nval[ib[jt], jt] = nval[ib[jt], jt] + 1
            ENDFOR

;        sum heat and fresh water fluxes as mass fluxes in kg/m2/s (all units SI)
;        convwf : kg/m2/s=mm/s -> m/s
            convwf = 1.e-3
            fheat[ji, jj, *] = ( -al/Cp)*hf
            fwafl[ji, jj, *] = (1000.+sig)*be*s*ep*convwf

;        volume transport per density class (m3/s)
;        period averaged density flux
            FOR jt = 0, (jpt-1) DO BEGIN
               fht = fheat[ji, jj, jt]*dA
               fwf = fwafl[ji, jj, jt]*dA
               hist[ib[jt], jt] = hist[ib[jt], jt] + (fht+fwf)/drho
               histwf[ib[jt], jt] = histwf[ib[jt], jt] + fwf/drho
               fluxbar = fluxbar + (fht+fwf)*dt
               fluxsal = fluxsal + (fwf)*dt
            ENDFOR

         ENDIF
;     end of loop on ji,jj
      ENDFOR
   ENDFOR


   print, ' '
   print, ' ----------------------------------------------------------------'
   print, '                    Transformation statitics'
   print, ' ----------------------------------------------------------------'
   print, '   Net heat flux in ocean               (PW)  : ', t_heat
   print, '   Net water flux in ocean              (Sv)  : ', t_wafl
   print, '   Period averaged density flux       (kg/s)  : ', fluxbar
   print, '      - heat flux contribution        (kg/s)  : ', fluxbar-fluxsal
   print, '      - fresh water flux contribution (kg/s)  : ', fluxsal
   print, '   Integral area (glob.=357.208)  (10^6 km2)  : ', t_area
   print, ' '

; output

   CASE todo OF ;
      'denflx': result = (fheat+fwafl)           ; density flux
      'denflw': result = (fwafl)                 ; fresh water density flux
      ELSE: result = -1
   ENDCASE

   return, result
END
