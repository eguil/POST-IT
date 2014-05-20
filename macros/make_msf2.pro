;+
;
; make MSF
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
; @todo
; where is msf_index procedure ?
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_msf2.pro 212 2010-03-12 15:40:05Z ericg $
;
;-
FUNCTION make_msf2, file_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ZMTYP=zmtyp
         , ALL_DATA=all_data
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='result=make_msf2(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ZMTYP=zmtyp'
         + ', ALL_DATA=all_data)'

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

; init jpl
;
   jpl = jpj
;
; Read U and V
;
   file_nam = strmid(file_name, 0, strlen(file_name)-4)
   v = nc_read(file_nam+'V.nc','vomecrty', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, /no_mean,/all_data)

   idx = where(v.data EQ valmask)
   IF idx[0] NE -1 THEN BEGIN
    v.data[idx] = 0.0
   ENDIF
;
; Mask bathymetry ?
;
   full_name = ''

   IF keyword_set(ZMTYP) THEN BEGIN
      IF strlen(zmtyp) GT 2 THEN BEGIN
         bat_name = strmid(zmtyp, 4, strlen(zmtyp)-4)
         IF bat_name NE 'lobal' and  bat_name NE 'obal' THEN BEGIN
            full_name = grep('ls -al '+hom_idl+'grids | grep orca.'+bat_name+' | awk ''NR == 1 {print $0}''', ' ', 8)
            full_name = strmid(full_name, 5, 50)
            maskzm = maskread(full_name, 'orca', /d3)
            maskzm = reform((reform(maskzm[*, *, *], jpi*jpj*jpk, /overwrite)#replicate(1, jpt)),jpi, jpj, jpk, jpt, /overwrite)
            v.data = v.data*maskzm
; reduction du champ de vitesse meridienne a la bathymetrique.
; Q: est ce que coupe la serie temporelle???
; R: sans doute visible dans maskread.pro

         ENDIF
      ENDIF
   ENDIF

   IF jpt EQ 1 THEN BEGIN
    v.data =reform(v.data,jpi,jpj,jpk,1)
   ENDIF

   strcmd = 'msf_mult, v.data, msf, msfmsk'
   res = execute(strcmd)
; mask msf with valmask
   IF full_name EQ 'atlantic' OR full_name EQ 'pacific' $
    OR full_name EQ 'indopacific' OR full_name EQ 'indian' THEN BEGIN
      idx = where (gphit[diaznl_idx+key_shift, *] LE -32.)
      msf[idx, *,*] = valmask
   ENDIF ELSE BEGIN
      msf[where(msfmsk EQ 0)] = valmask
   ENDELSE
; average in box ?

;pas de condition : equivalent a msf_mean=0  ---> plot yz a la date T
;field = {name: '', data: 0, legend: '', units: 'm3/s', origin: '', dim: 0, direc:''}
;field.origin ????

IF msf_mean EQ 2 THEN BEGIN
 msf_index, msf, index
 field = {name: '', data: index, legend: '', units: 'm3/s', origin: '', dim: 1, direc:'t'}
ENDIF

;pas de condition : equivalent a msf_mean=0  ---> plot yz a la date T
IF msf_mean EQ 1 THEN BEGIN
field = {name: '', data: msf[*,*,0], legend: '', units: 'm3/s', origin: '', dim: 2, direc:'yz'}
ENDIF

;donnees a 3 dimensions, stokage des msf pour plusieurs dates.
IF msf_mean EQ 0 THEN BEGIN
field = {name: '', data: msf ,legend: '', units: 'm3/s', origin: v.origin , dim: 3, direc:'yzt'}
ENDIF
;deux fonctions a aller voir maskz.pro
;et plt_map.pro
 return, field
END
