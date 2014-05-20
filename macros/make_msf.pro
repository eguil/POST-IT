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
; - fplod 20100119T093428Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_msf.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_msf, file_name, ncdf_db $
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
 usage='result=make_msf(file_name, ncdf_db ' $
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

; init jpl
;
   jpl = jpj
;
; Read U and V
;
   file_nam = strmid(file_name, 0, strlen(file_name)-4)
   v = nc_read(file_nam+'V.nc','vomecrty', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data, _EXTRA=EX)

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
         IF bat_name NE 'lobal' THEN BEGIN
            full_name = grep('ls -al '+hom_idl+'grids | grep orca.'+bat_name+' | awk ''NR == 1 {print $0}''', ' ', 8)
            full_name = strmid(full_name, 5, 50)
            maskzm = maskread(full_name, 'orca', /d3)
            v.data = v.data*maskzm
         ENDIF
      ENDIF
   ENDIF

   strcmd = 'msf_simple, v.data, msf, msfmsk'
   res = execute(strcmd)
; mask msf with valmask
   IF full_name EQ 'atlantic' OR full_name EQ 'pacific' $
    OR full_name EQ 'indopacific' OR full_name EQ 'indian' THEN BEGIN
      idx = where (gphit[diaznl_idx+key_shift, *] LE -32.)
      msf[idx, *] = valmask
   ENDIF ELSE BEGIN
      msf[where(msfmsk EQ 0)] = valmask
   ENDELSE

   field = {name: '', data: msf, legend: '', units: 'm3/s', origin: '', dim: 0, direc:''}

   field.origin = v.origin
   field.dim = 2
   field.direc = 'yz'

   return, field
END
