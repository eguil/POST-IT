;+
;
; make Barotropic Stream Function
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
; @todo
; where is diag_bsf function ?
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - Fri Apr 6 12:18:15 2001, Eric Guilyardi <ericg\@lemnos.rdg.ac.uk>
;
; @version
; $Id: make_bsf.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_bsf, file_name, ncdf_db $
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
 usage='result=make_bsf(file_name, ncdf_db '$
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

; Read taux and tauy
   file_nam = strmid(file_name, 0, strlen(file_name)-4)
; keep current settings
;   old_boite = [lon1, lon2, lat1, lat2, prof1, prof2]
;   domdef
; diag_bsf requires the whole domain
   jpi = 182
   jpj = 149
   key_offset = [0, 0, 0]
   key_shift_old = key_shift
   key_shift = 0
   ixminmesh =0
   ixmaxmesh =181
;
   iyminmesh =0
   iymaxmesh =148

   CASE config OF
      'ORCA_R2': BEGIN
         ncdf_meshlec, 'meshmask.orca.2d.nc' ; for ST7-type runs
;        ncdf_meshlec, 'meshmask_ORCA_R2.nc' ; for CT/TT-type runs
      END
      'ORCA_R4': BEGIN
         ncdf_meshlec, 'meshmask_orca4.nc'
      END
   ENDCASE
; make masks
   umaskr = umask()
   vmaskr = vmask()
   fmaskr = fmask()

   vargrid = 'U'
   u = nc_read(file_nam+'U.nc','vozocrtx', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, /ALL_DATA)
   vargrid = 'V'
   v = nc_read(file_nam+'V.nc','vomecrty', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, /ALL_DATA)

   IF time_1 EQ time_2 THEN BEGIN

      bsf = diag_bsf(u.data, v.data)
      bsfr = bsf[1:jpi-2, 0:jpj-2]
      bsfr = shift(bsfr, key_shift_old, 0)

   ENDIF ELSE BEGIN

      jpt = time_2-time_1+1
      bsfr = fltarr(180, 148, jpt)
      FOR t = 0, jpt DO BEGIN

         bsf= diag_bsf(u.data[*, *, *, t], v.data[*, *, *, t])
         bsft = bsf[1:jpi-2, 0:jpj-2]
         bsfr[*, *, t] = shift(bsft, key_shift_old, 0)
      ENDFOR

   ENDELSE

; extract sub-domain


   jpi = 180
   jpj = 148
   key_offset = [2, 0, 0]
   key_shift = key_shift_old
   ixminmesh =2
   ixmaxmesh =181
;
   iyminmesh =0
   iymaxmesh =147
;
   izminmesh =0
   izmaxmesh =30
   key_periodique = 1
   ncdf_meshlec, 'meshmask_ORCA_R2.nc'

   isign=where(glamt gt 380.)
   IF isign[0] NE -1 THEN BEGIN
    glamt[isign] = glamt[isign]-360.
   ENDIF
   isign=where(glamu gt 380.)
   IF isign[0] NE -1 THEN BEGIN
    glamu[isign] = glamu[isign]-360.
   ENDIF
   isign=where(glamv gt 380.)
   IF isign[0] NE -1 THEN BEGIN
    glamv[isign] = glamv[isign]-360.
   ENDIF
   isign=where(glamf gt 380.)
   IF isign[0] NE -1 THEN BEGIN
    glamf[isign] = glamf[isign]-360.
   ENDIF
;   domdef, old_boite

   field = {name: '', data: bsfr, legend: '', units: '', origin: '', dim: 0, direc: ''}

   field.origin = u.origin
   field.dim = u.dim
   field.direc = u.direc

   return, field
END
