;+
;
; Estimation of the Warm Water Volume
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
; @returns
; structure
; -1 in case of error
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_wwv.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_wwv, file_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ALL_DATA=all_data
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
   IF debug_w THEN BEGIN
    info = report('enter ...')
   ENDIF
;
 usage='result=make_wwv(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
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

; Read t
;
   vert_type = '0'

   tn = nc_read(file_name,macro_base_fld, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)

   IF tn.units EQ 'C' THEN BEGIN
      thermo = 20.
   ENDIF
   IF tn.units EQ 'K' THEN BEGIN
      thermo = 293.15
   ENDIF

; Compute depth of 20C isotherm (from diahth.F of OPA)
;
; ... initialization to the number of ocean w-point mbathy


   ik20c = lonarr(nxt,nyt,jpt)
   ik20c[*, *, *] = -1
   hd20 = fltarr(nxt,nyt,jpt)
   hd20[*, *, *] = 0.
   wwv = fltarr(jpt)
   wwv[*] = 0.
   tempm = fltarr(nxt,nyt,nzt)
   tempm[*, *, *] = 1

   grille,mask,glam,gphi,gdep,nx,ny,nz,firstx,firsty,firstz,lastx,lasty,lastz

; ... search the depth of 20 degrees isotherm
;     ( starting from the top, last level above 20C, if not exist, = 1)

   FOR jk = 0, nzt-1 DO  BEGIN
      idx = where ( tn.data[*, *, jk,*] GE thermo)
      IF idx[0] NE -1 THEN BEGIN
       ik20c[idx] = jk
      ENDIF
   ENDFOR

; depth is a linear interpolation
   bad = WHERE(~FINITE(tn.data[*, *, *, *]) )
   tn.data[bad] = 1.e20
   valmask = 1.e20
   tempm[bad] = 0

   IF debug_w THEN BEGIN
    help, hd20
    print, 'min(hd20), max(hd20) : ', min(hd20), max(hd20)
   ENDIF


   FOR jt = 0, jpt-1 DO BEGIN
      FOR jj = 0, nyt-1 DO BEGIN
         FOR ji = 0, nxt-1 DO BEGIN
            IF tn.data[ji, jj, 0, jt] LT valmask/10. THEN BEGIN
               idx20 = ik20c[ji, jj, jt]
               IF idx20 GE 0 THEN BEGIN
                  hd20[ji, jj, jt] = gdept[idx20] +$

                  ( gdept[idx20+1]- gdept[idx20] ) * $ ;DELTAZ entre les niveaux i et i+1
                   mask[ji, jj, idx20+1] * ( thermo - tn.data[ji, jj, idx20, jt] )  $ ;DELTAT entre la hd20 et le niveau i
                   / ( tn.data[ji, jj, idx20+1, jt] - tn.data[ji, jj, idx20, jt] ) ;variation de T entre les niveau i et i+1(dT/dz)
               ENDIF
            ENDIF ELSE BEGIN hd20[ji, jj, *] = -1.
            ENDELSE
         ENDFOR
      ENDFOR
   ENDFOR

;--------------------------------------------------------------------------------
;------------------------calcul du wwv-------------------------------------------
;--------------------------------------------------------------------------------
   IF debug_w THEN BEGIN
    help, hd20
    print, 'min(hd20), max(hd20) : ', min(hd20), max(hd20)
   ENDIF


   FOR jt = 0, jpt-1 DO BEGIN
      FOR jj = 0, nyt-1 DO BEGIN
         FOR ji = 0, nxt-1 DO BEGIN
            IF hd20[ji, jj, jt] NE -1. THEN BEGIN
               wwv[jt] = wwv[jt] + hd20[ji, jj,jt]*e1t[firstx+ji, firsty+jj]*e2t[firstx+ji, firsty+jj]
            ENDIF
         ENDFOR
      ENDFOR
   ENDFOR

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
; bound by the ocean depth, minimum value, first T-point depth
;          hd20[ji,jj] = min( zd20, fsdepw[ji,jj,mbathy[ji,jj]] )
   field = {name: '', data: wwv, legend: '', units: '', origin: '', dim: 1, direc:'t'}

   IF debug_w THEN BEGIN
    print, tn.origin, tn.direc
   ENDIF

   field.origin = tn.origin

   IF debug_w THEN BEGIN
    info = report('leaving...')
   ENDIF

   return, field
END
