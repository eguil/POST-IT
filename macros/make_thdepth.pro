;+
;
; make depth of 20C
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
; $Id: make_thdepth.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
FUNCTION make_thdepth, file_name, ncdf_db $
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

 usage='result=make_thdepth(file_name, ncdf_db ' $
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

;
; Read t
;
   vert_type = '0'

   tn = nc_read(file_name,'votemper', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2)
;
; Compute depth of 20C isotherm (from diahth.F of OPA)
;
; ... initialization to the number of ocean w-point mbathy

   ik20c = lonarr((size(tn.data))[1], (size(tn.data))[2])
   ik20c[*, *] = 1
   hd20 = fltarr((size(tn.data))[1], (size(tn.data))[2])
   zdiv = fltarr((size(tn.data))[1], (size(tn.data))[2])

; ... search the depth of 20 degrees isotherm
;     ( starting from the top, last level above 20C, if not exist, = 1)

   FOR jk = 0, nzt-1-1 DO  BEGIN
      idx = where (tn.data[*, *, jk] GE 20)
      IF idx[0] NE -1 THEN BEGIN
       ik20c[idx] = jk
      ENDIF
   ENDFOR

; depth is a linear interpolation

   hd20 = gdept[ik20c]
   IF debug_w THEN BEGIN
    help, hd20
    print, 'min(hd20), max(hd20) : ', min(hd20), max(hd20)
   ENDIF

   IF valmask EQ 0. THEN BEGIN
      tempm = tmask[firstxt:lastxt, firstyt:lastyt, firstzt:lastzt]
      idm = where (tempm EQ 0)
      tn.data[idm] = 1.e20
      valmask = 1.e20
      tempm = 0
   ENDIF

   FOR jj = 0, (size(tn.data))[2]-1 DO BEGIN
      FOR ji = 0, (size(tn.data))[1]-1 DO BEGIN
         IF tn.data[ji, jj, 0] LT valmask/10. THEN BEGIN
            hd20[ji, jj] = gdept[ik20c[ji, jj]] + $
             ( gdept[ik20c[ji, jj]+1]-gdept[ik20c[ji, jj]] ) $
             * tmask[ji, jj, ik20c[ji, jj]+1] * ( 20. - tn.data[ji, jj, ik20c[ji, jj]] ) $
             / ( tn.data[ji, jj, ik20c[ji, jj]+1] - tn.data[ji, jj, ik20c[ji, jj]] )
         ENDIF ELSE BEGIN
            hd20[ji, jj] = !values.f_nan
         ENDELSE
      ENDFOR
   ENDFOR

;   hd20 = min(hd20, 0.)
   IF debug_w THEN BEGIN
    help, hd20
    print, 'min(hd20), max(hd20) : ', min(hd20), max(hd20)
   ENDIF


; bound by the ocean depth, minimum value, first T-point depth
;          hd20[ji,jj] = min( zd20, fsdepw(ji,jj,mbathy(ji,jj)) )

   field = {name: '', data: hd20, legend: '', units: '', origin: '', dim: 2, direc:'xy'}

   IF debug_w THEN BEGIN
    print, tn.origin, tn.direc
   ENDIF

   field.origin = tn.origin

   IF debug_w THEN BEGIN
    info = report('leaving ...')
   ENDIF
   return, field
END
