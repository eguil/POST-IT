;+
;
; make Y transport in Sv at T point
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
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: make_ytran.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
FUNCTION make_ytran, file_name, ncdf_db $
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
 usage='result=make_ytran(file_name, ncdf_db ' $
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

; Read V
;
   file_nam = strmid(file_name, 0, strlen(file_name)-4)

   v = nc_read(file_nam+'V.nc','vomecrty', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
   dim1v = (size(v.data))[1]
   dim2v = (size(v.data))[2]
   dim3v = (size(v.data))[3]

   idx = where(v.data EQ valmask)
   IF idx[0] NE -1 THEN BEGIN
    v.data[idx] = 0.0
   ENDIF

;  Transport along Y : try = v*e1t*e3t (Sv)

   e1t3d = reform(e1t[*]#replicate(1, jpk), jpi, jpj, jpk)
   e3t3d = reform(replicate(1, jpi*jpj)#e3t, jpi, jpj, jpk)
   try = v.data*e1t3d*e3t3d

   try = total(try, 1)
; Petite astuce qui regle des problemes de domdef. On fait en sorte
; de garder la dimension initiale du tableau 3D alors que 'total' nous
; en fait perdre une.
   trym = reform(replicate(1, dim1v)#reform(try, dim2v*dim3v), dim1v, dim2v, dim3v)

   field = {name: '', data: trym, legend: '', units: '', origin: '', dim: 0, direc:''}

   field.origin = v.origin
   field.dim = v.dim
   IF cmdm.spec NE '-' THEN BEGIN
      field.legend = ' ['+cmdm.date1+'-'+cmdm.spec+']'
   ENDIF ELSE BEGIN
      field.legend = ' ['+cmdm.date1+']'
   ENDELSE
   field.direc = v.direc
   return, field
END
