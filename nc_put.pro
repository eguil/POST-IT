;+
;
; @param FLD {in}{required}{type=structure}
; field
; fdl.data, fld.units, fld.short_name, fld.long_name, fld.missing_value, fld.direc
; with fld.direc= data type (x,xy,xyzt,yt,etc...)
;
; @param FILE_NAME {in}{required}{type=string}
;
; @keyword NCDF_DB {required}{type=string}
; <location>:<path> or just <path>
;
; @keyword X {type=structure}
; attributes of X direction
; x.name, x.units, x.long_name, x.data
;
; @keyword Y {type=structure}
; attributes of Y direction
; y.name, y.units, y.long_name, y.data
;
; @keyword Z {type=structure}
; attributes of Z direction
; z.name, z.units, z.long_name, z.data
;
; @keyword T {type=structure}
; attributes of T direction
; t.name, t.units, t.long_name, t.data, t.calendar, t.origin
;
; @keyword GLOBAL {type=structure}
; global attributes: conventions, title, origin
;
; @examples
; IDL> fld={data:1.,  units:'', short_name:'', long_name:'', missing_value:'', direc:''}
; IDL> file_name='ginette'
; IDL> ncdf_db='local:./'
; IDL> nc_put, fld, file_name,NCDF_DB=ncdf_db
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; why ncfd_db is keyword here (argument elsewhere) ?
;
; check required keyword type
;
; error handling
;
; @history
;
; - fplod 20091210T132915Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: nc_put.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
PRO nc_put, fld, file_name $
    , NCDF_DB=ncdf_db $
    , X=x $
    , Y=y $
    , Z=z $
    , T=t $
    , GLOBAL=global
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
; Return to caller if errors
 ON_ERROR, 2
;
 usage='nc_put, fld, file_name'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(fld,/type)
 IF (arg_type NE 8) THEN BEGIN
   ras = report(['Incorrect arg type fld' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_struct_tags=TAG_NAMES(fld)

 tag=WHERE(STRMATCH(arg_struct_tags, 'DATA'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag DATA fld' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(fld.data,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type fld.data' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'UNITS'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag UNITS fld' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(fld.units,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type fld.units' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'SHORT_NAME'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag SHORT_NAME fld' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(fld.short_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type fld.short_name' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'LONG_NAME'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag LONG_NAME fld' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(fld.long_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type fld.long_name' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'MISSING_VALUE'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag MISSING_VALUE fld' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(fld.missing_value,/type)
 IF (arg_type NE 7 AND arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type fld.missing_value' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'DIREC'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag DIREC fld' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(fld.direc,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type fld.direc' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(file_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type file_name' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF


   dim = size(fld.data)
   dim_check = 0

   type = fld.direc

; open netCDF file

   cdfid = ncdf_create(ncdf_db+file_name, /clobber)
   IF debug_w THEN BEGIN
    print, '    type,cdfid = ', type, cdfid
   ENDIF
   ncdf_control, cdfid, /fill

; grid

   jpi=1
   jpj=1
   jpk=1

   xpos = strpos(type, 'x')
   ypos = strpos(type, 'y')
   zpos = strpos(type, 'z')
   tpos = strpos(type, 't')

   IF xpos NE -1 THEN BEGIN
      dim_check = dim_check+1
      jpi = dim[xpos+1]
   ENDIF
   xid = ncdf_dimdef(cdfid, 'lon', jpi)

   IF ypos NE -1 THEN BEGIN
      dim_check = dim_check+1
      jpj = dim[ypos+1]
   ENDIF
   yid = ncdf_dimdef(cdfid, 'lat', jpj)

   IF zpos NE -1 THEN BEGIN
      dim_check = dim_check+1
      jpk = dim[zpos+1]
   ENDIF
   zid = ncdf_dimdef(cdfid, 'depth', jpk)

   IF tpos NE -1 THEN BEGIN
      dim_check = dim_check+1
   ENDIF
   tid = ncdf_dimdef(cdfid, 'time', /UNLIMITED)

   IF debug_w THEN BEGIN
    print, '    dim check ', dim_check
   ENDIF

   IF dim_check NE dim[0] THEN BEGIN
      print, ' ERROR data size / type mismatch ', type, dim[0]
   ENDIF
   swx = 0
   swy = 0
   swz = 0
   swt = 0

   ; horizontal zonal axis
   IF debug_w THEN BEGIN
    print, '    building zonal axis, xid: ', xid
   ENDIF
;   IF strpos(type, 'x') NE -1 AND keyword_set(X) THEN BEGIN
      IF strpos (type, 'xy') NE -1 THEN BEGIN
         lonid = ncdf_vardef(cdfid,'lon', [xid, yid], /FLOAT)
      ENDIF ELSE BEGIN
         lonid = ncdf_vardef(cdfid,'nav_lon', [xid], /FLOAT)
      ENDELSE
      NCDF_ATTPUT, cdfid, lonid, 'units', x.units
      NCDF_ATTPUT, cdfid, lonid, 'long_name', x.long_name
      NCDF_ATTPUT, cdfid, lonid, 'valid_min', min(x.data), /float
      NCDF_ATTPUT, cdfid, lonid, 'valid_max', max(x.data), /float
      swx = 1
;   ENDIF

   ; horizontal meridional axis
   IF debug_w THEN BEGIN
    print, '    building Y axis, yid: ', yid
   ENDIF
   IF strpos (type, 'xy') NE -1 THEN BEGIN
      latid = ncdf_vardef(cdfid,'lat', [xid, yid], /FLOAT)
   ENDIF ELSE BEGIN
      latid = ncdf_vardef(cdfid,'nav_lat', [yid], /FLOAT)
   ENDELSE
   NCDF_ATTPUT, cdfid, latid, 'units', y.units
   NCDF_ATTPUT, cdfid, latid, 'long_name', y.long_name
   NCDF_ATTPUT, cdfid, latid, 'valid_min', min(y.data), /float
   NCDF_ATTPUT, cdfid, latid, 'valid_max', max(y.data), /float
   swy = 1

   ; vertical axis
   IF debug_w THEN BEGIN
    print, '    building Z axis, zid: ', zid
   ENDIF
   IF zpos NE -1 AND keyword_set(Z) THEN BEGIN
      vertid = ncdf_vardef(cdfid,z.name, [zid], /FLOAT)
      NCDF_ATTPUT, cdfid, vertid, 'units', z.units
      NCDF_ATTPUT, cdfid, vertid, 'long_name', z.long_name
      NCDF_ATTPUT, cdfid, vertid, 'valid_min', min(z.data), /float
      NCDF_ATTPUT, cdfid, vertid, 'valid_max', max(z.data), /float
      swz = 1
   ENDIF

   ; time axis
   IF debug_w THEN BEGIN
    print, '    building T axis, tid: ', tid
   ENDIF
   IF tpos NE -1 AND keyword_set(T) THEN BEGIN
      mid = NCDF_VARDEF(cdfid, 'time', [tid], /long)
      NCDF_ATTPUT, cdfid, mid, 'units', t.units
      NCDF_ATTPUT, cdfid, mid, 'calendar', t.calendar
      NCDF_ATTPUT, cdfid, mid, 'origin', t.origin
      NCDF_ATTPUT, cdfid, mid, 'long_name', t.long_name
      NCDF_ATTPUT, cdfid, mid, 'short_name', 'Time'
      NCDF_ATTPUT, cdfid, mid, 'valid_min', min(t.data), /long
      NCDF_ATTPUT, cdfid, mid, 'valid_max', max(t.data), /long
      time_data = t.data
      swt = 1
   ENDIF ELSE BEGIN
      mid = NCDF_VARDEF(cdfid, 'time', [tid], /long)
      NCDF_ATTPUT, cdfid, mid, 'units', "days since 1991-03-01 00:00:00"
      time_data = 1.
      swt = 1
      type = type+'t'
      CASE type OF
          'xt':   fld.data = reform(fld.data, jpi, 1)
          'yt':   fld.data = reform(fld.data, jpj, 1)
          'zt':   fld.data = reform(fld.data, jpk, 1)
          'xyt':  fld.data = reform(fld.data, jpi, jpj, 1)
          'xzt':  fld.data = reform(fld.data, jpi, jpk, 1)
          'yzt':  fld.data = reform(fld.data, jpj, jpk, 1)
          'xyzt': fld.data = reform(fld.data, jpi, jpj, jpk, 1)
      ENDCASE
   ENDELSE

   IF debug_w THEN BEGIN
    print, '    test 1 fld.short_name = ', fld.short_name
    print, '    swx,y,z,t = ', swx, swy, swz, swt
   ENDIF

; field attributes

   IF debug_w THEN BEGIN
    print, '    type, size fld.data ', type, size(fld.data)
   ENDIF

   varn = strcompress(fld.short_name, /remove_all)

   CASE type OF
      'x': fldid = NCDF_VARDEF(cdfid,varn, [xid], /FLOAT)
      'y': fldid = NCDF_VARDEF(cdfid,varn, [yid], /FLOAT)
      'z': fldid = NCDF_VARDEF(cdfid,varn, [zid], /FLOAT)
      't': fldid = NCDF_VARDEF(cdfid,varn, [tid], /FLOAT)
      'xy': fldid = NCDF_VARDEF(cdfid,varn, [xid, yid], /FLOAT)
      'xz': fldid = NCDF_VARDEF(cdfid,varn, [xid, zid], /FLOAT)
      'yz': fldid = NCDF_VARDEF(cdfid,varn, [yid, zid], /FLOAT)
      'xyz': fldid = NCDF_VARDEF(cdfid,varn, [xid, yid, zid], /FLOAT)
      'xyt': fldid = NCDF_VARDEF(cdfid,varn, [xid, yid, tid], /FLOAT)
      'xzt': fldid = NCDF_VARDEF(cdfid,varn, [xid, zid, tid], /FLOAT)
      'yzt':  fldid = NCDF_VARDEF(cdfid,varn, [xid, yid, zid, tid], /FLOAT)
      'xyzt': fldid = NCDF_VARDEF(cdfid,varn, [xid, yid, zid, tid], /FLOAT)
      ELSE:
   ENDCASE
   CASE type OF
      'xyzt':  fld.data = reform(fld.data,jpi,jpj,jpk,jpt)
      ELSE:
   ENDCASE
   NCDF_ATTPUT, cdfid, fldid, 'units', fld.units
   NCDF_ATTPUT, cdfid, fldid, 'long_name', fld.long_name
   NCDF_ATTPUT, cdfid, fldid, 'short_name', varn
   NCDF_ATTPUT, cdfid, fldid, 'missing_value', fld.missing_value
   NCDF_ATTPUT, cdfid, fldid, 'valid_min', min(fld.data[where (fld.data NE fld.missing_value)]), /float
   NCDF_ATTPUT, cdfid, fldid, 'valid_max', max(fld.data[where (fld.data NE fld.missing_value)]), /float


; global attributes

   NCDF_ATTPUT, cdfid, /GLOBAL, 'Conventions', global.conventions
   NCDF_ATTPUT, cdfid, /GLOBAL, 'Title', global.title
   NCDF_ATTPUT, cdfid, /GLOBAL, 'Origin', global.origin
   NCDF_ATTPUT, cdfid, /GLOBAL, 'Software', global.software

; put file in data mode

   NCDF_CONTROL, cdfid, /ENDEF

;
; put grid data
;
   print, ' '
   print, '    Writing '+varn+' to netCDF file ',ncdf_db+file_name


;   stop

   IF swx EQ 1 THEN BEGIN
      print, '       writing lon, size =', size(x.data)
      NCDF_VARPUT, cdfid, lonid, x.data
   END
   IF swy EQ 1 THEN BEGIN
      print, '       writing lat, size =', size(y.data)
      NCDF_VARPUT, cdfid, latid, y.data
   END
   IF swz EQ 1 THEN BEGIN
      print, '       writing depth, size =', size(z.data)
      NCDF_VARPUT, cdfid, vertid, z.data
   END
   IF swt EQ 1 THEN BEGIN
      print, '       writing time, size=', size (time_data)
      NCDF_VARPUT, cdfid, mid, time_data
   END

; put field data

    print, '       Field legend = ',fld.long_name
    print, '       Field dimensions = ',size(fld.data)
    print, '       Field min/max/unit = ',min(fld.data[where (fld.data NE fld.missing_value)]), max(fld.data[where (fld.data NE fld.missing_value)]), '   ', fld.units
    print, '       Global attributes= Conventions:  ', global.conventions
    print, '                           Title:        ', global.title
    print, '                           Origin:       ', global.origin

;    stop

    NCDF_VARPUT, cdfid, fldid, fld.data

; close file

    NCDF_CLOSE, cdfid
   IF debug_w THEN BEGIN
    info = report('leaving ...')
   ENDIF

END
