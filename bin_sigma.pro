;+
;
; Density bining of pfild
;
; @param CMD {in}{required}{type=structure}
; command line of window
;
; @param PFILDI {inout}{required}{type=structure}
; field to bin
;
; <pre>
; Method :
; --------
; a- read density
; b- define density grid
; c- project field
; </pre>
;
; @examples
; IDL> cmd={plt:'xy',var:''}
; IDL> pfildi={origin:'ginette',name:'',data:''}
; IDL> vert_type='z'
; IDL> bin_sigma, cmd, pfildi
; IDL> print,pfildi
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; <pro>grille</pro>
;
; <propost_it>make_eos</propost_it>
; <propost_it>bining2</propost_it>
; <propost_it>bining3</propost_it>
;
; @todo
;
; explication sur vert_type
;
; complete example
;
; @history
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - fplod 20091208T102329Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * syntax of array
;
; - EG 19-2-99
;
; @version
; $Id: bin_sigma.pro 188 2010-01-14 11:44:08Z pinsard $
;
;-
PRO bin_sigma, cmd, pfildi
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='bin_sigma, cmd, pfildi'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
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

 tag=WHERE(STRMATCH(arg_struct_tags, 'PLT'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag PLT cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(cmd.plt,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.plt' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 tag=WHERE(STRMATCH(arg_struct_tags, 'VAR'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag VAR cmd' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 arg_type = size(cmd.var,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.grid' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(pfildi,/type)
 IF (arg_type NE 8) THEN BEGIN
   ras = report(['Incorrect arg type pfildi' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_struct_tags=TAG_NAMES(pfildi)

 tag=WHERE(STRMATCH(arg_struct_tags, 'ORIGIN'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag ORIGIN pfildi' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(pfildi.origin,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type pfildi.origin' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'NAME'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag NAME pfildi' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(pfildi.name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type pfildi.origin' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 tag=WHERE(STRMATCH(arg_struct_tags, 'DATA'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag DATA pfildi' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(pfildi.data,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type pfildi.origin' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 common_type=size(vert_type,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type vert_type' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

   grille,mask,glam,gphi,gdep,nx,ny,nz,firstx,firsty,firstz,lastx,lasty,lastz

; a- read density and bowl [if sig_bowl=1]

   file_name = pfildi.origin
   file_nam = strmid(file_name, 0, strlen(file_name)-4)+'T.nc'
   vargrid = 'T'
   sig = make_eos(file_nam, '', time_1 = time1_r, time_2 = time2_r)
   IF sig_bowl EQ 1 THEN BEGIN
      sobwlmax = make_sobwlmax(file_nam, '', time_1 = time1_r, time_2 = time2_r)
   ENDIF ELSE BEGIN
      sobwlmax = {data:0}
   ENDELSE

; b- define density grid

   n_sig = (sig_max - sig_min)/sig_del + 1
   z_sig = sig_min+findgen(n_sig)*sig_del

; c- project field

   CASE pfildi.name OF
      '@@vodeptht': data_s = sig.data
      '@@vosigvol': data_s = sig.data
      '@@vosigthi': data_s = sig.data
      ELSE: data_s = pfildi.data
   ENDCASE
   final_dim = pfildi.dim
   IF strpos(cmd.plt, "_") NE -1 OR strpos(cmd.plt, "#") NE -1 THEN BEGIN
      print, '    Performing density bining (assuming reduced domain)...'
      IF cmd.var EQ '@@vosigvol' THEN BEGIN
         print, '    (using bining3)...'
         bining3, sig.data-1000., data_s, sobwlmax.data, sig_bowl, depth_bin, thick_bin, f_bin, bowl_bin, vol_bin, SIGMA = z_sig, DEPTH_T = gdept, DEPTH_W = gdepw, E1T = e1t, E2T = e2t, E3T = e3t, TMASK = mask
      ENDIF ELSE BEGIN
         print, '    (using bining2)...'
         bining2, sig.data-1000., data_s, sobwlmax.data, sig_bowl, depth_bin, thick_bin, f_bin, bowl_bin, SIGMA = z_sig, DEPTH_T = gdept, DEPTH_W = gdepw, E3T = e3t, E3W = e3w, TMASK = mask
      ENDELSE
   ENDIF ELSE BEGIN
      print, '    Performing density bining (assuming global domain)...'
      IF cmd.var EQ '@@vosigvol' THEN BEGIN
         print, '    (using bining3)...'
         bining3, sig.data-1000., data_s, sobwlmax.data, sig_bowl, depth_bin, thick_bin, f_bin, bowl_bin, vol_bin, SIGMA = z_sig, DEPTH_T = gdept, DEPTH_W = gdepw, E1T = e1t, E2T = e2t, E3T = e3t, TMASK = tmask
      ENDIF ELSE BEGIN
         print, '    (using bining2)...'
         bining2, sig.data-1000., data_s, sobwlmax.data, sig_bowl, depth_bin, thick_bin, f_bin, bowl_bin, SIGMA = z_sig, DEPTH_T = gdept, DEPTH_W = gdepw, E3T = e3t, E3W = e3w, TMASK = tmask
      ENDELSE
   ENDELSE

   ; output : depth_bin = depth of isopycnal surfaces (3D array)
   ;          thick_bin = thickness of isopycnal surfaces (3D array)
   ;              f_bin = pfild.data averaged for each sigma-layer (3D array)


;   print, '    Density domain / intervals : ', sig_min, sig_max, sig_del, n_sig

   IF sig_surf EQ 0. THEN BEGIN
      ; vertical section (pltz)

      CASE pfildi.name OF
         '@@vodeptht': nfild = depth_bin[*, *, 0:n_sig-1]
         '@@vosigthi': nfild = thick_bin[*, *, 0:n_sig-1]
         '@@vosigvol': BEGIN
             nfild = vol_bin
             final_dim = 1
;            e12t = e1t*e2t
;            e12t3 = reform((reform(e12t[firstx:lastx, firsty:lasty], nx*ny))#replicate(1,n_sig +2), nx, ny, n_sig+2)
;            nfild = (thick_bin*e12t3)[*, *, 0:n_sig-1]

         END
         ELSE: nfild = f_bin[*, *, 0:n_sig-1]
      ENDCASE

   ENDIF ELSE BEGIN
      ; horizontal projection on sig_surf (plt) example : votemper@s25
      ; find index of sig_surf in z_sig
      index = (where(z_sig LE sig_surf))
      index = index[n_elements(index)-1]

      CASE pfildi.name OF
         '@@vodeptht': nfild = depth_bin[*, *, index]
         '@@vosigthi': nfild = thick_bin[*, *, index]
         '@@vosigvol': BEGIN
            nfild = vol_bin
            final_dim = 1
;            e12t = e1t*e2t
;            e12t3 = reform((reform(e12t[firstx:lastx, firsty:lasty], nx*ny))#replicate(1,n_sig +2), nx, ny, n_sig+2)
;            nfild = ((thick_bin*e12t3)[*, *, index])
         END
         ELSE: nfild = f_bin[*, *, index]
      ENDCASE

   ENDELSE

   IF final_dim eq 3 THEN BEGIN
      indx = where(finite(nfild, /NAN))
      nfild[indx] = -1.e20
      nfild[indx] = !VALUES.F_NAN
      indx = where(tmask[*, *, 0] EQ 0)
      nfild[indx] = !VALUES.F_NAN
   ENDIF ELSE BEGIN

   ENDELSE

   pfild2 = {name: pfildi.name, data: nfild, legend: pfildi.legend, units: pfildi.units, origin: pfildi.origin, dim: final_dim, bowl: bowl_bin}
   pfildi = pfild2


END
