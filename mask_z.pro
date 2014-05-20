;+
;
; mask zonal mean
;
; mask pltz data according to bathy or box
;
; @param FLD
;
; @param CMD {in}{required}{type=structure}
; command line of window
;
; @param DIMPLOT {in}{required}{type=integer}
; dimension of plot (1,2)
;
; @param LEGZ {out}{type=string}
; @param BOITE_PLTZ {out}{type=1d array of integer}
;
; @examples
; IDL> cmd={plt:'xy',var:''}
; IDL> dimplot=1
; IDL> mesh_type='oce'
; IDL> splot=1
; IDL> sig_min=20.
; IDL> sig_max=28.
; IDL> data_domain='global'
; IDL> box_h=[20L,380L,-30L,30L]
; IDL> msf_mean = 0
; IDL> mask_z, fld, cmd, boite_pltz, dimplot, legz
; IDL> print, boite_pltz,legz
;          20         380         -30          30
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; <propost_it>grep</propost_it>
;
; @todo
; explication sur common oce, splot, sig_min, sig_max, data_domain, box_h, msf_mean
;
; ne plante pas si fld n'est pas défini
;
; @history
;
; - fplod 20091210T100350Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: mask_z.pro 213 2010-03-12 16:53:44Z ericg $
;
;-
PRO mask_z, fld, cmd, boite_pltz, dimplot, legz
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='mask_z, fld, cmd, boite_pltz, dimplot, legz'

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

 arg_type = size(dimplot,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type dimplot' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type=size(mesh_type,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type mesh_type' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type=size(splot,/type)
 IF (common_type NE 3) THEN BEGIN
   ras = report(['Incorrect common type splot' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type=size(sig_min,/type)
 IF (common_type NE 4) THEN BEGIN
   ras = report(['Incorrect common type sig_min' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type=size(sig_max,/type)
 IF (common_type NE 4) THEN BEGIN
   ras = report(['Incorrect common type sig_max' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type=size(data_domain,/type)
 IF (common_type NE 7) THEN BEGIN
   ras = report(['Incorrect common type data_domain' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type=size(box_h,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type box_h' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 common_nelem=size(box_h,/n_elements)
 IF (common_nelem NE 4) THEN BEGIN
   ras = report(['Incorrect common n_elements box_h' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 common_type=size(msf_mean,/type)
 IF ((common_type NE 2) AND (common_type NE 3)) THEN BEGIN
   ras = report(['Incorrect common type msf_mean' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF


   legz = ''

   CASE mesh_type OF
      'oce': BEGIN
         IF splot EQ 1 THEN BEGIN
          vertical_domain = [sig_min, sig_max]
         ENDIF ELSE BEGIN
          vertical_domain = [0, depth_z]
         ENDELSE
         CASE strmid(cmd.plt, 0, 2) OF
            'ys': data_domain = 'zonal'
            ELSE:
         ENDCASE  

         CASE data_domain OF
            'equator': BEGIN
               tmask[*, *, *] = 1
               IF dimplot EQ 1 THEN BEGIN
                boite_pltz = box_h
               ENDIF
               IF dimplot EQ 2 THEN BEGIN
                boite_pltz = [box_h, vertical_domain]
               ENDIF
            END
            'zonal': BEGIN
                tmask[*, *, *] = 1
                zbox = def_box(cmd.plt, dimplot, legz, time_stride)
                boite_pltz = zbox
                IF dimplot EQ 2 THEN BEGIN
                   IF n_elements(boite_pltz) EQ 4 THEN BEGIN
                    boite_pltz = [zbox, vertical_domain]
                   ENDIF
                ENDIF
                IF dimplot EQ 1 AND splot EQ 1 THEN BEGIN
                   boite_pltz = [zbox, vertical_domain]
                ENDIF
;               IF dimplot EQ 2 THEN BEGIN
;                boite_pltz = [box_h,vertical_domain]
;               ENDIF

                idxmsk=where(fld eq valmask)
                IF idxmsk[0] ne -1 THEN BEGIN
                    print, '     mask_z: building mask from fld valmask'
                    IF (size(fld))[0] EQ 3 THEN BEGIN
                        zmsk = fld[*,*,1] LT valmask/10.
                    ENDIF ELSE BEGIN
                        zmsk = fld LT valmask/10.
                    ENDELSE
                    tmask=reform(replicate(jpi,1)#zmsk[*],jpi,jpj,jpk,/overwrite)
                ENDIF
            END
            ELSE: BEGIN
               ; Full geographical domain
               IF strlen(cmd.plt) EQ 2 THEN BEGIN
                  IF cmd.var NE 'vozonbsf' THEN BEGIN
                     IF (dimplot EQ 1 AND (size(fld))[0] EQ 2) $
                      OR (dimplot EQ 2 AND (size(fld))[0] EQ 3) THEN BEGIN
                        IF cmd.grid EQ 'T' THEN BEGIN    
                           maskzm = 1-maskread('mediteranean', 'orca', /D3)
                           print, '     [masking out mediteranean]'
                           print, ' '
                           tmask = tmask*maskzm
                        ENDIF 
                     ENDIF
                     IF dimplot EQ 1 THEN BEGIN
                      boite_pltz = box_h
                     ENDIF
                     IF dimplot EQ 2 THEN BEGIN
                        CASE strmid(cmd.plt, 1, 1) OF
                           't': boite_pltz = box_h
                           ELSE: boite_pltz = [box_h, vertical_domain]
                        ENDCASE
                     ENDIF
                  ENDIF ELSE BEGIN
                     ; mask MSF as field=valmask
                     idx=where (fld eq valmask)
                     fld[idx] = !values.f_nan
                     boite_pltz = [box_h, vertical_domain]
                  ENDELSE
               ENDIF ELSE BEGIN
                  CASE strmid(cmd.plt, 0, 2) OF
                  'xy': BEGIN & char = strmid(cmd.plt,3, 1) & ideb = 4 & END
                  ELSE: BEGIN & char = strmid(cmd.plt,dimplot+1, 1) & ideb = dimplot+2 & END
                  ENDCASE

                  CASE char OF
                     '#': BEGIN
                                ; bathymetry mask for data
                        bat_name = strmid(cmd.plt, ideb, strlen(cmd.plt)-ideb)
                        full_name = grep('ls -al '+hom_idl+'grids | grep orca.'+bat_name+' | awk ''NR == 1 {print $0}''', ' ', 8)
                        full_name = strmid(full_name, 5, 50)
                        legz = full_name
                        legbox = full_name
                        IF cmd.var NE 'vozonbsf' THEN BEGIN
                           maskzm = maskread(full_name, 'orca', /D3)
                           tmask = tmask*maskzm
                           IF dimplot EQ 1 THEN BEGIN
                            boite_pltz = box_h
                           ENDIF
                           IF dimplot EQ 2 THEN BEGIN
                              CASE strmid(cmd.plt, 1, 1) OF
                                 't': boite_pltz = box_h
                                 ELSE: boite_pltz = [box_h, vertical_domain]
                              ENDCASE
                           ENDIF
                        ENDIF ELSE BEGIN
                           ; mask MSF as field=valmask
                           idx=where (fld eq valmask)
                           fld[idx] = !values.f_nan
                           boite_pltz = [box_h, vertical_domain]
                        ENDELSE
                     END
;                        '@': BEGIN
;                           ; density level
;                        END
                     ELSE: BEGIN
                        ; mask in box
                        zbox = def_box(cmd.plt, dimplot, legz, time_stride)
                        boite_pltz = zbox
                        IF dimplot EQ 2 THEN BEGIN
                           CASE strmid(cmd.plt, 1, 1) OF
                              't': boite_pltz = zbox
                              ELSE: IF n_elements(boite_pltz) EQ 4 THEN BEGIN
                                      boite_pltz = [zbox, vertical_domain]
                                     ENDIF
                           ENDCASE
                        ENDIF
                        IF dimplot EQ 1 THEN BEGIN
                           CASE strmid(cmd.plt, 1, 1) OF
                              't': boite_pltz = zbox
                              ELSE: BEGIN
                                 CASE strmid(cmd.plt, 0, 1) OF
                                    'x': boite_pltz = zbox
                                    'y': boite_pltz = zbox
                                    ELSE: IF n_elements(boite_pltz) EQ 4 THEN BEGIN
                                            boite_pltz = [zbox, vertical_domain]
                                          ENDIF
                                 ENDCASE
                              END
                           ENDCASE
                        ENDIF
                     END
                  ENDCASE

              ENDELSE
               IF cmd.var EQ 'vozonbsf' AND msf_mean EQ 0 THEN BEGIN
                   print, '     Mask_z special case for vozonbsf'
                   idx = where(gphit EQ max(gphit))
                   idx = idx MOD jpi
                   print, idx[0]
                   boite_pltz = [idx[0], idx[0], boite_pltz[2:5]]
                   domdef, boite_pltz, /xindex ; indice pour x
                   IF (size(fld))[0] EQ 3 THEN BEGIN
                       tmask[idx[0], firstyt:lastyt, *] = fld[*,*,1] LT valmask/10.
                   ENDIF ELSE BEGIN
                       tmask[idx[0], firstyt:lastyt, *] = fld LT valmask/10.
                   ENDELSE
               ENDIF
            END
         ENDCASE
      END
      ELSE: BEGIN ; atmosphere or data

;         IF masked_data EQ 0 THEN BEGIN
;          tmask[*] = 1
;         ENDIF
         CASE strmid(cmd.plt, 0, 2) OF
            'xy': BEGIN & char = strmid(cmd.plt,3, 1) & ideb = 4 & END
            ELSE: BEGIN & char = strmid(cmd.plt,dimplot+1, 1) & ideb = dimplot+2 & END
         ENDCASE


         IF char EQ '#' THEN BEGIN
                                ; mask from file
            bat_name = strmid(cmd.plt, ideb, strlen(cmd.plt)-ideb)
            full_name = grep('ls -al '+hom_idl+'grids | grep '+cmd.grid+'.'+bat_name+' | awk ''NR == 1 {print $0}''', ' ', 8)
            full_name = strmid(full_name, strlen(cmd.grid)+1, 50)
            legz = full_name
            legbox = full_name
            maskzm = maskread(full_name, cmd.grid)
            maskzm = reverse(maskzm, 2)
            maskzm = reform(maskzm, jpi*jpj)
            tmask = reform(maskzm#replicate(1, jpk), jpi, jpj, jpk)
            boite_pltz = box_h
            IF dimplot EQ 2 THEN BEGIN
               CASE strmid(cmd.plt, 1, 1) OF
                  't': boite_pltz = box_h
                  ELSE:  boite_pltz = [box_h, pres_min, pres_max ]
               ENDCASE
            ENDIF
         ENDIF ELSE BEGIN
           ; mask in box
            IF masked_data EQ 0 THEN BEGIN
               CASE atmos_msk OF
               0: BEGIN & mskt = 'ALL points' & tmask[*] = 1 & END
               1: BEGIN & mskt = 'ocean points only' & END
               2: BEGIN & mskt = 'land points only' & tmask = 1-tmask & END
               ENDCASE
               print, '    Using ',mskt, ' (atmos_msk =', atmos_msk, ')'
               print, ' '
            ENDIF

            zbox = def_box(cmd.plt, dimplot, legz, time_stride)
            boite_pltz = zbox
            prof1 = gdept[0]
            prof2 = gdept[0]
            IF dimplot EQ 2 THEN BEGIN
               CASE strmid(cmd.plt, 1, 1) OF
                  't': boite_pltz = zbox
                  ELSE: IF n_elements(boite_pltz) EQ 4 THEN BEGIN
                          boite_pltz = [zbox, pres_min, pres_max]
                         ENDIF
               ENDCASE
            ENDIF
         ENDELSE
      END
      ELSE:
   ENDCASE
END
