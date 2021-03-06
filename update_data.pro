;+
;
; @keyword TAB
;
; @keyword VNAME
;
; @keyword BOXZOOM
;
; @keyword ZUNITS
;
; @keyword ZINVAR
;
; @keyword SUFFIX
;
; @keyword D_DIREC
;
; @keyword TIME_1
;
; @keyword TIME_2
;
; @keyword NO_MEAN
;
; @keyword _EXTRA
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: update_data.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
PRO update_data $
    , TAB=tab $
    , VNAME=vname $
    , BOXZOOM=boxzoom $
    , ZUNITS=zunits $
    , ZINVAR=zinvar $
    , SUFFIX=suffix $
    , D_DIREC=d_direc $
    , TIME_1=time_1 $
    , TIME_2=time_2 $
    , NO_MEAN=no_mean $
    , _EXTRA=extra
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
 usage='update_data' $
    + ', TAB=tab' $
    + ', VNAME=vname' $
    + ', BOXZOOM=boxzoom' $
    + ', ZUNITS=zunits' $
    + ', ZINVAR=zinvar' $
    + ', SUFFIX=suffix' $
    + ', D_DIREC=d_direc' $
    + ', TIME_1=time_1' $
    + ', TIME_2=time_2' $
    + ', NO_MEAN=no_mean' $
    + ', _EXTRA=extra'
;
 nparam = N_PARAMS()
 IF (nparam NE 0) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

   ; Perform mean over vertical levels or extract one vertical level ?
   dim = (size(tab))[0]
   perform_mean = vert_switch GE 1 AND zinvar EQ 1 AND NOT keyword_set(no_mean)

   CASE dim OF

      1: BEGIN
         print, ' !!!! 1D case not implemented because !!!!'
         print, ' !!!! read_ncdf cannot handle 1D data !!!!'
      END

      ;; fichier 2d : surface (n_elements(gdept) = 0 or 1)
      2: BEGIN
         print, '    Found ', vname, ' (2D data) from file'
         d_direc = 'xy'
         IF perform_mean EQ 1 THEN BEGIN
            ; vert_mean[0] can be different from vert_mean[1] to select one level or layer
            CASE vert_type OF
               'z': BEGIN
                  IF vert_mean[0] EQ vert_mean[1] THEN BEGIN
                     suffix = ' at '+strtrim(string(long(vert_mean[0])), 2)+zunits+' '
                  ENDIF ELSE BEGIN
                     suffix = ' at '+strtrim(string(long(vert_mean[0])), 2)+'-'+strtrim(string(long(vert_mean[1])), 2)+zunits+' '
                  ENDELSE
               END
               'level':BEGIN ; case level (zindex)
                  suffix = ' at '+vert_type+' '+strtrim(string(long(gdept[vert_mean[0]]+.1)), 2)+' '
               END
            ENDCASE
         ENDIF
      END

      ;; fichier 3d : 2 cas = 1/ 2d espace + temps 2/ 3d espace (n_elements(gdept) > 1)
      3: BEGIN
         IF jpt EQ 1 THEN BEGIN
            print, '    Found ', vname, ' (3D data) from file'
            d_direc = 'xyz'
            IF perform_mean EQ 1 THEN BEGIN
               ;old_boite = [lon1, lon2, lat1, lat2, prof1, prof2]
               print, '      Average in vertical domain ', vert_type, vert_mean
               ;IF vert_mean[0] EQ vert_mean[1] THEN BEGIN
               ; stop, report('vert_mean[0] cannot be equal to vert_mean[1]')
               ; ENDIF
               CASE vert_type OF
                  'z': BEGIN
                     zmean = moyenne(temporary(tab), 'z', boite = boxzoom, /NAN)
                     suffix = ' averaged in ['+strtrim(string(long(vert_mean[0])), 2)+zunits+', '+strtrim(string(long(vert_mean[1])), 2)+zunits+']'
                  END
                  'level':BEGIN ; case level (zindex)
                     zmean = moyenne(temporary(tab), 'z', boite = boxzoom, /zindex, /NAN)
                     suffix = ' averaged in '+vert_type+' ['+strtrim(string(long(vert_mean[0])), 2)+','+strtrim(string(long(vert_mean[1])), 2)+']'
                  END
               ENDCASE
               tab = temporary(zmean)
               d_direc = 'xy'
            ENDIF
         ENDIF ELSE BEGIN
            print, '    Found ', vname, ' (2D data time serie)', strcompress(string(time_1)),'-', strtrim(string(time_2), 2), ' [every ',strtrim(string(time_stride), 2), ']   from file'
            d_direc = 'xyt'
         ENDELSE
      END

      ;; fichier 4d : volume + temps
      4: BEGIN
         IF debug_w THEN BEGIN
          print, ' jpt = ', jpt
         ENDIF
         IF jpt GT 1 THEN BEGIN
            print, '    Found ', vname, ' (3D data time serie)', strcompress(string(time_1)),'-', strtrim(string(time_2), 2), '   from file'
            d_direc = 'xyzt'
            IF perform_mean EQ 1 THEN BEGIN
               print, '      Average in vertical domain ', vert_type, vert_mean
               CASE vert_type OF
                  'z': BEGIN
                     zmean = grossemoyenne(tab, 'z', boite = boxzoom, /NAN)
                     suffix = ' averaged in ['+strtrim(string(long(vert_mean[0])), 2)+zunits+','+strtrim(string(long(vert_mean[1])), 2)+zunits+']'
                  END
                  'level': BEGIN ; case level (zindex)
                     zmean = grossemoyenne(temporary(tab), 'z', boite = boxzoom, /zindex, /NAN)
                     suffix = ' averaged in '+vert_type+' ['+strtrim(string(long(vert_mean[0])), 2)+','+strtrim(string(long(vert_mean[1])), 2)+']'
                  END
               ENDCASE
               tab = temporary(zmean)
               d_direc = 'xyt'
            ENDIF
         ENDIF
      END

      ELSE: BEGIN
         err_mess = '  *** nc_read : ERROR dimension > 4'
         tab = -1.0
      END

   ENDCASE

   IF debug_w THEN BEGIN
    info = report('leaving ...')
   ENDIF

END
