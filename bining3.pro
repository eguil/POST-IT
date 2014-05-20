;+
;
; @param DENSITY {in}
; density at T points (3D array)
;
; @param X1
; field at T points (e.g. Salinity or Temperature)
;
; @param SOBWLMAX
; bowl depth array
;
; @param SIG_BOWL
; switch for bowl overlay

; @param DEPTH_BIN {out}
; depth of layers (3D array)
;
; @param THICK_BIN {out}
; thickness of layers (3D array)
;
; @param X1_BIN {out}
; X averaged for each sigma-layer (3D array)
;
; @param BOWL_BIN {out}
; bowl depth binned on density
;
; @param VOL_BIN2 {out}
; volume of isopycnals
;
; @keyword SIGMA
; bining values
;
; @keyword DEPTH_T
; depth of T level
;
; @keyword DEPTH_W
; depth of W level
;
; @keyword E1T
;
; @keyword E2T
;
; @keyword E3T
;
; @keyword TMASK
; tmask (3D array)
;
; @todo
;
; x1 not used
;
; sobwlmax not used
;
; sig_bowl not used
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: bining3.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
PRO bining3 $
    , density $
    , x1 $
    , sobwlmax $
    , sig_bowl $
    , depth_bin $
    , thick_bin $
    , x1_bin $
    , bowl_bin $
    , vol_bin2 $
    , SIGMA=sigma $
    , DEPTH_T=depth_t $
    , DEPTH_W=depth_w $
    , E1T=e1t $
    , E2T=e2t $
    , E3T=e3t $
    , TMASK=tmask
;
  compile_opt idl2, strictarrsubs
;
 usage='bining3' $
    + ', density' $
    + ', x1' $
    + ', sobwlmax' $
    + ', sig_bowl' $
    + ', depth_bin' $
    + ', thick_bin' $
    + ', x1_bin' $
    + ', bowl_bin' $
    + ', vol_bin2' $
    + ', SIGMA=sigma' $
    + ', DEPTH_T=depth_t' $
    + ', DEPTH_W=depth_w' $
    + ', E1T=e1t' $
    + ', E2T=e2t' $
    + ', E3T=e3t' $
    + ', TMASK=tmask'
;
 nparam = N_PARAMS()
 IF (nparam LT 9) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(density,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type density' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

   size3d = size(density)
   jpi = size3d[1]
   jpj = size3d[2]
   jpk = size3d[3]

   s_s = sigma
   N_s = n_elements(s_s)

   depth_bin = fltarr(jpi, jpj, N_s)
   thick_bin = fltarr(jpi, jpj, N_s)
   x1_bin = fltarr(jpi, jpj, N_s)
   bowl_bin = fltarr(jpi, jpj)
   vol_bin = fltarr(jpi, jpj, N_s)
   vol_bin2 = fltarr(N_s)

   x1_bin[*, *, *] = !VALUES.F_NAN
   depth_bin[*, *, *] = !VALUES.F_NAN
   thick_bin[*, *, *] = !VALUES.F_NAN
   bowl_bin[*, *] = !VALUES.F_NAN
   vol_bin[*, *, *] =0
   vol_bin2[*] =0

   delta_sig = s_s[1]-s_s[0]

   vol_tot = 0.
   count_bin = fltarr(jpi, jpj, N_s)
   count_bin2 = fltarr(N_s)
   count_bin[*, *, *] =0
   count_bin2[*] =0

; perform bining - loop over i,j,k

   FOR i = 0, (jpi-1) DO BEGIN
      FOR j = 0, (jpj-1) DO BEGIN
         i_ocean = where(tmask[i, j, *] EQ 1)
         IF i_ocean[0] NE -1 THEN BEGIN ; on n entre que si il y a des points ocean
            i_bottom = i_ocean[n_elements(i_ocean)-1]
            FOR k = 0, i_bottom-1 DO BEGIN
               bin_index = floor((density[i, j, k]-s_s[0])/delta_sig+0.5)
               IF bin_index LT 0 OR bin_index GT (N_s-1) THEN BEGIN
;                  print, ' WARNING: bin index out of bounds - density[i, j, k], bin_index, N_s = ', density[i, j, k], bin_index, N_s
               ENDIF ELSE BEGIN
                  vol_bin[i, j, bin_index] = vol_bin[i, j, bin_index] + e1t[i,j]*e2t[i,j]*e3t[k]
                  vol_bin2[bin_index] = vol_bin2[bin_index] + e1t[i,j]*e2t[i,j]*e3t[k]
                  vol_tot = vol_tot + e1t[i,j]*e2t[i,j]*e3t[k]
                  count_bin[i, j, bin_index] = count_bin[i, j, bin_index]+1
                  count_bin2[bin_index] = count_bin2[bin_index]+1
               ENDELSE
            ENDFOR
         ENDIF
      ENDFOR
   ENDFOR

   mask_index = where(count_bin EQ 0)
   vol_bin[mask_index] = !VALUES.F_NAN
   mask_index2 = where(count_bin2 EQ 0)
;   vol_bin2[mask_index2] = !VALUES.F_NAN

   print, ' total volume of box (m3) = ', vol_tot
;   print, ' density bins = ', vol_bin2
;   print, ' binned volume (1e14 m3) = ', transpose(vol_bin)*1.e-14

END
