;+
;
; Non penetrative convective adjustment scheme. solve the static
; instability of the water column.
;	
; The algorithm used converges in a maximium of jpk iterations.
; instabilities are treated when the vertical density gradient
; is less than 1.e-5.
;
; References :Madec et al., 1991, JPO, 21, 9, 1349-1371.
;
; @param S3D {in}{required}{type=3D array of floats}
; potential density (what ever the reference is) at t-point
;
; @returns
; adjusted potential density field
; -1 in case of error
;
; @examples
;
; IDL> s3d=fltarr(1,1,1) ++
; IDL> result=npc(s3d)
; IDL> print, result
;
; @uses
; <pro>common</pro>
;
; @todo
; realistic example
; quelles dimensions pour s3d ?
;
; @history
;
; - fplod 20091210T141743Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - fplod 20091208T102329Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * syntax of array
;
; - Additions : 01-06 (G. Madec) Idl version
;
; - original : 90-09 (G. Madec)
;
; @version
; $Id: npc.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION npc, s3d
;
  compile_opt idl2, strictarrsubs
;
@common
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=npc(s3d)'
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(s3d,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type cmd' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

;; Definition des variables
;;
;   Profils selon les niveaux du modele (suffixe _z)
;
   s_z = fltarr(jpk)     ; profil de la densite
;
; Tableau de sortie
;
   rhos = fltarr(jpi, jpj, jpk)
   rhos = s3d
;
; ====================================
;  Loop over the horizontal domain (2D)
; ====================================
;
   ncompt = 0

   FOR i = 0, jpi-1 DO BEGIN
      FOR j = 0, jpj-1 DO BEGIN

        ;  Indices des points T dans l ocean
         i_ocean = where(tmask[i,j,*] EQ 1)

         IF (i_ocean[0] NE -1) THEN BEGIN         ; on n'entre que si il y a des points ocean
            ;
            ; density profil
                       s_z[*]= s3d[i,j,*]
                       ;s_z[*] = rho[i,j,*]
            ;
            ; 1. Static instability pointer
            ; -----------------------------
            ;
              ds =(shift(s_z,-1)-s_z)[i_ocean[0:n_elements(i_ocean)-2]]
              ind_c = where(ds LT 0.)
            ;
            ; 2. Vertical mixing for each instable portion of the density profil
            ;
              IF ( ind_c[0] NE -1 ) THEN BEGIN
                  ncompt=ncompt+1
            ;      print, 'static instability at i,j=', i,j
            ;
            ; -->> the density profil is statically instable :
            ; ikbot: last ocean level (just above the bottom)
                  ikbot = n_elements(i_ocean)-1
            ; vertical iteration
                  jiter = 0
                  WHILE ( (ind_c[0] NE -1) AND (jiter LT jpk-1) ) DO BEGIN		
                    jiter = jiter+1							
                  ; ikup : the first static instability from the sea surface
                    ikup = ind_c[0]														
                  ; the density profil is instable below ikup
                  ; ikdown : bottom of the instable portion of the density profil
                  ; search of ikdown and vertical mixing from ikup to ikdown
                    ze3tot= e3t[ikup]							
                    zraua = s_z[ikup]							
                    jkdown = ikup+1
;
                    WHILE (jkdown LE ikbot AND zraua GT s_z[jkdown] ) DO BEGIN
                      ze3dwn = e3t[jkdown]
                      ze3tot = ze3tot+ze3dwn
                      zraua = ( zraua*(ze3tot-ze3dwn) + s_z[jkdown]*ze3dwn )/ze3tot
                      jkdown = jkdown + 1
                    ENDWHILE

                    FOR jkp = ikup,jkdown-1 DO BEGIN
                       s_z[jpk] = zraua
                    ENDFOR
                    ds =(shift(s_z, -1)-s_z)[i_ocean[0:n_elements(i_ocean)-2]]
                    ind_c = where(ds LT 0.)						
                  ENDWHILE
              ENDIF
            ; save the modifications
              rhos[i,j,*] = s_z[*]
;
; <<-- no more static instability on slab jj
;
         ENDIF
       ENDFOR
     ENDFOR
;
     print, ' number of static instability treated : ', ncompt
; sortie:
   return, rhos

END
