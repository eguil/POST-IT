;+
;
; Calcul de la Fonction de Courant a travers les lignes J
;
; @param V {in}{required}{type= array of ++}
;
; @param MSF {out} {type=3D array of floats}
;
; @param MSFMSK {out} {type=3D array of floats}
;
; @keyword MSK
;
; @examples
;
; IDL> v=1.
; IDL> msf_mult, v, msf, msfmsk
; IDL> print,msf,msfmsk
;
; @uses
; <pro>common</pro>
;
; @todo
;
; ne plante pas si v est un scalaire v=1.
;
; realistic example
;
; @history
;
; - fplod 20091210T110019Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - 12/02/99 M Imbard creation
;
; - 12/11/98 G. Roullet
;
; - 18/11/98 G. Roullet
;
; @version
; $Id: msf_mult.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO msf_mult, v, msf, msfmsk $
    , MSK=msk
;
  compile_opt idl2, strictarrsubs
;
@common
;
;COMMON moy, zonmsk, mermsk, jpl
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='msf_mult, v, msf, msfmsk, MSK=msk'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(v,/type)
 IF (arg_type NE 4) THEN BEGIN
   ras = report(['Incorrect arg type v' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

IF n_elements(jpl) EQ 0 THEN BEGIN
 jpl = jpj
ENDIF
;
;  jpl bandes de latitudes
;
fm = fltarr(jpl, jpk, jpt)
msf = fltarr(jpl, jpk, jpt)
msfmsk = fltarr(jpl, jpk, jpt)
zv = v
z = zv
ze1v = zv
ze3v = zv
vert = replicate(1, jpk, jpt)
;
;  Masque de u pour un calcul de msf sur le sous domaine msk(jpi,jpj)
;
IF n_elements(msk) NE 0 THEN BEGIN
; msk est souvent vide
; donc cette procedure est ineffective.
      zvmask = boundperio( (msk +shift(msk, 0, -1) ) < 1 )
      zvmask = zvmask[*]#vert
      zv = zv*zvmask
ENDIF
;
;  Ecriture "tricky" optimisee... si vous avez compris comment
;  on calculait un flux a partir de champ 2D, vous avez fait le
;  plus dur : ici on generalise a 3D d''ou l''utilisation de #vert
;
;  calcul du flux
;
FOR i = 0, jpi-1 DO BEGIN for j= 0, jpj-1 do begin ze1v[i,j,*,*] = replicate(e1v[i,j],jpk*jpt) & endfor &endfor
FOR k = 0, jpk-1 DO BEGIN ze3v[*,*,k,*]=replicate(e3t[k],jpi*jpj*jpt) & endfor
;
z= -v*ze1v*ze3v
;la multiplication ne fait plus disparaitre la dimension temporelle
;maintenant que ze1v et ze3v ont ete modifie depuis msf_simple.pro
; integration zonale du flux !
;
fm = total(z, 1)
;
;  calcul de la msf en integrant depuis le fond
;
FOR k = jpk-2, 0, -1 DO begin msf[*, k,*] = msf[*, k+1,*]-fm[*, k,*] & endfor
;
;  msfmsk est le masque associe a msf (utilise pour les graphiques)
;
msfmsk = fm NE 0.
;
END
