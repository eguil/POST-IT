;+
;
; surimprimme des vecteur sur un champ trace par <pro>pltz</pro>
;
; @categories
; Graphics
;
; @param VECTEUR {in}{required}{type=structure}
; une structure a 2 elements contenant les 2 matrices U
;	et V des valeurs de la composante horizontale et verticale du
;	champ de vecteurs a tracer.
;
;       par ex:
;       vecteur={matriceu:lec('unsurface'),matricev:lec('vnsurface')}
;       rq:le nom des elements de vecteur n''a aucune importance.
;       vecteur={u:lec('unsurface'),v:lec('vnsurface')} convient aussi
;
; @keyword UNVECTSUR
; un scalaire n ou un tableau a 2 elements [n1,n2].
;
; Dans le premier cas, on tracera un vecteur sur n suivant les
; x et les y.
; Dans le second cas on tracera un vecteur sur n1 suivant x
; et un vecteur sur n2 suivant y
;
; Rq: pour tracer tous les vecteurs suivant y et 1 sur 2 suivant
; x mettre unvectsur=[2,1]
;
; @keyword VECTMIN
; norme minimun des vecteurs a tracer
;
; @keyword VECTMAX
; norme minimun des vecteurs a tracer
;
; @keyword VECTMIN
;
; @keyword VECTMAX
;
; @keyword _EXTRA
;
; @keyword TYPE
;
; @examples
;
; IDL> vecteur={matriceu:lec('unsurface'),matricev:lec('vnsurface')}
; IDL> ajoutvectz, vecteur, $
;    , UNVECTSUR=unvectsur $
;    , VECTMIN=vectmin $
;    , VECTMAX=vectmax $
;    , _EXTRA=extra $
;    , TYPE = type $
;    , BOITE = boite
;
; @uses
; <pro>common<pro>
; <pro>litchamp<pro>
;
; @todo
;
; fix pb in param vecteur description with idldoc. It seems that {...}
; have side effects
;
; in example current pb with lec('vnsurface')
;
; IDL> vecteur={matriceu:lec('unsurface'),matricev:lec('vnsurface')}
; % L.145 /usr/lodyc/incas/fplod/SAXO_DIR_ws/SRC/Obsolete/lec.pro rev. 378:
; % le nom du champ doit commencer par VO ou SO
;
; @history
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;   * remove vectlegend parameter (unused)
;
; - Eric Guilyardi (ericg\@lodyc.jussieu.fr) Sebastien Masson (smassom\@lodyc.jussieu.fr)
;
;   * 26/9/00 (on board R/V Marion Dufresne)
;
; @version
; $Id: ajoutvectz.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO ajoutvectz, vecteur $
    , UNVECTSUR=unvectsur $
    , VECTMIN=vectmin $
    , VECTMAX=vectmax $
    , _EXTRA=extra $
    , TYPE = type $
    , BOITE = boite
;
  compile_opt idl2, strictarrsubs
;
@common
;
  tempsun = systime(1)         ; pour key_performance
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='ajoutvectz, vecteur ' $
    + ', UNVECTSUR=unvectsur' $
    + ', VECTMIN=vectmin' $
    + ', VECTMAX=vectmax' $
    + ', _EXTRA=extra' $
    + ', TYPE = type' $
    + ', BOITE = boite'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(vecteur,/type)
 IF (arg_type NE 8) THEN BEGIN
   ras = report(['Incorrect arg type vecteur' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF


;----------------------------------------------------------------------------
;
   u = litchamp(vecteur.(0))
   grilleu = litchamp(vecteur.(0), /grid)
   vargrid = grilleu
   boitesave = boite
   u = checkfield(u, 'pltz', TYPE = type, BOITE = boite, /NOQUESTION, /VECTEUR)
   boite = boitesave

   w = litchamp(vecteur.(1))
   grillew = litchamp(vecteur.(1), /grid)
   vargrid = grillew
   w = checkfield(w, 'pltz', TYPE = type, BOITE = boite, /NOQUESTION, /VECTEUR)

;------------------------------------------------------------
; on recupere les eventuelles info sur les champs
;------------------------------------------------------------
   CASE type OF
      'yz': IF grilleu EQ '' THEN BEGIN
             grilleu = 'V'
            ENDIF
      'xz': IF grilleu EQ '' THEN BEGIN
             grilleu = 'U'
            ENDIF
      ELSE: IF grilleu EQ '' THEN BEGIN
             grilleu = 'V'
            ENDIF
   ENDCASE
   IF grillew EQ '' THEN BEGIN
    grillew = 'W'
   ENDIF

   IF grilleu EQ 'T' AND grillew EQ 'T' THEN BEGIN
    interpolle = 0
   ENDIF ELSE BEGIN
    interpolle = 1
   ENDELSE

;------------------------------------------------------------
; on trouve les points que u et v ont en communs
;------------------------------------------------------------
   if interpolle THEN BEGIN
      CASE type OF
         'yz': indicehx = (lindgen(jpj))[firstyv:firstyv+nyv-1]
         'xz': indicehx = (lindgen(jpi))[firstxu:firstxu+nxu-1]
      ENDCASE
      CASE type OF
         'yz': indicehw = (lindgen(jpj))[firstyt:firstyt+nyt-1]
         'xz': indicehw = (lindgen(jpi))[firstxt:firstxt+nxt-1]
      ENDCASE

      indicex = inter(indicehx, indicehw)

      indicezh = (lindgen(jpk))[firstzt:firstzt+nzt-1]
      indicezw = (lindgen(jpk))[firstzw:firstzw+nzw-1]

      indicez = inter(indicezh, indicezw)

      nx = n_elements(indicex)
      nz = n_elements(indicez)

      CASE type OF
      'yz': BEGIN & nxh = nyv & firsth = firstyv & END
      'xz': BEGIN & nxh = nxu & firsth = firstxu & END
      ENDCASE

;------------------------------------------------------------
; extraction de u et v sur le domaine qui convient
;------------------------------------------------------------
      if nxh NE nx THEN BEGIN
       if indicex[0] EQ firsth THEN BEGIN
        u = u[0:nx-1, *]
       ENDIF ELSE BEGIN
        u = u[1: nx, *]
       ENDELSE
      ENDIF
      IF nxt NE nx THEN BEGIN
       if indicex[0] EQ firstxt THEN BEGIN
        w = w[0:nx-1, *]
       ENDIF ELSE BEGIN
        w = w[1: nx, *]
       ENDELSE
      ENDIF
      IF nzt NE nz THEN BEGIN
       if indicez[0] EQ firstzt THEN BEGIN
        u = u[*, 0:nz-1]
       ENDIF ELSE BEGIN
        u = u[*, 1: nz]
       ENDELSE
      ENDIF
      IF nzw NE nz THEN BEGIN
       if indicez[0] EQ firstzw THEN BEGIN
        w = w[*, 0:nz-1]
       ENDIF ELSE BEGIN
        w = w[*, 1: nz]
       ENDELSE
      ENDIF
;------------------------------------------------------------------
; on reform u et w pour s'assurer qu'aucune dimension n'a ete ecrasee
;------------------------------------------------------------------
      if nz EQ 1 THEN BEGIN
         u = reform(u, nx, nz)
         w = reform(w, nx, nz)
      endif
;------------------------------------------------------------------
; construction de u et w aux pts T
;-----------------------------------------------------------

      terre = where(u GE valmask/10.)
      IF terre[0] NE -1 THEN BEGIN
       u[terre] = !VALUES.F_NAN
      ENDIF
      terre = where(w GE valmask/10.)
      IF terre[0] NE -1 THEN BEGIN
       w[terre] = !VALUES.F_NAN
      ENDIF

      a=u[0,*]
      u=(u+shift(u,1,0))/2.
      u[0,*]=a

      a=w[*,0]
      w=(w+shift(w,0,1))/2.
      w[*,0] = a

      vargrid='T'

   ENDIF
;-----------------------------------------------------------
; tracer qu'un vecteur sur
;-----------------------------------------------------------
   if keyword_set(unvectsur) THEN BEGIN
; indx est un vecteur contenant les numero des colonnes a selectionner
; indz est un vecteur contenant les numero des lignes a selectionner
      if n_elements(unvectsur) EQ 1 THEN BEGIN
         indx = where((lindgen(nx) MOD unvectsur[0]) eq 0)
         indz = where((lindgen(nz) MOD unvectsur[0]) eq 0)
      ENDIF ELSE BEGIN
         indx = where((lindgen(nx) MOD unvectsur[0]) eq 0)
         indz = where((lindgen(nz) MOD unvectsur[1]) eq 0)
      ENDELSE
; a partir de indx et indz on va construire un tableau d''indices 2d
; qui donnera les indices des points intersections des colonnes
; specifiee par indx
      indicereduit = indx#replicate(1,n_elements(indz))+nx*replicate(1,n_elements(indx))#indz
; on reduit les tableaux qui vont etre passes a vecteur.
      u = u[indicereduit]
      w = w[indicereduit]
;
   endif
;-----------------------------------------------------------
; trace des vecteurs
;----------------------------------------------------------

   CASE type OF
      'yz': xgrid = gphit[(firstxt+lastxt)/2, indicex[0]:indicex[nx-1]]
      'xz': xgrid = glamt[indicex[0]:indicex[nx-1], (firstyt+lastyt)/2]
   ENDCASE

   x0 = xgrid[*]#replicate(1, nz)
   y0 = replicate(1, nx)#gdept[indicez[0]:indicez[nz-1]]


;   print, !x.range, !y.range
;   print, '  Min/Max Hvect', min(u(where (u LE valmask/10.))), max(u(where (u LE valmask/10.)))
;   print, '  Min/Max Zvect', min(w(where (w LE valmask/10.))), max(w(where (w LE valmask/10.)))

   norme = sqrt(max(u(where (u LE valmask/10.)))^2+max(w(where (w LE valmask/10.)))^2)

;   modif du rapport d'aspect

   deltax = (!p.position[2]-!p.position[0])
   deltaz = (!p.position[3]-!p.position[1])
   rph = petitfeuille/grandfeuille*(  key_portrait)+1.*(1-key_portrait)
   rpz = grandfeuille/petitfeuille*(1-key_portrait)+key_portrait

   CASE !d.name OF
      'PS': scale = grandfeuille*deltax/(ABS(!x.range[1]-!x.range[0]))*.4
      ELSE: scale = grandfeuille*deltax/(ABS(!x.range[1]-!x.range[0]))*.4
   ENDCASE


;   print, ABS(!x.range[1]-!x.range[0]), ABS(!y.range[1]-!y.range[0])
;   print, deltax, deltaz
;   print, scale

   x1 = x0+u*rph*ABS(!x.range[1]-!x.range[0])/deltax*scale
   y1 = y0-w*rpz*ABS(!y.range[1]-!y.range[0])/deltaz*scale

   arrow, x0, y0, x1, y1, /data, thick = 1.5, hthick = 1.5, hsize = !D.X_SIZE / 400
;hsize = !D.X_SIZE / 500 or -0.4


sortie:
   if keyword_set(key_performance) NE 0 THEN BEGIN
    print, 'temps ajoutvectz', systime(1)-tempsun
   ENDIF

END
