;+
;
; trace des graphes hovmoller avec axe generalise
;
; @categories
; Graphics, trace des graphes hovmoller: gt
;
; @param TAB {in}{required}{type=array or struct}
; le champ dont on veut faire le hovmoller champ
;	peut etre de 2 types:
;	1) un tableau qui peut etre:
;          * 3d ou 4d: la derniere composante etant le temps. dans ce
;            cas, le tableau va passer dans grossemoyenne pour etre
;            moyenné suivant et devenir un tableau 2d ou 1d.
;          * 2d: si tableau est deja 2d, il n''est pas modifie
;            (attention les terres doivent etre masquees a la valeure
;            valmask) et type doit qd meme etre specifie pour qu''on
;            sache de quel trace il sagit. Pour avoir une legende
;            correcte, respecifier la zone d''extraction via BOITE
;          * 1d: uniquement pour les traces de type 't'. Type doit qd
;            meme etre specifie pour qu''on sache de quel trace il
;            sagit. Pour avoir une legende correcte, respecifier la zone
;            d''extraction via BOITE
;       2) une structure repondant aux critères specifies par
;       <pro>litchamp</pro>.
;       cf. IDL> xhelp,'litchamp'. Le tableau contennu ds
;       la structure repondant aux criteres du cas 1) cf. ci-dessus
;
; @param GAXIS {in}
;
; @param GIVENMAX {in}{optional}
; valeur maximum que l''on veut prendre en compte dans le trace
; des contours. Par defaut on prend le max de tableau (sur les pts mer)
;
; @param GIVENMIN {in}{optional}
; valeur minimum que l''on veut prendre en compte dans le trace
; des contours. Par defaut on prend le min de tableau (sur les pts
; mer)
;
; @param DATMIN {in}{optional}
; c''est la borne inf de l''axe temporel. c''est un
;       longinteger de la forme yyyymmdd (ou bien yymmdd).
;
; @param DATMAX {in}{optional}
; c''est la borne max de l''axe temporel. c''est un
;       longinteger de la forme yyyymmdd (ou bien yymmdd)
;
; @keyword BOITE
; vecteur indiquant la zone geographique (3d) sur laquelle doit
; etre fait l''extraction du champ pour faire le hovmoeller.
; Si BOITE a :
; 1 element : l''extraction est faite sur [lon1, lon2, lat1, lat2, 0.,boite[0]]
; 2 elements: l''extraction est faite sur [lon1, lon2, lat1, lat2, boite[0],boite[1]]
; 4 elements: l''extraction est faite sur [Boite, prof1, prof2]
; 5 elements: l''extraction est faite sur [Boite[0:3], 0, Boite[4]]
; 6 elements: l''extraction est faite sur Boite
;
; Ou lon1, lon2,lat1,lat2,prof1,prof2 sont les variables globales
; definies lors du dernier domdef!
;
; @keyword CONTINTERVALLE
; lorsque CONTOUR est active, valeur d''un
; intervalle entre deux isolignes traces par un trait ds la couleur
; c_lab. Il peut ainsi etre different de celui specifie par INTERVALLE
; qui, cas ce cas, ne controle que les isolignes coloriees en
; couleurs. Si aucun min n''est specifie, on choisit un contour min qui va
; bien avec l''intervalle specifie!. Si ce mot cle n''est pas
; specifie, on trace 20 isolignes du min au max.
;
; @keyword CONTLABEL
; un entier n. lorsque CONTOUR est active, si n
; different de 0, choisit le type de label correspondant aux cas n
; pour les isolignes tracees par un trait. Pour specifier le type de
; label du contour en couleur, utiliser LABEL
;
; @keyword CONTMAX
; lorsque CONTOUR est active, valeur maximum que l''on
; veut prendre en compte dans le trace des isolignes traces par un
; trait de couleur c_lab. Par defaut on prend le max (sur les pts mer)
; du tableau passe ds le mot cle CONTOUR.
;
; @keyword CONTMIN
; lorsque CONTOUR est active, valeur minimum que l''on
; veut prendre en compte dans le trace des isolignes traces par un
; trait de couleur c_lab. Par defaut on prend le max (sur les pts mer)
; du tableau passe ds le mot cle CONTOUR.
;
; @keyword CONTNLEVEL
; lorsque CONTOUR est active, nombre de contours
; trace par un trait de couleur c_lab a dessiner (actif si
; CONTLABEL=0) par defaut =20.
;
; @keyword CONTOUR
; si on veut tracer les contours d''un champ different que celui
; que l''on dessin en couleur (par ex E-P en couleur et QSR en
; contours). Doit etre un champ reponadnt aux meme caracteristiques
; que l''argument numero 1 de pltt
;
; @keyword ENDPOINTS
; mot clef specifiant que l'on veut faire une coupe
;       verticale en diagonale. les coordonnees des extremites de
;       celle-ci sont alors definies les 4 elements du vecteur
;       ENDPOINTS: [x1,y1,x2,y2] qui sont les coordonnees
;
; @keyword EXCHANGE_XY
; permet d''intervertir les axes.
;
; @keyword FILTER
; applique une moyenne glissante de largeur FILTER
;
; @keyword INTERVALLE
; valeur d''un intervalle entre deux isolignes. Si
; aucun min n''est specifie, on choisit un contour min qui va bien
; avec l''intervalle specifie!. Si ce mot cle n''est pas specifie, on
; trace 20 isolignes du min au max. Rq: Qd CONTOUR est active
; INTERVALLE ne specifie que intervalle entre 2 isolignes coloriees en
; couleur. Pour specifier l''intervalle entre 2 isolignes traces par un trait
; de couleur c_lab utiliser CONTINTERVALLE.
;
; @keyword INV
; inverse le vecteur couleur utilise pour colorier le graphe
;             (sans toucher au noir, au blanc et a la palette utilisee)
;
; @keyword LABEL
; un entier n. si n different de 0, choisit le type de
; label correspondant aux cas n. cf <pro>label</pro>
; Rq: Qd CONTOUR est active, ne specifie le type de label que pour les
; isolignes coloriees en couleur. Pour celles tracees par un trait de
; couleur c_lab utiliser CONTLABEL.
;
; @keyword COL1d
; --OBSOLETE--
; numero de la couleur qd on fait un trace 1d
;       par defaut, 0. il faut mieux utiliser le mot cle COLOR utilise
;       par plot
;
; @keyword MAXIN
; permet de specifier la valeur maximum que l''on veut
;       prendre en compte dans le trace des contours a l''aide d''un
;       mot cle plutot que par l''argument max. Si l''argument et le
;       mot cle sont specifies en meme temps, c''est la valeur
;       specifiee par le mot cle qui est retenue.
;
; @keyword MININ
; permet de specifier la valeur minimum que l''on veut
;       prendre en compte dans le trace des contours a l''aide d''un
;       mot cle plutot que par l''argument min. Si l''argument et le
;       mot cle sont specifies en meme temps, c''est la valeur
;       specifiee par le mot cle qui est retenue.
;
; @keyword NLEVEL
; nombre de contours a dessiner. par defaut =20. actif si
; LABEL=0 ou n''est pas specifie.
;
; @keyword CONTNLEVEL
; nombre de contours a dessiner qd on utilise ajoutcontour
; active par le mot cle CONTOUR.(actif si CONTLABEL=0) par defaut =20.
;
; @keyword OV1D
; permet de surimprimer un courbe 1d a un precedent trace 1d.
;
; @keyword OVERPLOT
; pour faire un pltt par dessus un autre. Rq:
;       contrairemnet a l''utilisation de CONTOUR,
;       l''utilisation de ce mot clef ne modifie pas la legende ou/et
;       la barre de couleur. dans le cas d''un plot 1d, contrairement
;       a ov1d, on peut changer les axes et les ranges.
;
; @keyword REPEAT_C
; =n pour repeter une serie temporelle n fois
;
; @keyword STRICTFILL
; activer ce mot clef pour que le remplissage des
;       contours ce fasse precisement entre le min et le max specifie
;       en laissant en banc les valeurs inferieurs au min specifie et
;       superieurs au max specifie.
;
; @keyword STYLE
; style de tracer a adopter pour dessiner les isolignes par
; defaut style=0. cf <pro>style</pro>
;
; @keyword STY1D
; --OBSOLETE--
; numero du style utilise lors d''un trace
;       1d. Il faut mieux utiliser le mot cle LINESTYLE qui est celui
;       de plot. Attention ce mot cle est encore utile si on veut
;       faire des barres plutot qu''une courbe, mettre sty1d='bar'
;
; @keyword TREND_TYPE
; modify data by calling <propost_it>trends</propost_it>
;
; @keyword TYPEIN
; permet de specifier la valeur type de hovmoller que
;        l''on veut faire:
;             'xt','yt','zt','t'
;       a l''aide d''un mot cle plutot que par l''argument type Si
;       l''argument et le mot cle sont specifies en meme temps, c''est
;       la valeur specifiee par le mot cle qui est retenue.
;
; @keyword XT
;
; @keyword YT
;
; @keyword ZT
;
; @keyword TT
;
; @keyword _EXTRA
;
; @uses
; <pro>common</pro>
; <pro>juldate</pro>
;
; @todo
; return dans un PRO ?!
;
; validation de type TAB
;
; replace juldate call be a non-obsolete function
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - Sebastien Masson (smasson\@lodyc.jussieu.fr)
;			27/5/98
;
; - Jerome Vialard (adapting plt to hovmoller drawing)
;                       2/7/98
;
; - Sebastien Masson 14/8/98 (continents,barres)
;                       15/1/98
; adaptation pour les tableaux 3 et 4d pour que la moyenne soit faite
; dans pltt plutot que lors de la lecture.
;
; - Sebastien Masson 14/8/98
;
; - Eric Guilyardi 29/7/99
;
;   * FILTER, TREND_TYPE, REPEAT_C
;
; - Sebastien Masson 08/02/2000
;
;   * checkfield and usetri keyword.
;
; @version
; $Id: plttg.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
PRO plttg,tab,gaxis,givenmin,givenmax,datmin,datmax $
    , BOITE=boite $
    , CONTOUR=contour $
    , ENDPOINTS=endpoints $
    , INTERVALLE=intervalle $
    , INV=inv $
    , CONTINTERVALLE=contintervalle $
    , LABEL=label $
    , CONTLABEL=contlabel $
    , STYLE=style  $
    , CONTMAX=contmax $
    , CONTMIN=contmin $
    , NLEVEL=nlevel $
    , CONTNLEVEL=contnlevel $
    , COL1D=col1d  $
    , STY1D=sty1d $
    , MININ=minin $
    , MAXIN=maxin $
    , OV1D=ov1d $
    , FILTER=filter $
    , TREND_TYPE=trend_type $
    , REPEAT_C=repeat_c $
    , TYPEIN=typein $
    , XT=xt $
    , YT=yt $
    , ZT=zt $
    , TT=tt $
    , STRICTFILL=strictfill $
    , OVERPLOT=overplot $
    , EXCHANGE_XY=exchange_xy $
    , _EXTRA=extra
;
  compile_opt idl2, strictarrsubs
;
@common

 usage='plttg,tab,gaxis,givenmin,givenmax,datmin,datmax' $
    + ', BOITE=boite' $
    + ', CONTOUR=contour' $
    + ', ENDPOINTS=endpoints' $
    + ', INTERVALLE=intervalle' $
    + ', INV=inv' $
    + ', CONTINTERVALLE=contintervalle' $
    + ', LABEL=label' $
    + ', CONTLABEL=contlabel' $
    + ', STYLE=style ' $
    + ', CONTMAX=contmax' $
    + ', CONTMIN=contmin' $
    + ', NLEVEL=nlevel' $
    + ', CONTNLEVEL=contnlevel' $
    + ', COL1D=col1d ' $
    + ', STY1D=sty1d' $
    + ', MININ=minin' $
    + ', MAXIN=maxin' $
    + ', OV1D=ov1d' $
    + ', FILTER=filter' $
    + ', TREND_TYPE=trend_type' $
    + ', REPEAT_C=repeat_c' $
    + ', TYPEIN=typein' $
    + ', XT=xt' $
    + ', YT=yt' $
    + ', ZT=zt' $
    + ', TT=tt' $
    + ', STRICTFILL=strictfill' $
    + ', OVERPLOT=overplot' $
    + ', EXCHANGE_XY=exchange_xy' $
    + ', _EXTRA=extra'
;
 nparam = N_PARAMS()
 IF (nparam LT 6) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

   giventype = 'yt'
;------------------------------------------------------------
   tempsun = systime(1)         ; pour key_performance
;--------------------------------------------------------------
; I2) reinitialisation. !p.,!x.,!y.
; Rq: on ne reinitialise pas qd on rapelle plt en boucle pour utiliser contour
;--------------------------------------------------------------
   IF n_elements(contour) ne 4 AND NOT keyword_set(overplot) $
    AND NOT keyword_set(ov1d) THEN BEGIN
    reinitplt
   ENDIF
;--------------------------------------------------------------
; I1) lecture du champ
;--------------------------------------------------------------
   IF keyword_set(boite) THEN BEGIN
    oldboite = [lon1, lon2, lat1, lat2, prof1, prof2]
   ENDIF
   IF n_elements(giventype) NE 0 THEN BEGIN
    type = giventype
   ENDIF
   IF n_elements(givenmin) NE 0 THEN BEGIN
    min = givenmin
   ENDIF
   IF n_elements(givenmax) NE 0 THEN BEGIN
    max = givenmax
   ENDIF
   IF n_elements(minin) NE 0 THEN BEGIN
    min = minin
   ENDIF
   IF n_elements(maxin) NE 0 THEN BEGIN
    max = maxin
   ENDIF
   IF keyword_set(typein) THEN BEGIN
      IF size(type, /type) NE 7 AND size(type, /type) NE 0 THEN BEGIN
         IF n_elements(min) NE 0 THEN BEGIN
          max = min
         ENDIF
         min = type
      ENDIF
      type = typein
   ENDIF

   z2d = tab
   mask = 1

;---------------------------------------------------------------
; selection du type de graphique.
;---------------------------------------------------------------
;---------------------------------------------------------------
; repetition de la serie temporelle
;---------------------------------------------------------------

   IF NOT keyword_set(repeat_c) THEN BEGIN
    repeat_c = 1
   ENDIF

   temps = time[0:jpt-1]
   IF repeat_c GT 1 THEN BEGIN
      taille=size(z2d)
      CASE taille[0] OF
         1: z2d = reform(z2d#replicate(1, repeat_c), taille[1]*repeat_c)
         2: BEGIN
            z2d = z2d[*]#replicate(1, repeat_c)
            z2d = reform(z2d, taille[1], taille[2]*repeat_c, /over)
         END
         ELSE:
      ENDCASE
      temps = [temps, (lindgen(jpt*(REPEAT_c-1))+1)*(temps[1]-temps[0])+temps[jpt-1]]
   ENDIF

;----------------------------------------------------------------------------
;   determination du mi:min et du ma:max de z2d ainsi que de max: max et
;    min: min pour le dessin.
;-----------------------------------------------------------------------------
; faudra-t-il faire un autoscale ???
   autoscale = testvar(var = min) EQ testvar(var = max) AND NOT keyword_set(intervalle)
   determineminmax, z2d, mask, mi, ma, MININ = min, MAXIN = max, nan = nan, INTERVALLE = intervalle
   IF z2d[0] EQ -1 THEN BEGIN
    return
   ENDIF
; on fait un autoscale si besoin
   IF autoscale THEN BEGIN
    autoscale, min, max, intervalle
   ENDIF
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
   IF n_elements(contour) ne 4  AND NOT keyword_set(overplot) THEN BEGIN
    placedessin, 'pltt',posfenetre, posbar, contour = contour, direc = direc, type = type, endpoints = endpoints, _EXTRA=extra
   ENDIF
;--------------------------------------------------------------
;--------------------------------------------------------------
; 2eme partie: dessin
;--------------------------------------------------------------
;-----------------------------------------------------------
;   definition des axes
;----------------------------------------------------------
;-----------------------------------------------------------------------------
; definition des vecteurs abscisse et ordonee
; la triangulation est definie pour que le trace soit effectue du bas
; a gauche vers le haut a droite. il faut donc la matrice e contourer
; se presente de cette maniere, d''ou certains transpose et reverse
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
; definition des bornes de l''axe temporel
;-----------------------------------------------------------------------------
   CASE N_PARAMS() OF
      5 : BEGIN
         tempsmin = juldate(datmin, _EXTRA=extra)
         tempsmax = temps[(jpt*repeat_c)-1]
      END
      6 : BEGIN
         tempsmin = juldate(datmin, _EXTRA=extra)
         tempsmax = juldate(datmax, _EXTRA=extra)
      END
      ELSE: BEGIN
         tempsmin = temps[0]
         tempsmax = temps[(jpt*repeat_c)-1]
      END
   ENDCASE
;-----------------------------------------------------------------------------
; on shift l''axe du temps pour des questions de precision sur les
; dates du calendier julien en long qui sont passes en float ds les axes
   yy = gaxis
   xx = temps-tempsmin
;--------------------------------------------------------------
   IF NOT keyword_set(overplot) THEN BEGIN
    axe, type, tempsmin, tempsmax, plttg = 1, _EXTRA=extra
   ENDIF ELSE BEGIN
      !x.range=!x.range-tempsmin
      !x.tickv=!x.tickv-tempsmin
   ENDELSE
;------------------------------------------------------------
;------------------------------------------------------------
; dessin
;------------------------------------------------------------
; 2d
;------------------------------------------------------------
;--------------------------------------------------------------
; choix des labels
;-----------------------------------------------------------
   IF keyword_set(intervalle) AND NOT keyword_set(label) THEN BEGIN
    label=1
   ENDIF
   IF keyword_set(label) eq 0 THEN BEGIN
    cas=0
   ENDIF ELSE BEGIN
    cas=label
   ENDELSE
   label,cas,min,max,ncontour,level_z2d,NLEVEL=nlevel,INTERVALLE=intervalle, strictfill = strictfill
;--------------------------------------------------------------
; choix de style
;-----------------------------------------------------------
   IF not keyword_set(style) THEN BEGIN
    style=0
   ENDIF
   style,style,level_z2d,linestyle,thick
   IF keyword_set(inv) THEN BEGIN
    couleur=reverse(couleur)
   ENDIF
;----------------------------------------------------------------------
   nby = n_elements(yy)
   nbx = n_elements(xx)
   xx = xx#replicate(1, nby)    ; on passe les axes en tableaux 2d
   yy = replicate(1, nbx)#yy

   triangulation = -1
;----------------------------------------------------------------------
   pltbase,z2d, xx, yy, mask,xx, yy, level_z2d, couleur, contour = contour,/noerase $
    , c_linestyle=linestyle,c_labels=1-(indgen(n_elements(level_z2d)) MOD 2) $
    , trichamp = triangulation, trimsk = triangulation, overplot = overplot $
    , c_thick=thick, performance = key_performance, usetri = keyword_set(nan) $
    , coinmontemask=coinmontemask, coindescendmask=coindescendmask, _EXTRA=extra
;------------------------------------------------------------
   IF n_elements(contour) eq 4 THEN BEGIN ; c''est la 2eme fois que je passe ds pltt
      contour = {mietma:[mi, ma], unit:varunit, inter:intervalle} ; je renvoie le min, le max et l''unite
      return
   ENDIF
;------------------------------------------------------------
   IF keyword_set(contour) THEN BEGIN
      pourlegende = [1, 1, 1, 1]
      oldattributs = saveatt()
      oldcouleur = couleur
      plttg,contour,gaxis, contmin,contmax,CONTOUR=pourlegende, /noerase, USETRI = usetri $
       ,INTERVALLE=contintervalle,LABEL=contlabel,STYLE=style, ENDPOINTS = endpoints $
       ,NLEVEL=contnlevel,YSURX=ysurx, BOITE = boite, STRICTFILL = strictfill, _EXTRA=extra
      couleur = oldcouleur
      restoreatt, oldattributs
   ENDIF
;----------------------------------------------------------------------
;----------------------------------------------------------------------
   IF keyword_set(overplot) THEN BEGIN
    GOTO, fini
   ENDIF
;------------------------------------------------------------
; legendes + affichage de celles-ci
;------------------------------------------------------------
   legende,mi,ma,type, CONTOUR = pourlegende, DIREC = direc, INTERVALLE = intervalle $
    , plttg = 1, _EXTRA=extra
   plot,[0],[0],/noerase,/nodata,color=c_cote, xstyle = 1, ystyle = 1

;------------------------------------------------------------
; barre de couleur
;------------------------------------------------------------
   couleur = couleur[0:ncontour-1-keyword_set(strictfill)]
   barrecouleur, position=posbar,divisions=(ncontour-keyword_set(strictfill))/2,min=min $
    , max=max,discret=couleur,_EXTRA=extra
;------------------------------------------------------------
fini:
;------------------------------------------------------------
; on remet l''axe du temps en jours julien IDL et non pas en jours
; juliens comptes a partir tempsmin
;------------------------------------------------------------
   !x.range=!x.range+tempsmin
   !x.tickv=!x.tickv+tempsmin
; on fait un faux plot pour que ces valeurs soient prises en
; consideration
   plot,[0], [0], /noerase,xstyle=5, ystyle = 5, title = '', subtitle = '', ytitle = '', xtitle = ''
;------------------------------------------------------------
;------------------------------------------------------------
   terminedessin, _EXTRA=extra
   IF keyword_set(oldboite) THEN BEGIN
    domdef, oldboite,GRILLE=[vargrid, 'T']
   ENDIF
;------------------------------------------------------------
   IF n_elements(key_performance) NE 0 THEN BEGIN
    IF key_performance EQ 1 THEN BEGIN
     print, 'temps pltt', systime(1)-tempsun
    ENDIF
   ENDIF
end
