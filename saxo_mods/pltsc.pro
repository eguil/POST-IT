;+
;
; scatter plot (inspired from plt1d)
;
; @param TAB1 {in}{required}{type=array of floats}
;
; @param TAB2 {in}{required}{type=array of floats}
;
; @param MIN1 {inout}{type=float}
;
; @param MAX1 {inout}{type=float}
;
; @param MIN2 {inout}{type=float}
;
; @param MAX2 {inout}{type=float}
;
; @param VARNAME2 {in}{required}{type=string}
;
; @keyword BOITE
;
; @keyword COL1D
;
; @keyword STY1D
;
; @keyword OV1D
;
; @keyword THICKN
;
; @keyword FRACTION
;
; @keyword _EXTRA
; Used to pass keywords
;
; @uses
; <pro>common</pro>
; <pro>domdef</pro>
;
; @history
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: pltsc.pro 216 2010-04-01 13:17:02Z ericg $
;
;-
PRO pltsc,tab1,tab2, min1, max1, min2, max2, varname2 $
    , BOITE=boite $
    , COL1D=col1d $
    , STY1D=sty1d $
    , OV1D=ov1d $
    , THICKN=thickn $
    , FRACTION=fraction $
    , _EXTRA=extra
;
  compile_opt idl2, strictarrsubs
;
@common

usage='pltsc, tab1, tab2, min1, max1, min2, max2, varname2 ' $
    + ', BOITE=boite ' $
    + ', COL1D=col1d ' $
    + ', STY1D=sty1d ' $
    + ', OV1D=ov1d ' $
    + ', THICKN=thickn ' $
    + ', FRACTION=fraction ' $
    + ', _EXTRA=extra'

 nparam = N_PARAMS()
 IF (nparam LT 7) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(tab1,/type)
 IF ((arg_type NE 3) AND (arg_type NE 4) AND (arg_type NE 5)) THEN BEGIN
   ras = report(['Incorrect arg type tab1' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(tab2,/type)
 IF ((arg_type NE 3) AND (arg_type NE 4) AND (arg_type NE 5)) THEN BEGIN
   ras = report(['Incorrect arg type tab2' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

 arg_type = size(varname2,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type varname2' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

c_cote = 0

; Rq: on ne reinitialise pas qd on rapelle pltsc
   IF NOT keyword_set(ov1d) THEN BEGIN
    reinitplt
   ENDIF
   IF NOT keyword_set(thickn) THEN BEGIN
    thickn = 2
   ENDIF
   IF NOT keyword_set(fraction) THEN BEGIN
    fraction = 1.
   ENDIF
   ;; reduce data xyzt domain

   oldboite = [lon1, lon2, lat1, lat2, prof1, prof2]
   IF keyword_set(boite) THEN BEGIN
      IF n_elements(integration3d) EQ 0 THEN BEGIN
       integration3d = n_elements(boite) NE 4
      ENDIF
      CASE 1 Of
         N_Elements(Boite) Eq 1:bte=[lon1, lon2, lat1, lat2, 0.,boite[0]]
         N_Elements(Boite) Eq 2:bte=[lon1, lon2, lat1, lat2, boite[0],boite[1]]
         N_Elements(Boite) Eq 4:bte=[Boite, prof1, prof2]
         N_Elements(Boite) Eq 5:bte=[Boite[0:3], 0, Boite[4]]
         N_Elements(Boite) Eq 6:bte=Boite
         ELSE: BEGIN
            ras = report('Mauvaise Definition de Boite')
            stop
         END
      ENDCASE
   ENDIF ELSE BEGIN
      bte=[lon1, lon2, lat1, lat2, prof1, prof2]
   ENDELSE

   domdef, bte,GRILLE=vargrid

; extract indexes to plot

   indexm = where(tab1 LE valmask/10.)
   tab1 = tab1[indexm]
   tab2 = tab2[indexm]

   npts = (size(indexm))[1]

; deal with min and max of plot
   IF finite(min1) EQ 0 THEN BEGIN
    min1 = min(tab1)
   ENDIF
   IF finite(max1) EQ 0 THEN BEGIN
    max1 = max(tab1)
   ENDIF
   IF finite(min2) EQ 0 THEN BEGIN
    min2 = min(tab2)
   ENDIF
   IF finite(max2) EQ 0 THEN BEGIN
    max2 = max(tab2)
   ENDIF


; init plot if not overlay

   IF NOT keyword_set(ov1d) THEN BEGIN
    placedessin, 'yfx', posfenetre, posbar, $
    contour = contour, _EXTRA=EXTRA
   ENDIF
   yy = tab1
   xx = tab2

; axis range

   fraction = 0.

   !x.range = [min2-fraction*abs(max2-min2)/5.,max2+fraction*abs(max2-min2)/5.]
   !y.range = [min1-fraction*abs(max1-min1)/5.,max1+fraction*abs(max1-min1)/5.]

   IF (NOT keyword_set(sty1d)) THEN BEGIN
    sty1d = 0
   ENDIF
   IF (NOT keyword_set(col1d)) THEN BEGIN
    col1d = 0
   ENDIF
   IF NOT keyword_set(ov1d) THEN BEGIN
      legende, min1, max1, 'yfx', VARNAME2 = varname2, NPTS = npts, _EXTRA=extra
;
      plot,xx,yy, background = 255, psym = sty1d+1, color=col1d, thick=thickn $
       , title = '', subtitle = '',_EXTRA=extra

      IF n_elements(ex) NE 0 THEN BEGIN
; pour avoir un cadre de la couleur c_cote et trace une ligne a y=0
         IF (where(tag_names(ex) EQ 'COLOR'))[0] NE -1 THEN BEGIN
          ex.COLOR = c_cote
         ENDIF
         IF (where(tag_names(ex) EQ 'LINESTYLE'))[0] NE -1 THEN BEGIN
          ex.LINESTYLE= 0
         ENDIF
      ENDIF
      plot, !x.range, [0, 0],/noerase,color=c_cote, xstyle = 1, ystyle = 1, _EXTRA=extra
; trace une ligne a x=0
      plot, [0, 0],!y.range ,/noerase,color=c_cote, title = '', subtitle = '', _EXTRA=extra
   ENDIF ELSE BEGIN
      oplot,xx,yy,color=col1d,psym=sty1d+1,thick=thickn, _EXTRA=extra
   ENDELSE
;------------------------------------------------------------
;------------------------------------------------------------
; 3eme partie: impression eventuelle
;------------------------------------------------------------
;------------------------------------------------------------
fini:
   domdef, oldboite,GRILLE=vargrid
   terminedessin, _EXTRA=extra
;------------------------------------------------------------
   IF n_elements(key_performance) NE 0 THEN BEGIN
    IF key_performance EQ 1 THEN BEGIN
     print, 'temps plt1d', systime(1)-tempsun
    ENDIF
   ENDIF
END
