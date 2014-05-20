;+
;
; @param IMAGE
;
; @keyword MIN
;
; @keyword MAX
;
; @keyword GAMMA
;
; @keyword MSK
;
; @keyword ERASE
;
; @keyword IBASE
;
; @keyword JBASE
;
; @keyword INTERP
;
; @keyword SCALE_X
;
; @keyword SCALE_Y
;
; @keyword PS
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: tvnplot.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO tvnplot, image $
    , MIN=min $
    , MAX=max $
    , GAMMA=gamma $
    , MSK=msk $
    , ERASE=erase $
    , IBASE=ibase $
    , JBASE=jbase $
    , INTERP=interp $
    , SCALE_X=scale_x $
    , SCALE_Y=scale_y $
    , PS=ps $
    , _EXTRA=extra
;
  compile_opt idl2, strictarrsubs
;
 usage='tvnplot, image' $
    + ', MIN=min' $
    + ', MAX=max' $
    + ', GAMMA=gamma' $
    + ', MSK=msk' $
    + ', ERASE=erase' $
    + ', IBASE=ibase' $
    + ', JBASE=jbase' $
    + ', INTERP=interp' $
    + ', SCALE_X=scale_x' $
    + ', SCALE_Y=scale_y' $
    + ', PS=ps' $
    + ', _EXTRA=extra'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
;
   IF keyword_set(min)     EQ 0 THEN BEGIN
    min = min(image)
   ENDIF
   IF keyword_set(max)     EQ 0 THEN BEGIN
    max = max(image)
   ENDIF
   IF keyword_set(scale_x) EQ 0 THEN BEGIN
    scale_x = 1.
   ENDIF
   IF keyword_set(scale_y) EQ 0 THEN BEGIN
    scale_y = 1.
   ENDIF
   IF keyword_set(ibase)   EQ 0 THEN BEGIN
    ibase = 0.
   ENDIF
   IF keyword_set(jbase)   EQ 0 THEN BEGIN
    jbase = 0.
   ENDIF
   sample = 0
   IF keyword_set(interp)  EQ 0 THEN BEGIN
    sample = 1
   ENDIF

   x_env_save = !x
   y_env_save = !y
   p_env_save = !p

   !p.background = 255
   !p.color = 0
   !p.charsize=1

   image = reform(image)
   IF (size(image))[0] NE 2 THEN BEGIN
      print, ' tvnplot needs a 2D array'
      stop
   ENDIF

   ni = (size(image))[1]
   nj = (size(image))[2]

   IF keyword_set(gamma) EQ 0 THEN BEGIN
      gamma = 600/max([ni, nj])
   ENDIF
   IF n_elements(ps) EQ 1 THEN BEGIN
      gamma = 5
   ENDIF

   image2 = ((min > image < max) -min)*(!d.n_colors-2)/(max-min)+1
   IF n_elements(msk) NE 0 THEN BEGIN
      image2[where(msk EQ 0)] = 0
   ENDIF
   image2 = rebin(image2, gamma*ni, gamma*nj, sample = sample)

   IF n_elements(ps) EQ 0 THEN BEGIN
      xsize = ni*gamma+200
      ysize = nj*gamma+200
      IF (!d.x_size LT xsize) OR (!d.y_size LT ysize) THEN BEGIN
       window, xsize = ni*gamma+200, ysize = nj*gamma+200
      ENDIF

      xcmperpix = 1.
      ycmperpix = 1.
   ENDIF ELSE BEGIN
      set_plot, 'ps'
      device, xsize = 19, ysize = 20, xoffset = 1, yoffset = 1, bits = 8, /color
      device, xsize = 25, ysize = 19, xoffset = 1, yoffset = 28, bits = 8, /color, /land

      xcmperpix = !d.X_PX_CM/50.
      ycmperpix = !d.y_PX_CM/50.
      print, xcmperpix, ycmperpix
   ENDELSE

      xsize = gamma*ni*xcmperpix
      ysize = gamma*nj*ycmperpix

      x0 = (!d.x_size-xsize)/2. ; coordonnees de l''image
      y0 = (!d.y_size-ysize)/2. ;
      x1 = x0/!d.x_size
      y1 = y0/!d.y_size

      !x.range = ibase+[0, ni*scale_x]
      !y.range = jbase+[0, nj*scale_y]
      !x.style = 1
      !y.style = 1
      !p.position = [x1, y1, 1-x1, 1-y1] ; position du cadre
      subtitle = 'MIN='+string(min)+' / MAX='+string(max)

      IF keyword_set(erase) NE 0 THEN BEGIN
       erase, 255
      ENDIF

      tv, image2, x0, y0, xsize = xsize, ysize = ysize, _EXTRA=extra
      plot, !x.crange, !y.crange, subtitle = subtitle, /nodata, /noerase, $
       xticklen = 1, yticklen = 1, color = 0, _EXTRA=extra ; cadre

      x0 = !x.window[0]
      y0 = !y.window[0]
      dx = !x.window[1]-!x.window[0]
      dy = !y.window[1]-!y.window[0]

      deltax = (!x.window[1]-!x.window[0])
      deltay = (!y.window[1]-!y.window[0])
      x00 = !x.window[1]
      y00 = !y.window[0]

      colorbar,/vert,color=0,bottom = 1, $
       min=min,max=max,$
       divisions=10, /right, $
       position =[x00+deltax*0.01,y00+deltay*0.1,x00+deltax*0.04,y00+deltay*0.9]

      IF n_elements(ps) EQ 0 THEN BEGIN
         print, 'button 1 = value / button 2 = zoom / button 3 = exit'
         REPEAT BEGIN
            cursor,x,y,/normal, /down
            button = !mouse.button
            i=0 > floor(ni*(x-x0)/dx) < (ni-1)
            j=0 > floor(nj*(y-y0)/dy) < (nj-1)
            CASE button OF
               1: BEGIN
                  print, 'i=', i+ibase, ' / j=', j+jbase, ' / z[i,j]=', image[i, j]
                  wait, .2
               END
               2: BEGIN
                  i0 = i
                  j0 = j
                  cursor,x,y,/normal, /down
                  i1=0 > floor(ni*(x-x0)/dx) < (ni-1)
                  j1=0 > floor(nj*(y-y0)/dy) < (nj-1)
                  IF i1 LE i0 THEN BEGIN
                   i1 = (i0+10) < (ni-1)
                  ENDIF
                  IF j1 LE j0 THEN BEGIN
                   j1 = (j0+10) < (nj-1)
                  ENDIF
                  tvnplot, image[i0:i1, j0:j1], _EXTRA=extra, /erase, $
                   ibase = i0+ibase, jbase = j0+jbase, MIN = min, Max = max
               END
               4: stop
               ELSE:
            ENDCASE
         ENDREP UNTIL (button EQ 4)
      ENDIF

      x_env_save = !x
      y_env_save = !y
      p_env_save = !p


   IF n_elements(ps) EQ 0 THEN BEGIN

   ENDIF ELSE BEGIN
      device, /close
;      tvlct, r, g, b
      set_plot, 'x'
   ENDELSE

   !x = x_env_save
   !y = y_env_save
   !p = p_env_save

END
