;+
;
; @keyword I_INDEX
;
; @keyword DELTA_I
;
; @keyword J_INDEX
;
; @keyword DELTA_J
;
; @keyword K_INDEX
;
; @keyword DELTA_K
;
; @keyword ZONAL
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; duplicate of mesh_glosea.pro ?
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: build_mesh_glosea.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO mesh_glosea $
    , I_INDEX=i_index $
    , DELTA_I=delta_i $
    , J_INDEX=j_index $
    , DELTA_J=delta_j $
    , K_INDEX=k_index $
    , DELTA_K=delta_k $
    , ZONAL=zonal
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
print, '    Init GLOSEA mesh'

 usage='mesh_glosea' $
    + ', I_INDEX=i_index' $
    + ', DELTA_I=delta_i' $
    + ', J_INDEX=j_index' $
    + ', DELTA_J=delta_j' $
    + ', K_INDEX=k_index' $
    + ', DELTA_K=delta_k' $
    + ', ZONAL=zonal'
;
 nparam = N_PARAMS()
 IF (nparam NE 0) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

;
; NCO tranfo for glosea netCDF files (GLOSEA_40_10y_01_10_grid_T_glosea.nc )
; ncwa -a unspecified
; ncrename -h -d t,time_counter -d longitude,x -d latitude,y
; -v longitude,nav_lon -v latitude,nav_lat -v depth,deptht
; -v temp,votemper -v salinity,vosaline  -v field611,sobarstg
; -v field653,somixhgt, -v PLE,sowaflup

;----------------------------------------------------------
; bornes de la grille
;----------------------------------------------------------
;
   jpi = 288
   jpj = 192
   jpk = 40

   jpiglo = jpi
   jpjglo = jpj
   jpkglo = jpk

   ixminmesh =0
   ixmaxmesh =jpi-1
;
   iyminmesh =0
   iymaxmesh =jpj-1
;
   izminmesh =0
   izmaxmesh =jpk-1


; init shift and periodicity

   key_shift = !VALUES.F_NAN

   no_lon_shift = 0

   key_periodique = 1

; reduce grid in latitude ?

   IF keyword_set(J_INDEX) THEN BEGIN

      IF NOT keyword_set(DELTA_J) THEN BEGIN
       delta_j = 1
      ENDIF
      delta_j = delta_j - 1

      iyminmesh = j_index
      iymaxmesh = j_index+delta_j

      print, '      set GLOSEA grid to j_index in ', j_index, j_index+delta_j

   ENDIF

; reduce grid in longitude ?

   IF keyword_set(I_INDEX) THEN BEGIN

      no_lon_shift = no_lon_shift + 1

      IF NOT keyword_set(DELTA_I) THEN BEGIN
       delta_i = 1
      ENDIF
      delta_i = delta_i - 1

      ixminmesh = i_index
      ixmaxmesh = i_index+delta_i

      print, '      set GLOSEA grid to i_index in ', i_index, i_index+delta_i

      key_periodique = 0

      key_offset = [0, 0, 0]

      diaznl_idx = 0

      key_shift = 0

   ENDIF

; reduce grid in depth ?

   IF keyword_set(K_INDEX) THEN BEGIN

      IF NOT keyword_set(DELTA_K) THEN BEGIN
       delta_k = 1
      ENDIF
      delta_k = delta_k - 1

      izminmesh = k_index-1
      izmaxmesh = k_index-1+delta_k

      print, '      set GLOSEA grid to k_index in ', k_index-1, k_index-1+delta_k

   ENDIF
;
;------------------------------------------------------
; calcul de la grille
;------------------------------------------------------

; 1.  Define longitudes

   inilon = 0.

   glamt = 360.0*findgen(jpi)/float(jpi)+inilon
   glamt = glamt#replicate(1, jpj)

   inilon = 0.625

   glamu = 360.0*findgen(jpi)/float(jpi)+inilon
   glamu = glamu#replicate(1, jpj)

; 2.  Define latitudes

   gphit = [-89.375,-88.125,-86.875,-85.625,-84.375,-83.125,-81.875,-80.625,-79.375,-78.125,-76.875,-75.625,$
            -74.375,-73.125,-71.875,-70.625,-69.375,-68.125,-66.875,-65.625,-64.375,-63.125,-61.875,-60.625,$
            -59.375,-58.125,-56.875,-55.625,-54.375,-53.125,-51.875,-50.625,-49.375,-48.125,-46.875,-45.625,$
            -44.375,-43.125,-41.877,-40.632,-39.391,-38.156,-36.928,-35.709,-34.500,-33.302,-32.117,-30.946,$
            -29.791,-28.651,-27.529,-26.426,-25.342,-24.279,-23.237,-22.217,-21.221,-20.249,-19.300,-18.377]

   gphit = [gphit, $
            -17.480,-16.608,-15.762,-14.943,-14.150,-13.384,-12.644,-11.931,-11.245,-10.584,-9.950,-9.341,-8.757, $
            -8.197,-7.661,-7.148,-6.658,-6.189,-5.740,-5.311,-4.901,-4.508,-4.132,-3.771,-3.423,-3.089,-2.766,$
            -2.453,-2.148,-1.851,-1.559,-1.272,-0.988,-0.706,-0.423,-0.141,0.141,0.423,0.706,0.988,1.272,1.559,$
            1.851,2.148,2.453,2.766,3.089,3.423,3.771,4.132,4.508,4.901,5.311,5.740,6.189,6.658,7.148,7.661,8.197,$
            8.757,9.341,9.950,10.584,11.245,11.931,12.644,13.384,14.150,14.943,15.762,16.608,17.480,18.377,19.300]
    gphit = [gphit, $
            20.249,21.221,22.217,23.237,24.279,25.342,26.426,27.529,28.651,29.791,30.946,32.117,33.302,34.500, $
            35.709,36.928,38.156,39.391,40.632,41.877,43.125,44.375,45.625,46.875,48.125,49.375,50.625,51.875, $
            53.125,54.375,55.625,56.875,58.125,59.375,60.625,61.875,63.125,64.375,65.625,66.875,68.125,69.375,$
            70.625,71.875,73.125,74.375,75.625,76.875,78.125,79.375,80.625,81.875,83.125,84.375,85.625,86.875,$
            88.125,89.375]
   gphit = replicate(1, jpi)#gphit

   gphiu = [-88.750,-87.500,-86.250,-85.000,-83.750,-82.500,-81.250,-80.000,-78.750,-77.500,-76.250,-75.000,-73.750,-72.500,-71.250,-70.000,-68.750,-67.500,-66.250,-65.000,-63.750,-62.500,-61.250,-60.000,-58.750,-57.500,-56.250,-55.000,-53.750,-52.500,-51.250,-50.000,-48.750,-47.500,-46.250,-45.000,-43.750,-42.501,-41.254,-40.010,-38.772,-37.540,-36.316,-35.102,-33.898,-32.707,-31.528,-30.365,-29.217,-28.086,-26.973,-25.879,-24.805,-23.752,-22.722,-21.713,-20.729]

 gphiu = [gphiu, -19.768,-18.833,-17.922,-17.037,-16.178,-15.346,-14.539,-13.760,-13.007,-12.281,-11.581,-10.908,-10.261,-9.639,-9.043,-8.471,-7.923,-7.399,-6.897,-6.418,-5.959,-5.521,-5.102,-4.700,-4.316,-3.948,-3.594,-3.253,-2.925,-2.607,-2.298,-1.998,-1.704,-1.415,-1.130,-0.847,-0.565,-0.282,-0.000,0.282,0.565,0.847,1.130,1.415,1.704,1.998,2.298,2.607,2.925,3.253,3.594,3.948,4.316,4.700,5.102,5.521,5.959,6.418,6.897,7.399,7.923,8.471,9.043,9.639,10.261,10.908,11.581,12.281]

 gphiu = [gphiu,13.007,13.760,14.539,15.346,16.178,17.037,17.922,18.833,19.768,20.729,21.713,22.722,23.752,24.805,25.879,26.973,28.086,29.217,30.365,31.528,32.707,33.898,35.102,36.316,37.540,38.772,40.010,41.254,42.501,43.750,45.000,46.250,47.500,48.750,50.000,51.250,52.500,53.750,55.000,56.250,57.500,58.750,60.000,61.250,62.500,63.750,65.000,66.250,67.500,68.750,70.000,71.250,72.500,73.750,75.000,76.250,77.500,78.750,80.000,81.250,82.500,83.750,85.000,86.250,87.500,88.750,90.000]
   gphiu = replicate(1, jpi)#gphiu

; 3 Define scale factors

   zrad=6371229.0

   ; Exact method: integration on a sphere

   e1t = abs(2*!pi*zrad*cos(gphit*!pi/180.0)/jpi)
   e1u = abs(2*!pi*zrad*cos(gphiu*!pi/180.0)/jpi)

   e2t = [1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.249,1.247,1.243,1.238,1.232,1.224,1.214,1.204,1.192,1.178,1.164,1.148,1.131,1.113,1.094,1.074,1.053,1.031,1.008,0.985,0.960,0.936,0.911,0.885,0.859,0.833,0.806,0.779,0.753,0.726,0.700,0.673,0.647,0.622,0.596,0.572,0.548,0.524,0.501]

e2t = [e2t, 0.480,0.459,0.438,0.419,0.401,0.384,0.368,0.354,0.341,0.329,0.318,0.308,0.301,0.294,0.289,0.285,0.283,0.282,0.282,0.282,0.282,0.282,0.282,0.283,0.285,0.289,0.294,0.301,0.308,0.318,0.329,0.341,0.354,0.368,0.384,0.401,0.419,0.438,0.459,0.480,0.501,0.524,0.548,0.572,0.596,0.622,0.647,0.673,0.700,0.726,0.753,0.779,0.806,0.833,0.859,0.885,0.911,0.936]

e2t = [e2t,0.960,0.985,1.008,1.031,1.053,1.074,1.094,1.113,1.131,1.148,1.164,1.178,1.192,1.204,1.214,1.224,1.232,1.238,1.243,1.247,1.249,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250,1.250]
   e2t = replicate(1, jpi)#e2t

; 4. Define mask
; open mask file
   tmask = lonarr(jpi, jpj, jpk)
   s_file = hom_idl+'grids/mask_glosea'
   restore, s_file

; 5. vertical grid (m)

   gdept = [10.,20.,30.,40.,50.,60.,70.,80.,90.,100.,110.,120.,130.,141.,156.,176.,204.,243.,293.,358.,439.,538.,655.,794.,954.,1137.,1344.,1576.,1832.,2113.,2419.,2745.,3085.,3430.,3775.,4120.,4465.,4810.,5155.,5500.]
   gdepw = [5.,15.,25.,35.,45.,55.,65.,75.,85.,95.,105.,115.,125.,136.,148.,166.,190.,224.,268.,326.,399.,488.,596.,724.,874.,1046.,1241.,1460.,1704.,1972.,2266.,2582.,2915.,3258.,3602.,3948.,4292.,4638.,4982.,5328.]

   e3t = [10.,10.,10.,10.,10.,10.,10.,10.,10.,10.,10.,10.,10.,11.,15.,20.,28.,38.,51.,65.,81.,98.,118.,138.,160.,183.,207.,231.,256.,281.,306.,326.,340.,345.,345.,345.,345.,345.,345.,345.]
   e3w = [5.,10.,10.,10.,10.,10.,10.,10.,10.,10.,10.,10.,10.,11.,13.,17.,24.,33.,44.,58.,73.,90.,108.,128.,149.,172.,195.,219.,244.,269.,293.,316.,333.,343.,345.,345.,345.,345.,345.,345.]




   isign=where(glamt gt 380.)
   IF isign[0] NE -1 THEN BEGIN
    glamt[isign] = glamt[isign]-360.
   ENDIF
   isign=where(glamu gt 380.)
   IF isign[0] NE -1 THEN BEGIN
    glamu[isign] = glamu[isign]-360.
   ENDIF
   isign=where(glamv gt 380.)
   IF isign[0] NE -1 THEN BEGIN
    glamv[isign] = glamv[isign]-360.
   ENDIF
   isign=where(glamf gt 380.)
   IF isign[0] NE -1 THEN BEGIN
    glamf[isign] = glamf[isign]-360.
   ENDIF

; reduce grid to zonal mean

   IF keyword_set(ZONAL) THEN BEGIN

      no_lon_shift = no_lon_shift + 1

      jpi = 1
      ixminmesh = 0
      ixmaxmesh = 0

      print, '   set GLOSEA grid to zonal mean '

      diaznl_idx = 0

      glamt = glamt[diaznl_idx, *]
      glamu = glamu[diaznl_idx, *]
      glamv = glamv[diaznl_idx, *]
      glamf = glamf[diaznl_idx, *]
      gphit = gphit[diaznl_idx, *]
      gphiu = gphiu[diaznl_idx, *]
      gphiv = gphiv[diaznl_idx, *]
      gphif = gphif[diaznl_idx, *]

      e1t = e1t[diaznl_idx, *]
      e2t = e2t[diaznl_idx, *]
      e1u = e1u[diaznl_idx, *]
      e2u = e2u[diaznl_idx, *]
      e1v = e1v[diaznl_idx, *]
      e2v = e2v[diaznl_idx, *]

; read offset

      key_offset = [0, 0, 0]

      key_shift = 0

   ENDIF


   IF no_lon_shift EQ 0 THEN BEGIN

; read offset

      key_offset = [0, 0, 0]
;
; indice i pour grille j moyenne zonale
;
      diaznl_idx = 0

   ENDIF

END
