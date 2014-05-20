;+
;
; @param TRUNC {in}{required}{type=integer}
;
; @keyword IBEG
;
; @keyword IFIN
;
; @keyword JBEG
;
; @keyword JFIN
;
; @keyword ZOOM
;
; @keyword NO_SHIFT
;
; @keyword WHOLE_ARRAYS
;
; @keyword GLAMBOUNDARY
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
; $Id: mesh_gaussian.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
PRO mesh_gaussian, trunc $
    , IBEG=ibeg $
    , IFIN=ifin $
    , JBEG=jbeg $
    , JFIN=jfin $
    , ZOOM=zoom $
    , NO_SHIFT=no_shift $
    , WHOLE_ARRAYS=whole_arrays $
    , GLAMBOUNDARY=glamboundary
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='mesh_gaussian, trunc' $
    + ', IBEG=ibeg' $
    + ', IFIN=ifin' $
    + ', JBEG=jbeg' $
    + ', JFIN=jfin' $
    + ', ZOOM=zoom' $
    + ', NO_SHIFT=no_shift' $
    + ', WHOLE_ARRAYS=whole_arrays' $
    + ', GLAMBOUNDARY=glamboundary'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

 arg_type = size(trunc,/type)
 IF ((arg_type NE 2) AND (arg_type NE 3)) THEN BEGIN
   ras = report(['Incorrect arg type trunc' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

; init grid, sf, masks for gaussian grid for SINTEX
;
   print,' T',trunc, ' gaussian grid inits.',format='(A5,I3,A20)'

   key_shift = 0

; initialisation of character variables used in the execution of initncdf
 ; (and computegrid as a consequence)
   shift_txt = ''
   plain_txt = ''

   IF keyword_set(NO_SHIFT) THEN BEGIN
      shift_txt = ', SHIFT = 0'
   ENDIF
   IF keyword_set(WHOLE_ARRAYS) THEN BEGIN
      plain_txt = ', PLAIN = 1'
      ;; Force YREVERSE = 0, ZREVERSE = 0, PERIODIC = 0, SHIFT = 0, STRIDE = [1, 1, 1] and
      ;; suppress the automatic redefinition of the domain in case of x periodicity overlap,
      ;; y periodicity overlap (ORCA type only) and mask border to 0.
   ENDIF

   IF debug_w THEN BEGIN
    print, ' key_yreverse = ', key_yreverse
   ENDIF

   CASE trunc OF
      30: BEGIN & jpi = 96L & jpj = 48L & jpk = 14L & END
      42: BEGIN & jpi = 128L & jpj = 64L & jpk = 14L & END
      62: BEGIN & jpi = 192L & jpj = 94L & jpk = 17L & END
      106: BEGIN & jpi = 320L & jpj = 160L & jpk = 17L & key_shiftg = [jpi/2, 0]& END
      ELSE: BEGIN
         print, ' '
         print, ' mesh_gaussian: truncature ',trunc, ' not implemented.'
         stop
      END
   ENDCASE

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

; 1.  Define longitudes

   glamt = 360.0*findgen(jpi)/float(jpi)

; 2 Define latitudes
   CASE trunc OF
      30: BEGIN
         gphit = [ $
      -87.16 ,   -83.48 ,   -79.78 ,   -76.07  ,  -72.36 ,   -68.65, $
       -64.94 ,   -61.23 ,   -57.52 ,   -53.81  ,  -50.10 ,   -46.39, $
       -42.68 ,   -38.97 ,   -35.26 ,   -31.54  ,  -27.83 ,   -24.12, $
       -20.41 ,   -16.70 ,   -12.99 ,    -9.28  ,   -5.57 ,    -1.86, $
       1.86 ,     5.57 ,     9.28 ,    12.99  ,   16.70 ,    20.41, $
       24.12 ,    27.83 ,    31.54 ,    35.26  ,   38.97 ,    42.68, $
       46.39 ,    50.10 ,    53.81 ,    57.52  ,   61.23 ,    64.94, $
       68.65 ,    72.36 ,    76.07 ,    79.78  ,   83.48 ,    87.16 ]
         gdept = [1000,    850,     700,     500,     400,    300, $
           250,     200,     150,     100,      70,     50, $
           30,      10]
      END
      42: BEGIN
         gphit = [ $
        -87.8638, -85.09653, -82.31291, -79.5256, -76.7369, -73.94752,  $
    -71.15775, -68.36776, -65.57761, -62.78735, -59.99702, -57.20663, $
    -54.4162, -51.62573, -48.83524, -46.04473, -43.2542, -40.46365, $
    -37.67309, -34.88252, -32.09195, -29.30136, -26.51077, -23.72017, $
    -20.92957, -18.13897, -15.34836, -12.55776, -9.767145, -6.976533, $
    -4.185921, -1.395307, 1.395307, 4.185921, 6.976533, 9.767145, 12.55776, $
    15.34836, 18.13897, 20.92957, 23.72017, 26.51077, 29.30136, 32.09195, $
    34.88252, 37.67309, 40.46365, 43.2542, 46.04473, 48.83524, 51.62573, $
    54.4162, 57.20663, 59.99702, 62.78735, 65.57761, 68.36776, 71.15775, $
    73.94752, 76.7369, 79.5256, 82.31291, 85.09653, 87.8638 ]
      gdept = [1000,    850,     700,     500,     400,    300, $
           250,     200,     150,     100,      70,     50, $
           30,      10]
      END
      62: BEGIN
         gphit = [ $
          88.542, 86.6531, 84.7532, 82.8508, 80.9473, 79.0435, 77.1394, 75.2351,  $
    73.3307, 71.4262, 69.5217, 67.6171, 65.7125, 63.8079, 61.9033, 59.9986,  $
    58.0939, 56.1893, 54.2846, 52.3799, 50.4752, 48.5705, 46.6658, 44.7611,  $
    42.8564, 40.9517, 39.047, 37.1422, 35.2375, 33.3328, 31.4281, 29.5234,  $
    27.6186, 25.7139, 23.8092, 21.9044, 19.9997, 18.095, 16.1902, 14.2855,  $
    12.3808, 10.47604, 8.57131, 6.66657, 4.76184, 2.8571, 0.952368,  $
    -0.952368, -2.8571, -4.76184, -6.66657, -8.57131, -10.47604, -12.3808]
      gphit = [ gphit, $
    -14.2855, -16.1902, -18.095, -19.9997, -21.9044, -23.8092, -25.7139,  $
    -27.6186, -29.5234, -31.4281, -33.3328, -35.2375, -37.1422, -39.047,  $
    -40.9517, -42.8564, -44.7611, -46.6658, -48.5705, -50.4752, -52.3799,  $
    -54.2846, -56.1893, -58.0939, -59.9986, -61.9033, -63.8079, -65.7125,  $
    -67.6171, -69.5217, -71.4262, -73.3307, -75.2351, -77.1394, -79.0435,  $
    -80.9473, -82.8508, -84.7532, -86.6531, -88.542 ]
;      gphit = reverse(gphit, 1)
      gdept = [10,    30,     50,     70,     100,    150, $
           200,     250,     300,     400,      500,     600, $
           700,      775, 850, 925, 1000]

      END
      106: BEGIN
         gphit = [$
     89.14152, 88.02943, 86.91077, 85.79063, 84.66992, 83.54894, $
    82.42781, 81.30659, 80.1853, 79.06398, 77.94262, 76.82124, 75.69984, $
    74.57843, 73.457, 72.33557, 71.21413, 70.09269, 68.97124, 67.84978, $
    66.72832, 65.60686, 64.4854, 63.36393, 62.24246, 61.12099, 59.99952, $
    58.87804, 57.75657, 56.63509, 55.51361, 54.39213, 53.27065, 52.14917, $
    51.02769, 49.90621, 48.78473, 47.66325, 46.54176, 45.42028, 44.29879, $
    43.17731, 42.05582, 40.93434, 39.81285, 38.69136, 37.56988, 36.44839, $
    35.3269, 34.20542, 33.08393, 31.96244, 30.84095, 29.71947, 28.59798, $
    27.47649, 26.355, 25.23351, 24.11202, 22.99054, 21.86905, 20.74756, $
    19.62607, 18.50458, 17.38309, 16.2616, 15.14011, 14.01862, 12.89713, $
    11.77564, 10.65415, 9.532663, 8.411174, 7.289684, 6.168194, 5.046704, $
    3.925215, 2.803725, 1.682235, 0.5607449 ]
      gphit = [ gphit, $
    -0.5607449, -1.682235, $
    -2.803725, -3.925215, -5.046704, -6.168194, -7.289684, -8.411174, $
    -9.532663, -10.65415, -11.77564, -12.89713, -14.01862, -15.14011, $
    -16.2616, -17.38309, -18.50458, -19.62607, -20.74756, -21.86905, $
    -22.99054, -24.11202, -25.23351, -26.355, -27.47649, -28.59798, $
    -29.71947, -30.84095, -31.96244, -33.08393, -34.20542, -35.3269, $
    -36.44839, -37.56988, -38.69136, -39.81285, -40.93434, -42.05582, $
    -43.17731, -44.29879, -45.42028, -46.54176, -47.66325, -48.78473, $
    -49.90621, -51.02769, -52.14917, -53.27065, -54.39213, -55.51361, $
    -56.63509, -57.75657, -58.87804, -59.99952, -61.12099, -62.24246, $
    -63.36393, -64.4854, -65.60686, -66.72832, -67.84978, -68.97124, $
    -70.09269, -71.21413, -72.33557, -73.457, -74.57843, -75.69984, $
    -76.82124, -77.94262, -79.06398, -80.1853, -81.30659, -82.42781, $
    -83.54894, -84.66992, -85.79063, -86.91077, -88.02943, -89.14152 ]
      gphit = reverse(gphit, 1)
      gdept = [10,    30,     50,     70,     100,    150, $
           200,     250,     300,     400,      500,     600, $
           700,      775, 850, 925, 1000]
   END
   ENDCASE

; 4. Define mask

; open mask file

   CASE trunc OF
      62: BEGIN ; NCEP mask
         s_file = hom_idl+'grids/mask_t62.nc'
         tmask = nc_get(s_file, 'lsmask')
;         tmask = reverse(1-tmask, 2)
         tmask = 1-tmask
      END
      ELSE: BEGIN
         nummsk = 12
         s_file = hom_idl+'grids/mask_t'+strtrim(string(trunc), 2)
         openr, nummsk, s_file, /get_lun, /f77, /swap_if_little_endian
         tmask = lonarr(jpi, jpj)
         readu, nummsk, tmask
         CASE trunc OF
            106: tmask = 1-tmask
             30: BEGIN
                tmask = reverse(1-tmask, 2)
;                tmask = shift(tmask, jpi/2, 0)
             END
            ELSE: BEGIN
               tmask = reverse(1-tmask, 2)
            END
         ENDCASE
         close, nummsk
         free_lun, nummsk
      END
   ENDCASE
;
; compute grid
;
   computegrid, xaxis = glamt, yaxis = gphit, mask = tmask, GLAMBOUNDARY = glamboundary, /periodic

   key_offset = [0, 0, 0]

   print,'    End of initialisation for gaussian grid T', trunc

END
