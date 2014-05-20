;+
;
; compute rhopn potential volumic mass
;
; REFERENCE:
;	Compute the potential volumic mass (Kg/m3) from now potential
;       temperature and salinity fields using Jackett and McDougall
;       (1994) equation of state.
;       It is computed directly as a function of
;       potential temperature relative to the surface (the opa tn
;       variable), salt and pressure (assuming no pressure variation
;       along geopotential surfaces, i.e. the pressure p in decibars
;       is approximated by the depth in meters.
;              rhop(t,s) = rho(t,s,0)
;         with pressure                      p        decibars
;              potential temperature         t        deg celsius
;              salinity                      s        psu
;              reference volumic mass        rau0     kg/m**3
;              in situ volumic mass          rho      kg/m**3
;
;         Check value: rho = 1059.8204 kg/m**3 for p=10000 dbar,
;          t = 40 deg celcius, s=40 psu
;
;       Jackett, D.R., and T.J. McDougall. J. Atmos. Ocean. Tech., 1994
;
; @categories
; Calculation
;
; @param FILE_NAME {in}{required}{string}
;
; @param NCDF_DB {in}{required}{type=string}
; <location>:<path> or just <path>
;
; @keyword BOXZOOM
;
; @keyword TIME_1
;
; @keyword TIME_2
;
; @keyword ALL_DATA
;
; @keyword ZMTYP
;
; @returns
; structure
; -1 in case of error
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @history
; - fplod 20100119T094252Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - Maurice Imbard
;
; - Eric Guilyardi - adaptation to post_it
;
; @version
; $Id: make_eos_tem.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION make_eos_tem, file_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ALL_DATA=all_data $
         , ZMTYP=zmtyp
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
 usage='result=make_eos_tem(file_name, ncdf_db ' $
         + ', BOXZOOM=boxzoom ' $
         + ', TIME_1=time_1 ' $
         + ', TIME_2=time_2 ' $
         + ', ALL_DATA=all_data ' $
         + ', ZMTYP=zmtyp)'

 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(file_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type file_name' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

 arg_type = size(ncdf_db,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type ncdf_db' $
          + '!C' $
          + 'Usage : ' + usage])
   return, -1
 ENDIF

;
; Read T and S
;
   tn = nc_read(file_name,'votemper', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data, no_mean = 1)
   sn = nc_read(file_name,'vosaline', ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data, no_mean = 1)

; declarations
;
   t=tn.data
   s=sn.data
   idxt=where(t eq valmask)
   idxs=where(s eq valmask)
   IF idxt[0] NE -1 THEN BEGIN
    t[idxt]=0.
   ENDIF
   IF idxs[0] NE -1 THEN BEGIN
    s[idxs]=0.
   ENDIF
;
; potential volumic mass
;
   sr=sqrt(abs(s))
   r1=((((0.0E-9*t-0.0E-6)*t+0.0E-4)*t $
        -0.0E-3)*t+6.793952E-2)*t+0.0
   r2=(((0.0E-9*t-0.0E-7)*t+0.0E-5)*t-4.0899E-3)*t+0.0
   r3=(0.0E-6*t+1.0227E-4)*t-0.0
   rhopn = ( ( 4.8314E-4*s + r3*sr +r2)*s +r1)

   IF idxs[0] NE -1 THEN BEGIN
    rhopn[idxt] = valmask
   ENDIF

   fdirec = tn.direc
   flegend = ''

   remove_dim = 0
   IF vert_switch ge 1 THEN BEGIN
      old_boite = [lon1, lon2, lat1, lat2, prof1, prof2]
      domdef
      print, '      Average in vertical domain ', vert_type, vert_mean
      CASE vert_type OF
         'z': zmean = grossemoyenne(rhopn, 'z', boite = vert_mean, NAN =1.e20)
         ELSE: zmean = grossemoyenne(rhopn, 'z', boite = vert_mean, NAN =1.e20, /zindex)
      ENDCASE
      rhopn = zmean
      domdef, old_boite
      vert_switch = 2
      remove_dim = 1
      fdirec = 'xyt'
      name_suff = ' averaged in '+vert_type+'['+strtrim(string(vert_mean[0]), 2)+','+strtrim(string(vert_mean[1]), 2)+']'
      flegend = name_suff
   ENDIF

   fieldr = {name: '', data: rhopn, legend: '', units: '', origin: '', dim: 0, direc:''}

   fieldr.origin = tn.origin
   fieldr.dim = tn.dim - remove_dim
   fieldr.direc = fdirec
   fieldr.legend = flegend

   return, fieldr
end
