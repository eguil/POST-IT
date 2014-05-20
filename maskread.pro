;+
;
; lit un fichier bathymetry ORCA ou un mask atmos
;
; @categories
; lecture de fichier
;
; @param BATHY {in}{required}{type=string}
; part of bathymetry file
;
; @param MODEL {in}{required}{type=string}
; part of bathymetry file ++ or grid type ?
;
; @keyword D3
;
; @returns
; -1 in case of error
;
; @examples
;
; To read bathymetry of pacific for ORCA (model/grid?) file :
; IDL> bathy='pacific'
; IDL> model='orca'
; IDL> result=maskread(bathy, model)
; IDL> print, result
; ++
;    Reading orca.pacific mask
;
; % Subscript range values of the form low:high must be >= 0, < size, with low <= high: ZBAT.
;
;
; To check good errors handling : ++
; IDL> bathy='ginette'
; IDL> model='arthur'
; IDL> result=maskread(bathy, model)
; IDL> print, result
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; <propost_it>mask_z</propost_it>
;
; @todo
; error handling
;
; make realistic example work !
;
; @history
;
; - fplod 20091210T103244Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - Maurice Imbard (mimlod\@ipsl.jussieu.fr) 12/05/99
;
; @version
; $Id: maskread.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
FUNCTION maskread, bathy, model $
         , D3=d3
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 usage='result=maskread( bathy, model, D3=d3)'
;
 nparam = N_PARAMS()
 IF (nparam LT 2) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(bathy,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type bathy' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(model,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type model' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

   IF n_elements(nbathys) EQ 0 THEN BEGIN
      read_bathy = 'yes'
   ENDIF ELSE BEGIN
      i = 0
      WHILE i LE nbathys-1 DO BEGIN
         IF bathy_read[i].name EQ bathy THEN BEGIN
            zbat = bathy_read[i].mask
            IF keyword_set(D3) THEN BEGIN
               zbat = 0 > zbat
               ; Build 3D from pseudo 3D
               zbat=reform(zbat[*]#(1./(indgen(jpkglo)+1)),(size(zbat))[1],(size(zbat))[2],jpkglo)
               zbat=floor(zbat)<1
            ENDIF ELSE BEGIN
               zbat = zbat <  1
            ENDELSE
            return, zbat
         ENDIF ELSE BEGIN
            read_bathy = 'yes'
         ENDELSE
         i = i+1
      ENDWHILE
   ENDELSE

   IF read_bathy EQ 'yes' THEN BEGIN
      z = -1
;;
;---------------------------------------------------------------------
; definition de la taille du fichier a aller chercher: jpidta,jpjdta,jpkdta...
;---------------------------------------------------------------------
      if n_elements(jpidta) EQ 0 THEN BEGIN
         if n_elements(ixmindta) EQ 0 OR n_elements(ixmaxdta) EQ 0 THEN BEGIN
          jpidta = jpiglo
         ENDIF ELSE BEGIN
          jpidta = ixmaxdta-ixmindta+1
         ENDELSE
      endif
      if n_elements(jpjdta) EQ 0 THEN BEGIN
         if n_elements(iymindta) EQ 0 OR n_elements(iymaxdta) EQ 0 THEN BEGIN
          jpjdta = jpjgloo
         ENDIF ELSE BEGIN
          jpjdta = iymaxdta-iymindta+1
         ENDELSE
      endif
      if n_elements(jpkdta) EQ 0 THEN BEGIN
         if n_elements(izmindta) EQ 0 OR n_elements(izmaxdta) EQ 0 THEN BEGIN
          jpkdta = jpkglo
         ENDIF ELSE BEGIN
          jpkdta = izmaxdta-izmindta+1
         ENDELSE
      endif
;--------------------------------------------------------------------
; ouverture du fichier a l'adresse s_fichier
;--------------------------------------------------------------------
;;
      iname = bathy
      iname_file = hom_idl+'grids/'+model+'.'+iname
      openr, iunit, /get_lun,iname_file
      print, '    Reading ', model+'.'+iname, ' mask '
      print, ' '
;---------------------------------------------------------------------
; lecture des champs directement vers le champ
;---------------------------------------------------------------------
      ifreq = 40
      ifin = jpiglo/ifreq+1
      irest = jpiglo-(ifin-1)*ifreq
      zbati = intarr(ifreq)
      zbati2 = intarr(irest)
      ;;
      zbat = intarr(jpiglo,jpjglo)
;
      readf, iunit, FORMAT = '(16x,2i8)', iim, ijm
      readf, iunit, FORMAT = '(/)'
      readf, iunit, FORMAT = '(/)'
      il1 = 0
      FOR jn = 1, ifin-1 DO BEGIN
         readf, iunit, FORMAT = '(/)'
         readf, iunit, FORMAT = '(/)'
         il2 = min([ jpiglo-1, il1+ifreq-1] )
         readf, iunit, FORMAT = '(/)'
         readf, iunit, FORMAT = '(/)'
         readf, iunit, FORMAT = '(/)'
         il3 = il2-(jn-1)*ifreq
         iformat = string('(', il3+2, 'i3)')
         FOR jj = jpjglo-1, 0, -1  DO BEGIN
            zbati[0:ifreq-1] = 0
            readf, iunit, FORMAT = iformat, ij, zbati
            zbat[il1:il2, jj] = zbati[0:il3]
         END
         il1 = il1 + ifreq
      END
      readf, iunit, FORMAT = '(/)'
      readf, iunit, FORMAT = '(/)'
      il2 = min([ jpiglo-1, il1+ifreq-1] )
      readf, iunit, FORMAT = '(/)'
      readf, iunit, FORMAT = '(/)'
      readf, iunit, FORMAT = '(/)'
      il3 = il2-(ifin-1)*ifreq
      iformat = string('(', il3+2, 'i3)')
      FOR jj = jpjglo-1, 0, -1  DO BEGIN
         zbati2[0:irest-1] = 0
         readf, iunit, FORMAT = iformat, ij, zbati2
         zbat[il1:il2, jj] = zbati2[0:il3]
      END
      ;;
;---------------------------------------------------------------------
; on initialise les ixmindta, iymindta  au besoin
;---------------------------------------------------------------------
;;
      ixmindta = 0
      ixmaxdta = jpiglo-1
      iymindta = 0
      iymaxdta = jpjglo-1
      izmindta = 0
      izmaxdta = jpkglo-1
;---------------------------------------------------------------------
; on reduit z selon les valeurs de ixmindta, iymindta, ...+ shift
;---------------------------------------------------------------------
      z = zbat[ixminmesh-ixmindta:ixmaxmesh-ixmindta $
               ,iyminmesh-iymindta:iymaxmesh-iymindta ]
      z = shift(z,key_shift, 0)
;---------------------------------------------------------------------
      IF n_elements(nbathys) EQ 0 THEN BEGIN
         nbathys = 1
         a = z*0
         bathy_read = {name: '', mask:a}
         bathy_read = replicate(bathy_read, 4)
         bathy_read[0].name = bathy
         bathy_read[0].mask = z
      ENDIF ELSE BEGIN
         IF nbathys LE 3 THEN BEGIN
            nbathys = nbathys + 1
         ENDIF ELSE BEGIN
            nbathys = 1
         ENDELSE
         bathy_read[nbathys-1].name = bathy
         bathy_read[nbathys-1].mask = z
      ENDELSE

;---------------------------------------------------------------------
fini:
      free_lun,iunit
      close, iunit
;---------------------------------------------------------------------
sortie:
      zbat = z
      IF keyword_set(D3) THEN BEGIN
         zbat = 0 > zbat
         ; Build 3D from pseudo 3D
         zbat=reform(zbat[*]#(1./(indgen(jpkglo)+1)),(size(zbat))[1],(size(zbat))[2], jpkglo)
         zbat=floor(zbat)<1
      ENDIF ELSE BEGIN
         zbat = zbat <  1
      ENDELSE
      return, zbat
   ENDIF
END
