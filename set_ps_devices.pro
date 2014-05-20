;+
;
; Set PostScript devices
;
; @todo
; usefull ? why no use <pro>openps</pro> ?
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: set_ps_devices.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
PRO set_ps_devices
;
  compile_opt idl2, strictarrsubs

 usage='set_ps_devices'
;
 nparam = N_PARAMS()
 IF (nparam NE 0) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF


; font
   !p.font = 0
   device, /helvetica
;   help, /device
END
