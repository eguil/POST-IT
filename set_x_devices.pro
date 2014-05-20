;+
;
; Set X devices
;
; @history
; - fplod 20100119T160644Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: set_x_devices.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
PRO set_x_devices
;
  compile_opt idl2, strictarrsubs
;
 usage='set_x_devices'
;
 nparam = N_PARAMS()
 IF (nparam NE 0) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF

; font
   !p.font = -1
   device, true_color = 24
END
