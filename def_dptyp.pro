;+
;
; Define data/plot type
;
; @param CMD {in}{required}{type=structure}
; only cmd.plt is used
;
; @examples
;
; IDL> cmd={plt: 'xy'}
; IDL> result=def_dptyp(cmd)
; IDL> help,result,/struct
; ** Structure <84684bc>, 6 tags, length=56, data length=56, refs=1:
;   PLTTYP          STRING    'plt'
;   HOTYP           STRING    '-'
;   DIMPLOT         LONG                 2
;   PLTZTYP         STRING    '-'
;   SPLOT           LONG                 0
;   PLT1DTYP        STRING    '-'
;
; @returns
; -1 in case of error
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
; check commons
;
; @examples
;
; @history
;
; - fplod 20091209T094824Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: def_dptyp.pro 203 2010-01-25 13:44:20Z pinsard $
;
;-
FUNCTION def_dptyp, cmd
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2

 usage='result=def_dptyp(cmd)'
;
 nparam = N_PARAMS()
 IF (nparam LT 1) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_type = size(cmd,/type)
 IF (arg_type NE 8) THEN BEGIN
   ras = report(['Incorrect arg type cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF
 arg_struct_tags=TAG_NAMES(cmd)
 tag=WHERE(STRMATCH(arg_struct_tags, 'PLT'))
 IF (tag EQ -1) THEN BEGIN
   ras = report(['Incorrect arg tag PLT cmd' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF

 arg_type = size(cmd.plt,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmd.plt' $
          + '!C' $
          + 'Usage : ' + usage])
    return, -1
 ENDIF


   splot = 0
   pltztyp = '-'
   plt1dtyp = '-'

   CASE strmid(cmd.plt, 0, 2) OF

       'xy': BEGIN & plttyp = 'plt'   & hotyp = '-'  & dimplot = 2 & END
       'yz': BEGIN & plttyp = 'pltz'  & hotyp = '-'  & dimplot = 2 & pltztyp = 'yz' & END
       'xz': BEGIN & plttyp = 'pltz'  & hotyp = '-'  & dimplot = 2 & pltztyp = 'xz' & END
       'xt': BEGIN & plttyp = 'pltt'  & hotyp = 'xt' & dimplot = 2 & END
       'yt': BEGIN & plttyp = 'pltt'  & hotyp = 'yt' & dimplot = 2 & END
       'zt': BEGIN & plttyp = 'pltt'  & hotyp = 'zt' & dimplot = 2 & END
       't_': BEGIN & plttyp = 'pltt'  & hotyp = 't'  & dimplot = 1 & END
       'x_': BEGIN & plttyp = 'plt1d' & hotyp = '-'  & dimplot = 1 & plt1dtyp = 'x' & END
       'y_': BEGIN & plttyp = 'plt1d' & hotyp = '-'  & dimplot = 1 & plt1dtyp = 'y' & END
       'z_': BEGIN & plttyp = 'plt1d' & hotyp = '-'  & dimplot = 1 & plt1dtyp = 'z' & END

       'xs': BEGIN & plttyp = 'pltz'  & hotyp = '-'  & dimplot = 2 & splot = 1 & pltztyp = 'xz' & END
       'ys': BEGIN & plttyp = 'pltz'  & hotyp = '-'  & dimplot = 2 & splot = 1 & pltztyp = 'yz' & END
       'st': BEGIN & plttyp = 'pltt'  & hotyp = 'zt' & dimplot = 2 & splot = 1 & END
       's_': BEGIN & plttyp = 'plt1d' & hotyp = '-'  & dimplot = 1 & splot = 1 & plt1dtyp = 'z' & END

       ELSE: BEGIN
              print, ' unknown projection plot ', cmd.plt
              return, -1
             END
   ENDCASE

   data_type = {plttyp: plttyp, hotyp: hotyp, dimplot: dimplot, pltztyp: pltztyp, splot: splot, plt1dtyp: plt1dtyp}

   return, data_type
END
