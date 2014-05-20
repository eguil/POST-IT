;+
;
; define additional text for <pro>pltt</pro>
;
; TODO
; ====
;
; should be a function
;
; @version
; $Id: add_txt_pltt.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
            ; time/space filter ?
               IF strpos(cmd.plt, '@f') GT 1 THEN BEGIN
                  filter = long(strmid(cmd.plt, strpos(cmd.plt, '@f')+3, strlen(cmd.plt)-strpos(cmd.plt, '@f')-3))
                  fildirec = strmid(cmd.plt, strpos(cmd.plt, '@f')+2, 1)
                  filtxt = ',filter=filter,fildirec=fildirec'
                  filleg = '  ['+fildirec+'-filter='+string(filter, format = '(I3)')+']'
               ENDIF ELSE BEGIN
                  filtxt = ''
                  filleg = ''
               ENDELSE
            ; anomaly
               IF strmid(cmd.trend, 0, 1) EQ '4' THEN BEGIN
                  IF strmid(cmd.trend, 1, strlen(cmd.trend)-1) EQ 12 THEN BEGIN
                   filleg = filleg+'  [seasonal anomaly n=12]'
                  ENDIF ELSE BEGIN
                   filleg = filleg+'  [n='+strmid(cmd.trend, 1, strlen(cmd.trend)-1)+ ' anomaly]'
                  ENDELSE
               ENDIF
            ; integral
               IF field_int EQ 1 AND (size(fld))[0] EQ 1 THEN BEGIN
                  filleg = filleg+' - integral'
               ENDIF
            ; inverse trend
               IF strmid(cmd.trend, 0, 1) EQ '3' THEN BEGIN
                  filleg = filleg+'  [n='+strmid(cmd.trend, 1, strlen(cmd.trend)-1)+ ' mean removed]'
               ENDIF
            ; running std dev
               IF run_stddev GT 0 THEN BEGIN
                  filleg = filleg+' running Std Dev ['+string(run_stddev, format = '(I3)')+']'
               ENDIF
