;+
;
; @todo
; should be a function
;
; @version
; $Id: def_file_suff_var.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
   IF debug_w THEN BEGIN
    info = report('enter ...')
    print, '   cmd=', cmd
   ENDIF

      macro_base_fld = cmd.var
      IF strpos(cmd.var, '@@') NE -1 THEN BEGIN
         CASE cmd.var OF
            '@@voenergy': BEGIN
               IF debug_w THEN BEGIN
                print, '   cmd.grid in def_file_suff_var = ', cmd.grid
               ENDIF
               CASE cmd.grid OF
                  'T': file_suff_var = '_votemper'
                  'T05': file_suff_var = '_votemper'
                 ELSE: file_suff_var = '_so'
               ENDCASE
            END
            ELSE: BEGIN
               IF strpos(cmd.var, '@@') NE -1 THEN BEGIN
                  @def_macro_base_fld
                  IF strpos(macro_base_fld, ',') NE -1 THEN BEGIN
                   macro_base_fld = (strsplit(macro_base_fld, ',', /EXTRACT))[0]
                  ENDIF
                  file_suff_var = '_'+macro_base_fld
               ENDIF ELSE BEGIN
                  print, ' Define base field for grid for macro in def_grid'
                  stop
               ENDELSE
            END
         ENDCASE
      ENDIF ELSE BEGIN
         IF strpos(cmd.var, '(next)') NE -1 THEN BEGIN
            idx = strpos(cmd.var, '=')
            var1 = strmid(cmd.var, 0, idx)
            sl_pos = strpos(cmd.var, '/')
            IF sl_pos NE -1 THEN BEGIN
             var1 = strmid(cmd.var, 0, sl_pos)
            ENDIF
            file_suff_var = '_'+var1
         ENDIF ELSE BEGIN
            file_suff_var = '_'+cmd.var
         ENDELSE
      ENDELSE
   IF debug_w THEN BEGIN
    print, '   file_suff_var = ', file_suff_var
    info = report('leaving...')
   ENDIF
