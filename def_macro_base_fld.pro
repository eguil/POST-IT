;+
;
; @version
; $Id: def_macro_base_fld.pro 206 2010-01-26 10:33:28Z pinsard $
;
;-
                  var_name = cmd.var
                  @get_macro_def
                  IF debug_w THEN BEGIN
                   print, '   macro_def in def_file_suff_var =', macro_def
                  ENDIF
                  CASE macro_def[1] OF
                     '=:': BEGIN
                        ids1 = strpos(line, '\')
                        ids2 = strpos(line, '/')
                        IF ids1*ids2 GE 0 THEN BEGIN
                           macro_base_fld = extract_str(line, '\', '/')
                        ENDIF ELSE BEGIN
                           macro_base_fld = 'undefined'
                        ENDELSE
                     END
                     ELSE: macro_base_fld = macro_def[1]
                  ENDCASE
                  macro_base_fld_1 = (strsplit(macro_base_fld, ',', /EXTRACT))[0]
