;+
;
; Read macro definition
;
; @todo
; get rid of spawn
;
; should be a function
;
; @version
; $Id: get_macro_def.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-
   IF debug_w THEN BEGIN
    info = report('enter ...')
   ENDIF

; name of macro file
   file_mac = hom_def+'fld_macros.def'
   IF debug_w THEN BEGIN
    print, '    var_name in get_macro_def: ', var_name
   ENDIF
;
; special case for stddev monthly
   stddev_mth = '00'
   pos_underscore = strpos(var_name,'_')
   IF pos_underscore GE 0 THEN BEGIN
      stddev_mth = STRMID(var_name, pos_underscore + 1, 6)
      var_name = STRMID(var_name, 0, pos_underscore)
   ENDIF

; find definition of macro
   spawn, 'grep -i " '+STRMID(var_name, 2, 16)+' " '+file_mac, line
   line = strcompress(strtrim(line[0], 2))
   length = strlen(line)
   IF debug_w THEN BEGIN
    print, '    line: ', line
   ENDIF

   IF length EQ 0 THEN BEGIN
      print, ' *** WARNING : No macro definition for field ', var_name, $
       ' in file ', file_mac
      stop
   ENDIF


; Separate macro def and legend+unit
;
   leg_pos = strpos(line,'<')
   line_def = STRMID(line,0,leg_pos)
   line_def = strcompress(strtrim(line_def[0], 2))
   macro_def = strsplit(line_def, ' ', /EXTRACT)
   nfields = n_elements(macro_def)
;
; extract legend and unit
;
   legend = extract_str(line, '<', '>')
   unit = extract_str(line, '[', ']')

   IF debug_w THEN BEGIN
    info = report('leaving ...')
   ENDIF
