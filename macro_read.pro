;+
;
; Read Macro field Data
;
; @param FILE_NAME {in}{required}{type=string}
;
; @param VAR_NAME {in}{required}{type=string}
; variable name
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
; @keyword _EXTRA
;
; @returns
; structure
; -1 in case of error
;
; @examples
;
; IDL> file_name='ginette'
; IDL> var_name='temp_1'
; IDL> ncdf_db='local:./'
; IDL> result=macro_read(file_name, var_name, ncdf_db)
; IDL> help, result,/structure
; IDL> print,result
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; @todo
;
; realistic example
; error handling
;
; @history
; - fplod 20091210T095242Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; @version
; $Id: macro_read.pro 235 2010-06-25 08:46:06Z ericg $
;
;-
FUNCTION macro_read, file_name, var_name, ncdf_db $
         , BOXZOOM=boxzoom $
         , TIME_1=time_1 $
         , TIME_2=time_2 $
         , ALL_DATA=all_data $
         , _EXTRA=extra
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg
;
; Return to caller if errors
 ON_ERROR, 2
;
 IF debug_w THEN BEGIN
  info = report('enter ...')
  print, '    keyword_set(ALL_DATA) : ', keyword_set(ALL_DATA)
 ENDIF
;
 usage='result=macro_read(file_name, var_name, ncdf_db ' $
          + ', BOXZOOM=boxzoom' $
          + ', TIME_1=time_1' $
          + ', TIME_2=time_2' $
          + ', ALL_DATA=all_data' $
          + ', _EXTRA=extra'

 nparam = N_PARAMS()
 IF (nparam LT 3) THEN BEGIN
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

 arg_type = size(var_name,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type var_name' $
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
; 1) read macro definition of field
;
   @get_macro_def

   print, '    Computing macro : ', macro_def

; 2) loop to decode definition and build field

   CASE macro_def[1] OF
      '=:': BEGIN

         ; 2.1 call specific function

         ; extract base field if existant
         BEGIN
            ids1 = strpos(line, '\')
            ids2 = strpos(line, '/')
            IF ids1*ids2 GE 0 THEN BEGIN
               macro_base_fld = extract_str(line, '\', '/')
            ENDIF ELSE BEGIN
               macro_base_fld = 'undefined'
            ENDELSE
         END
         strcall = 'field_lec = '+macro_def[2]+'(file_name, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data, _EXTRA=extra)'
         IF debug_w THEN BEGIN 
            print, '---> in macro_read, cmd is: ', strcall
         ENDIF 
         res = execute(strcall)

         fieldo = {name: '', data: field_lec.data, legend: '', units: '', origin: '', dim: 0, direc:''}
         fieldo.name = var_name
         fieldo.legend = legend+field_lec.legend
         fieldo.units = unit
         fieldo.origin = field_lec.origin
         fieldo.dim = field_lec.dim
         fieldo.direc = field_lec.direc  ;;;;; causes problems for @@vosigthi because field_lec.direc is not defined in make_depth

      END
      ELSE: BEGIN

        ; 2.2 add fields

         mult = 1.
         first = 1
         FOR index = 2, nfields-1 DO BEGIN

            command = macro_def[index]

            IF command EQ '-' THEN BEGIN
               mult = -1.
            ENDIF ELSE IF command EQ '+' THEN BEGIN
               mult = 1.
            ENDIF ELSE BEGIN
               command2 = macro_def[(index+1) <(nfields-1) ]
;              print, 'command 2 = '+command2
               IF command2 EQ '*' THEN BEGIN
                  mult = mult*float(command)
                  index = index + 2
                  command = macro_def[index]
               ENDIF
               ; read field
               field_lec = nc_read(file_name, command, ncdf_db, BOXZOOM = boxzoom, TIME_1 = time_1, TIME_2 = time_2, ALL_DATA = all_data)
               IF first EQ 1 THEN BEGIN
                  first = 0
                  fieldo = {name: '', data: field_lec.data*mult, legend: '', units: '', origin: '', dim: 0, direc:''}
                  fieldo.name = var_name
                  fieldo.legend = legend
                  fieldo.units = unit
                  fieldo.origin = field_lec.origin+' macro'
                  fieldo.dim = field_lec.dim
                  fieldo.direc = field_lec.direc
               ENDIF ELSE BEGIN
                  ; check units
                  IF field_lec.units NE fieldo.units OR field_lec.dim NE fieldo.dim THEN BEGIN
                     print, ' *** WARNING in macro : units/dim mismatch : ',field_lec.units,' vs. ', fieldo.units,'  and ', field_lec.dim,' vs. ', fieldo.dim
                  ENDIF
                                ; add field
                  fieldo.data = fieldo.data + field_lec.data*mult
               ENDELSE
            ENDELSE

         ENDFOR

      END
   ENDCASE

   IF debug_w THEN BEGIN
    info = report('leaving ...')
   ENDIF

   return, fieldo
END
