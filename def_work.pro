;+
;
; reads each command line
;
; calls <propost_it>plt_map</pro_post_it> for each plot/window/overlay
;
; @param DATA_BASE_LIST {in}{required}{type=string}
;
; @param OUT_PS {in}{required}{type=string}
;
; @param CMDLINE {in}{required}{type=array of strings}
;
; @param OUT_ALL {in}{required}{type=string}
;
; @param OTHER_FILE {in}{required}{type=string}
;
; @param SPEC_BASE_LIST {in}{required}{type=string}
;
; @examples
; IDL> data_base_list='local:.'
; IDL> out_ps='./'
; IDL> cmdline=['temp_1 0 HH1 umath xy 1m 199201 - 1 1 v']
; IDL> out_all='-'
; IDL> other_file='-'
; IDL> spec_base_list='local:.'
; IDL> def_work, data_base_list, out_ps, cmdline, out_all, other_file, spec_base_list
;
; @uses
; <pro>common</pro>
; <propost_it>com_eg</propost_it>
;
; <propost_it>decode_cmd</propost_it>
; <propost_it>plt_map</propost_it>
;
; @todo
; get rid of spawn
;
; realistic examples
;
; test if it is allowed to open and write ./.hist_post_it
;
; what is the purpose of  ./.hist_post_it ?
;
; @history
; - fplod 20091209T094630Z aedon.locean-ipsl.upmc.fr (Darwin)
;
;   * check parameters
;
; - 11-6-99 EG
;
; @version
; $Id: def_work.pro 211 2010-03-12 15:38:28Z ericg $
;
;-
PRO def_work, data_base_list, out_ps, cmdline, out_all, other_file, spec_base_list
;
  compile_opt idl2, strictarrsubs
;
@common
@com_eg

; Return to caller if errors
 ON_ERROR, 2
;
 IF debug_w THEN BEGIN
  info = report('enter ...')
 ENDIF
;
 usage='def_work, data_base_list, out_ps, cmdline, out_all, other_file, spec_base_list'
;
 nparam = N_PARAMS()
 IF (nparam LT 6) THEN BEGIN
    ras = report(['Incorrect number of arguments.' $
          + '!C' $
          + 'Usage : ' + usage])
    stop
 ENDIF
 arg_type = size(data_base_list,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type data_base_list' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 arg_type = size(out_ps,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type out_ps' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 arg_type = size(cmdline,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type cmdline' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 arg_type = size(out_all,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type out_all' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 arg_type = size(other_file,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type other_file' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF
 arg_type = size(spec_base_list,/type)
 IF (arg_type NE 7) THEN BEGIN
   ras = report(['Incorrect arg type spec_base_list' $
          + '!C' $
          + 'Usage : ' + usage])
   stop
 ENDIF

; Data bases
   data_bases = data_base_list
   spec_bases = spec_base_list

;
; Graphic init
; set plot graphic defaults
   resolve_routine, 'plt_def'
   plt_def

; other inits
   fld_flag = 1
;
; other input file ?
;
IF other_file NE '-' THEN BEGIN

   resolve_routine, other_file
   res = execute(other_file)

   cmdline = cmdline2

ENDIF

cmdline_main = cmdline

;
; Field init
;
   fld_prev = '-'
   movie_count = 0

; Open history file
;
   get_lun, nulhis
   openw, nulhis, '.hist_post_it'
;
;
; Interpret command lines
;
   nlines = n_elements(cmdline)
   IF nlines LE 1 THEN BEGIN
    info = report('cmdline empty so nothing to be plot',/SIMPLE)
    stop
   ENDIF
;
   iline = 0
   iplot = 0

   WHILE iline LE nlines-2 DO BEGIN
;      IF debug_w THEN print, '    iline = ', iline, ' ', cmdline(iline)

      cmd = decode_cmd(cmdline, iline)

      IF out_all NE '-'  THEN BEGIN
       cmd.out = out_all
      ENDIF

      IF cmd.on EQ 1 THEN BEGIN
         IF debug_w THEN BEGIN
          print, '    iline = ', iline
         ENDIF
         iplot = iplot + 1
         print, ' '
         print, ' -------------'
         print, ' Plot No.', iplot, format = '(A10,I2)'
         print, ' -------------'
         print, ' '
         printf, nulhis, ' '
         printf, nulhis, ' -------------'
         printf, nulhis, ' Plot No.', iplot, format = '(A10,I2)'
         printf, nulhis, ' -------------'
         printf, nulhis, ' '


         ; format of output

         posP = rstrpos(cmd.disp, 'P')
         posL = rstrpos(cmd.disp, 'L')
         len = strlen(cmd.disp)
         IF posP GE 0 THEN BEGIN
            landscape = 0
            key_portrait = 1
         ENDIF ELSE BEGIN
            landscape = 1
            key_portrait = 0
         ENDELSE

         IF strmid(cmd.out, 0, 2) EQ 'ps' THEN BEGIN
            reinitplt, /z,/invert
            fileps = 'idl_out_p'+strtrim(string(iplot), 2)+'.ps'
            openps, out_ps+fileps
            set_ps_devices
         ENDIF ELSE BEGIN
            set_plot,dev_type
            set_x_devices
         ENDELSE

         ; windows management : decode cmd.disp = n[xm][or]

         display = cmd.disp
         IF posP GT 0 OR posL GT 0 THEN BEGIN
          display = strmid(cmd.disp, 0, len-1)
         ENDIF
         posx = rstrpos(display, 'x')
         IF posx GT 0 THEN BEGIN
            nwin = long(strmid(display, 0, posx))
            mwin = long(strmid(display, posx+1, len-posx+1))
         ENDIF ELSE BEGIN
            IF posP GT 0 THEN BEGIN
               nwin = 1
               mwin = long(display)
            ENDIF ELSE BEGIN
               nwin = long(display)
               mwin = 1
            ENDELSE
         ENDELSE
         nwin_tot = nwin*mwin

         ; plot inits

         idx_pal = 0
         iwin = 1
         nb_lines = 0

         ; make loop on number of windows
         WHILE iwin LE nwin*mwin DO BEGIN

            index_over = 0

            win = [nwin, mwin, iwin]

            idx = iline + nb_lines

            cmdi = decode_cmd(cmdline, idx)
            cmdm = cmdi

            ; specific formatting for legend
            leg_format = ''
            IF strpos(cmdi.proj, '[') NE -1 THEN BEGIN
               leg_format = extract_str(cmdi.proj, '[', ']')
               cmdi.proj = strmid(cmdi.proj, 0, strpos(cmdi.proj, '['))
            ENDIF ELSE BEGIN
               leg_format = default_txt_format
            ENDELSE

            ; decode proj for number of overlays in window (min=1)
            overl = strpos(cmdi.proj, 'o')
            IF overl GE 0 THEN BEGIN
               nover = 1+max([1, long(strmid(cmdi.proj, overl+1, strlen(cmdi.proj)-overl-1))])
            ENDIF ELSE BEGIN
               nover = 1
            ENDELSE

            nadd = nover

            ; make loop on number of overlays
            iover = 1
            inext = 0
            WHILE iover LE nover DO BEGIN

               idx = iline + nb_lines + iover + inext - 1
               cmdo = decode_cmd(cmdline, idx)
               idx_main=idx

               IF debug_w THEN BEGIN
                  print, ' In def_work:'
                  print, ' plot,window,overlay, inext = ', iplot, iwin, iover, inext
                  print, '     max win, max over = ', nwin, nover
                  print, ' index = ', idx
               ENDIF

               ; make overlay

               plt_map, cmdo, iplot, win, iover, landscape

               ; end of loop on overlays
               iover = iover+1
               ; special case y=f(next) on 2 lines

               IF strpos(cmdi.var, '(next)') GT -1 THEN BEGIN
                  inext = inext + 1
                  nadd = nadd + 1
               ENDIF ELSE BEGIN
                  inext = 0
               ENDELSE

            ENDWHILE
            nb_lines = nb_lines + nadd


         ; end of loop on windows
            iwin = iwin + 1
         ENDWHILE


         ; close ps

         IF strmid(cmd.out, 0, 2) EQ 'ps' THEN BEGIN
            iodir = out_ps
            closeps
            ; save to file
            IF save_ps GE 1 THEN BEGIN
                  CASE file_naming OF
                     'prompt': BEGIN ; interactive mode
                        IF strmid(cmd.out, 0, 3) ne 'psm' OR  movie_count EQ 0 THEN BEGIN
                        ; open last name file
                           get_lun, nullst
                           openr, nullst, '.last_name_post'
                           dir_name = ' '
                           file_name = ' '
                           readf, nullst, dir_name
                           readf, nullst, file_name
                           close, nullst
                           free_lun, nullst
                           print, ' Save PostScript '+strtrim(string(iplot), 2)+' file to ( - to ignore / save_ps=0 to turn off - NO EXTENSIONS PLEASE) :'
                           print, '  directory is (d to change) : '+dir_name
                           nfile_name = xquestion('       ', file_name, /chkwid)
                           nfile_name = strtrim(nfile_name, 2)
                           IF nfile_name NE '-' THEN BEGIN
                              IF nfile_name EQ 'd' THEN BEGIN
                                 dir_name = xquestion(' New Directory ', dir_name, /chkwid)
                                 nfile_name = xquestion(' New file', file_name, /chkwid)
                              ENDIF
                              openw, nullst, '.last_name_post'
                              printf, nullst, dir_name
                              printf, nullst, nfile_name
                              close, nullst
                              free_lun, nullst
                           ENDIF ELSE BEGIN
                              print, '  Do not save - OK'
                           ENDELSE
                           IF strmid(cmd.out, 0, 3) eq 'psm' THEN BEGIN
;              movie stuff (slide 1)
                              print, '  movie series, will create file_<n> files'
                              file_suffix = '_'+strtrim(string(movie_count+1), 2)
                              movie_count = movie_count + 1
                           ENDIF ELSE BEGIN
                              file_suffix = ''
                           ENDELSE
                        ENDIF ELSE BEGIN
;              movie stuff (slides >1 )
                           get_lun, nullst
                           openr, nullst, '.last_name_post'
                           dir_name = ' '
                           file_name = ' '
                           readf, nullst, dir_name
                           readf, nullst, nfile_name
                           close, nullst
                           free_lun, nullst
                           file_suffix = '_'+strtrim(string(movie_count+1), 2)
                           movie_count = movie_count + 1
                           print, '  saving file <base name>= '+nfile_name+file_suffix
                        ENDELSE
                     END
                     ELSE: BEGIN ; auto mode
;                        def_out_name, fileps >>> TO IMPLEMENT (analyse content of plots for proper naming)
                        dir_name = out_ps
                        nfile_name = cmd.exp+'_'+ cmd.var+'_'+cmd.plt+'_'+cmd.timave+'_'+cmd.date1+'_'+cmd.spec+'_'+cmd.disp
                        file_suffix = '_p'+strtrim(string(iplot), 2)
                     END
                  ENDCASE
                  ; conversion (pdf or gif)
                  IF save_ps EQ 2 THEN BEGIN
                                ; PDF
                     conv = 'ps2pdf  -g6000x8300 '
                     ;;IF landscape EQ 1 THEN BEGIN
                     ;;   conv = conv+'-rotate -90 '
                     ;;ENDIF
                     print, '  converting to pdf...'
                     ext = '.pdf'

                  ENDIF ELSE BEGIN
                     conv = 'cp '
                     ext = '.ps'
                  ENDELSE
                  IF strpos(cmd.out, 'gif') NE -1 THEN BEGIN
                     conv = 'convert '
                     IF landscape EQ 1 THEN BEGIN
                        conv = conv+'-rotate -90 '
                     ENDIF
                     print, '  converting to gif... to make the movie use:'
                     print, '     convert -delay 50 -loop 100 image_?.gif image_??.gif image_???.gif movie.gif'
                     ext = '.gif'
                  ENDIF

                  line = conv+out_ps+'/'+fileps+' '+dir_name+'/'+nfile_name+file_suffix+ext
                  IF debug_w THEN BEGIN
                   print, '  Convert command (set in def_work.pro) = ', line 
                  ENDIF
                  spawn, line, prtout
                  print, '   '
                  print, ' -> saving file ', dir_name+nfile_name+file_suffix+ext

            ENDIF
            IF strlen(cmd.out) EQ 3 AND strmid(cmd.out, 0, 3) NE 'psm' THEN BEGIN
               CASE cmd.out OF
                  'psb': BEGIN & mess = 'printer' & cmdf = prt_BW & END
                  'psc': BEGIN & mess = 'color printer' & cmdf = prt_col & END
                  'pst': BEGIN & mess = 'transp printer' & cmdf = prt_tra & END
                  ELSE: BEGIN & mess = 'ghostview ' & cmdf = ghost & END
               ENDCASE
               print, ' '
               print, '    Sending ', nfile_name+'_'+cmdf, ' to ', mess, ' (',cmdf, ' + option ',lp_opt, ')'
               line = 'cd '+out_ps+'; \cp '+fileps+' '+fileps+'_'+cmdf+'; '+homedir+'bin/'+cmdf+' '+fileps+'_'+cmdf+' '+lp_opt+'; cd '+hom_idl
               spawn, line, prtout
               print, prtout

            ENDIF ELSE BEGIN
               mess = 'ps'
            ENDELSE
         ENDIF ELSE BEGIN

           ; NO ps: next plot/data

            CASE cmd.out OF
               'cdf':
               ELSE: BEGIN
                  ready = ''
                  read,'<Return> for next plot ', ready
               END
            ENDCASE
         ENDELSE

      ENDIF ELSE BEGIN
         nb_lines = 1
      ENDELSE

      ; end of loop on command lines
      iline = iline + nb_lines

   ENDWHILE

   ; close history file

   free_lun, nulhis
   close, nulhis

   IF debug_w THEN BEGIN
    info = report('leaving ...')
   ENDIF

END
