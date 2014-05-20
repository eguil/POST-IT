;+
;
;  Read/compute monthly density projection to build annual (Paul Williams)
;
; @version
; $Id: densit_pltmap_read.pro 205 2010-01-26 09:46:13Z pinsard $
;
;-

      really_1m_st = 0
      start_year = cmd.date1
      end_year = cmd.spec
      boxst = def_box(cmd.plt, dimplot, boxname, time_stride)
      domdef, boxst
      grille,mask,glam,gphi,gdep,nx,ny,nz,firstx,firsty,firstz,lastx,lasty,lastz
      output = fltarr( nx, ny, (sig_max - sig_min)/sig_del + 1, uint(cmd.spec)-uint(cmd.date1)+1)
      FOR year_s = uint(cmd.date1), uint(cmd.spec) DO BEGIN
         print, 'plt_map: Constructing annual mean from monthly means for year', year_s
         cmd.timave = '1m'
         cumulative = 0.
         FOR i = 1, 12 DO BEGIN
            IF strlen(strmid(strcompress(string(year_s)),1)) eq 1 THEN BEGIN
             year_s_string = '0'+strmid(strcompress(string(year_s)),1)
            ENDIF
            IF strlen(strmid(strcompress(string(year_s)),1)) GT 1 THEN BEGIN
             year_s_string = strmid(strcompress(string(year_s)),1)
            ENDIF
            IF i LE 9 THEN BEGIN
               cmd.date1 = year_s_string+'0'+strtrim(string(i), 2)
            ENDIF ELSE BEGIN
               cmd.date1 = year_s_string+strtrim(string(i), 2)
            ENDELSE
            cmd.spec = cmd.date1
            print, 'Date = ', cmd.date1
            pltcmd = 'fields = data_read(cmd,'''+hotyp+''','''+plttyp+''','+string(dimplot)+','+string(iover)+all_data_str+', ZMTYP = '''+cmd.plt+''')'
            printf, nulhis, strcompress(pltcmd)
            printf, nulhis, ' '
            res = execute(pltcmd)
            cumulative = cumulative + fields.data
         ENDFOR
         output[*, *, *, year_s-uint(start_year)] = cumulative/12.
      ENDFOR
;   redefine cmd.* to be their initial values before the monthly loop
      cmd.timave = '1y'
      cmd.date1 = start_year
      cmd.spec = end_year
;   call to compute_time to redefine timearr (see data_read)
      timearr = compute_time(cmd.timave, cmd.date1, cmd.spec)
      jpt = timearr.count
      time = timearr.scale
      field = {name: fields.name, data: output, legend: fields.legend, units: fields.units, origin: fields.origin, dim: 2, direc: ''}
