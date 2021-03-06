; Extract ensemble member(s) as requested (could be done in nc_read via boxzoom: less memory used)
; included in data_read.pro


   ensbl_legend_member = ' '
   IF debug_w THEN BEGIN 
      print, '  -> in ens_data_extract.pro'
      print, '     ensbl_code = ', ensbl_code
      print, '     ensbl_diff_other = ', ensbl_diff_other
      print, '     toggle_ensbl_diff = ', toggle_ensbl_diff
      print, '     ensbl_code_diff = ', ensbl_code_diff
   ENDIF 
   IF ensbl_code NE '' AND toggle_ensbl_diff EQ 0 THEN BEGIN
      IF ensbl_diff_other EQ 1 THEN BEGIN
         toggle_ensbl_diff = 1
      ENDIF ELSE BEGIN 
         toggle_ensbl_diff = 0
      ENDELSE 

     ; decode ensbl_code = m<Mi/a/e>m<Nj/a/e> a=all, e=ensemble mean (mema not relevant)
      nmods = ensbl_def.models
      nens = ensbl_def.members
      nmchar = strtrim(string(nmods), 2)
      nechar = strtrim(string(nens), 2)
      ntotchar = strtrim(string(nmods*nens), 2)
      dim = (size(fldr.data))[0]
      idxm0 = findgen(nens)

      CASE ensbl_code OF
         'mama': BEGIN ; keep all N members and all M models
            tab = fldr.data
            ensbl_legend = nmchar+' models /'+nechar+' members)'
            ensbl_legend_member = ' all mods/memb'
            ensbl_dim = nmods*nens
         END 
         'meme': BEGIN ; ensemble mean of all M models
            tab = total(temporary(fldr.data), 3, /NaN)/float(nens*nmods) 
            ensbl_legend = 'multimodel ensemble mean ('+ntotchar+' members)'
            ensbl_legend_member = ' ens mean'
            ensbl_dim = 1
            jpk = 1 & jpdta = 1 & jpkglo = 1 
            CASE dim OF
               3: BEGIN 
                  tab = reform(tab, nxt, nyt)
               END 
               4: BEGIN 
                  tab = reform(tab, nxt, nyt, jpt)
               END 
            ENDCASE 
         END 
         'mame': BEGIN ; compute ensemble mean for each model
            CASE dim OF
               3: BEGIN 
                  tab = fldr.data[*, *, idxm0]
                  tab = reform(total(temporary(tab), 3, /NaN)/float(nens), nxt*nyt)
                  imod = 1
                  WHILE imod LE nmods -1 DO BEGIN 
                     tabc = fldr.data[*, *, idxm0+imod*nens]
                     tab = [tab, reform(total(temporary(tabc), 3, /NaN)/float(nens), nxt*nyt)]
                     imod = imod + 1
                  ENDWHILE
                  tab = reform(tab, nxt, nyt, nmods)
               END 
               4: BEGIN  
                  tab = fldr.data[*, *, idxm0, *]
                  tab = reform(total(temporary(tab), 3, /NaN)/float(nens), nxt*nyt, jpt)
                  imod = 1
                  WHILE imod LE nmods -1 DO BEGIN 
                     tabc = fldr.data[*, *, idxm0+imod*nens, *]
                     tab = [tab, reform(total(temporary(tabc), 3, /NaN)/float(nens), nxt*nyt, jpt)]
                     imod = imod + 1
                  ENDWHILE 
                  tab = reform(tab, nxt, nyt, nmods, jpt)
               END 
            ENDCASE 
            ensbl_legend = nechar+' ensemble mean for each model'
            ensbl_legend_member = ' ens mean'
            ensbl_dim = nmods
            jpk = nmods & jpdta = nmods & jpkglo = nmods
         END 
         'mema': BEGIN ; not relevant
            print, ' OPTION mema not relevant !'
            return, -1
         END 
         ELSE: BEGIN ; select model(s)/member(s)
            argn = strsplit(ensbl_code, 'm', /EXTRACT)
            IF argn[1] EQ 'e' THEN BEGIN ; ensemble mean for model Mi
               imod = long(argn[0])
               cmd.exp = ensbl_def.model_names[imod-1]
               CASE dim OF
                  3: BEGIN
                     tab = fldr.data[*, *, idxm0+(imod-1)*nens]
                     tab = reform(total(temporary(tab), 3, /NaN)/float(nens), nxt, nyt)
                  END 
                  4: BEGIN  
                     tab = fldr.data[*, *, idxm0+(imod-1)*nens, *]
                     tab = reform(total(temporary(tab), 3, /NaN)/float(nens), nxt, nyt, jpt)
                  END 
               ENDCASE 
               lname = ensbl_def.model_names[argn[0]-1]
               ensbl_legend = nechar+' ensemble mean for model '+lname
               ensbl_legend_member = ' ens mean'
            ensbl_dim = 1
            jpk = 1 & jpdta = 1 & jpkglo = 1
            ENDIF ELSE IF argn[1] EQ 'a' THEN BEGIN ; all members for model Mi
               imod = long(argn[0])
               cmd.exp = ensbl_def.model_names[imod-1]
               CASE dim OF
                  3: BEGIN
                     tab = fldr.data[*, *, idxm0+(imod-1)*nens]
                  END 
                  4: BEGIN  
                     tab = fldr.data[*, *, idxm0+(imod-1)*nens, *]
                  END 
               ENDCASE 
               lname = ensbl_def.model_names[argn[0]-1]
               ensbl_legend = 'all members for model '+lname
               ensbl_legend_member = ' all memb'
               ensbl_dim = nens
               jpk = nens & jpdta = nens & jpkglo = nens
            ENDIF ELSE IF argn[0] EQ 'a' THEN BEGIN ; extract member Nj for all models
               iens = long(argn[1])
               print, ' mam<Nj> not implemented'
               tab = 0
               return, -1
            ENDIF ELSE BEGIN    ; extract member Nj of model Mi
               imod = long(argn[0])
               iens = long(argn[1])
               CASE dim OF
                  3: BEGIN
                     tab = reform(fldr.data[*, *, (imod-1)*nens+iens-1], nxt, nyt)
                  END 
                  4: BEGIN  
                     tab = reform(fldr.data[*, *, (imod-1)*nens+iens-1, *], nxt, nyt, jpt)
                  END 
               ENDCASE 
               lname = ensbl_def.model_names[argn[0]-1]
               ensbl_legend = 'model '+lname+'/member '+argn[1]
               ensbl_legend_member = ' #'+argn[1]
               ensbl_dim = 1
               jpk = 1 & jpdta = 1 & jpkglo = 1
            ENDELSE 
         END 
      ENDCASE  
      

      ; difference within ensemble

      IF ensbl_diff EQ 1 AND ensbl_diff_other NE 1 THEN BEGIN
         CASE ensbl_code_diff OF
            'mama': BEGIN       ; not relevant
               print, ' OPTION mama not relevant to remove!'
               return, -1
            END 
            'meme': BEGIN       ; remove ensemble mean of all M models
               tabd = total(temporary(fldr.data), 3, /NaN)/float(nens*nmods) 
               ensbl_legend = ensbl_legend+' minus multimodel ensemble mean'
               ensbl_legend_member = ensbl_legend_member+' - ens mean'
               CASE dim OF
                  3: BEGIN 
                     tabd = reform(tabd, nxt, nyt)
                  END 
                  4: BEGIN 
                     tabd = reform(tabd, nxt, nyt, jpt)
                  END 
               ENDCASE 
            END 
            'mame': BEGIN        ; not relevant
               print, ' OPTION mame not relevant to remove!'
               return, -1

            END 
            'mema': BEGIN       ; not relevant
               print, ' OPTION mema not relevant to remove!'
               return, -1
            END 
            ELSE: BEGIN         ; select model(s)/member(s)
               argn = strsplit(ensbl_code_diff, 'm', /EXTRACT)
               IF argn[1] EQ 'e' THEN BEGIN ; ensemble mean for model Mi
                  imod = long(argn[0])
                  CASE dim OF
                     3: BEGIN
                        tabd = fldr.data[*, *, idxm0+(imod-1)*nens]
                        tabd = reform(total(temporary(tabd), 3, /NaN)/float(nens), nxt, nyt)
                     END 
                     4: BEGIN  
                        tabd = fldr.data[*, *, idxm0+(imod-1)*nens, *]
                        tabd = reform(total(temporary(tabd), 3, /NaN)/float(nens), nxt, nyt, jpt)
                     END 
                  ENDCASE 
                  lname = ensbl_def.model_names[argn[0]-1]
                  ensbl_legend = ensbl_legend+' minus '+nechar+' ensemble mean for model '+lname
                  ensbl_legend_member = ensbl_legend_member+' - ens mean'
               ENDIF ELSE IF argn[1] EQ 'a' THEN BEGIN ; all members for model Mi not relevant
                  print, ' OPTION mam<j> not relevant to remove!'
                  return, -1

               ENDIF ELSE IF argn[0] EQ 'a' THEN BEGIN ; extract member Nj for all models
                  print, ' OPTION m<i>ma not relevant to remove!'
                  return, -1
               ENDIF ELSE BEGIN ; extract member Nj of model Mi
                  imod = long(argn[0])
                  iens = long(argn[1])
                  CASE dim OF
                     3: BEGIN
                        tabd = reform(fldr.data[*, *, (imod-1)*nens+iens-1], nxt, nyt)
                     END 
                     4: BEGIN  
                        tabd = reform(fldr.data[*, *, (imod-1)*nens+iens-1, *], nxt, nyt, jpt)
                     END 
                  ENDCASE 
                  lname = ensbl_def.model_names[argn[0]-1]
                  ensbl_legend = ensbl_legend+' minus model '+lname+'/member '+argn[1]
                  ensbl_legend_member = ensbl_legend_member+' - #'+argn[1]
               ENDELSE 
            END 
         ENDCASE  
         ; adapt dimension of array to remove (tabd) to ensbl case
         IF ensbl_dim GT 1 THEN BEGIN 
            CASE dim OF
               3: BEGIN 
                  tabd = reform((reform(tabd, nxt*nyt, /overwrite))#replicate(1, ensbl_dim), nxt, nyt, ensbl_dim)
               END 
               4: BEGIN 
                  tabd = reform((reform(tabd, nxt*nyt*jpt, /overwrite))#replicate(1, ensbl_dim), nxt, nyt, jpt, ensbl_dim)
                  tabd = transpose(tabd, [0, 1, 3, 2])
               END 
            ENDCASE 
         ENDIF 
         tab = tab - tabd
         undefine, tabd
         fldr.origin = 'diff'
      ENDIF 
      nzt = jpk & nzw = jpk
      firstzt = 0 &  firstwt = 0 & lastzt = jpk-1 & lastwt = jpk-1
      IF debug_w THEN BEGIN 
         print, '   size fldr.data entry ensbl_data_extract', size(fldr.data)
         print, '   size tab in ensbl_data_extract         ', size(tab)
         print, '   ensbl_legend in ensbl_data_extract         ', ensbl_legend
         print, '   ensbl_legend_member in ensbl_data_extract  ', ensbl_legend_member
      ENDIF 
      fldr = {name: fldr.name, data: tab, legend: fldr.legend, units: fldr.units, origin: fldr.origin, dim: size(tab, /N_DIMENSIONS) , direc: fldr.direc}
      IF debug_w THEN BEGIN 
         print, '   size fldr.data out ensbl_data_extract  ', size(fldr.data)
      ENDIF 
   ENDIF ELSE BEGIN 
      ensbl_dim = 0
   ENDELSE 
