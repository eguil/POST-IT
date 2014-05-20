;+
;
; Binning plot y=bin(x)
; called by <propost_it>plt_map</propost_it>
;
; @todo
; should be a function
;
; @version
; $Id: ybinx.pro 216 2010-04-01 13:17:02Z ericg $
;
;-
   IF debug_w THEN BEGIN
      info = report('enter ...')
   ENDIF

               ;read in the bins
               IF large_domain EQ 1 THEN bin_interval =  bin_interval2 ELSE bin_interval =  bin_interval1
               
               ; organise density bins: either read array or build form single number and min/max in fld_glo_mmx.def
              
               IF (size(bin_interval))[0] EQ 1 THEN BEGIN 
                                ; build... 
               ENDIF 
               
               nbins = (size(bin_interval))[1]

               bin_interval = float(bin_interval)
               bin_plt = (float(bin_interval)+shift(float(bin_interval), -1))/2.
               bin_plt = [2*bin_plt[0]-bin_plt[1], bin_plt[0:nbins-2], 2*bin_plt[nbins-2]-bin_plt[nbins-3]]
               
               IF debug_w THEN print, '    bin_plt= ', bin_plt
               IF debug_w THEN print, '      nbins= ', nbins
               
               ; calculate larger domain mean of fld2
               ; (e.g. tropical Pacific) for each month
               large_mean = fltarr(12)
               im2 = 0
               IF large_domain EQ 1 THEN BEGIN
                  WHILE im2 LE 11 DO BEGIN 
                     idxt = (lindgen(jpt))[im2:*:12]
                     fld2m = fld2[*,*,idxt]
                     jpt =  jpt/12
                     large_mean[im2] =  grossemoyenne(fld2m,'xyt', boite = box_h)
                     jpt =  jpt*12
                     im2 =  im2+1
                  ENDWHILE
                  print, 'Mean monthly values of field in larger domain:', large_mean
                  
                  domdef, box_h
                  firstxt_l =  firstxt
                  lastxt_l =  lastxt
                  firstyt_l =  firstyt
                  lastyt_l =  lastyt
                  
                  box_small = def_box(small_domain, dimplot, legbox, time_stride)
                  print, box_small
                  domdef, box_small
                  firstxt_s =  firstxt
                  lastxt_s =  lastxt
                  firstyt_s =  firstyt
                  lastyt_s =  lastyt
                  
                  firstxt =  firstxt_s - firstxt_l ;(+1?)
                  lastxt = lastxt_s - firstxt_l
                  firstyt =  firstyt_s - firstyt_l
                  lastyt = lastyt_s - firstyt_l
                  
                  IF debug_w THEN print,  '    firstxt,firstyt,firstzt=', firstxt, firstyt, firstzt
                  IF debug_w THEN print,  '    lastxt,lastyt,lastzt=', lastxt, lastyt, lastzt   
                  
                  fld2 =  fld2[firstxt:lastxt, firstyt:lastyt, *]
                  
               ENDIF
              
               ; 3rd field to bin regression ?
         
               IF var3_ybinx NE "" AND var3_ybinx NE "w500" THEN sw3 = 1 ELSE sw3 = 0
               IF var3_ybinx EQ "w500" THEN sw32 = 1 ELSE sw32 = 0
            
         
               ; mask fields with NaN for regression computation

               IF sw3 THEN BEGIN 
                  idxmsk = where(fld GT valmask/10.)
                  IF idxmsk[0] NE -1 THEN fld[idxmsk] = valmask
                  IF idxmsk[0] NE -1 THEN fld3[idxmsk] = valmask
               ENDIF 

               ; print min/max of field for debug
               idxmskp = where(fld LE valmask/10.)
               idxmskp2 = where(fld2 LE valmask/10.)
               IF debug_w THEN print, '    Min/max fld= ', min(fld[idxmskp]), max(fld[idxmskp])
               IF debug_w THEN print, '    Min/max fld2= ', min(fld2[idxmskp2]), max(fld2[idxmskp2])

               ; remove mean seasonal cycle if required

               IF cmd.trend GT 0 THEN BEGIN
                  fld = trends(fld, cmd.trend, 'xyt')
                  IF sw3 THEN fld3 = trends(fld3, cmd.trend, 'xyt')
               ENDIF 
               IF cmd2.trend GT 0 THEN BEGIN
                  fld2 = trends(fld2, cmd2.trend, 'xyt')
               ENDIF 

               ; select months if required

               ntxt = "All months"
               
               IF stddev_mth NE '00' THEN BEGIN 
                  @mth_decode
                  
                  fldn = (fld)[*, *, reform(idxm[0,*], njpt)]
                  fld2n = (fld2)[*, *, reform(idxm[0,*], njpt)]
                  IF sw3 THEN fld3n = (fld3)[*, *, reform(idxm[0,*], njpt)]
                  
                  FOR imth = 1, nmth-1 DO BEGIN 
                     
                     fldn = [fldn, (fld)[*, *, reform(idxm[imth,*], njpt)]]
                     fld2n = [fld2n, (fld2)[*, *, reform(idxm[imth,*], njpt)]]
                     IF sw3 THEN fld3n = [fld3n, (fld3)[*, *, reform(idxm[imth,*], njpt)]]
                     
                  ENDFOR 
                  jpt = njpt
                  
                  fld = fldn
                  fld2 = fld2n
                  IF sw3 THEN fld3 = fld3n
               ENDIF   
               ; for now just 2d fields

               IF nzt NE 1 THEN BEGIN 
                  print, '***** 2D field only for now in ybinx ****'
                  
               ENDIF 
               ;----------------------------------------
               ;Alpha_SW regression binning - new method
               ;Create monthly arrays of point-wise
               ;regression and average fld2
               ;----------------------------------------
               
               IF sw3 THEN BEGIN
                  imr = 0 
                  pt_linfit = fltarr(nxt, nyt,12)
                  pt_err = fltarr(nxt, nyt, 12)
                  pt_corr = fltarr(nxt, nyt, 12)
                  pt_linfit[*, *, *] = 0
                  pt_err[*, *, *] = 1.1
                  pt_corr[*, *, *] = 0.
                  fld2mean = fltarr(nxt,nyt,12)

                  WHILE imr LE 11 DO BEGIN
                     idxr = (lindgen(jpt))[imr:*:12]
                     fldmr = fld[*,*,idxr]
                     fld2mr = fld2[*,*,idxr]
                     fld3mr = fld3[*,*,idxr]
                     
                     FOR idx = 0, nxt-1 DO FOR idy = 0, nyt-1 DO BEGIN
                        
                        IF fldmr[idx, idy, imr] LT valmask/10. THEN BEGIN 
                           
                           ;x1i = reform(fldmr(idx, idy, idxr), nval)
                           ;x2i = reform(fld3mr(idx, idy, idxr), nval)
                           x1i = (fldmr[idx, idy, *])
                           x2i = (fld3mr[idx, idy, *])
                           x1 = x1i-mean(x1i)
                           x2 = x2i-mean(x2i)
                       
                           coeffl = linfit(x2, x1, CHISQ = linerrl, PROB = proberrl, SIGMA = sigmaerrl)
                           correl = c_timecorrelate(x2,x1)
                           pt_linfit[idx, idy, imr]  = pt_linfit[idx, idy, imr]+coeffl(1)
                           pt_err[idx, idy, imr] = min(pt_err[idx, idy, imr], proberrl)
                           pt_corr[idx, idy, imr] = pt_corr[idx, idy, imr] + correl
                   
                        ENDIF ELSE BEGIN
                           pt_linfit[idx, idy, imr] = valmask
                           pt_err[idx, idy, imr] = 0.
                           pt_corr[idx, idy, imr] = 0.
                        ENDELSE
                        
                        IF (abs(pt_corr[idx, idy, imr]) LT corr_thres) THEN BEGIN
                           pt_linfit[idx,idy,imr] = !values.f_nan
                        ENDIF 
                        
                     ENDFOR
 
                     box_small = def_box(small_domain, dimplot, legbox, time_stride)
                     jpt = jpt/12
                     fld2mean[*,*,imr] =  grossemoyenne(fld2mr,'t') ;monthly array of mean fld2  
                     imr = imr+1
                     jpt = jpt*12
                  ENDWHILE
               ENDIF
              
               ; find indexes of var2 in each bin

               binpop = lonarr(nbins+1, 12)
               idxb = lonarr(nbins+1, nxt*nyt*jpt)
               im =  0 
               yplt = fltarr(nbins+1,12)
               yerr = fltarr(nbins+1, 12)
               mean_fld = fltarr(12)
               
               ;loop over months  
               WHILE im LE 11 DO BEGIN 
   
                  ;idxb[*,*] = !values.f_nan
                  idxt = (lindgen(jpt))[im:*:12]
                  ;fld2m = fld2(*,*,idxt)
                  ;fldm = fld(*,*,idxt)        
                  
                  IF sw3 THEN BEGIN 
                     fld2m =  fld2mean[*,*,im]
                     fldm =  pt_linfit[*,*,im]
                  ENDIF ELSE IF sw32 THEN BEGIN
                     fld3m = fld3[*,*,idxt]
                     fld2m = fld2[*,*,idxt]
                     fldm = fld[*,*,idxt]
                  ENDIF ELSE BEGIN
                     fld2m = fld2[*,*,idxt]
                     fldm = fld[*,*,idxt]
                  ENDELSE
                  
                  IF large_domain EQ 1 THEN BEGIN ;remove monthly large domain mean to bin anomalies
                     fld2m = fld2m - large_mean[im] 
                     print, 'Subtract mean of larger domain from bin values'
                  ENDIF   

                  ib = 1
                  
                  WHILE ib LE nbins-1 DO BEGIN 
                     
                     indices = where(fld2m GT bin_interval[ib-1] AND fld2m LE bin_interval[ib])
                     IF sw32 THEN BEGIN
                        indices2 = where(fld3m GT 0)
                        FOR count = 0,(n_elements(indices)-1) DO BEGIN
                           find = where(indices2 EQ indices[count])
                           IF find EQ [-1] THEN indices[count] = 0
                        ENDFOR
                        newindex = where(indices NE 0)
                        IF newindex NE [-1] THEN indices = indices[newindex]
                     ENDIF
                   
                     binpop[ib, im] = n_elements(indices) 
                     idxb[ib, 0:binpop[ib, im]-1] =  indices

                      ;idxb[ib, 0:binpop(ib, im)-1] =
                                ;where(fld2m GT bin_interval(ib-1) AND
                                ;fld2m LE bin_interval(ib))

                     
                     ib = ib + 1
                  ENDWHILE 
                  
                  index0 = where(fld2m LE bin_interval[0])
                  indexm = where(fld2m GT bin_interval[nbins-1])
                  ;indexm = n_elements(where(fld2m GT bin_interval(nbins-1))) 
                  
                  IF sw32 THEN BEGIN
                     indices3 = where(fld3m GT 0)
                     FOR count = 0,(n_elements(index0)-1) DO BEGIN
                        find = where(indices3 EQ index0[count])
                        IF find EQ [-1] THEN index0[count] = 0
                     ENDFOR
                     newindex0 = where(index0 NE 0)
                     IF newindex0 NE [-1] THEN index0 = index0[newindex0]
                     FOR count = 0,(n_elements(indexm)-1) DO BEGIN
                        find = where(indices3 EQ indexm[count])
                        IF find EQ [-1] THEN indexm[count] = 0
                     ENDFOR
                     newindexm = where(indexm NE 0)
                     IF newindexm NE [-1] THEN indexm = indexm[newindexm]
                  ENDIF

                  binpop[0, im] =  n_elements(index0)
                  idxb[0, 0:binpop[0, im]-1] = index0
       
                  binpop[nbins, im] = n_elements(indexm)
                  idxb[nbins, 0:binpop[nbins, im]-1] = indexm
                 
                 ; compute maximum number of indices for one bin
                  
                  max_idx = max(binpop)
                  
                  IF debug_w THEN print, '    Max bin size ', max_idx
                  
                  fldy = fltarr(nbins+1, max_idx, 12)
                  fldy[*, *, *] = !values.f_nan 
                  fldys = fltarr(nbins+1, max_idx, 12)
                  fldys[*, *, *] = !values.f_nan 
                  surfb = fltarr(nbins+1, max_idx, 12)
                  surfb[*, *, *] = !values.f_nan 
                  
                ;  IF sw3 THEN BEGIN 
                ;    fldy2 = fltarr(nbins+1, max_idx, 12)
                ;     fldy2[*, *, *] = !values.f_nan
                ;  ENDIF
               ; extract fld1/fld3 arrays in each bin

               ; first build fld*e1te2t for later average

                  surf = reform(e1t[firstxt:lastxt,firstyt:lastyt]*e2t[firstxt:lastxt,firstyt:lastyt], nxt*nyt)
                  surf = reform(surf#replicate(1, jpt), nxt, nyt, jpt)
                  flds = fldm*surf
                  
                  ib = 0
                  WHILE ib LE nbins DO BEGIN 
                     
                     binsz = binpop[ib, im] 
                     
                     IF binsz GT 1 THEN BEGIN 
                        fldy[ib, 0:binsz-1, im] = fldm[idxb[ib, 0:binsz-1]]
                        fldys[ib, 0:binsz-1, im] = flds[idxb[ib, 0:binsz-1]]
                        surfb[ib, 0:binsz-1, im] = surf[idxb[ib, 0:binsz-1]]
                                ;IF sw3 THEN fldy2(ib, 0:binsz-1, im) = fld3m(idxb(ib, 0:binsz-1))
                     ENDIF 
                     ib = ib + 1
                  ENDWHILE  
                  
                                ;IF NOT sw3 THEN BEGIN 
                  
                                ; if binning only fld1, compute average in each bin and plot
                                ; average
                  
                  ib = 0
                  print, '    Month: ', im+1
                  WHILE ib LE nbins DO BEGIN 
                     
                     binsz = binpop[ib, im]
                     
                     IF binsz GT 1 THEN BEGIN  
                   
                        sfc_tot = total(surfb[ib, 0:binsz-1, im], NAN = 1)
                        yplt[ib, im] = total(fldys[ib, 0:binsz-1, im], NAN = 1)/sfc_tot
                        
                        IF (n_elements(where(finite(fldy[ib, 0:binsz-1, im]) EQ 1))) GT 1 THEN BEGIN
                        yerr[ib, im] = sqrt((moment(fldy[ib, 0:binsz-1, im],  NAN = 1))[1])
                        ENDIF
                        mean_fld[im] = mean_fld[im] + yplt[ib,im]*float(binpop[ib,im]) 
                        
                        ; print bin info
                        
                        IF ib GT 0 AND ib LT nbins THEN print, '    Bin size, occurence, average: ', bin_interval[ib-1], bin_interval[ib], binpop[ib,im], (binpop[ib, im]/total(binpop[*, im]))*100., yplt[ib,im]
                        IF ib EQ 0 THEN print, '    Bin size, occurence, average:      min' , bin_interval[ib], binpop[ib,im], (binpop[ib,im]/total(binpop[*, im]))*100. , yplt[ib,im]
                        IF ib EQ nbins THEN print, '    Bin size, occurence, average: ', bin_interval[ib-1],'     max ', binpop[ib,im], (binpop[ib,im]/total(binpop[*, im]))*100. , yplt[ib,im]
                     ENDIF ELSE yplt[ib,im] = !values.f_nan
             
                     ib = ib + 1
                  ENDWHILE 
                 
                  mean_fld[im] = mean_fld[im]/total(binpop[*,im])
                  
                  print, '    Total bin population = ', total(binpop[*,im])
                  print, '    Mean field in domain = ', mean_fld[im]
                   
                  ;ENDIF ELSE BEGIN 
                     
                     ;---------------------------
                                ;Alpha_SW regression binning - old method
                                ;(regression inside the bin)
                     ;---------------------------

;                     ib = 0
;                     print, '    Month: ', im+1
;                    WHILE ib LE nbins DO BEGIN 
;                        
;                        binsz = binpop(ib, im)
;     
;                        IF binsz GT 1 THEN BEGIN 

                           ;idx1 = where(fldy(ib, 0:binsz-1, im) NE valmask)
                           ;idx2 = where(fldy2(ib, 0:binsz-1, im) NE valmask)
                           ;tab1 = (fldy(ib, 0:binsz-1, im))(idx1)
                           ;tab2 = (fldy2(ib, 0:binsz-1, im))(idx2)
                           ;coeff = linfit(tab2, tab1, CHISQ = linerr, PROB = proberr, SIGMA = sigmaerr)
                           ;yplt(ib, im) = coeff(1)
                           ;yerr(ib, im) = sigmaerr(1)
                           
                           ;mean_fld(im) = mean_fld(im) + yplt(ib, im)*float(binpop(ib, im)) 
                        
                        ; print bin info
                           ;IF ib GT 0 AND ib LT nbins THEN print, '    Bin size, occurence, regress.: ', bin_interval(ib-1), bin_interval(ib), binpop(ib, im), (binpop(ib, im)/total(binpop(*, im)))*100., yplt(ib, im)
                           ;IF ib EQ 0 THEN print, '    Bin size, occurence, regress.:      min' , bin_interval(ib), binpop(ib, im), (binpop(ib, im)/total(binpop(*, im)))*100. , yplt(ib, im)
                           ;IF ib EQ nbins THEN print, '    Bin size, occurence, regress.: ', bin_interval(ib-1),'     max ', binpop(ib, im), (binpop(ib, im)/total(binpop(*, im)))*100. , yplt(ib, im)
                           
                        ;ENDIF ELSE yplt(ib, im) = !values.f_nan
                        ;ib = ib + 1 
                     ;ENDWHILE   
                  
                     ;mean_fld(im) = mean_fld(im)/total(binpop(*, im))
                     
                     ;print, '    Total bin population      = ', total(binpop(*, im))
                     ;print, '    Mean regression in domain = ', mean_fld(im)
                    
                     im = im + 1 
                  ENDWHILE  
           
           ;Calculate average value in each bin                                                                                                                                                                                                                 
                  ib2 =  0
                  yplt2 = fltarr(nbins+1)
                  yerr2 = fltarr(nbins+1)
         
               WHILE ib2 LE nbins DO BEGIN
                  weight1 =  0
                  weight2 =  0
                  w1 =  0
                  w2 = 0
                  im3 =  0
                  WHILE im3 LE 11 DO BEGIN
                     w1 =  binpop[ib2,im3]*yplt[ib2,im3]
                     w2 =  binpop[ib2,im3]*(yerr[ib2,im3]^2)
                     IF w1 GT 0. OR w1 LT 0. THEN weight1 =  weight1 + w1 ;test for real value
                     IF w2 GT 0. OR w2 LT 0. THEN weight2 =  weight2 + w2
                     im3 =  im3+1
                  ENDWHILE
                  yplt2[ib2] = weight1/total(binpop[ib2,*])
                  yerr2[ib2] =  sqrt(weight2/total(binpop[ib2,*]))
                  IF yplt2[ib2] GT 0. THEN finalpib =  ib2
                  IF yplt2[ib2] EQ 0. THEN yplt2[ib2] = !values.f_nan  
                  ib2 = ib2+1
               ENDWHILE  
            
               ; Print mean
            
               print, '    1D times-serie info         : mean ', (moment(fld2))[0]
               
               ; plot               
              
               ;Add 0.5 to bin_plt to plot in middle of bin
          
               ;bin_plt =  bin_plt + 0.5
               
               IF NOT sw3 THEN BEGIN 
                  ; compute threshold by linear interpolation
                  
                  ;comment following 6 lines for LH binning
   
                  ;binp =  bin_plt(finalpib)
                  ;binn =  bin_plt(finalpib+1)
                  ;abovet =  yplt2(finalpib)
                  ;belowt =  yplt2(finalpib+1)
                  ;threshold =  binp + (abovet/(abovet-belowt))*(binn-binp)
                  ;print, '    The threshold is', threshold
           
                  ; compute 'convection occurrence'
                  
                  ;idx = where((fld2 - large_mean) GT threshold)
                  ;fldn = fld2(idx)
                  ;perc = float((size(fldn))(1))/float((size(fld2))(5))*100.               
            
                  ;commented out following 4 lines,  25/03/09
                  ;idx =  where(fld LT 0)
                  ;fldn = fld(idx)
                  ;perc = float((size(fldn))(1))/float((size(fld))(5))*100.  
                  ;print, '    Convection occurrence =', perc, ' %'
               ENDIF

                 ; specify plot attributes
               IF sw3 THEN BEGIN
                  varname = 'Regression of '+field.name+' and '+field.name3
               ENDIF ELSE BEGIN 
                  varname = varlegend2                     
               ENDELSE
               
               ; define text, line color, thickness and type
   
               boxybinx = def_box(cmd.plt, dimplot, leg_name, time_stride)
               vardate = date_txt
               varunit = ' ['+ntxt+' in box '+leg_name+']'
               
               overc = overlay_type(iover, dimplot)
               color1d = (strsplit(overc,',',/EXTRACT))[1]

               pltcmd = 'pltsc,yplt2,bin_plt,minc,maxc,minc2,maxc2,varlegend'+com_strplt+overc+',STY1D=-6,subtitle=""'
               
               printf, nulhis, strcompress(pltcmd)
               IF debug_w THEN print, '   ', pltcmd
               res = execute(pltcmd)

               @legend_overlay
               ; plot +/- 1 stdedv for field binning
               
               pltcmd = 'pltsc,yplt2-yerr2[*,0],bin_plt,minc,maxc,minc2,maxc2,varlegend'+com_strplt+','+color1d+',ov1d=1, thick=1, STY1D=-1,subtitle=""'
               res = execute(pltcmd)      
               pltcmd = 'pltsc,yplt2+yerr2[*,0],bin_plt,minc,maxc,minc2,maxc2,varlegend'+com_strplt+','+color1d+',ov1d=1, thick=1, STY1D=-1,subtitle=""'

               res = execute(pltcmd)
               
               IF debug_w THEN print, "    ... Exit ybinx"
