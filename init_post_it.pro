;+
;
; post_it specific inits
;
; @uses
; <propost_it>com_eg</propost_it>
;
; @version
; $Id: init_post_it.pro 243 2010-07-08 15:43:46Z ericg $
;
;-
@com_eg
;
;---------------------------
; directory definitions
;----------------------------
   hom_idl = homedir
   hom_def = homedir+'config/'
   iodir=homedir[0]+'database/'
   asciidir = homedir+'out/'

   meshlec_type = '---'
   data_dom_type = 'global'
   data_dom_type = ' '
   h_config_oce_type = ' '
   v_config_oce_type = ' '
   orca_mask_version_type = ' '
   box_h_prev = [0, 0, 0, 0]
   cmd_prev = {var:'', on: 0, exp:'', grid:'',plt:'', timave:'', date1:'', spec:'', disp:'', proj:'', out:'', var2:''}
   force_all_data_read = 0
   var3_ybinx = ""
   stddev_txt = ""
   toggle_ensbl_diff = 0
   ensbl_diff_other = 0
   ensbl_legend_member = ''
   remove_1mm = 0

; plt_def default values

   debug_w = 0
   default_txt_format = 'E'
   xchartxt = 1.0
   ychartxt = 1.0
   linfit_sep = 0.0
   nino_plot = 0
   bin_interval = 0
   corr_thresh = 0.2
   common_time = 0
   large_domain = 0
   file_naming = 'auto'

