;+
;
; @version
; $Id: com_eg.pro 246 2010-07-12 14:03:17Z ericg $
;
;-
;
; com_eg for post_it under IDL
;
; attributs horizontal+vertical means
;
COMMON zm_att, box_h, depth_z, zoom_z, diaznl_idx, box_plot, legbox, pres_max, pres_min, max_spec, spec_win, vert_type, vert_mean, vert_switch, glamboundary_box, msf_mean, name_level, nino_plot, spec_prev
;
; attributs hoevmoeller
;
COMMON hovmoel, trend_typ, calendar_type, nb_cycles, def_stride, time_stride, field_int, time_array, key_offset, ioverchk, hotypchk, fld_prev_t, c_correl, lag_correl, c_normal, lag_correlation, boot_win, run_stddev, tukey, time_prev, common_time, stddev_txt, time_domain, remove_1mm, rdat1, rdat2, rtime, rcmd
;
; attributs formats + div
;
COMMON formats, look, free_1d_minmax, lat_axis, cont_fill, multi_win, atmos_msk, line_thick, line_style, line_color, title_type, fill_space, vector_sample, symbol_style, symbol_color, symbol_families, contour_options, mean_sc_only, nc_grids_list, cont_real, symbol_size, xchartxt, ychartxt, linfit_sep, linfit_map, bin_interval1, bin_interval2, var3_ybinx
COMMON switches, cmd1_back, cmd2_back, file_name1, file_name2, ncdf_db1, ncdf_db2, grid1_full, grid2_full, corr_thres, large_domain, small_domain, file_naming

;
; attributs couleur
;
COMMON color2, shading, pal_type, grey_shade, grey_shade_1, grey_shade_2, col_palette, leg_txt_length
;
; unites + write switch
;
COMMON units, nulhis, write_data, marge_option, debug_w
;
; switchs de controle + grilles
;
COMMON switch0, meshlec_type, data_domain, data_dom_type, suff_domain, masked_data, mesh_type, mesh_type2, base_file_name, base_suffix, file_suff_var, pseudo_3d_msk
COMMON switch1, f_suffix, idx_main, cmdline_main, cmd2, sw_diffg, h_config_oce, cmd_wrk
COMMON switch2, h_config_oce_type, v_config_oce, v_config_oce_type, box_h_prev
COMMON switch3, orca_mask_version, orca_mask_version_type, read_grid_from_file, force_all_data_read
;
; printers
;
COMMON printers, prt_BW, prt_col, prt_tra, ghost, lp_opt, save_ps
;
; attributs bathymetrie
;
COMMON bathys, bathy_read, nbathys
;
; Legend common
;
COMMON legend, nwin_tot, default_txt_format, leg_format, nover, line_thick_txt, line_style_txt, colov
;
; attributs champs
;
COMMON fld_att, fldatt, field, fld_prev, index_over, fldatt_bak, cmd_prev, key_shift_map, vecplot, time1_r, time2_r, cmdline2, cmdm, macro_base_fld, datyp, datyp2, fldrem_t1, fldrem_t2, fld_flag, vargrid1, vargrid2, mean_sc, var_read_grd_file
;
; stats
;
COMMON stats_com, stddev_mth, stddev_diff
;
; gmt palettes
;
COMMON gmt_pal, ncont_gmt, levels_gmt, coul_gmt, max_gmt, idx_pal
;
;
COMMON directories2, hom_def, hom_idl, data_bases, dev_type, asciidir, spec_bases
;
; density projections
;
COMMON sigma_plots, splot, sig_min, sig_max, sig_del, sig_surf, sig_bowl, really_1m_st,  bin_read
;
; masks
;
COMMON masks, umaskr, vmaskr, fmask
;
; write to netCDF file attributes and grids
;
COMMON netCDF, x_att, y_att, z_att, t_att, global_attributes
;
; Ensembles common
;
COMMON ensembles, ensemble_base_description, ensbl_def, ensbl_code, ensbl_lt, ensbl_lt2, ensbl_legend, ensbl_mean, ensbl_thick_member, ensbl_thick_mean, ensbl_style_member, ensbl_style_mean, ensbl_color_member, ensbl_diff, ensbl_diff_other, toggle_ensbl_diff, ensbl_legend_member, ensbl_code_diff, ensbl_dim

