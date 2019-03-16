library(devtools)
library(usethis)
load_all()

sitetool_path <- "tools/Site Tool_Tanzania_20190315103207.xlsx"

readxl::excel_sheets(sitetool_path)

#import TX tab
  df_tx <-
    import_tab(sitetool_path, "TX") %>%
    add_groupings() %>%
    convert_na()

#generate TX_CURR distribution
  tx_curr_dist <- gen_distro(df_tx, `TX_CURR.N.Age/Sex/HIVStatus.20T`, psnu, Mechanism, ageCoarse, Age, Sex)

#apply TX_CURR distribution to relevant indicators
  df_tx_adj <- apply_distro(df_tx, tx_curr_dist,
                         `TX_CURR.N.Age/Sex/HIVStatus.20T`,
                         `TX_PVLS.D.Age/Sex/Indication/HIVStatus.20T.Routine`,
                         `TX_PVLS.N.Age/Sex/Indication/HIVStatus.20T.Routine`)
#generate TX_NEW distribution
  tx_new_dist <- gen_distro(df_tx, `TX_NEW.N.Age/Sex/HIVStatus.20T`, psnu, Mechanism, ageCoarse, Age, Sex)

#apply TX_NEW distribution to relevant indicators
  df_tx_adj <- apply_distro(df_tx_adj, tx_new_dist,
                     `TX_NEW.N.Age/Sex/HIVStatus.20T`)
#clean
  df_tx_clean <- df_tx_adj %>%
    round_values() %>%
    convert_zeros() %>%
    match_orig()

#export TX tab
  #TODO




#import CXCA tab
  df_cxca <-
    import_tab(sitetool_path, "CXCA") %>%
    add_groupings() %>%
    convert_na()

#apply TX_CURR distribution to CXCA indicators
  df_cxca_adj <- apply_distro(df, tx_curr_dist, `CXCA_SCRN.N.Age/Sex/HIVStatus.20T`) %>%
    round_values() %>%
    convert_zeros()

#import PMTCT tab
  df_pmtct <-
    import_tab(sitetool_path, "PMTCT_STAT_ART") %>%
    add_groupings() %>%
    convert_na()



#OVC
  df_ovc <-
    import_tab(sitetool_path, "OVC") %>%
    add_groupings() %>%
    convert_na()

  order <- names(df_ovc)

  df_ovc_lng <- df_ovc %>%
    tidyr::gather(indicatorcode, target, dplyr::starts_with("OVC"))

  df_ovc_lng_dist <- df_ovc_lng %>%
    dplyr::filter(indicatorcode != "OVC_HIVSTAT.N.total.20T") %>%
    gen_distro(target, psnu, Mechanism, ageCoarse, Age, Sex, indicatorcode)

 df_ovc_adj <- df_ovc_lng %>%
    apply_distro(df_ovc_lng_dist, target) %>%
    round_values() %>%
    dplyr::mutate(hiv_stat_vals = ifelse(!Age %in% c("18+", NA), target, NA)) %>%
    dplyr::group_by(psnu, sitename, Mechanism) %>%
    dplyr::mutate(target = ifelse(indicatorcode == "OVC_HIVSTAT.N.total.20T" & is.na(Age),
                               sum(hiv_stat_vals, na.rm = TRUE), target)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-hiv_stat_vals) %>%
    tidyr::spread(indicatorcode, target, fill = 0) %>%
    dplyr::select(order) %>%
    convert_zeros()


#TX_TB & TB_PREV

 df_tb_tx_prev <-
   import_tab(sitetool_path, "TB_TX_PREV") %>%
   add_groupings() %>%
   convert_na()

 tx_new_dist_coarse <- aggr_group(tx_new_dist, distro, psnu, Mechanism, ageCoarse, Sex)

 tb_tx_prev_vars <- dplyr::select_if(tb_tx_prev, is.numeric) %>% names()

 df_tb_adj <- apply_distro(tb_tx_prev, tx_new_dist_coarse, tb_vars)


#TB_STAT

 df_tb_stat <-
   import_tab(sitetool_path, "TB_STAT_ART") %>%
   add_groupings() %>%
   convert_na()

 #generate TX_NEW distribution
   df_tb_stat_dist <- gen_distro(df_tb_stat, `TB_STAT.D.Age/Sex.20T`, psnu, Mechanism, ageCoarse, Age, Sex)

 #apply TX_NEW distribution to relevant indicators
   df_tb_stat_vars <- dplyr::select_if(df_tb_stat, is.numeric) %>% names()

   df_tb_stat_adj <- apply_distro(df_tb_stat, df_tb_stat_dist,
                                  df_tb_stat_vars)
 #clean
   df_tb_stat_clean <- df_tb_stat_adj %>%
     round_values() %>%
     convert_zeros() %>%
     match_orig()








 #HTS

  df_hts <-
    import_tab(sitetool_path, "HTS") %>%
    add_groupings() %>%
    convert_na()


  df_hts_tidy <- df_hts %>%
    tidyr::gather(indicatorcode, target, dplyr::starts_with("HTS")) %>%
    clean_indicators() %>%
    dplyr::filter(stringr::str_detect(indicator, "HTS_(TST|INDEX)"))

  df_tx_aggr <- aggr_group(df_tx,`TX_NEW.N.Age/Sex/HIVStatus.20T`, psnu, Mechanism, ageCoarse, Age, Sex) %>%
    dplyr::rename(tx_new = `TX_NEW.N.Age/Sex/HIVStatus.20T`) %>%
    dplyr::left_join(tx_new_dist, by = c("psnu", "Mechanism", "ageCoarse", "Age", "Sex"))

  df_hts_aggr <- aggr_group(df_hts_tidy, target, psnu, Mechanism, ageCoarse, Age, Sex) %>%
    dplyr::rename(hts_tst = target)

  df_combo <- dplyr::full_join(df_tx_aggr, df_hts_aggr, by = c("psnu", "Mechanism", "ageCoarse", "Age", "Sex") )


