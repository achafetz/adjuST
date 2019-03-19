library(devtools)
library(usethis)
load_all()

sitetool_path <- "tools/Site Tool_Tanzania_20190315103207.xlsx"

readxl::excel_sheets(sitetool_path)




# TX_CURR, TX_NEW, CXCA, TX_NEW -------------------------------------------


  #import TX tab
    df_tx <-
      import_tab(sitetool_path, "TX") %>%
      add_groupings() %>%
      convert_na()

  #generate TX_CURR distribution
    tx_curr_dist <- gen_distro(df_tx, `TX_CURR.N.Age/Sex/HIVStatus.20T`,
                               psnu, Mechanism, ageCoarse, Age, Sex)

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

  #import CXCA tab
    df_cxca <-
      import_tab(sitetool_path, "CXCA") %>%
      add_groupings() %>%
      convert_na()

  #apply TX_CURR distribution to CXCA indicators
    df_cxca_clean <-
      apply_distro(df_cxca, tx_curr_dist, `CXCA_SCRN.N.Age/Sex/HIVStatus.20T`) %>%
      round_values() %>%
      convert_zeros()


  #clean up
    rm(df_tx_adj, tx_curr_dist, tx_new_dist, tx_adj, df_cxca)

# OVC_SERV ----------------------------------------------------------------

  #import OVC tab
    df_ovc <-
      import_tab(sitetool_path, "OVC") %>%
      add_groupings() %>%
      dplyr::mutate(ageCoarse = dplyr::case_when(Age == "18+" ~ "18+",
                                                 !is.na(Age)   ~ "<18")) %>%
      convert_na()

  #store header order for applying at end to clean up
    order <- names(df_ovc)

  #reshape long
    df_ovc_lng <- df_ovc %>%
      tidyr::gather(indicatorcode, target, dplyr::starts_with("OVC"))

    df_ovc_lng_dist <- df_ovc_lng %>%
      dplyr::filter(indicatorcode != "OVC_HIVSTAT.N.total.20T") %>%
      gen_distro(target, psnu, Mechanism, ageCoarse, Age, Sex, indicatorcode)


   #  df_ovc_lng %>%
   #    dplyr::filter(indicatorcode == "OVC_SERV.N.Age/Sex/ProgramStatus.20T.Active",
   #                  !is.na(ageCoarse)) %>%
   #    aggr_group(target, psnu, Mechanism, ageCoarse)
   #
   #
   # df_ovc_adj <- df_ovc_lng %>%
   #    apply_distro(df_ovc_lng_dist, target) %>%
   #    round_values()
   #
   # %>%
   #    dplyr::mutate(hiv_stat_vals = ifelse(!Age %in% c("18+", NA), target, NA)) %>%
   #    dplyr::group_by(psnu, sitename, Mechanism) %>%
   #    dplyr::mutate(target = ifelse(indicatorcode == "OVC_HIVSTAT.N.total.20T" & is.na(Age),
   #                               sum(hiv_stat_vals, na.rm = TRUE), target)) %>%
   #    dplyr::ungroup() %>%
   #    dplyr::select(-hiv_stat_vals) %>%
   #    tidyr::spread(indicatorcode, target, fill = 0) %>%
   #    dplyr::select(order) %>%
   #    convert_zeros()
   #


# TX_TB & TB_PREV ---------------------------------------------------------

  #import
     df_tb_tx_prev <-
       import_tab(sitetool_path, "TB_TX_PREV") %>%
       add_groupings() %>%
       convert_na()

  #adj distro to coarse
    tx_new_dist_coarse <- aggr_group(tx_new_dist, distro, psnu, Mechanism, ageCoarse, Sex)
    tx_curr_dist_coarse <- aggr_group(tx_curr_dist, distro, psnu, Mechanism, ageCoarse, Sex)

  #store names
    tb_tx_prev_vars <- dplyr::select_if(df_tb_tx_prev, is.numeric) %>% names()

  #reshape long
    df_tb_tx_prev_lng <- df_tb_tx_prev %>%
      tidyr::gather(indicatorcode, target, dplyr::starts_with("TX_"), dplyr::starts_with("TB_")) %>%
      clean_indicators()

    df_tb_tx_prev_lng %>%
      #gen_distro(df_tb_tx_prev_lng, target,  distro, psnu, Mechanism, ageCoarse, Sex)
      dplyr::group_by(psnu, Mechanism, ageCoarse, Age, Sex, numeratordenom, otherdisaggregate) %>%
      dplyr::summarize_at(dplyr::vars(target), sum, na.rm = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(psnu, Mechanism, ageCoarse, Age, Sex) %>%
      dplyr::mutate(distro = target / sum(target)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-target) %>%
      tidyr::spread(indicatorcode, distro)

  #apply distribuiton to TX_TB
   df_tb_tx_adj <- apply_distro(df_tb_tx_prev, tx_new_dist_coarse, tb_tx_prev_vars)

 #clean
 df_tb_tx_clean <- df_tb_tx_adj %>%
   round_values() %>%
   convert_zeros() %>%
   match_orig()



# TB_STAT -----------------------------------------------------------------


 df_tb_stat <-
   import_tab(sitetool_path, "TB_STAT_ART") %>%
   add_groupings() %>%
   convert_na()

 tb_order <- names(df_tb_stat)

 #generate TB_STAT_D distribution
   df_tb_stat_dist <- gen_distro(df_tb_stat, `TB_STAT.D.Age/Sex.20T`, psnu, Mechanism, ageCoarse, Age, Sex)

 #apply TB_STAT_D distribution to relevant indicators
   df_tb_stat_adj <- apply_distro(df_tb_stat, df_tb_stat_dist, `TB_STAT.D.Age/Sex.20T`)

 #determine distribution within TB_STAT N disaggs
   df_tb_stat_disagg_distro <- df_tb_stat_adj %>%
     tidyr::gather(indicatorcode, target, dplyr::starts_with("TB")) %>%
     dplyr::filter(stringr::str_detect(indicatorcode, "TB_STAT.N")) %>%
     #gen_distro(target, psnu, Mechanism, ageCoarse, Age, Sex, indicatorcode) %>%
     dplyr::group_by(psnu, Mechanism, ageCoarse, Age, Sex, indicatorcode) %>%
     dplyr::summarize_at(dplyr::vars(target), sum, na.rm = TRUE) %>%
     dplyr::ungroup() %>%
     dplyr::group_by(psnu, Mechanism, ageCoarse, Age, Sex) %>%
     dplyr::mutate(distro = target / sum(target)) %>%
     dplyr::ungroup() %>%
     dplyr::select(-target) %>%
     tidyr::spread(indicatorcode, distro)

   df_tb_stat_adj <- df_tb_stat_adj %>%
     dplyr::select(-dplyr::starts_with("TB_STAT.N")) %>%
     dplyr::left_join(df_tb_stat_disagg_distro) %>%
     dplyr::mutate_at(dplyr::vars(dplyr::starts_with("TB_STAT.N")), ~ `TB_STAT.D.Age/Sex.20T` * .) %>%
     dplyr::mutate(`TB_ART.N.Age/Sex/NewExistingART/HIVStatus.20T.Already` = `TB_STAT.N.Age/Sex/KnownNewPosNeg.20T.KnownPos`,
                   `TB_ART.N.Age/Sex/NewExistingART/HIVStatus.20T.New`  = `TB_STAT.N.Age/Sex/KnownNewPosNeg.20T.NewPos`)

 #clean
   df_tb_stat_clean <- df_tb_stat_adj %>%
     dplyr::select(tb_order) %>%
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
    dplyr::filter(stringr::str_detect(indicator, "HTS_TST"))

  df_tx_orig_aggr <- aggr_group(df_tx,`TX_NEW.N.Age/Sex/HIVStatus.20T`, psnu, Mechanism, ageCoarse, Age, Sex) %>%
    dplyr::rename(tx_new_orig = `TX_NEW.N.Age/Sex/HIVStatus.20T`) %>%
    dplyr::left_join(tx_new_dist, by = c("psnu", "Mechanism", "ageCoarse", "Age", "Sex"))

  df_tx_aggr <- df_tx_clean %>%
    add_groupings() %>%
    aggr_group(`TX_NEW.N.Age/Sex/HIVStatus.20T`, psnu, Mechanism, ageCoarse, Age, Sex) %>%
    dplyr::rename(tx_new = `TX_NEW.N.Age/Sex/HIVStatus.20T`)

  df_hts_aggr <- aggr_group(df_hts_tidy, target, psnu, Mechanism, ageCoarse, Age, Sex, resultstatus) %>%
    tidyr::spread(resultstatus, target, fill = 0) %>%
    dplyr::rename_at(dplyr::vars("Negative", "Positive"),
                     ~ paste0("hts_tst_", stringr::str_sub(. , end = 3) %>% tolower(.))) %>%
    dplyr::mutate(hts_tst = hts_tst_pos + hts_tst_neg)

  df_combo <- dplyr::full_join(df_tx_orig_aggr, df_tx_aggr, by = c("psnu", "Mechanism", "ageCoarse", "Age", "Sex")) %>%
    dplyr::full_join(df_hts_aggr, by = c("psnu", "Mechanism", "ageCoarse", "Age", "Sex"))

  df_combo <- df_combo %>%
    dplyr::mutate(linkage = tx_new_orig / hts_tst_pos,
                  hts_tst_pos_new = tx_new /linkage,
                  hts_tst_new = hts_tst_pos_new / (hts_tst_pos/hts_tst),
                  hts_tst_neg_new = hts_tst_new - hts_tst_pos_new)





  hts_mod_distro <- aggr_group(df_hts_tidy, target, psnu, Mechanism, ageCoarse, Age, Sex, resultstatus, modality) %>%
    gen_distro(target, psnu, Mechanism, ageCoarse, Age, Sex, resultstatus, modality) %>%
    dplyr::mutate(resultstatus = stringr::str_sub(resultstatus, end = 3)) %>%
    tidyr::unite(modstatus, c(modality, resultstatus), sep = "_") %>%
    tidyr::spread(modstatus, distro)



