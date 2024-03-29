## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE, setup--------------------------------------------------
library(Tplyr)
library(dplyr, warn.conflicts = FALSE)
library(knitr)

## -----------------------------------------------------------------------------
tplyr_table(tplyr_adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx (xx.x%)', distinct_n, distinct_pct))
  ) %>% 
  build() %>% 
  head() %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_table(tplyr_adae, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>%
  set_pop_treat_var(TRT01A) %>%
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx (xx.x%)', distinct_n, distinct_pct))
  ) %>% 
  build() %>% 
  head() %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_adsl <- tplyr_adsl %>% 
  mutate(DCSREAS = ifelse(DCSREAS == '', 'Completed', DCSREAS))
         
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(DCSREAS)
  ) %>% 
  build() %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(DCSREAS, by=SEX)
  ) %>% 
  build() %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(DCSREAS, by=SEX) %>% 
      set_denoms_by(SEX, TRT01P)
  ) %>% 
  build() %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, AVISIT)) %>%
      set_format_strings(f_str("xx (xxx.x%)", n, pct)) %>%
      # This is the default, the 3x3 box formed by the target variables
      set_denoms_by(TRTA, PARAM, AVISIT) 
  ) %>%
  build() %>%
  kable()

## -----------------------------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, AVISIT)) %>%
      set_format_strings(f_str("xx (xxx.x%)", n, pct)) %>%
      set_denoms_by(TRTA, PARAM, AVISIT, BNRIND) # Each row made by TRTA, BNRIND
  ) %>%
  build() %>%
  arrange(ord_layer_1, ord_layer_2, ord_layer_3) %>% 
  head() %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA, where = PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, AVISIT)) %>%
      set_format_strings(f_str("xx (xx.xx%)", n, pct)) %>%
      set_denoms_by(TRTA, ANRIND) # Use the column total as the denominator
  ) %>%
  build() %>%
  arrange(ord_layer_1, ord_layer_2, ord_layer_3) %>% 
  head() %>%
  kable()

## -----------------------------------------------------------------------------
tplyr_adsl2 <- tplyr_adsl %>% 
  mutate(DISCONTEXT = if_else(DISCONFL == 'Y', 'DISCONTINUED', 'COMPLETED'))

t <- tplyr_table(tplyr_adsl2, TRT01P, where = SAFFL == 'Y') %>%
  add_layer(
    group_count(DISCONTEXT)
  ) %>%
  add_layer(
    group_count(DCSREAS, where = DISCONFL == 'Y')
  ) %>%
  add_layer(
    group_count(DCSREAS, where = DISCONFL == 'Y') %>% 
    set_denom_where(TRUE)
  ) %>%
  build() %>%
  arrange(ord_layer_index, ord_layer_1) 

t %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_adae2 <- tplyr_adae
tplyr_adae2[sample(nrow(tplyr_adae2), 50), "AESEV"] <- NA

t <- tplyr_table(tplyr_adae2, TRTA) %>%
  add_layer(
    group_count(AESEV) %>%
      set_format_strings(f_str("xxx (xx.xx%)", n, pct)) %>%
      set_missing_count(f_str("xxx", n), sort_value=Inf, denom_ignore=TRUE, Missing = NA)
  ) %>%
  build() %>% 
  arrange(ord_layer_1)

t %>% 
  kable()

## ----missing_subs1------------------------------------------------------------
  missing_subs <- tplyr_table(tplyr_adae, TRTA) %>%
    set_pop_data(tplyr_adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) %>%
        set_nest_count(TRUE) %>% 
        set_distinct_by(USUBJID) %>%
        add_missing_subjects_row(f_str("xx (XX.x%)", distinct_n, distinct_pct), sort_value = Inf) %>% 
        set_missing_subjects_row_label("Missing Subjects")
    ) %>%
    build()

  tail(missing_subs) %>% 
    select(-starts_with('ord')) %>% 
    kable()

## -----------------------------------------------------------------------------
tplyr_adsl2 <- tplyr_adsl
tplyr_adsl2[sample(nrow(tplyr_adsl2), 50), "AGEGR1"] <- NA

tplyr_table(tplyr_adsl2, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by=SEX) %>% 
      set_denoms_by(TRT01P, SEX) %>%  # This gives me a Total row each group
      add_total_row(f_str("xxx", n), count_missings=TRUE, sort_value=-Inf) %>% 
      set_total_row_label("All Age Groups") %>% 
      set_missing_count(f_str("xx (xx.x%)", n, pct), Missing = NA, sort_value=Inf)
  ) %>% 
  build() %>% 
  arrange(ord_layer_1, ord_layer_2) %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_table(tplyr_adsl2, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by=SEX) %>% 
      set_denoms_by(TRT01P, SEX) %>%  # This gives me a Total row each group
      add_total_row(f_str("xxx", n), count_missings=FALSE, sort_value=-Inf) %>% 
      set_total_row_label("All Age Groups") %>% 
      set_missing_count(f_str("xxx", n), Missing = NA, sort_value=Inf, denom_ignore=TRUE)
  ) %>% 
  build() %>% 
  arrange(ord_layer_1, ord_layer_2) %>% 
  kable()

