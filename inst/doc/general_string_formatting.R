## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE, setup--------------------------------------------------
library(Tplyr)
library(dplyr)
library(tidyr)

## ----example_1----------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE) %>% 
      set_format_strings(
        f_str("xx (xx.x%)", n, pct)
      )
  ) %>% 
  add_layer(
    group_desc(AGE) %>% 
      set_format_strings(
        "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd)
      )
  ) %>% 
  build() %>% 
  select(1:3)

## ----example_2----------------------------------------------------------------
f_str("xx (xx.x%)", n, pct)

## ----example_3----------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE) %>% 
      set_format_strings(
        f_str("xx (xx.x%)", n, pct)
      )
  ) %>% 
  build() %>% 
  select(1:3)

## ----example_4----------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE) %>% 
      set_format_strings(
        f_str("x (x.x%)", n, pct)
      )
  ) %>% 
  build() %>% 
  select(1:3)

## ----example_5----------------------------------------------------------------

tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE) %>% 
      set_format_strings(
        f_str("xx (XX.x%)", n, pct)
      )
  ) %>% 
  build() %>% 
  select(1:3)

## ----example_6----------------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD %in% c("CA", "URATE")) %>% 
  add_layer(
    group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a (a.a+1)', mean, sd)
      ) %>% 
      set_precision_by(PARAMCD)
  ) %>% 
  build() %>% 
  select(1:5)

## ----example_7----------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE) %>% 
      set_format_strings(f_str("a (xxx.x%)", n, pct))
  ) %>% 
  build() %>% 
  select(1:3)

## ----example_8----------------------------------------------------------------
tplyr_table(tplyr_adae, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_format_strings(f_str("a (XX.x%) [A]", distinct_n, distinct_pct, n)) %>% 
      set_distinct_by(USUBJID)
  ) %>% 
  build() %>% 
  select(1:3)

