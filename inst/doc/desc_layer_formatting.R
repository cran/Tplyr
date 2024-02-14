## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(tidyverse) 
library(magrittr)
library(Tplyr)
library(knitr)

## ----missing------------------------------------------------------------------
tplyr_adsl$TRT01P <- as.factor(tplyr_adsl$TRT01P)
tplyr_adlb$TRTA <- as.factor(tplyr_adlb$TRTA)

tplyr_adlb_2 <- tplyr_adlb %>% 
  filter(TRTA != "Placebo")

tplyr_table(tplyr_adlb_2, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01P) %>% 
  add_layer(
    group_desc(AVAL, by=PARAMCD) %>% 
      set_format_strings('Mean (SD)' = f_str('xxx (xxx)', mean, sd))
  ) %>% 
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>% 
  kable()

## ----missing1-----------------------------------------------------------------
tplyr_table(tplyr_adlb_2, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01P) %>% 
  add_layer(
    group_desc(AVAL, by=PARAMCD) %>% 
      set_format_strings('Mean (SD)' = f_str('xxx.xx (xxx.xxx)', mean, sd, empty=c(.overall="MISSING")))
  ) %>% 
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>% 
  kable()

## ----missing2-----------------------------------------------------------------
tplyr_table(tplyr_adlb_2, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01P) %>% 
  add_layer(
    group_desc(AVAL, by=PARAMCD) %>% 
      set_format_strings('Mean (SD)' = f_str('xxx.xx (xxx.xxx)', mean, sd, empty=c("NA")))
  ) %>% 
  build() %>% 
  head() %>%
  select(-starts_with("ord")) %>%
  kable()

## ----autoprecision1-----------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd)
      )
  ) %>% 
  build() %>% 
  head(20) %>% 
  select(-starts_with("ord")) %>% 
  kable()

## ----autoprecision2-----------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
        cap = c(int=3, dec=2)
      )
  ) %>% 
  build() %>% 
  head(20) %>% 
  select(-starts_with("ord")) %>%
  kable()

## ----precision3---------------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(vars(AVAL, CHG, BASE), by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd, empty="NA"),
        cap = c(int=3, dec=2)
      ) %>% 
      set_precision_on(AVAL) %>% 
      set_precision_by(PARAMCD)
  ) %>%
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>%
  kable()

## ----external-precision-------------------------------------------------------
prec_data <- tibble::tribble(
  ~PARAMCD, ~max_int, ~max_dec,
  "BUN",   1, 0,
  "CA",    2, 4,
  "CK",    3, 1,
  "GGT",   3, 0,
  "URATE", 3, 1,
)
  
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd, empty="NA")
      ) %>% 
      set_precision_on(AVAL) %>% 
      set_precision_by(PARAMCD) %>%
      set_precision_data(prec_data)
  ) %>%
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>%
  kable()


## ----external-precision2------------------------------------------------------
prec_data <- tibble::tribble(
  ~PARAMCD, ~max_int, ~max_dec,
  "BUN", 1, 0,
  "CA",  2, 4,
  "CK",  3, 1,
  "GGT", 3, 0,
)
  
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd, empty="NA")
      ) %>% 
      set_precision_on(AVAL) %>% 
      set_precision_by(PARAMCD) %>%
      set_precision_data(prec_data, default="auto")
  ) %>%
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>%
  kable()

## ----standard-----------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)", where= SAFFL=="Y") %>% 
      set_format_strings(
        "n"        = f_str("xx", n),
        "Mean (SD)"= f_str("xx.x (xx.xx)", mean, sd),
        "Median"   = f_str("xx.x", median),
        "Q1, Q3"   = f_str("xx, xx", q1, q3),
        "Min, Max" = f_str("xx, xx", min, max),
        "Missing"  = f_str("xx", missing)
      )
  ) %>% 
  build() %>% 
  select(-starts_with('ord'))

## ----manual_hugging-----------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA, PARAMCD == "CK") %>% 
  add_layer(
    group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>% 
      set_format_strings(
        TEST = f_str("xxx.x (XXX.x)", mean, sd, empty="NA")
      ) %>% 
      set_precision_by(PARAMCD)
  ) %>% 
  build() %>% 
  head() %>% 
  select(-starts_with('ord'))

## ----auto_hugging-------------------------------------------------------------
tplyr_table(tplyr_adlb, TRTA, PARAMCD == "CK") %>% 
  add_layer(
    group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>% 
      set_format_strings(
        TEST = f_str("a.a (A.a)", mean, sd, empty="NA")
      ) %>% 
      set_precision_by(PARAMCD)
  ) %>% 
  build() %>% 
  head() %>% 
  select(-starts_with('ord'))

