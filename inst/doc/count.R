## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE, setup--------------------------------------------------
library(Tplyr)
library(dplyr, warn.conflicts = FALSE)
library(knitr)
load("adsl.Rdata")
load("adae.Rdata")
load("adlb.Rdata")

## -----------------------------------------------------------------------------
t <- tplyr_table(adsl, TRT01P, where = SAFFL == "Y") %>%
  add_total_group() %>%
  add_treat_grps(Treated = c("Xanomeline Low Dose", "Xanomeline High Dose")) %>%
  add_layer(
    group_count(DCDECOD) %>%
      set_order_count_method("bycount") %>%
      set_ordering_cols(Total)
  ) %>%
  build() %>%
  arrange(desc(ord_layer_1)) %>%
  select(starts_with("row"), var1_Placebo, `var1_Xanomeline Low Dose`,
         `var1_Xanomeline High Dose`, var1_Treated, var1_Total)

kable(t)

## -----------------------------------------------------------------------------
t <- tplyr_table(adae, TRTA) %>%
  add_layer(
    group_count(AEDECOD) %>%
      set_distinct_by(USUBJID) %>%
      set_format_strings(f_str("xxx (xx.xx%) [xxx]", distinct_n, distinct_pct, n))
  ) %>%
  build() %>%
  head()

kable(t)

## -----------------------------------------------------------------------------
t <- tplyr_table(adae, TRTA) %>%
  add_layer(
    group_count(AEDECOD) %>%
      set_distinct_by(USUBJID) %>%
      set_format_strings(f_str("xxx (XXX.xx%) [A]", distinct_n, distinct_pct, n))
  ) %>%
  build() %>%
  head() %>% 
  select(row_label1, `var1_Xanomeline Low Dose`)

t

## -----------------------------------------------------------------------------
tplyr_table(adae, TRTA) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD))
  ) %>%
  build() %>%
  head() %>% 
  kable()

## -----------------------------------------------------------------------------
tplyr_table(adae, TRTA) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% 
      set_nest_count(TRUE) %>% 
      set_indentation("--->")
  ) %>%
  build() %>%
  head() %>% 
  kable()

