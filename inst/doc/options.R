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
load("adae.Rdata")
load("adsl.Rdata")
load("adlb.Rdata")
op <- options()

## ----options_table, echo=FALSE------------------------------------------------
suppressMessages(read_csv('tplyr_options.csv')) %>% 
  kable(align="cl", col.names = c('Option', 'Description'))

## ----default_formats1, eval=FALSE---------------------------------------------
#  options(
#      # Count layer defaults
#    tplyr.count_layer_default_formats =
#      list(n_counts = f_str("xxx (xx%)", n, pct),
#           riskdiff = f_str('xx.xxx', dif)
#           ),
#  
#    # Desc layer defaults
#    tplyr.desc_layer_default_formats =
#      list("n"        = f_str("xxx", n),
#           "Mean (SD)"= f_str("a.a+1 (a.a+2)", mean, sd),
#           "Median"   = f_str("a.a+4", median)
#           ),
#  
#    # Shift layer defaults
#    tplyr.shift_layer_default_formats = list(f_str("xxx", n))
#  )
#  

## ----default_formats2---------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>% 
  add_layer(
    group_count(AGEGR1, by = "Categorical Age Groups")
  ) %>% 
  build() %>% 
  kable()
  

## ----scoping1-----------------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  set_count_layer_formats(n_counts = f_str("xx (xxx%)", n, pct)) %>% 
  set_desc_layer_formats("Mean (SD)" = f_str("a.a+1 (a.a+2)", mean, sd)) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  add_layer(
    group_count(AGEGR1, by = "Categorical Age Groups")
  ) %>% 
  add_layer(
    group_count(ETHNIC, by = "Ethnicity") %>% 
      set_format_strings(f_str("xxxxx (xx.xxx%)", n, pct))
  ) %>% 
  build() %>% 
  kable()

## ----precision_cap1-----------------------------------------------------------
options(tplyr.precision_cap = c('int'=2, 'dec'=2))

## ----precision_cap2-----------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(HEIGHTBL, by = "Height at Baseline") %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a (a.a+1)', mean, sd)
      )
  ) %>% 
  add_layer(
    group_desc(HEIGHTBL, by = "Height at Baseline (Limited)") %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a (a.a+1)', mean, sd),
        cap = c('int'= 1, 'dec'=0)
      )
  ) %>% 
  build() %>% 
  kable()


## ----custom_summaries1--------------------------------------------------------
options(tplyr.custom_summaries = rlang::quos(
  geometric_mean = exp(sum(log(.var[.var > 0]),na.rm=TRUE) / length(.var))
))


## ----custom_summaries2--------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE) %>% 
      set_format_strings('Geometric Mean' = f_str('xx.xx', geometric_mean))
  ) %>% 
  build() %>% 
  kable()

## ----scipen1------------------------------------------------------------------
options(scipen = 0) # This is the default
.0001

options(scipen = 1) # Require 5 decimal places instead

.0001
.00001

options(scipen = -1) # Only require 3 decimal places
.001


## ----reset_settings1, include=FALSE-------------------------------------------
options(op)

## ----scipen2------------------------------------------------------------------
options(tplyr.scipen = -3)
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      add_risk_diff(c('Xanomeline Low Dose', 'Placebo'))
  )

suppressWarnings(build(t)) %>% # Chi-squared warnings occur with small samples
  head() %>% 
  kable()

## ----reset_settings2, include=FALSE-------------------------------------------
options(op)

## ----quantile1----------------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(CUMDOSE) %>% 
      set_format_strings("Q1, Q3" = f_str('xxxxx, xxxxx', q1, q3))
  ) %>% 
  build() %>%
  kable()

## ----quantile2----------------------------------------------------------------
options(tplyr.quantile_type = 3)

tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(CUMDOSE) %>% 
      set_format_strings("Q1, Q3" = f_str('xxxxx, xxxxx', q1, q3))
  ) %>% 
  build() %>% 
    select(-starts_with("ord")) %>% 
  kable()

