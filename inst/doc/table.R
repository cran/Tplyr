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

## ----table_params1------------------------------------------------------------
tplyr_table(adsl, TRT01P, where= SAFFL =="Y", cols = SEX) %>% 
  add_layer(
    group_count(RACE, by = "Race")
  ) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()

## ----table_params2------------------------------------------------------------
tplyr_table(adsl, TRT01P, where= SAFFL =="Y", cols = vars(SEX, RACE)) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()

## ----treat_grps---------------------------------------------------------------
tplyr_table(adsl, TRT01P) %>%
  add_treat_grps('Treated' = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>% 
  add_total_group() %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()

## ----pop_data1----------------------------------------------------------------
t <- tplyr_table(adae, TRTA, where = AEREL != "NONE") %>% 
  set_pop_data(adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  set_pop_where(TRUE) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID)
  )
  
t %>% 
  build() %>% 
  kable()

## ----pop_data2----------------------------------------------------------------
header_n(t) %>% 
  kable()

