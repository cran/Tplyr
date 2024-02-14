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

## ----table_params1------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P, where= SAFFL =="Y", cols = SEX) %>% 
  add_layer(
    group_count(RACE, by = "Race")
  ) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()

## ----table_params2------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P, where= SAFFL =="Y", cols = vars(SEX, RACE)) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()

## ----treat_grps---------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>%
  add_treat_grps('Treated' = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>% 
  add_total_group() %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()

## ----pop_data1----------------------------------------------------------------
t <- tplyr_table(tplyr_adae, TRTA, where = AEREL != "NONE") %>% 
  set_pop_data(tplyr_adsl) %>% 
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

## ----data_comp, echo=FALSE----------------------------------------------------
kable(head(tplyr_adpe))

## ----data_comp1---------------------------------------------------------------
tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_count(AVALC, by = vars(PARAM, AVISIT))
  ) %>% 
  build() %>% 
  select(-starts_with('ord')) %>% 
  head(18) %>% 
  kable()


## ----data_comp2---------------------------------------------------------------
tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_count(AVALC, by = vars(PARAM, AVISIT)) %>% 
      set_limit_data_by(PARAM, AVISIT)
  ) %>% 
  build() %>% 
  select(-starts_with('ord')) %>% 
  head(12) %>% 
  kable()

## ----data_comp3---------------------------------------------------------------
tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_count(AVALC, by = vars(PARAM, AVISIT)) %>% 
      set_limit_data_by(PARAM, AVISIT, AVALC)
  ) %>% 
  build() %>% 
  select(-starts_with('ord')) %>% 
  kable()

