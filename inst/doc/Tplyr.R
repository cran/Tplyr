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

## ----tplyr_table--------------------------------------------------------------
t <- tplyr_table(adsl, TRT01P, where = SAFFL == "Y")
t

## ----tplyr_layer--------------------------------------------------------------
cnt <- group_count(t, AGEGR1)
cnt

dsc <- group_desc(t, AGE)
dsc

shf <- group_shift(t, vars(row=COMP8FL, column=COMP24FL))
shf


## ----add_layer----------------------------------------------------------------
t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories n (%)")
  )


## ----add_layer_with_piping----------------------------------------------------
t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories n (%)") %>% 
      set_format_strings(f_str("xx (xx.x%)", n, pct)) %>% 
      add_total_row()
  )


## ----add_layers---------------------------------------------------------------
t <- tplyr_table(adsl, TRT01P) 

l1 <- group_count(t, AGEGR1, by = "Age categories n (%)")
l2 <- group_desc(t, AGE, by = "Age (years)")

t <- add_layers(t, l1, l2)

## ----build--------------------------------------------------------------------
t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories n (%)")
  )

t %>% 
  build() %>% 
  kable()


## ----get_numeric_data---------------------------------------------------------
get_numeric_data(t) %>% 
  head() %>% 
  kable()

## ----format_strings_1---------------------------------------------------------

t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)") %>% 
      set_format_strings(
        'n' = f_str('xx', n),
        'Mean (SD)' = f_str('xx.xx (xx.xxx)', mean, sd)
      )
  )

t %>% 
  build() %>% 
  kable()


## ----format_strings_2---------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories") %>% 
      set_format_strings(f_str('xx (xx.x)',n,pct))
  ) %>% 
  build() %>% 
  kable()

tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories") %>% 
      set_format_strings(f_str('xx',n))
  ) %>% 
  build() %>% 
  kable()


## ----format_strings_3---------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories") %>% 
      set_format_strings(f_str('xx (•◡•) xx.x%',n,pct))
  ) %>% 
  build() %>% 
  kable()

## ----desc1--------------------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>% 
  build() %>% 
  kable()

## ----custom_summaries---------------------------------------------------------
tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_desc(AGE, by = "Sepal Length") %>%
      set_custom_summaries(
        geometric_mean = exp(sum(log(.var[.var > 0]), na.rm=TRUE) / length(.var))
      ) %>%
      set_format_strings(
        'Geometric Mean (SD)' = f_str('xx.xx (xx.xxx)', geometric_mean, sd)
      )
  ) %>% 
  build() %>% 
  kable()


## ----desc2--------------------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(vars(AGE, AVGDD), by = "Age and Avg. Daily Dose")
  ) %>% 
  build() %>% 
  kable()


## ----count_total1-------------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories") %>% 
      add_total_row()
  ) %>% 
  build() %>% 
  kable()


## ----count_distinct-----------------------------------------------------------
tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count('Subjects with at least one adverse event') %>% 
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx', n))
  ) %>% 
  build() %>% 
  kable()


## ----count_nested-------------------------------------------------------------
tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD))
  ) %>% 
  build() %>% 
  head() %>% 
  kable()

## ----shift1-------------------------------------------------------------------
# Tplyr can use factor orders to dummy values and order presentation
adlb$ANRIND <- factor(adlb$ANRIND, c("L", "N", "H"))
adlb$BNRIND <- factor(adlb$BNRIND, c("L", "N", "H"))

tplyr_table(adlb, TRTA, where = PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row=BNRIND, column=ANRIND), by=PARAM) %>% 
      set_format_strings(f_str("xx (xxx%)", n, pct))
  ) %>% 
  build() %>% 
  kable()


