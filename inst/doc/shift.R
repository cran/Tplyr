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
tplyr_table(adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT))
  ) %>%
  build() %>%
  head(20) %>%
  kable()

## -----------------------------------------------------------------------------
adlb$ANRIND <- factor(adlb$ANRIND, levels=c("L", "N", "H"))
adlb$BNRIND <- factor(adlb$BNRIND, levels=c("L", "N", "H"))
tplyr_table(adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT))
  ) %>%
  build() %>%
  head(20) %>%
  kable()

