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
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT))
  ) %>%
  build() %>%
  head(20) %>%
  kable()

## -----------------------------------------------------------------------------
tplyr_adlb$ANRIND <- factor(tplyr_adlb$ANRIND, levels=c("L", "N", "H"))
tplyr_adlb$BNRIND <- factor(tplyr_adlb$BNRIND, levels=c("L", "N", "H"))
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT))
  ) %>%
  build() %>%
  head(20) %>%
  kable()

