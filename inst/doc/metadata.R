## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(dplyr)
library(tidyr)
library(magrittr)
library(Tplyr)
library(knitr)
load("adsl.Rdata")

## ----table_creation-----------------------------------------------------------
t <- tplyr_table(adsl, TRT01P, where = SAFFL == "Y") %>% 
  add_layer(
    group_count(RACE)
  ) %>% 
  add_layer(
    group_desc(AGE, where = EFFFL == "Y")
  )

dat <- t %>% build(metadata=TRUE)

kable(dat)

## ----meta_subset--------------------------------------------------------------
get_meta_subset(t, 'c2_1', 'var1_Placebo') %>% 
  kable()

## ----add_vars-----------------------------------------------------------------
get_meta_subset(t, 'c2_1', 'var1_Placebo', add_cols = vars(USUBJID, SEX)) %>% 
  kable()

## ----desc_stats---------------------------------------------------------------
get_meta_subset(t, 'd1_2', 'var1_Xanomeline High Dose') %>% 
  head(10) %>% 
  kable()

## ----tplyr_meta---------------------------------------------------------------
get_meta_result(t, 'd1_2', 'var1_Xanomeline High Dose')

## ----unpack-------------------------------------------------------------------
m <- get_meta_result(t, 'd1_2', 'var1_Xanomeline High Dose')

adsl %>% 
  filter(!!!m$filters) %>% 
  select(!!!m$names) %>% 
  head(10) %>% 
  kable()

## ----to string print, eval=FALSE----------------------------------------------
#  cat(c("adsl %>%\n",
#    "   filter(\n      ",
#    paste(purrr::map_chr(m$filters, ~ rlang::as_label(.)), collpase=",\n      "),
#    ") %>%\n",
#    paste("   select(", paste(purrr::map_chr(m$names, rlang::as_label), collapse=", "), ")", sep="")
#  ))

## ----to string content, results='asis', echo=FALSE----------------------------
cat(c("adsl %>%\n",
  "   filter(\n      ",
  paste(purrr::map_chr(m$filters, ~ rlang::as_label(.)), collpase=",\n      "),
  ") %>%\n",
  paste("   select(", paste(purrr::map_chr(m$names, rlang::as_label), collapse=", "), ")", sep="")
))

## ---- out.width=850, out.extra='style="border: 1px solid #464646;" allowfullscreen="" allow="autoplay"', echo=FALSE----
knitr::include_app("https://michael-stackhouse.shinyapps.io/Tplyr-shiny-demo/", height = "900px")

