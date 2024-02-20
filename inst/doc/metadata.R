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

## ----table_creation-----------------------------------------------------------
t <- tplyr_table(tplyr_adsl, TRT01P, where = SAFFL == "Y") %>% 
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

tplyr_adsl %>% 
  filter(!!!m$filters) %>% 
  select(!!!m$names) %>% 
  head(10) %>% 
  kable()

## ----to string print, eval=FALSE----------------------------------------------
#  cat(c("tplyr_adsl %>%\n",
#    "   filter(\n      ",
#    paste(purrr::map_chr(m$filters, ~ rlang::as_label(.)), collpase=",\n      "),
#    ") %>%\n",
#    paste("   select(", paste(purrr::map_chr(m$names, rlang::as_label), collapse=", "), ")", sep="")
#  ))

## ----anti_join1---------------------------------------------------------------
t <- tplyr_table(tplyr_adae, TRTA) %>%
  set_pop_data(tplyr_adsl) %>%
  set_pop_treat_var(TRT01A) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>%
      set_distinct_by(USUBJID) %>%
      add_missing_subjects_row(f_str("xx (XX.x%)", distinct_n, distinct_pct), sort_value = Inf)
  )

x <- build(t, metadata=TRUE)

tail(x) %>% 
  select(starts_with('row'), var1_Placebo) %>% 
  kable()

## ----anti_join2---------------------------------------------------------------
m <- get_meta_result(t, 'c23_1', 'var1_Placebo')
m

## ----anti_join3---------------------------------------------------------------
head(get_meta_subset(t, 'c23_1', 'var1_Placebo'))

## ----anti_join4---------------------------------------------------------------
head(get_meta_subset(t$metadata, 'c23_1', 'var1_Placebo', 
                     target=t$target, pop_data=t$pop_data))

## ----to string content, results='asis', echo=FALSE----------------------------
cat(c("tplyr_adsl %>%\n",
  "   filter(\n      ",
  paste(purrr::map_chr(m$filters, ~ rlang::as_label(.)), collpase=",\n      "),
  ") %>%\n",
  paste("   select(", paste(purrr::map_chr(m$names, rlang::as_label), collapse=", "), ")", sep="")
))

## ---- out.width=850, out.extra='style="border: 1px solid #464646;" allowfullscreen="" allow="autoplay"', echo=FALSE----
knitr::include_app("https://michael-stackhouse.shinyapps.io/Tplyr-shiny-demo/", height = "900px")

