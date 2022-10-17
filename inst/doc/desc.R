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
load("adlb.Rdata")
load("adsl.Rdata")

## ----intro--------------------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
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
  kable()

## ----varnames, echo=FALSE-----------------------------------------------------
x <- data.frame(
  Statistic = c('N', 'Mean', "Standard Deviation", "Median", "Variance", "Minimum",
                "Maximum", "Interquartile Range", "Q1", "Q3", "Missing"),
  `Variable Names` = c("n", "mean", "sd", "median", "variance", "min", "max", "iqr", "q1", "q3", "missing"),
  `Function Call` = c("n()", "mean(.var, na.rm=TRUE)", "sd(.var, na.rm=TRUE)", "median(.var, na.rm=TRUE)",
                      "var(.var, na.rm=TRUE)", "min(.var, na.rm=TRUE)", "max(.var, na.rm=TRUE)", 
                      "IQR(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type')",
                      "quantile(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type'))[[2]]", 
                      "quantile(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type'))[[4]]", 
                      "sum(is.na(.var))")
)

x %>% 
  kable(align="ccl", col.names=c('Statistic', 'Variable Names', 'Function Call'))


## ----quantile_types_default---------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(CUMDOSE) %>% 
      set_format_strings("Q1, Q3" = f_str('xxxxx, xxxxx', q1, q3))
  ) %>% 
  build() %>%
  select(-starts_with("ord")) %>% 
  kable()

## ----quantile_types_sas-------------------------------------------------------
options(tplyr.quantile_type = 3)
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(CUMDOSE) %>% 
      set_format_strings("Q1, Q3" = f_str('xxxxx, xxxxx', q1, q3))
  ) %>% 
  build() %>% 
    select(-starts_with("ord")) %>% 
  kable()

## ----echo=FALSE---------------------------------------------------------------
options(tplyr.quantile_type=7)

## ----multi-custom-------------------------------------------------------------
tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_desc(vars(AGE, HEIGHTBL), by = "Sepal Length") %>%
      set_custom_summaries(
        geometric_mean = exp(sum(log(.var[.var > 0]), na.rm=TRUE) / length(.var))
      ) %>%
      set_format_strings(
        'Geometric Mean (SD)' = f_str('xx.xx (xx.xxx)', geometric_mean, sd)
      )
  ) %>% 
  build() %>% 
  select(-starts_with("ord")) %>% 
  kable()

## ----custom_options-----------------------------------------------------------
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE) %>% 
      set_format_strings("Mean" = f_str('xx.xx', mean))
  ) %>% 
  build() %>% 
  kable()

## ----custom_options_trimmed---------------------------------------------------
options(tplyr.custom_summaries = 
          rlang::quos(
            mean = mean(.var, na.rm=TRUE, trim=0.4)
          )
        )

tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE) %>% 
      set_format_strings("Mean" = f_str('xx.xx', mean))
  ) %>% 
  build() %>% 
  kable()

## ----missing------------------------------------------------------------------
adsl$TRT01P <- as.factor(adsl$TRT01P)
adlb$TRTA <- as.factor(adlb$TRTA)

adlb_2 <- adlb %>% 
  filter(TRTA != "Placebo")

tplyr_table(adlb_2, TRTA) %>% 
  set_pop_data(adsl) %>% 
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
tplyr_table(adlb_2, TRTA) %>% 
  set_pop_data(adsl) %>% 
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
tplyr_table(adlb_2, TRTA) %>% 
  set_pop_data(adsl) %>% 
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
tplyr_table(adlb, TRTA) %>% 
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
tplyr_table(adlb, TRTA) %>% 
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
tplyr_table(adlb, TRTA) %>% 
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
  
tplyr_table(adlb, TRTA) %>% 
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
  
tplyr_table(adlb, TRTA) %>% 
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

