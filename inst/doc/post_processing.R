## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE, setup--------------------------------------------------
library(Tplyr)
library(dplyr)
library(knitr)
load('adsl.Rdata')
load('adae.Rdata')

## ----str_indent_wrap----------------------------------------------------------
dat <- tibble(
  row_label1 = c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS"),
  var1_Placebo = c(" 5 (50.0%)", " 3 (30.0%)")
)

dat %>% 
  mutate(
    row_label1 = str_indent_wrap(row_label1, width = 10)
  )

## ----row_mask1----------------------------------------------------------------
dat <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE, by = "Race n (%)")
  ) %>% 
  build() %>% 
  select(1:3)

kable(dat)

## ----row_masks2---------------------------------------------------------------
dat %>% 
  apply_row_masks() %>% 
  kable()

## ----row_masks3---------------------------------------------------------------
dat <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE, by = "Race n (%)")
  ) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>% 
  build()
  
dat %>% 
  apply_row_masks(row_breaks=TRUE) %>% 
  kable()

## ----apply_conditional_format1------------------------------------------------
string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")

apply_conditional_format(string, 2, x == 0, " 0        ", full_string=TRUE)

apply_conditional_format(string, 2, x == 0, "")

## ----apply_conditional_format2------------------------------------------------
apply_conditional_format(string, 2, x < 1, "(<1%)")

## ----apply_conditional_format3------------------------------------------------
dat_new <- dat %>% 
  mutate(
    across(starts_with('var'),  # Apply to variables that start with `var`
    ~ if_else(
      ord_layer_index == 1,     # Target the count layer
      apply_conditional_format( 
        string = .,             # This is dplyr::across syntax
        format_group = 2,       # The percent field
        condition = x == 0,     # Our condition
        replacement = ""        # Replacement value
      ),
      .
      )
    )
  )

kable(dat_new)

## ----fmt_group1---------------------------------------------------------------
string <- c(" 5  (5.8%)", " 8  (9.3%)", "78 (90.7%)")

# Get the n counts
str_extract_fmt_group(string, 1)

# Get the pct counts
str_extract_fmt_group(string, 2)

## ----fmt_group2---------------------------------------------------------------
dat <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE)
  ) %>% 
  build()

dat %>% 
  mutate(
    across(starts_with('var'), 
           ~ str_extract_fmt_group(., 1),
           .names = "{.col}_n"),
    across(starts_with('var'), 
           ~ str_extract_fmt_group(., 2),
           .names = "{.col}_pct")
  ) %>% 
  select(row_label1, var1_Placebo_n, var1_Placebo_pct) %>% 
  kable()

## ----num1---------------------------------------------------------------------
dat <- tplyr_table(adae, TRTA) %>% 
  set_pop_data(adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_format_strings(f_str("xx (XX.x%) [A]", distinct_n, distinct_pct, n))  %>% 
      set_distinct_by(USUBJID)
  ) %>% 
  build()

dat %>% 
  head() %>% 
  kable()

## ----num2---------------------------------------------------------------------
dat_ord <- dat  %>% 
  mutate(
    across(starts_with('var1'),
           ~str_extract_num(., 1),
           .names = "{.col}_ord")
  ) 

dat_ord %>% 
  head() %>% 
  select(row_label1, matches('^var.*ord$'))

## ----apply_formats------------------------------------------------------------
mtcars %>% 
  mutate(
    new_column = apply_formats("xx (xx.x)", gear, mpg)
  ) %>% 
  select(gear, mpg, new_column) %>% 
  head() %>% 
  kable()

