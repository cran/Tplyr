## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(Tplyr)
library(knitr)

## ----creating a template------------------------------------------------------
new_layer_template(
  "example_template", 
  group_count(...) %>% 
    set_format_strings(f_str("xx (xx%)", n, pct))
)

## ----using a template---------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    use_template("example_template", RACE, by=ETHNIC)
  ) %>% 
  build() %>% 
  kable()

## ----extending a template-----------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    use_template("example_template", RACE) %>% 
      add_total_row()
  ) %>% 
  build() %>% 
  kable()

## ----template with params-----------------------------------------------------
new_layer_template("example_params",
  group_count(...) %>% 
    set_format_strings(f_str("xx (xx.x%)", n, pct)) %>% 
    set_order_count_method({sort_meth}) %>% 
    set_ordering_cols({sort_col})
 )

## ----using params-------------------------------------------------------------
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    use_template('example_params', RACE, add_params = 
                   list(
                     sort_meth = "bycount",
                     sort_col = Placebo
                   ))
  ) %>% 
  build() %>% 
  kable()

## ----view templates-----------------------------------------------------------
get_layer_templates()

## ----get a template-----------------------------------------------------------
get_layer_template("example_params")

## ----remove a template--------------------------------------------------------
remove_layer_template("example_params")
get_layer_templates()

