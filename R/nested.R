#' @noRd
process_nested_count_target <- function(x) {

  evalq({

    if(is.null(indentation)) indentation <- "   "

    assert_that(quo_is_symbol(target_var[[2]]),
                msg = "Inner layers must be data driven variables")

    if(is.factor(target[[as_name(target_var[[1]])]])) {
      warning(paste0("Factors are not currently supported in nested count layers",
                     " that have two data driven variables. Factors will be coerced into character vectors"),
              immediate. = TRUE)
    }
    if(is.factor(target[[as_name(target_var[[2]])]]) && quo_is_symbol(target_var[[1]])) {
      warning(paste0("Factors are not currently supported in nested count layers",
                     " that have two data driven variables. Factors will be coerced into character vectors"),
              immediate. = TRUE)
    }

    if (isTRUE(include_total_row)) {
      abort("You can't include total rows in nested counts. Instead, add a seperate layer for total counts.")
    }

    if (!is.null(denoms_by)) {
      change_denom_ind <- map_chr(denoms_by, as_name) %in% "summary_var"
      second_denoms_by <- denoms_by
      second_denoms_by[change_denom_ind] <- quos(!!target_var[[1]])
    } else {
      denoms_by <- c(treat_var, cols)
      second_denoms_by <- denoms_by
    }

    # Missing subject counts should not occur in the outer layer
    fl <- group_count(current_env(), target_var = !!target_var[[1]],
                      by = vars(!!!by), where = !!where)
    fl$include_missing_subjects_row <- FALSE
    outer_ <- TRUE
    first_layer <- process_summaries(fl)


    outer_ <- FALSE
    second_layer <- process_summaries(group_count(current_env(), target_var = !!target_var[[2]],
                                                  by = vars(!!target_var[[1]], !!!by), where = !!where) %>%
                                        set_count_row_prefix(indentation) %>%
                                        set_denoms_by(!!!second_denoms_by))

    first_layer_final <- first_layer$numeric_data

    second_layer_final <- second_layer$numeric_data %>%
      filter_numeric(
        numeric_cutoff = numeric_cutoff,
        numeric_cutoff_stat = numeric_cutoff_stat,
        numeric_cutoff_column = numeric_cutoff_column,
        treat_var = treat_var
      ) %>%
      group_by(!!target_var[[1]]) %>%
      do(filter_nested_inner_layer(., target, target_var[[1]], target_var[[2]], indentation,
                                   missing_subjects_row_label))

    ignored_filter_rows <- ifelse(include_total_row,
                                  ifelse(is.null(total_row_label),
                                         "Total",
                                         total_row_label),
                                  character(0))

    # Bind the numeric data together
    numeric_data <- bind_rows(first_layer_final, second_layer_final) %>%
      filter_nested_numeric(
        numeric_cutoff,
        numeric_cutoff_stat,
        numeric_cutoff_column,
        treat_var,
        target_var,
        ignored_filter_rows
      )

    # Save the original by and target_vars incase the layer is rebuilt
    by_saved <- by
    target_var_saved <- target_var
    is_built_nest <- TRUE

    by <- vars(!!target_var[[1]], !!!by)
    target_var <- vars(!!target_var[[2]])


  }, envir = x)

}

#' This function is meant to remove the values of an inner layer that don't
#' appear in the target data
#' @noRd
filter_nested_inner_layer <- function(.group, target, outer_name, inner_name, indentation,
                                      missing_subjects_row_label) {

  # Is outer variable text? If it is don't filter on it
  text_outer <- !quo_is_symbol(outer_name)
  outer_name <- as_name(outer_name)
  inner_name <- as_name(inner_name)

  if(text_outer) {
    lvs <- levels(target[[inner_name]])
    target_inner_values <- target %>%
      select(any_of(inner_name)) %>%
      unlist() %>%
      c(lvs) %>%
      unique() %>%
      paste0(indentation, .)

  } else {
    current_outer_value <- unique(.group[, outer_name])[[1]]

    target_inner_values <- target %>%
      filter(!!sym(outer_name) == current_outer_value) %>%
      select(any_of(inner_name)) %>%
      unlist() %>%
      paste0(indentation, .) %>%
      unique()
  }

  target_inner_values <- c(target_inner_values %>% unique(),
                           paste0(indentation, missing_subjects_row_label))

  .group %>%
    filter(summary_var %in% target_inner_values)

}

#' This function resets the variables for a nested layer after it was built
#' @noRd
refresh_nest <- function(x) {
  env_bind(x, by = env_get(x, "by_saved"))
  env_bind(x, target_var = env_get(x, "target_var_saved"))
}


filter_nested_numeric <- function(.data,
                                  numeric_cutoff,
                                  numeric_cutoff_stat,
                                  numeric_cutoff_column,
                                  treat_var,
                                  target_var,
                                  ignored_rows) {

  if (is.null(numeric_cutoff)) {
    return(.data)
  }

  # All of the non NA values in target_var[[1]] are what we want to keep here.
  # Because those are the 'inner' values that passed the filter
  vals <- .data %>%
    {if (is.null(numeric_cutoff_column)) . else filter(., !!treat_var == numeric_cutoff_column)} %>%
    filter(!is.na(!!target_var[[1]])) %>%
    extract2(as_name(target_var[[1]])) %>%
    as.character()

  .data %>%
    filter(!is.na(!!target_var[[1]]) | summary_var %in% c(vals, ignored_rows))


}
