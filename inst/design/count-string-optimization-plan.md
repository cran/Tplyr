# Count String Formatting Optimization Plan

## Overview

This plan outlines the optimization of `construct_count_string()` and related functions to use efficient vectorized operations instead of row-by-row processing via `map_chr()`.

## Current State

### Performance Profile
- `construct_count_string()` accounts for ~15% of total build time
- Primary bottleneck: `map_chr()` calls to `num_fmt()` for each numeric value
- Profiler shows repeated calls to `format()` → `format.default()` → `prettyNum()`

### Current Architecture
1. **Data summarization** (`process_count_n`, `process_single_count_target`):
   - Produces `numeric_data` with columns: `n`, `distinct_n`, `total`, `distinct_total`
   - Percentages (`pct`, `distinct_pct`) are NOT pre-computed

2. **Formatting** (`process_formatting.count_layer` → `construct_count_string`):
   - Calculates percentages on-the-fly: `pcts <- replace(.n/.total, is.na(.n/.total), 0)`
   - Formats each value individually via `map_chr(.n, num_fmt, ...)`
   - Handles 4 row types: regular, missing, total, missing_subjects
   - Assembles final strings with `sprintf()`

### Key Functions
- `construct_count_string()` - Main entry point ([count.R:667-780](R/count.R#L667-L780))
- `count_string_switch_help()` - Formats individual variables ([count.R:792-819](R/count.R#L792-L819))
- `num_fmt()` - Scalar number formatting ([num_fmt.R:15-83](R/num_fmt.R#L15-L83))
- `pad_formatted_data()` - Adds padding ([num_fmt.R:96-115](R/num_fmt.R#L96-L115))

## Design Goals

1. **Move percentage calculation to summarization step** - Logically belongs with numeric processing, not character formatting
2. **Vectorize number formatting** - Replace `map_chr(values, num_fmt, ...)` with batch operations
3. **Maintain 100% backwards compatibility** - All existing format string features must work identically
4. **Preserve special formatting features**:
   - Hug character formatting (`X`/`A` specifiers)
   - Different format strings for missing/total/missing_subjects rows
   - IBM rounding option
   - Empty string handling for NA values

## Implementation Plan

### Phase 1: Move Percentage Calculation to Summarization

**Objective**: Compute `pct` and `distinct_pct` columns in `numeric_data` during the summarization step.

#### 1.1 Modify `process_single_count_target()`

Location: [count.R:153-245](R/count.R#L153-L245)

After the `get_denom_total_vectorized()` call (which adds `total` and `distinct_total`), add percentage calculation:

```r
numeric_data <- bind_rows(summary_stat, total_stat, missing_subjects_stat) %>%
  rename("summary_var" = !!target_var[[1]]) %>%
  get_denom_total_vectorized(denoms_by, denoms_df_prep, "n") %>%
  mutate(
    summary_var = prefix_count_row(summary_var, count_row_prefix),
    # Add percentage columns
    pct = replace(n / total, is.na(n / total), 0),
    distinct_pct = replace(distinct_n / distinct_total, is.na(distinct_n / distinct_total), 0)
  )
```

#### 1.2 Update nested count processing

Location: [nested.R:78-115](R/nested.R#L78-L115)

Ensure the `first_layer_final` and `second_layer_final` data frames have `pct` and `distinct_pct` columns before binding.

#### 1.3 Update `filter_numeric()`

Location: [count.R:1066-1091](R/count.R#L1066-L1091)

Remove the temporary `pct` calculation since it will now exist in the data:

```r
# Before (current):
vals <- .data %>%
  mutate(
    pct = n/total,
    distinct_pct = distinct_n/distinct_total
  ) %>%
  filter(...)

# After:
vals <- .data %>%
  filter(...)  # pct already exists
```

#### 1.4 Update sorting functions

Location: [sort.R:582-594](R/sort.R#L582-L594)

Remove the temporary `pct` calculation in `add_data_order()` since it will now exist.

---

### Phase 2: Create Vectorized Number Formatting

**Objective**: Create `num_fmt_vec()` that formats an entire numeric vector at once.

#### 2.1 Create `num_fmt_vec()` function

Location: New function in `R/num_fmt.R`

```r
#' Vectorized number formatting
#'
#' Formats an entire numeric vector using f_str settings, replacing
#' the row-by-row map_chr(values, num_fmt, ...) pattern.
#'
#' @param vals Numeric vector to format
#' @param i Index of the format setting within the f_str object
#' @param fmt f_str object with formatting information
#'
#' @return Character vector of formatted values
#' @noRd
num_fmt_vec <- function(vals, i, fmt) {


  # Extract format settings (done once, not per-value)
  int_len <- fmt$settings[[i]]$int
  decimals <- fmt$settings[[i]]$dec
  hug_char <- fmt$settings[[i]]$hug_char
  nsmall <- decimals

  # Calculate display width
  width <- int_len + ifelse(decimals > 0, decimals + 1, 0)

  # Handle IBM rounding option
  if (getOption("tplyr.IBMRounding", FALSE)) {
    warn(paste0(c("You have enabled IBM Rounding. This is an experimental feature.",
                  " If you have feedback please get in touch with the maintainers!")),
         .frequency = "regularly", .frequency_id = "tplyr.ibm", immediate. = TRUE)
    rounded <- ut_round(vals, nsmall)
  } else {
    rounded <- round(vals, nsmall)
  }

  # Vectorized formatting
  if (is.na(hug_char)) {
    # Standard formatting - format() is already vectorized
    fmt_nums <- format(rounded, width = width, nsmall = nsmall)
  } else {
    # Hug character formatting
    fmt_nums <- str_pad(
      paste0(hug_char, format(rounded, nsmall = nsmall)),
      width = width
    )
  }


  # Handle NA values (vectorized)
  empty_str <- fmt$empty[1]
  if (is.na(hug_char)) {
    na_replacement <- str_pad(empty_str, width, side = "left")
  } else {
    na_replacement <- str_pad(paste0(hug_char, empty_str), width, side = "left")
  }

  fmt_nums[is.na(vals)] <- na_replacement

  fmt_nums
}
```

#### 2.2 Create helper for building sprintf format arguments

```r
#' Build vectorized format arguments for count strings
#'
#' @param vars_ord Character vector of variable names in order (e.g., c("n", "pct"))
#' @param count_fmt f_str object
#' @param n Numeric vector of counts
#' @param pct Numeric vector of percentages (pre-calculated)
#' @param distinct_n Numeric vector of distinct counts
#' @param distinct_pct Numeric vector of distinct percentages (pre-calculated)
#' @param total Numeric vector of totals
#' @param distinct_total Numeric vector of distinct totals
#'
#' @return List suitable for do.call(sprintf, ...)
#' @noRd
build_count_format_args <- function(vars_ord, count_fmt, n, pct, distinct_n,
                                     distinct_pct, total, distinct_total) {

  args <- list(count_fmt$repl_str)

for (i in seq_along(vars_ord)) {
    var_name <- vars_ord[i]

    formatted <- switch(var_name,
      "n" = num_fmt_vec(n, i, count_fmt),
      "pct" = num_fmt_vec(pct * 100, i, count_fmt),
      "distinct_n" = num_fmt_vec(distinct_n, i, count_fmt),
      "distinct_pct" = num_fmt_vec(distinct_pct * 100, i, count_fmt),
      "total" = num_fmt_vec(total, i, count_fmt),
      "distinct_total" = num_fmt_vec(distinct_total, i, count_fmt)
    )

    args[[i + 1]] <- formatted
  }

  args
}
```

---

### Phase 3: Create Vectorized `construct_count_string()`

**Objective**: Replace the current implementation with fully vectorized operations.

#### 3.1 Rewrite `construct_count_string()`

The new implementation will:
1. Accept pre-calculated `pct` and `distinct_pct` columns
2. Use boolean masks to identify row types (regular, missing, total, missing_subjects)
3. Format all rows of each type in one vectorized operation
4. Assemble results using vectorized assignment

```r
construct_count_string <- function(.n, .total, .distinct_n = NULL, .distinct_total = NULL,
                                   .pct = NULL, .distinct_pct = NULL,  # NEW parameters
                                   count_fmt = NULL, max_layer_length, max_n_width,
                                   missing_string, missing_f_str, summary_var,
                                   indentation_length, total_count_format,
                                   missing_subjects_count_format, total_row_label,
                                   missing_subjects_row_label, has_missing_count) {

  # Handle defaults
  if (is.null(max_layer_length)) max_layer_length <- 0
  if (is.null(max_n_width)) max_n_width <- 0

  # Use passed percentages or calculate if not provided (backwards compatibility)
  if (is.null(.pct)) {
    .pct <- replace(.n / .total, is.na(.n / .total), 0)
  }
  if (is.null(.distinct_pct)) {
    .distinct_pct <- replace(.distinct_n / .distinct_total, is.na(.distinct_n / .distinct_total), 0)
  }

  # Set up missing format
  if (has_missing_count && is.null(missing_f_str)) {
    missing_f_str <- count_fmt
  }

  # Build row type masks
  summary_var_trimmed <- str_sub(summary_var, indentation_length + 1)

  missing_rows <- if (!is.null(missing_f_str)) {
    summary_var_trimmed %in% missing_string
 } else {
    rep(FALSE, length(summary_var))
  }

  total_rows <- if (!is.null(total_count_format)) {
    summary_var_trimmed %in% total_row_label
  } else {
    rep(FALSE, length(summary_var))
  }

  missing_subject_rows <- if (!is.null(missing_subjects_count_format)) {
    summary_var_trimmed %in% missing_subjects_row_label
  } else {
    rep(FALSE, length(summary_var))
  }

  regular_rows <- !missing_rows & !total_rows & !missing_subject_rows

  # Initialize result vector
  result <- character(length(.n))

  # Format each row type (vectorized within each type)
  if (any(regular_rows)) {
    vars_ord <- map_chr(count_fmt$vars, as_name)
    args <- build_count_format_args(
      vars_ord, count_fmt,
      .n[regular_rows], .pct[regular_rows],
      .distinct_n[regular_rows], .distinct_pct[regular_rows],
      .total[regular_rows], .distinct_total[regular_rows]
    )
    result[regular_rows] <- do.call(sprintf, args)
  }

  if (any(missing_rows)) {
    vars_ord <- map_chr(missing_f_str$vars, as_name)
    args <- build_count_format_args(
      vars_ord, missing_f_str,
      .n[missing_rows], .pct[missing_rows],
      .distinct_n[missing_rows], .distinct_pct[missing_rows],
      .total[missing_rows], .distinct_total[missing_rows]
    )
    result[missing_rows] <- do.call(sprintf, args)
  }

  if (any(total_rows)) {
    vars_ord <- map_chr(total_count_format$vars, as_name)
    args <- build_count_format_args(
      vars_ord, total_count_format,
      .n[total_rows], .pct[total_rows],
      .distinct_n[total_rows], .distinct_pct[total_rows],
      .total[total_rows], .distinct_total[total_rows]
    )
    result[total_rows] <- do.call(sprintf, args)
  }

  if (any(missing_subject_rows)) {
    vars_ord <- map_chr(missing_subjects_count_format$vars, as_name)
    args <- build_count_format_args(
      vars_ord, missing_subjects_count_format,
      .n[missing_subject_rows], .pct[missing_subject_rows],
      .distinct_n[missing_subject_rows], .distinct_pct[missing_subject_rows],
      .total[missing_subject_rows], .distinct_total[missing_subject_rows]
    )
    result[missing_subject_rows] <- do.call(sprintf, args)
  }

  # Apply padding
  pad_formatted_data(result, 0, max_n_width)
}
```

#### 3.2 Update call site in `process_formatting.count_layer()`

Location: [count.R:574-596](R/count.R#L574-L596)

```r
formatted_data <- numeric_data %>%
  filter_numeric(...) %>%
  mutate(n = {
    construct_count_string(
      .n = n,
      .total = total,
      .distinct_n = distinct_n,
      .distinct_total = distinct_total,
      .pct = pct,                    # NEW - pass pre-calculated
      .distinct_pct = distinct_pct,  # NEW - pass pre-calculated
      count_fmt = format_strings[['n_counts']],
      # ... rest unchanged
    )
  }) %>%
  ...
```

---

### Phase 4: Vectorize `pad_formatted_data()`

Location: [num_fmt.R:96-115](R/num_fmt.R#L96-L115)

The current implementation uses `map_chr()` for padding. Replace with vectorized `str_pad()`:

```r
pad_formatted_data <- function(x, right_pad, left_pad) {
  # Vectorized left padding
  if (nchar(x[1]) < left_pad) {
    x <- str_pad(x, left_pad, side = "left")
  }

  # Vectorized right padding
  if (right_pad > max(nchar(x))) {
    x <- str_pad(x, right_pad, side = "right")
  }

  x
}
```

---

### Phase 5: Clean Up Deprecated Code

#### 5.1 Mark `count_string_switch_help()` as deprecated

Keep for one release cycle with deprecation warning, then remove.

#### 5.2 Remove percentage calculation from formatting

After Phase 1 is complete and tested, remove the on-the-fly percentage calculation code paths.

---

## Testing Strategy

### Unit Tests

1. **`num_fmt_vec()` tests**:
   - Basic formatting matches `map_chr(vals, num_fmt, ...)` output exactly
   - NA handling
   - Hug character formatting
   - IBM rounding option
   - Various decimal precisions

2. **Percentage calculation tests**:
   - Verify `pct` and `distinct_pct` columns exist in `numeric_data`
   - Division by zero handling (0/0 → 0, not NA)
   - Values match current behavior

3. **`construct_count_string()` tests**:
   - Regular rows format correctly
   - Missing rows use correct format string
   - Total rows use correct format string
   - Missing subjects rows use correct format string
   - Mixed row types in same call
   - Nested layer indentation handling

### Integration Tests

1. **Backwards compatibility**:
   - Run full test suite (911 tests)
   - Compare output of existing tables character-by-character

2. **Performance benchmarks**:
   - Nested count layer benchmark (existing)
   - Simple count layer benchmark
   - Large table with many treatment groups

### Snapshot Tests

Add snapshots for representative count layer outputs to catch any formatting regressions.

---

## Implementation Order

1. **Phase 2.1**: Create `num_fmt_vec()` with comprehensive tests
2. **Phase 1.1-1.2**: Add percentage columns to summarization
3. **Phase 1.3-1.4**: Update dependent code (filter_numeric, sorting)
4. **Phase 2.2**: Create `build_count_format_args()` helper
5. **Phase 3**: Rewrite `construct_count_string()`
6. **Phase 4**: Vectorize `pad_formatted_data()`
7. **Phase 5**: Clean up deprecated code
8. **Full regression testing**

---

## Expected Performance Impact

| Component | Current | Expected | Improvement |
|-----------|---------|----------|-------------|
| `construct_count_string` | ~15% of build time | ~3-5% of build time | 3-5x faster |
| Overall nested count build | 17.9s baseline | ~15-16s | ~10-15% faster |
| Combined with prior optimizations | 2.39x vs original | ~2.7-3x vs original | Additional 10-15% |

---

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Floating point differences in percentages | Use `all.equal()` with tolerance in tests |
| Format string edge cases | Comprehensive unit tests for all format specifiers |
| Hug character formatting differences | Dedicated test cases comparing old vs new |
| IBM rounding behavior change | Explicit tests with option enabled |
| Backwards compatibility for direct `construct_count_string` calls | Keep signature compatible, new params have defaults |

---

## Rollback Plan

If issues are discovered post-implementation:

1. Revert to commit `47fd333` (current optimization checkpoint)
2. All changes are additive (new functions) until Phase 5
3. Can maintain both code paths with a feature flag if needed during transition
