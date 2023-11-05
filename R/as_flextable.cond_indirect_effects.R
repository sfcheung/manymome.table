#' @title Convert an 'indirect_list' Object to a `flextable` Object
#'
#' @description The 'as_flextable' method
#' for the output of 'manymome::many_indirect_effects()'.
#'
#' @details It converts an `indirect_list`
#' object,
#' which is usually created by
#' [manymome::many_indirect_effects()],
#' to a `flextable` object. The output
#' can be further modified by functions
#' from the package `flextable`.
#'
#' @return
#' A `flextable` object.
#'
#' @param x The object to be converted.
#' Should be of the class `indirect_list`
#' from the package `manymome`.
#'
#' @param add_sig Whether a column is
#' added to denote significant effects
#' based on confidence intervals. Default
#' is `FALSE`. Not used for now.
#'
#' @param pvalue If confidence intervals
#' are stored, whether asymmetric *p*-values
#' are reported. Default is `FALSE`.
#'
#' @param se Whether standard errors
#' are reported if confidence intervals
#' are stored. Default is `TRUE`.
#'
#' @param var_labels A named vectors.
#' Used to replace variable names by
#' other names. For example,
#' `c(x = "I.V", y = "D.V.")` replaces
#' `x` by `"I.V"` and `y` by `"D.V."`
#' in the output.
#'
#' @param digits The number of digits
#' to be displayed for most columns,
#' such as indirect effects. Default
#' is 3.
#'
#' @param pval_digits The number of
#' digits to be displayed for the
#' *p*-value column, if present. Default
#' is 3.
#'
#' @param pcut Any *p*-value less than
#' `pcut` will be displayed as `p<[pcut]`,
#' `"[pcut]"` replaced by the value of
#' `pcut`. Default is .001. Not used
#' for now.
#'
#' @param use_arrow If `TRUE`, the
#' default, use the arrow symbol in
#' the paths.
#'
#' @param indirect_raw If `TRUE`, the
#' default, report unstandardized effects
#' even if standardization was done.
#'
#' @param indirect_raw_ci If `TRUE`, the
#' default, report the confidence intervals
#' of unstandardized effects
#' even if standardization was done
#' and confidence intervals were stored.
#'
#' @param indirect_raw_se If `TRUE`, the
#' default, report the standard errors
#' of unstandardized effects
#' even if standardization was done
#' and confidence intervals were stored.
#'
#' @param footnote If `TRUE`, the
#' default,
#' add footnote(s) regarding the results.
#'
#' @param show_indicators Whether the values
#' of indicators (dummy variables) will
#' be shown for categorical moderators.
#' Default is `FALSE`.
#'
#' @param show_wvalues Whether the values
#' of moderators will be shown. If `FALSE`,
#' no values will be shown, even for
#' categorical moderators. Default is
#' `TRUE`.
#'
#' @param show_path Whether the paths
#' being moderated will be displayed.
#' Default is `TRUE`.
#'
#' @param ... Additional arguments.
#' Ignored.
#'
#' @examples
#' # TO FIX
#'
#' library(manymome)
#' data(data_med_complicated)
#' lm_m11 <- lm(m11 ~ x1 + x1 + x2 + c1 + c2, data_med_complicated)
#' lm_m12 <- lm(m12 ~ m11 + x1 + x2 + c1 + c2, data_med_complicated)
#' lm_m2 <- lm(m2 ~ x1 + x2 + c1 + c2, data_med_complicated)
#' lm_y2 <- lm(y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, data_med_complicated)
#' fit <- lm2list(lm_m11, lm_m12, lm_m2, lm_y2)
#' # All indirect paths
#' paths <- all_indirect_paths(fit,
#'                            x = c("x1", "x2"),
#'                            y = c("y2"),
#'                            exclude = c("c1", "c2"))
#' # Indirect paths from x1 to y2
#' paths_x1y2 <- all_indirect_paths(fit,
#'                            x = c("x1"),
#'                            y = c("y2"),
#'                            exclude = c("c1", "c2"))
#' # Indirect effect estimates
#' ind <- many_indirect_effects(paths,
#'                              fit = fit)
#' ind_x1y2 <- many_indirect_effects(paths_x1y2,
#'                                   fit = fit)
#' library(flextable)
#' ind_ft <- as_flextable(ind)
#' ind_ft
#' ind_x1y2 <- as_flextable(ind_x1y2)
#' ind_x1y2
#'
#' @export
#' @importFrom flextable as_flextable

as_flextable.cond_indirect_effects <- function(x,
                                       add_sig = FALSE,
                                       pvalue = FALSE,
                                       se = TRUE,
                                       var_labels = NULL,
                                       digits = 3,
                                       pval_digits = 3,
                                       pcut = .001,
                                       use_arrow = TRUE,
                                       indirect_raw = TRUE,
                                       indirect_raw_ci = TRUE,
                                       indirect_raw_se = TRUE,
                                       footnote = TRUE,
                                       show_indicators = FALSE,
                                       show_wvalues = TRUE,
                                       show_path = TRUE,
                                       ...) {
    # Adapted from the print method from manymome.
    full_output <- attr(x, "full_output")
    x_i <- full_output[[1]]
    std_x <- isTRUE(x_i$standardized_x)
    std_y <- isTRUE(x_i$standardized_y)

    x_list <- list2indirect_list(full_output)
    coef0 <- manymome::indirect_effects_from_list(x_list,
                                                  add_sig = add_sig,
                                                  pvalue = pvalue,
                                                  se = se)
    # Fix the column names
    if (std_x || std_y) {
        colnames(coef0)[colnames(coef0) %in% "ind"] <- "std"
      }

    x0 <- x_i$x
    m0 <- x_i$m
    y0 <- x_i$y
    has_m <- !is.null(m0)
    if (has_m) {
        path_names <- paste0(x0, " -> ",
                              paste0(eval(m0), collapse = " -> "),
                              " -> ", y0)
      } else {
        path_names <- paste(x0, "->", y0)
      }

    # Use arrow
    if (use_arrow) {
        path_names <- gsub(" -> ", " \U2192 ",
                           path_names,
                           fixed = TRUE)
      }

    has_pvalue <- "pvalue" %in% colnames(coef0)
    has_ci <- "CI.lo" %in% colnames(coef0)
    has_se <- "SE" %in% colnames(coef0)

    if (has_ci) {
        level <- x_i$level
        level_str <- paste0(formatC(level * 100, digits = 1, format = "f"),
                            "% CI")
      } else {
        level <- NULL
        level_str <- character(0)
      }

    if (!is.null(x_i$boot_ci)) {
        has_ci <- TRUE
        ci_type <- "boot"
        ind_name <- "boot_indirect"
        se_name <- "boot_se"
        ci_name <- "nonparametric bootstrap"
      }
    if (!is.null(x_i$mc_ci)) {
        has_ci <- TRUE
        ci_type <- "mc"
        ind_name <- "mc_indirect"
        se_name <- "mc_se"
        ci_name <- "Monte Carlo"
      }

    if ((std_x || std_y) && indirect_raw) {
        ind_raw <- sapply(full_output,
                          function(xx) {
                              xx$indirect_raw
                            })
        if (has_ci && indirect_raw_ci) {
            ind_raw_ci <- sapply(full_output,
                            function(xx) {
                                stats::confint(xx) * xx$scale_y / xx$scale_x
                              })
            ind_raw_ci <- t(ind_raw_ci)
            colnames(ind_raw_ci) <- c("ind_raw_CI.lo", "ind_raw_CI.hi")
            ind_raw <- cbind(ind_raw, ind_raw_ci)
          }
        if (has_ci && indirect_raw_se && se) {
            ind_raw_scale <- sapply(full_output,
                                function(xx) {
                                    xx$scale_y / xx$scale_x
                                  })
            ind_raw_SE <- coef0$SE * ind_raw_scale
            ind_raw <- cbind(ind_raw, ind_raw_SE = ind_raw_SE)
          }
        coef0 <- data.frame(ind_raw,
                            coef0)
      } else {
        coef0 <- data.frame(coef0)
      }
    if (has_pvalue) {
        p_j <- which(colnames(coef0) %in% "pvalue")
        coef0 <- cbind(coef0[-p_j], coef0[p_j])
      }

    # Add columns for w-variables
    x_df <- as.data.frame(x)
    wvars_columns <- paste0("[", get_wvars(x), "]")
    wvalues_columns <- paste0("(", get_wvalues(x), ")")
    w_columns <- c(wvars_columns, wvalues_columns)
    w_df <- x_df[, colnames(x_df) %in% w_columns, drop = FALSE]
    if (!show_indicators) {
        tmp <- colnames(w_df) %in% paste0("(", get_indicators(x), ")")
        w_df <- w_df[, !tmp, drop = FALSE]
      }
    if (!show_wvalues) {
        tmp <- colnames(w_df) %in% wvalues_columns
        w_df <- w_df[, !tmp, drop = FALSE]
      }
    coef0 <- cbind(w_df, coef0)

    ft <- flextable::flextable(coef0)

    # Format Cells

    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in%
                                            c("ind", "std", "SE", "ind_raw", "ind_raw_SE")),
                                      digits = digits)
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% w_columns),
                                      digits = digits)
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("pvalue")),
                                      digits = pval_digits)
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("CI.lo", "ind_raw_CI.lo")),
                                      digits = digits,
                                      prefix = "[")
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("CI.hi", "ind_raw_CI.hi")),
                                      digits = digits,
                                      prefix = "; ",
                                      suffix = "]")
    ft <- flextable::autofit(ft)
    if (has_ci) {
        j0 <- which(colnames(coef0) == "CI.lo")
        ft <- flextable::padding(ft, j = j0, padding.right = 0)
        ft <- flextable::padding(ft, j = j0 + 1, padding.left = 0)
        ft <- flextable::align(ft, j = j0 + 1, align = "left")
        ft <- flextable::align(ft, j = j0 - 1, align = "right")
        ft <- flextable::merge_at(ft, j = c(j0, j0 + 1), part = "header")
        ft <- flextable::align(ft, j = j0, align = "center", part = "header")
      }
    if (isTRUE("ind_raw_CI.lo" %in% colnames(coef0))) {
        j0 <- which(colnames(coef0) == "ind_raw_CI.lo")
        ft <- flextable::padding(ft, j = j0, padding.right = 0)
        ft <- flextable::padding(ft, j = j0 + 1, padding.left = 0)
        ft <- flextable::align(ft, j = j0 + 1, align = "left")
        ft <- flextable::align(ft, j = j0 - 1, align = "right")
        ft <- flextable::merge_at(ft, j = c(j0, j0 + 1), part = "header")
        ft <- flextable::align(ft, j = j0, align = "center", part = "header")
      }

    # Format Headers

    ft <- flextable::align(ft,
                           j = (colnames(coef0) %in%
                                  c("ind", "std", "SE", "ind_raw", "ind_raw_SE")),
                           align = "center",
                           part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("CI.lo" = level_str,
                                          "ind_raw_CI.lo" = level_str,
                                          "SE" = "S.E.",
                                          "ind_raw_SE" = "S.E."),
                               part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("pvalue" = "p-value"),
                               part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("ind" = "Effect",
                                          "std" = "Std. Effect",
                                          "ind_raw" = "Effect"),
                               part = "header")
    if (!is.null(var_labels)) {
        var_labels_1 <- paste0("(", var_labels, ")")
        var_labels_2 <- paste0("[", var_labels, "]")
        names(var_labels_1) <- paste0("(", names(var_labels), ")")
        names(var_labels_2) <- paste0("[", names(var_labels), "]")
        ft <- flextable::labelizor(ft,
                                  j = (colnames(coef0) %in% w_columns),
                                  labels = c(var_labels_1,
                                             var_labels_2),
                                  part = "header")
      }

    # Add footer

    if (footnote) {
        msg <- "Note:"
        msg <- c(msg,
                 paste("[w] is the meaning of a level of moderator 'w',",
                       "or the label of a group,"))
        if (any(colnames(coef0) %in% wvalues_columns)) {
            if (is.null(get_indicators(x)) || !show_indicators) {
                msg <- c(msg,
                        paste("(w) is the value of a level",
                              "of moderator 'w'."))
              } else {
                msg <- c(msg,
                        paste("(w) is the value of a level",
                              "of moderator 'w', or its indicators if 'w' is categorical."))
              }
          }
        if (has_ci) {
            msg <- c(msg,
                     paste("CI is",
                           ci_name,
                           "confidence interval."))
          }
        if (has_pvalue) {
            msg <- c(msg,
                     paste("The p-value is asymmetric bootstrap p-value."))
          }
        if (has_se) {
            msg <- c(msg,
                     paste("SE is",
                           ci_name,
                           "standard error."))
          }
        if (length(msg) > 1) {
            msg <- paste(msg, collapse = " ")
            ft <- flextable::add_footer_lines(ft, msg)
          }
      }

    if (show_path) {
        ft <- flextable::add_header_row(ft,
                                        values = paste("Path: ",
                                                       path_names),
                                        colwidths = flextable::ncol_keys(ft))
      }

    ft <- flextable::autofit(ft)
    ft
  }
