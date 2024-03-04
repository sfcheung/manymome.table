#' @title Convert an 'cond_indirect_effects' Object to a 'flextable' Object
#'
#' @description The 'as_flextable' method
#' for the output of 'manymome::many_indirect_effects()'.
#'
#' @details It converts an `cond_indirect_effects`
#' object,
#' which is usually created by
#' [manymome::cond_indirect_effects()],
#' to a `flextable` object. The output
#' can be further modified by functions
#' from the `flextable` package.
#'
#' @return
#' A `flextable` object.
#'
#' @param x The object to be converted.
#' Should be of the class `cond_indirect_effects`
#' from the package `manymome`.
#'
#' @param pvalue If bootstrap confidence
#' intervals are stored, whether
#' asymmetric *p*-values are reported.
#' Default is `FALSE`.
#' See
#' `manymome::print.cond_indirect_effects()`
#' for the computational details.
#'
#' @param se Whether standard errors
#' are reported if confidence intervals
#' are stored. Default is `TRUE`.
#' See
#' `manymome::print.cond_indirect_effects()`
#' for the computation details.
#'
#' @param var_labels A named vectors.
#' Used to replace variable names by
#' other names when generating the
#' table. For example,
#' `c(x = "I.V", y = "D.V.")` replaces
#' `x` by `"I.V"` and `y` by `"D.V."`
#' in the output.
#'
#' @param digits The number of digits
#' to be displayed for most numerical
#' columns,
#' such as effect estimates, standard
#' errors, and confidence intervals.
#' Default is 2.
#'
#' @param pval_digits The number of
#' digits to be displayed for the
#' *p*-value column, if present. Default
#' is 3.
#'
#' @param use_arrow If `TRUE`, the
#' default, use the arrow symbol in
#' the paths.
#'
#' @param indirect_raw If `TRUE`, the
#' default, report unstandardized effects
#' even if standardization was done.
#'
#' @param indirect_raw_ci If `TRUE`,
#' report the confidence intervals
#' of unstandardized effects
#' even if standardization was done
#' and confidence intervals were stored.
#' Default to be equal to `indirect_raw`.
#' NOTE: Not used for now. Always `FALSE`.
#'
#' @param indirect_raw_se If `TRUE`,
#' report the standard errors
#' of unstandardized effects
#' even if standardization was done
#' and confidence intervals were stored.
#' Default to be equal to `indirect_raw`.
#' NOTE: Not used for now. Always `FALSE`.
#'
#' @param footnote If `TRUE`, the
#' default,
#' add footnote(s) regarding the results
#' to the bottom of the table.
#'
#'
#' @param show_wvalues Whether the values
#' of moderators will be shown. If `FALSE`,
#' no values will be shown, even for
#' categorical moderators. Default is
#' `TRUE`.
#'
#' @param show_indicators Whether the values
#' of indicators (dummy variables) will
#' be shown for categorical moderators.
#' Default is `FALSE`.
#'
#' @param show_path Whether the paths
#' being moderated will be displayed.
#' Default is `TRUE`.
#'
#' @param pcut Any *p*-value less than
#' `pcut` will be displayed as
#' `<[pcut]`, `"[pcut]"` replaced by
#' the value of `pcut`. Default is .001.
#'
#'
#' @param ... Additional arguments.
#' Ignored.
#'
#' @examples
#'
#' library(manymome)
#' library(flextable)
#'
#' # List of indirect effects
#'
#' dat <- data_med_mod_a
#' lm_m <- lm(m ~ x*w + c1 + c2, dat)
#' lm_y <- lm(y ~ m + x + c1 + c2, dat)
#' fit_lm <- lm2list(lm_m, lm_y)
#'
#' # Should set R to 5000 or 10000 in real research
#' boot_out_lm <- do_boot(fit_lm,
#'                        R = 100,
#'                        seed = 54532,
#'                        parallel = FALSE,
#'                        progress = FALSE)
#'
#' out_xmy_on_w <- cond_indirect_effects(wlevels = "w",
#'                                       x = "x",
#'                                       y = "y",
#'                                       m = "m",
#'                                       fit = fit_lm,
#'                                       boot_ci = TRUE,
#'                                       boot_out = boot_out_lm)
#'
#' std_xmy_on_w <- cond_indirect_effects(wlevels = "w",
#'                                       x = "x",
#'                                       y = "y",
#'                                       m = "m",
#'                                       fit = fit_lm,
#'                                       boot_ci = TRUE,
#'                                       boot_out = boot_out_lm,
#'                                       standardized_x = TRUE,
#'                                       standardized_y = TRUE)
#'
#' ft1 <- as_flextable(out_xmy_on_w,
#'                     var_labels = c(w = "Moderator"))
#' ft1
#'
#' ft2 <- as_flextable(std_xmy_on_w,
#'                     var_labels = c(w = "Moderator"),
#'                     se = FALSE,
#'                     digits = 3)
#' ft2
#'
#' @export
#' @importFrom flextable as_flextable

as_flextable.cond_indirect_effects <- function(x,
                                       pvalue = FALSE,
                                       se = TRUE,
                                       var_labels = NULL,
                                       digits = 2,
                                       pval_digits = 3,
                                       use_arrow = TRUE,
                                       indirect_raw = TRUE,
                                       indirect_raw_ci = indirect_raw,
                                       indirect_raw_se = indirect_raw,
                                       footnote = TRUE,
                                       show_wvalues = TRUE,
                                       show_indicators = FALSE,
                                       show_path = TRUE,
                                       pcut = .001,
                                       ...) {
    # TODO: Remove after an update to manymome
    indirect_raw_ci <- FALSE
    indirect_raw_se <- FALSE

    # Adapted from the print method from manymome.
    full_output <- attr(x, "full_output")
    x_i <- full_output[[1]]
    std_x <- isTRUE(x_i$standardized_x)
    std_y <- isTRUE(x_i$standardized_y)

    x_list <- list2indirect_list(full_output)
    coef0 <- manymome::indirect_effects_from_list(x_list,
                                                  add_sig = FALSE,
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
        level_str <- paste0(format(level * 100),
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
            # TOFIX: Wait for an update to manymome
            ind_raw_ci <- sapply(full_output,
                            function(xx) {
                                stats::confint(xx) * xx$scale_y / xx$scale_x
                              })
            ind_raw_ci <- t(ind_raw_ci)
            colnames(ind_raw_ci) <- c("ind_raw_CI.lo", "ind_raw_CI.hi")
            ind_raw <- cbind(ind_raw, ind_raw_ci)
          }
        if (has_ci && indirect_raw_se && se) {
            # TOFIX: Wait for an update to manymome
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
    # ft <- flextable::colformat_double(ft,
    #                                   j = (colnames(coef0) %in% c("pvalue")),
    #                                   digits = pval_digits)
    ft <- flextable::set_formatter(ft,
                                   pvalue = function(x) {
                                           format_p(x, pcut = pcut, pval_digits = pval_digits)
                                        })
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("CI.lo", "ind_raw_CI.lo")),
                                      digits = digits,
                                      prefix = "[")
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("CI.hi", "ind_raw_CI.hi")),
                                      digits = digits,
                                      prefix = ", ",
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
                           align = "right",
                           part = "header")
    if (has_ci) {
        ft <- flextable::labelizor(ft,
                                  labels = c("CI.lo" = level_str,
                                             "ind_raw_CI.lo" = level_str),
                                  part = "header")
      }
    if (has_se) {
        ft <- flextable::compose(ft,
                                i = 1,
                                j = (colnames(coef0) %in%
                                      c("SE", "ind_raw_SE")),
                                part = "header",
                                flextable::as_paragraph(flextable::as_i("SE")))
      }
    if (has_pvalue) {
        ft <- flextable::compose(ft,
                                i = 1,
                                j = "pvalue",
                                part = "header",
                                flextable::as_paragraph(flextable::as_i("p")))
      }
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
        first_note <- TRUE
        ft <- flextable::add_footer_lines(ft, values = "")
        ft <- flextable::compose(ft,
                i = 1,
                j = 1,
                value = flextable::as_paragraph(flextable::as_i("Note")),
                part = "footer")
        ft <- flextable::append_chunks(ft,
                flextable::as_chunk(": "),
                part = "footer")
        if (all_w_numeric(x)) {
            ft <- flextable::append_chunks(ft,
                    flextable::as_chunk("[w] is the meaning of a level of moderator 'w'"),
                    part = "footer")
          }
        if (all_w_categorical(x)) {
            ft <- flextable::append_chunks(ft,
                    flextable::as_chunk("[w] is the label of a group in moderator 'w'"),
                    part = "footer")
          }
        if (!all_w_categorical(x) && !all_w_numeric((x))) {
            ft <- flextable::append_chunks(ft,
                    flextable::as_chunk("[w] is the meaning of a level of moderator 'w' or the label of a group in moderator 'w'"),
                    part = "footer")
          }
        first_note <- FALSE
        if (any(colnames(coef0) %in% wvalues_columns)) {
            if (first_note) {
                first_note <- FALSE
              } else {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk("; "),
                        part = "footer")
              }
            if (is.null(get_indicators(x)) || !show_indicators) {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk("(w) is the value of a level of moderator 'w'"),
                        part = "footer")
              } else {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk("(w) is the value of a level of moderator 'w', or its indicators if 'w' is categorical"),
                        part = "footer")
              }
          }
        if (has_ci) {
            if (first_note) {
                first_note <- FALSE
              } else {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk(": "),
                        part = "footer")
              }
            ft <- flextable::append_chunks(ft,
                    flextable::as_chunk("CI = confidence interval"),
                    part = "footer")
          }
        if (has_pvalue) {
            if (first_note) {
                first_note <- FALSE
                ft <- flextable::append_chunks(ft,
                        flextable::as_i("P "),
                        part = "footer")
              } else {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk("; "),
                        flextable::as_i("p "),
                        part = "footer")
              }
            ft <- flextable::append_chunks(ft,
                    flextable::as_chunk("is asymmetric bootstrap "),
                    flextable::as_i("p"),
                    flextable::as_chunk("-value"),
                    part = "footer")
          }
        if (isTRUE(xor(std_x, std_y))) {
            if (first_note) {
                first_note <- FALSE
              } else {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk("; "),
                        part = "footer")
              }
            ft <- flextable::append_chunks(ft,
                    flextable::as_chunk("Std. Effect is partially standardized effect, "),
                    flextable::as_chunk(paste0("with ",
                                            ifelse(std_x,
                                                   "the predictor",
                                                   "the outcome"),
                                            " standardized")),
                    part = "footer")
          }
        if (std_x && std_y) {
            if (first_note) {
                first_note <- FALSE
              } else {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk("; "),
                        part = "footer")
              }
            ft <- flextable::append_chunks(ft,
                    flextable::as_chunk("Std. Effect is completely standardized effect"),
                    part = "footer")
          }
        ft <- flextable::append_chunks(ft,
                flextable::as_chunk("."),
                part = "footer")
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
