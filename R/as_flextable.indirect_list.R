#' @title Convert an 'indirect_list' Object to a 'flextable' Object
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
#'
#' @param pvalue If bootstrap confidence
#' intervals are stored, whether
#' asymmetric *p*-values are reported.
#' Default is `FALSE`.
#' See
#' `manymome::print.indirect_list()`
#' for the computational details.
#'
#' @param se Whether standard errors
#' are reported if confidence intervals
#' are stored. Default is `TRUE`.
#' See
#' `manymome::print.indirect_list()`
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
#' @param group_by_x If `TRUE`, the
#' default, the rows will be grouped by
#' x-variables if the paths have more
#' than one x-variable.  Default is `TRUE`.
#'
#' @param group_by_y If `TRUE`, the
#' default, the rows will be grouped by
#' y-variables if the paths have more than
#' one y-variable. Default is `TRUE`.
#'
#' @param y_first If group by both
#' x- and y-variables, group by
#' y-variables first if `TRUE`, the
#' default.
#' Otherwise, group by x-variables.
#'
#' @param total_indirect If `TRUE`, the
#' default, total indirect effect
#' will be computed and added to
#' the output.
#'
#' @param footnote If `TRUE`, the
#' default,
#' add footnote(s) regarding the results
#' to the bottom of the table.
#'
#' @param pcut Any *p*-value less than
#' `pcut` will be displayed as
#' `<[pcut]`, `"[pcut]"` replaced by
#' the value of `pcut`. Default is .001.
#'
#' @param ... Additional arguments.
#' Ignored.
#'
#' @examples
#'
#' library(flextable)
#' library(manymome)
#'
#' data(data_med_complicated)
#' lm_m11 <- lm(m11 ~ x1 + x2, data_med_complicated)
#' lm_m2 <- lm(m2 ~ x1 + x2, data_med_complicated)
#' lm_y1 <- lm(y1 ~ m11 + m2 + x1 + x2, data_med_complicated)
#' fit <- lm2list(lm_m11, lm_m2, lm_y1)
#'
#' # All indirect paths
#' paths <- all_indirect_paths(fit,
#'                            x = c("x1", "x2"),
#'                            y = c("y1"))
#'
#' # Indirect paths from x1 to y1
#' paths_x1y1 <- all_indirect_paths(fit,
#'                            x = c("x1"),
#'                            y = c("y1"))
#'
#' # Indirect effect estimates
#' ind <- many_indirect_effects(paths,
#'                              fit = fit)
#' ft_ind <- as_flextable(ind)
#' ft_ind
#' ft_ind <- as_flextable(ind, group_by_x = FALSE)
#' ft_ind
#'
#' ind_x1y1 <- many_indirect_effects(paths_x1y1,
#'                                   fit = fit)
#' ft_ind_x1y1 <- as_flextable(ind_x1y1)
#' ft_ind_x1y1
#'
#' # Should set R to 5000 or 10000 in real research
#' boot_out_lm <- do_boot(fit,
#'                        R = 100,
#'                        seed = 54532,
#'                        parallel = FALSE,
#'                        progress = FALSE)
#' ind_x1y1_ci <- many_indirect_effects(paths_x1y1,
#'                                      fit = fit,
#'                                      boot_ci = TRUE,
#'                                      boot_out = boot_out_lm)
#' ft_ind_x1y1_ci <- as_flextable(ind_x1y1_ci)
#' ft_ind_x1y1_ci
#'
#' @export
#' @importFrom flextable as_flextable

as_flextable.indirect_list <- function(x,
                                       pvalue = FALSE,
                                       se = TRUE,
                                       var_labels = NULL,
                                       digits = 2,
                                       pval_digits = 3,
                                       use_arrow = TRUE,
                                       indirect_raw = TRUE,
                                       indirect_raw_ci = indirect_raw,
                                       indirect_raw_se = indirect_raw,
                                       group_by_x = TRUE,
                                       group_by_y = TRUE,
                                       y_first = TRUE,
                                       total_indirect = TRUE,
                                       footnote = TRUE,
                                       pcut = .001,
                                       ...) {
    # TODO: Remove after an update to manymome
    indirect_raw_ci <- FALSE
    indirect_raw_se <- FALSE

    if (total_indirect) {
        x_total <- all_total_indirect_effects(x)
        x <- c(x,
               x_total)
        class(x) <- c("indirect_list", class(x))
      }
    path_names <- set_path_names(x,
                                 var_labels = var_labels)
    # Use arrow
    if (use_arrow) {
        path_names <- gsub(" -> ", " \U2192 ",
                           path_names,
                           fixed = TRUE)
      }

    coef0 <- manymome::indirect_effects_from_list(x,
                                                  add_sig = FALSE,
                                                  pvalue = pvalue,
                                                  se = se)
    vars_x <- unique(sapply(x, function(xx) xx$x))
    vars_y <- unique(sapply(x, function(xx) xx$y))
    p_x <- length(vars_x)
    p_y <- length(vars_y)
    if (p_x == 1) group_by_x <- FALSE
    if (p_y == 1) group_by_y <- FALSE

    has_pvalue <- "pvalue" %in% colnames(coef0)
    has_ci <- "CI.lo" %in% colnames(coef0)
    has_se <- "SE" %in% colnames(coef0)
    ci_type <- NULL
    if (isTRUE(!is.null(x[[1]]$boot_ci))) {
        ci_type <- "boot"
        ind_name <- "boot_indirect"
        ci_name <- "nonparametric bootstrap"
      }
    if (isTRUE(!is.null(x[[1]]$mc_ci))) {
        ci_type <- "mc"
        ind_name <- "mc_indirect"
        ci_name <- "Monte Carlo"
      }
    R <- ifelse(has_ci,
                length(x[[1]][[ind_name]]),
                NA)
    std_x <- isTRUE(x[[1]]$standardized_x)
    std_y <- isTRUE(x[[1]]$standardized_y)

    if (has_ci) {
        level <- x[[1]]$level
        level_str <- paste0(format(level * 100),
                            "% CI")
      } else {
        level <- NULL
        level_str <- character(0)
      }

    if ((std_x || std_y) && indirect_raw) {
        ind_raw <- sapply(x,
                          function(xx) {
                              xx$indirect_raw
                            })
        if (has_ci && indirect_raw_ci) {
            # TOFIX: Wait for an update to manymome
            ind_raw_ci <- sapply(x,
                            function(xx) {
                                stats::confint(xx) * xx$scale_y / xx$scale_x
                              })
            ind_raw_ci <- t(ind_raw_ci)
            colnames(ind_raw_ci) <- c("ind_raw_CI.lo", "ind_raw_CI.hi")
            ind_raw <- cbind(ind_raw, ind_raw_ci)
          }
        if (has_ci && indirect_raw_se && se) {
            # TOFIX: Wait for an update to manymome
            ind_raw_scale <- sapply(x,
                                function(xx) {
                                    xx$scale_y / xx$scale_x
                                  })
            ind_raw_SE <- coef0$SE * ind_raw_scale
            ind_raw <- cbind(ind_raw, ind_raw_SE = ind_raw_SE)
          }
        coef0 <- data.frame(Path = path_names,
                            ind_raw,
                            coef0)
      } else {
        coef0 <- data.frame(Path = path_names,
                            coef0)
      }
    if (has_pvalue) {
        p_j <- which(colnames(coef0) %in% "pvalue")
        coef0 <- cbind(coef0[-p_j], coef0[p_j])
      }

    coef0 <- group_ind_df(coef0,
                          ind_list = x,
                          group_by_x = group_by_x,
                          group_by_y = group_by_y,
                          y_first = y_first)

    ft <- flextable::flextable(coef0)

    # Format Cells

    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in%
                                            c("ind", "std", "SE", "ind_raw", "ind_raw_SE")),
                                      digits = digits)
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
    if (!is.null(var_labels)) {
        ft <- flextable::labelizor(ft,
                                  j = (colnames(coef0) %in% c("x", "y")),
                                  labels = var_labels)
      }
    ft <- flextable::align(ft,
                           j = (colnames(coef0) %in% c("x", "y")),
                           align = "left",
                           part = "header")
    ft <- flextable::align(ft,
                           j = (colnames(coef0) %in%
                                  c("ind", "std", "SE", "ind_raw", "ind_raw_SE")),
                           align = "right",
                           part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("y" = "Outcome",
                                          "x" = "Predictor"),
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

    # Add footer

    if (footnote &&
        any(has_ci, has_pvalue, total_indirect, std_x, std_y)) {
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
        if (has_ci) {
            first_note <- FALSE
            ft <- flextable::append_chunks(ft,
                    flextable::as_chunk("CI = confidence interval"),
                    part = "footer")
          }
        if (has_pvalue) {
            first_note <- FALSE
            if (first_note) {
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
        if (total_indirect) {
            first_note <- FALSE
            if (first_note) {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk("Paths with '..' are total indirect effects"),
                        part = "footer")
              } else {
                ft <- flextable::append_chunks(ft,
                        flextable::as_chunk("; "),
                        flextable::as_chunk("paths with '..' are total indirect effects"),
                        part = "footer")
              }
          }
        if (isTRUE(xor(std_x, std_y))) {
            first_note <- FALSE
            if (first_note) {
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
            first_note <- FALSE
            if (first_note) {
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

    ft <- flextable::autofit(ft)
    ft
  }
