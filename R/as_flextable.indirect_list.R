#' @title Convert an 'indirect_list' Object to a `flextable` Object
#'
#' @description The 'as_flextable' method
#' for the output of 'manymome::many_indirect_effects()'.
#'
#' @details It converts an `indirect_list`
#' object,
#' which is usually crated by
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
#' is `FALSE`.
#'
#' @param pvalue If confidence intervals
#' are stored, whether asymmetric *p*-values
#' are reported.
#'
#' @param se Whether standard errors
#' are reported.
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
#' even if standardization was done.
#'
#' @param indirect_raw_se If `TRUE`, the
#' default, report the standard errors
#' of unstandardized effects
#' even if standardization was done.
#'
#' @param group_by_x If `TRUE`, the
#' default, the rows will be grouped by
#' x-variables. Default is `TRUE`.
#'
#' @param group_by_y If `TRUE`, the
#' default, the rows will be grouped by
#' y-variables. Default is `TRUE`.
#'
#' @param y_first If grouped by both
#' x- and y-variables, grouped by
#' y-variables first if `TRUE`, the
#' default.
#' Otherwise, grouped by x-variables.
#'
#' @param ... Additional arguments.
#' Ignored.
#'
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
#' @importFrom flextable as_flextable

as_flextable.indirect_list <- function(x,
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
                                       group_by_x = TRUE,
                                       group_by_y = TRUE,
                                       y_first = TRUE,
                                       ...) {
    path_names <- set_path_names(x,
                                 var_labels = var_labels)
    # Use arrow
    if (use_arrow) {
        path_names <- gsub(" -> ", " \U2192 ",
                           path_names,
                           fixed = TRUE)
      }

    coef0 <- manymome::indirect_effects_from_list(x,
                                                  add_sig = add_sig,
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
    std_x <- isTRUE(x[[1]]$standardized_x)
    std_y <- isTRUE(x[[1]]$standardized_y)

    if (has_ci) {
        level <- x[[1]]$level
        level_str <- paste0(formatC(level * 100, digits = 1, format = "f"),
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
            ind_raw_ci <- sapply(x,
                            function(xx) {
                                stats::confint(xx) * xx$scale_y / xx$scale_x
                              })
            ind_raw_ci <- t(ind_raw_ci)
            colnames(ind_raw_ci) <- c("ind_raw_CI.lo", "ind_raw_CI.hi")
            ind_raw <- cbind(ind_raw, ind_raw_ci)
          }
        if (has_ci && indirect_raw_se && se) {
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
    if (!is.null(var_labels)) {
        ft <- flextable::labelizor(ft,
                                  j = (colnames(coef0) %in% c("x", "y", "Path")),
                                  labels = var_labels)
      }
    ft <- flextable::align(ft,
                           j = (colnames(coef0) %in% c("x", "y")),
                           align = "left",
                           part = "header")
    ft <- flextable::align(ft,
                           j = (colnames(coef0) %in%
                                  c("ind", "std", "SE", "ind_raw", "ind_raw_SE")),
                           align = "center",
                           part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("y" = "Outcome",
                                          "x" = "Predictor"),
                               part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("CI.lo" = level_str,
                                          "SE" = "S.E.",
                                          "ind_raw_SE" = "S.E."),
                               part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("CI.lo" = level_str,
                                          "ind_raw_CI.lo" = level_str,
                                          "SE" = "S.E.",
                                          "pvalue" = "p-value"),
                               part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("ind" = "Effect",
                                          "std" = "Std. Effect",
                                          "ind_raw" = "Effect"),
                               part = "header")
    ft <- flextable::autofit(ft)
    ft
  }
