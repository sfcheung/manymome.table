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
    coef0 <- data.frame(Path = path_names,
                        coef0)
    has_pvalue <- "pvalue" %in% colnames(coef0)
    has_ci <- "CI.lo" %in% colnames(coef0)
    std_x <- isTRUE(x$standardized_x)
    std_y <- isTRUE(x$standardized_y)
    if (has_ci) {
        level <- x[[1]]$level
        level_str <- paste0(formatC(level * 100, digits = 1, format = "f"),
                            "% CI")
      } else {
        level <- NULL
        level_str <- character(0)
      }
    ft <- flextable::flextable(coef0)

    # Format Cells

    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("ind", "std", "SE")),
                                      digits = digits)
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("pvalue")),
                                      digits = pval_digits)
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("CI.lo")),
                                      digits = digits,
                                      prefix = "[")
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("CI.hi")),
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
    ft <- flextable::autofit(ft)

    # Format Headers
    ft <- flextable::align(ft,
                           j = (colnames(coef0) %in% c("ind", "std", "SE")),
                           align = "center",
                           part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("CI.lo" = level_str,
                                          "SE" = "S.E."),
                               part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("CI.lo" = level_str,
                                          "SE" = "S.E.",
                                          "pvalue" = "p-value"),
                               part = "header")
    ft <- flextable::labelizor(ft,
                               labels = c("ind" = "Effect",
                                          "std" = "Std. Effect"),
                               part = "header")
    if (!is.null(var_labels)) {
        ft <- flextable::labelizor(ft,
                                  j = 1,
                                  labels = var_labels)
      }
    ft
  }
