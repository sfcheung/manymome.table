# ON HOLD. The as_flextable approach is better.

#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param arg1 Argument description.
#' @param ... Additional arguments.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. Advance online publication. *Health Psychology*.
#' \doi{10.1037/hea0001188}
#'
#' @seealso [functionname()]
#'
#' @family relatedfunctions
#'
#' @examples
#' \donttest{
#' }
#'
#'
#' @noRd
to_table.indirect_list <- function(object,
                                   add_sig = TRUE,
                                   pvalue = FALSE,
                                   se = FALSE,
                                   var_labels = NULL,
                                   digits = 3,
                                   pcut = .001) {
    coef0 <- manymome::indirect_effects_from_list(object,
                                                  add_sig = add_sig,
                                                  pvalue = pvalue,
                                                  se = se)
    has_ci <- ifelse("CI.lo" %in% colnames(coef0), TRUE, FALSE)
    has_se <- ifelse("SE" %in% colnames(coef0), TRUE, FALSE)
    has_pvalue <- ifelse("pvalue" %in% colnames(coef0), TRUE, FALSE)
    has_sig <- ifelse("Sig" %in% colnames(coef0), TRUE, FALSE)

    if (has_ci) {
        level <- object[[1]]$level
        ci <- format_ci(coef0,
                        level = level,
                        digits = digits,
                        cilo_name = "CI.lo",
                        cihi_name = "CI.hi",
                        brackets = c("[", "]"),
                        sep = ", ")
        level_str <- paste0(level * 100, "% CI")
      }
    if (has_se) {
        se <- formatC(coef0$SE,
                      digits = digits,
                      format = "f")
      }
    if (has_pvalue) {
        pvalue <- sapply(coef0$pvalue,
                         format_pvalue,
                         pcut = pcut)
      }
    if (has_sig) {
        sig <- coef0$Sig
      }
    path_names <- set_path_names(object,
                                 var_labels = var_labels)
    out <- data.frame(Path = path_names,
                      Effect = formatC(coef0$ind,
                                       digits = digits,
                                       format = "f"))
    if (has_se) {
        out$`S.E.` <- se
      }
    if (has_pvalue) {
        out$p <- pvalue
      }
    if (has_ci) {
        out$tmp <- ci
        colnames(out)[which(colnames(out) == "tmp")] <- level_str
      }
    if (has_sig) {
        out$Sig <- sig
      }
    out
  }
