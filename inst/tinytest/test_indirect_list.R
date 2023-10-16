# WIP. Skip.
if (FALSE) {

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
#' @export
#'
#' @describeIn topic Description of this function
#' @order 1
to_table.indirect_list <- function(object,
                                   add_sig = TRUE,
                                   pvalue = FALSE,
                                   se = FALSE,
                                   var_labels = NULL,
                                   digits = 3) {
    coef0 <- indirect_effects_from_list(object,
                                        add_sig = add_sig,
                                        pvalue = pvalue,
                                        se = se)
    level <- object[[1]]$level
    ci <- format_ci(coef0,
                    level = level,
                    digits = digits,
                    cilo_name = "CI.lo",
                    cihi_name = "CI.hi",
                    brackets = c("[", "]"),
                    sep = ", ")
    level_str <- paste0(level * 100, "% CI")
    path_names <- set_path_names(object,
                                 var_labels = var_labels)
    coef1 <- data.frame(Path = path_names,
                        Effect = formatC(coef0$ind,
                                          digits = digits,
                                          format = "f"),
                        tmp = ci)
    colnames(coef1)[which(colnames(coef1) == "tmp")] <- level_str
    coef1
  }

library(manymome)
suppressMessages(library(lavaan))

# List of indirect effects

dat <- data_serial_parallel
mod <-
"
m11 ~ x + c1 + c2
m12 ~ m11 + x + c1 + c2
m2 ~ x + c1 + c2
y ~ m12 + m2 + m11 + x + c1 + c2
"
fit <- sem(mod, data_serial_parallel,
           fixed.x = FALSE)
# All indirect paths from x to y
paths <- all_indirect_paths(fit,
                           x = "x",
                           y = "y")

fit2boot <- do_boot(fit = fit,
                    R = 100,
                    seed = 53253,
                    parallel = FALSE,
                    progress = FALSE)

fit_mc <- do_mc(fit = fit,
                    R = 100,
                    seed = 53253,
                    parallel = FALSE,
                    progress = FALSE)

# Indirect effect estimates
out_noboot <- many_indirect_effects(paths,
                                    fit = fit)
out_boot <- many_indirect_effects(paths,
                                  fit = fit,
                                  boot_ci = TRUE,
                                  boot_out = fit2boot,
                                  progress = FALSE)
out_mc <- many_indirect_effects(paths,
                                fit = fit,
                                mc_ci = TRUE,
                                boot_out = fit_mc,
                                progress = FALSE)

print(out_boot, se = TRUE)
print(out_boot, se = TRUE, pvalue = TRUE)
print(out_mc, se = TRUE)
print(out_mc, se = TRUE, pvalue = TRUE)

tmp1 <- to_table.indirect_list(out_boot,
                              var_labels = c(x = "IV1",
                                             y = "DV1"),
                              se = TRUE,
                              pvalue = TRUE,
                              digits = 4)
tmp2 <- to_table.indirect_list(out_boot)
tmp3 <- to_table.indirect_list(out_noboot)

tmp1
tmp2
tmp3

library(flextable)
ft_all <- flextable(tmp1) |>
            autofit() |>
            align(j = c("95% CI"),
                  align = "center") |>
            align(j = c("p"),
                  align = "right") |>
            align(j = c("Effect"),
                  align = "right") |>
            align(align = "center",
                  part = "header")
ft_all
save_as_docx("All Indirect Effects" = ft_all,
              path = "All_Indirect_Effects.docx")

}
