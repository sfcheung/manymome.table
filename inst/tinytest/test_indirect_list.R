# WIP. Skip.
if (FALSE) {

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
ft_all <- flextable(tmp2) |>
            autofit() |>
            align(j = c("95% CI"),
                  align = "center") |>
            align(j = c("Effect"),
                  align = "right") |>
            align(align = "center",
                  part = "header")
ft_all
ft_all <- flextable(tmp3) |>
            autofit() |>
            align(j = c("Effect"),
                  align = "right") |>
            align(align = "center",
                  part = "header")
ft_all

save_as_docx("All Indirect Effects" = ft_all,
              path = "All_Indirect_Effects.docx")


as_flextable.indirect_list <- function(x,
                                       add_sig = FALSE,
                                       pvalue = FALSE,
                                       se = TRUE,
                                       var_labels = NULL,
                                       digits = 3,
                                       pval_digits = 3,
                                       pcut = .001,
                                       ...) {
    path_names <- set_path_names(x,
                                 var_labels = var_labels)
    coef0 <- manymome::indirect_effects_from_list(x,
                                                  add_sig = add_sig,
                                                  pvalue = pvalue,
                                                  se = se)
    coef0 <- data.frame(Path = path_names,
                        coef0)
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
    ft <- flextable::colformat_double(ft,
                                      j = (colnames(coef0) %in% c("ind", "SE")),
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

tmp1 <- as_flextable.indirect_list(out_boot,
                                   var_labels = c(x = "IV1",
                                                 y = "DV1"),
                                   se = TRUE,
                                   pvalue = TRUE,
                                   digits = 4)
tmp2 <- as_flextable.indirect_list(out_boot)
tmp3 <- as_flextable.indirect_list(out_noboot)


tmp1
tmp2
tmp3

}

