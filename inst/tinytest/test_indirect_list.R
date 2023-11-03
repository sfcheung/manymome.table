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

std_mc <- many_indirect_effects(paths,
                                fit = fit,
                                mc_ci = TRUE,
                                standardized_x = TRUE,
                                boot_out = fit_mc,
                                progress = FALSE)

print(out_boot, se = TRUE)
print(out_boot, se = TRUE, pvalue = TRUE)
print(out_mc, se = TRUE)
print(out_mc, se = TRUE, pvalue = TRUE)

# tmp1 <- to_table.indirect_list(out_boot,
#                               var_labels = c(x = "IV1",
#                                              y = "DV1"),
#                               se = TRUE,
#                               pvalue = TRUE,
#                               digits = 4)
# tmp2 <- to_table.indirect_list(out_boot)
# tmp3 <- to_table.indirect_list(out_noboot)

# tmp1
# tmp2
# tmp3

# library(flextable)
# ft_all <- flextable(tmp1) |>
#             autofit() |>
#             align(j = c("95% CI"),
#                   align = "center") |>
#             align(j = c("p"),
#                   align = "right") |>
#             align(j = c("Effect"),
#                   align = "right") |>
#             align(align = "center",
#                   part = "header")
# ft_all
# ft_all <- flextable(tmp2) |>
#             autofit() |>
#             align(j = c("95% CI"),
#                   align = "center") |>
#             align(j = c("Effect"),
#                   align = "right") |>
#             align(align = "center",
#                   part = "header")
# ft_all
# ft_all <- flextable(tmp3) |>
#             autofit() |>
#             align(j = c("Effect"),
#                   align = "right") |>
#             align(align = "center",
#                   part = "header")
# ft_all

# save_as_docx("All Indirect Effects" = ft_all,
#               path = "All_Indirect_Effects.docx")


tmp1 <- as_flextable(out_boot,
                     var_labels = c(x = "IV1",
                                   y = "DV1"),
                     se = TRUE,
                     pvalue = TRUE,
                     digits = 4)
tmp2 <- as_flextable(out_boot)
tmp3 <- as_flextable(out_noboot)
tmp4 <- as_flextable(std_mc)

tmp1
tmp2
tmp3
tmp4

}

