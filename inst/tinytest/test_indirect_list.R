# WIP. Skip.
if (FALSE) {

library(manymome)
suppressMessages(library(lavaan))

# List of indirect effects

dat <- data_med_complicated
dat$y1 <- 10 * dat$y1
dat$y2 <- 20 * dat$y2
dat$x1 <- dat$x1 / 2
dat$x2 <- dat$x2 / 3
lm_m11 <- lm(m11 ~ x1 + x1 + x2 + c1 + c2, dat)
lm_m12 <- lm(m12 ~ m11 + x1 + x2 + c1 + c2, dat)
lm_m2 <- lm(m2 ~ x1 + x2 + c1 + c2, dat)
lm_y1 <- lm(y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat)
lm_y2 <- lm(y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat)
fit <- lm2list(lm_m11, lm_m12, lm_m2, lm_y1, lm_y2)
# All indirect paths from x to y
paths <- all_indirect_paths(fit,
                           x = c("x1", "x2"),
                           y = c("y1", "y2"),
                           exclude = c("c1", "c2"))
paths_x1y2 <- all_indirect_paths(fit,
                           x = c("x1"),
                           y = c("y2"),
                           exclude = c("c1", "c2"))
paths_y2 <- all_indirect_paths(fit,
                           x = c("x2", "x1"),
                           y = c("y2"),
                           exclude = c("c1", "c2"))
paths_x2 <- all_indirect_paths(fit,
                           x = c("x2"),
                           y = c("y2", "y1"),
                           exclude = c("c1", "c2"))

fit2boot <- do_boot(fit = fit,
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
                                  boot_out = fit2boot)
stdx_boot <- many_indirect_effects(paths,
                                  fit = fit,
                                  standardized_x = TRUE,
                                  boot_ci = TRUE,
                                  boot_out = fit2boot)
stdy_boot <- many_indirect_effects(paths,
                                  fit = fit,
                                  standardized_y = TRUE,
                                  boot_ci = TRUE,
                                  boot_out = fit2boot)
std_boot <- many_indirect_effects(paths,
                                  fit = fit,
                                  standardized_x = TRUE,
                                  standardized_y = TRUE,
                                  boot_ci = TRUE,
                                  boot_out = fit2boot)

x1y2_boot <- many_indirect_effects(paths_x1y2,
                                   fit = fit,
                                   boot_ci = TRUE,
                                   boot_out = fit2boot)
y2_boot <- many_indirect_effects(paths_y2,
                                   fit = fit,
                                   boot_ci = TRUE,
                                   boot_out = fit2boot)
x2_boot <- many_indirect_effects(paths_x2,
                                   fit = fit,
                                   boot_ci = TRUE,
                                   boot_out = fit2boot)


# out_mc <- many_indirect_effects(paths,
#                                 fit = fit,
#                                 mc_ci = TRUE,
#                                 boot_out = fit_mc,
#                                 progress = FALSE)

# std_mc <- many_indirect_effects(paths,
#                                 fit = fit,
#                                 mc_ci = TRUE,
#                                 standardized_x = TRUE,
#                                 boot_out = fit_mc,
#                                 progress = FALSE)

print(out_boot, se = TRUE)
print(out_boot, se = TRUE, pvalue = TRUE)
# print(out_mc, se = TRUE)
# print(out_mc, se = TRUE, pvalue = TRUE)

tmp1 <- as_flextable(out_boot,
                     var_labels = c(x1 = "IV_A",
                                    x2 = "IV_B",
                                    y1 = "DV_A",
                                    y2 = "DV_B"),
                     se = TRUE,
                     pvalue = TRUE,
                     digits = 4)
tmp2 <- as_flextable(out_boot)
tmp3 <- as_flextable(out_noboot)
# tmp4 <- as_flextable(std_mc)
# tmp5 <- as_flextable(stdx_boot, pvalue = TRUE)
tmp4 <- as_flextable(stdx_boot, pvalue = TRUE)
tmp5 <- as_flextable(stdy_boot, pvalue = TRUE)
tmp6 <- as_flextable(std_boot, pvalue = TRUE)
tmp7 <- as_flextable(x1y2_boot)
tmp8 <- as_flextable(y2_boot)
tmp9 <- as_flextable(x2_boot)

tmp1
tmp2
tmp3
tmp4
tmp5
tmp6
tmp7
tmp8
tmp9

# save_as_docx("All Indirect Effects" = ft_all,
#               path = "All_Indirect_Effects.docx")

}

