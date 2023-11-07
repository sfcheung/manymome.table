if (requireNamespace("officer", quietly = TRUE) &&
    length(unclass(packageVersion("manymome.table"))[[1]]) == 4) {

library(tinytest)
library(manymome)
library(officer)
library(flextable)

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

tmp1 <- as_flextable(out_boot,
                     var_labels = c(x1 = "IV_A",
                                    x2 = "IV_B",
                                    y1 = "DV_A",
                                    y2 = "DV_B"),
                     se = TRUE,
                     pvalue = TRUE,
                     digits = 4)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 8)
expect_equal(nrow_part(tmp1), 28)
expect_match(tmp, "Outcome")
expect_match(tmp, "Predictor")
expect_match(tmp, "95% CI")
expect_match(tmp, "SE")
expect_match(tmp, formatC(coef(out_boot)[1], digits = 4, format = "f"))
expect_match(tmp, formatC(confint(out_boot)[5, 2], digits = 4, format = "f"))

tmp1 <- as_flextable(out_noboot, footnote = FALSE)
tmp <- to_html(tmp1)
expect_false(grepl("SE", tmp, fixed = TRUE))
expect_false(grepl("95% CI", tmp, fixed = TRUE))
expect_false(grepl("Note:", tmp, fixed = TRUE))

tmp1 <- as_flextable(stdx_boot, pvalue = TRUE, pcut = .005, group_by_x = FALSE)
tmp <- to_html(tmp1)
expect_match(tmp, "Outcome")
expect_false(grepl("Predictor", tmp, fixed = TRUE))
tmp2 <- indirect_effects_from_list(stdx_boot, pvalue = TRUE)
tmp3 <- formatC(tmp2$pvalue[7], digits = 2, format = "f")
expect_match(tmp, tmp3)
expect_match(tmp, formatC(coef(stdx_boot)[4], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(out_boot)[5], digits = 2, format = "f"))

tmp1 <- as_flextable(stdy_boot, pvalue = TRUE, pcut = .0001, group_by_y = FALSE)
tmp <- to_html(tmp1)
expect_match(tmp, "Predictor")
expect_false(grepl("Outcome", tmp, fixed = TRUE))
expect_match(tmp, formatC(coef(stdy_boot)[4], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(out_boot)[4], digits = 2, format = "f"))

tmp1 <- as_flextable(std_boot, pvalue = TRUE, pcut = .05, group_by_y = FALSE, group_by_x = FALSE)
tmp <- to_html(tmp1)
expect_false(grepl("Predictor", tmp, fixed = TRUE))
expect_false(grepl("Outcome", tmp, fixed = TRUE))
expect_match(tmp, formatC(coef(std_boot)[4], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(out_boot)[4], digits = 2, format = "f"))

tmp1 <- as_flextable(x1y2_boot)
tmp <- to_html(tmp1)
expect_false(grepl("Predictor", tmp, fixed = TRUE))
expect_false(grepl("Outcome", tmp, fixed = TRUE))

tmp1 <- as_flextable(y2_boot)
tmp <- to_html(tmp1)
expect_match(tmp, "Predictor")
expect_false(grepl("Outcome", tmp, fixed = TRUE))

tmp1 <- as_flextable(x2_boot)
tmp <- to_html(tmp1)
expect_match(tmp, "Outcome")
expect_false(grepl("Predictor", tmp, fixed = TRUE))

tmp1 <- as_flextable(x2_boot, total_indirect = FALSE)
tmp <- to_html(tmp1)
expect_false(grepl("\\.\\.", tmp, fixed = TRUE))
expect_false(grepl("total indirect effects", tmp, fixed = TRUE))

}

