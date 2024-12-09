if (requireNamespace("officer", quietly = TRUE) &&
    length(unclass(packageVersion("manymome.table"))[[1]]) == 4) {

library(tinytest)
library(manymome)
library(officer)
library(flextable)

dat <- data_med_mod_a
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)
fit_lm <- lm2list(lm_m, lm_y)

boot_out_lm <- do_boot(fit_lm,
                       R = 100,
                       seed = 54532,
                       parallel = FALSE,
                       progress = FALSE)

out_xmy_on_w <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "y",
                                      m = "m",
                                      fit = fit_lm,
                                      boot_ci = TRUE,
                                      boot_out = boot_out_lm)

out_mome <- index_of_mome(x = "x",
                          y = "y",
                          m = "m",
                          w = "w",
                          fit = fit_lm,
                          boot_ci = TRUE,
                          boot_out = boot_out_lm)

std_xmy_on_w <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "y",
                                      m = "m",
                                      fit = fit_lm,
                                      boot_ci = TRUE,
                                      boot_out = boot_out_lm,
                                      standardized_x = TRUE,
                                      standardized_y = TRUE)

tmp1 <- as_flextable(out_xmy_on_w,
                     var_labels = c(w = "Moderator"),
                     se = TRUE,
                     pvalue = TRUE,
                     digits = 4)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 7)
expect_match(tmp, "[Moderator]")
expect_match(tmp, "(Moderator)")
expect_match(tmp, "95% CI")
expect_match(tmp, "SE")
expect_match(tmp, formatC(coef(out_xmy_on_w)[1], digits = 3, format = "f"))
expect_match(tmp, formatC(confint(out_xmy_on_w)[2, 2], digits = 3, format = "f"))

tmp1 <- as_flextable(out_xmy_on_w, show_wvalues = FALSE)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 5)
expect_match(tmp, "[w]")
expect_false(grepl("(w)", tmp, fixed = TRUE))

tmp1 <- as_flextable(std_xmy_on_w)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 7)
expect_match(tmp, "Std. Effect")
expect_match(tmp, formatC(coef(std_xmy_on_w)[1], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(out_xmy_on_w)[1], digits = 2, format = "f"))

tmp1 <- as_flextable(std_xmy_on_w, indirect_raw = FALSE, pvalue = TRUE, pcut = .10)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 7)
expect_false(grepl(formatC(coef(out_xmy_on_w)[1], digits = 2, format = "f"), tmp, fixed = TRUE))

# From the tests of manymome

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)
fit <- lm2list(lm_m1, lm_m2, lm_m3, lm_y)

# Moderated mediation

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)
out_2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE)
out_3 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_y = TRUE)
out_4 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE)
out_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               boot_ci = TRUE, seed = 87415,
                               parallel = FALSE, progress = FALSE,
                               R = 100)
out_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = out_5)
out_7 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = out_5)
out_8 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = out_5)

# Moderation only

outmo_mm_1 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)

outmo_1 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit)
outmo_2 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE)
outmo_3 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_y = TRUE)
outmo_4 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE)

outmo_5 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               boot_ci = TRUE, seed = 87415,
                               parallel = FALSE, progress = FALSE,
                               R = 100)
outmo_6 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = outmo_5)
outmo_7 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = outmo_5)
outmo_8 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = outmo_5)

tmp1 <- as_flextable(out_1)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 4)
expect_match(tmp, "[gp]")
expect_match(tmp, "[gp2]")
expect_match(tmp, "(w4)")
expect_false(grepl("gpgp2", tmp, fixed = TRUE))

tmp1 <- as_flextable(out_2)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 5)
expect_match(tmp, "Std. Effect")

tmp1 <- as_flextable(out_3)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(coef(out_3)[1], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(out_1)[3], digits = 2, format = "f"))

tmp1 <- as_flextable(out_4)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(coef(out_4)[1], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(out_1)[3], digits = 2, format = "f"))

tmp1 <- as_flextable(out_5, show_indicators = TRUE)
tmp <- to_html(tmp1)
expect_match(tmp, "[gpgp3]")
expect_match(tmp, formatC(confint(out_5)[3, 2], digits = 2, format = "f"))

tmp1 <- as_flextable(out_6, show_wvalues = FALSE)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(confint(out_6)[3, 2], digits = 2, format = "f"))
expect_false(grepl(formatC(confint(out_5)[1, 1], digits = 2, format = "f"), tmp, fixed = TRUE))

tmp1 <- as_flextable(out_7, show_wvalues = FALSE, digits = 4)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(confint(out_7)[3, 2], digits = 4, format = "f"))
expect_match(tmp, formatC(coef(out_5)[2], digits = 4, format = "f"))
expect_false(grepl("(w4)", tmp, fixed = TRUE))

tmp1 <- as_flextable(out_8, show_wvalues = FALSE, digits = 4, pvalue = TRUE, pval_digits = 5)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(confint(out_8)[3, 2], digits = 4, format = "f"))
tmp2 <- attr(out_8, "full_output")
class(tmp2) <- c("indirect_list", class(tmp2))
tmp3 <- indirect_effects_from_list(tmp2, pvalue = TRUE)
tmp4 <- formatC(tmp3$pvalue[3], digits = 5, format = "f")
tmp4 <- gsub("^0.", ".", tmp4)
expect_match(tmp, tmp4)

# In the latest version of manymome,
# SE-based CI will be included for moderation-only unstandardized paths.
tmp1 <- as_flextable(outmo_1, digits = 4)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 5)
expect_match(tmp, "[gp]")
expect_false(grepl("gpgp2", tmp, fixed = TRUE))
expect_match(tmp, formatC(coef(outmo_1)[1], digits = 4, format = "f"))

tmp1 <- as_flextable(outmo_2)
tmp <- to_html(tmp1)
expect_equal(ncol_keys(tmp1), 3)
expect_match(tmp, "Std. Effect")
expect_match(tmp, formatC(coef(outmo_2)[1], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(outmo_1)[3], digits = 2, format = "f"))

tmp1 <- as_flextable(outmo_3)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(coef(outmo_3)[1], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(outmo_1)[3], digits = 2, format = "f"))

tmp1 <- as_flextable(outmo_4)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(coef(outmo_4)[1], digits = 2, format = "f"))
expect_match(tmp, formatC(coef(outmo_1)[3], digits = 2, format = "f"))

tmp1 <- as_flextable(outmo_5, show_indicators = TRUE)
tmp <- to_html(tmp1)
expect_match(tmp, "[gpgp3]")
expect_match(tmp, formatC(confint(outmo_5)[3, 2], digits = 2, format = "f"))

tmp1 <- as_flextable(outmo_6, show_wvalues = FALSE, se = FALSE)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(confint(outmo_6)[3, 2], digits = 2, format = "f"))
expect_false(grepl("S.E.", tmp))

tmp1 <- as_flextable(outmo_7, show_wvalues = FALSE, digits = 4, se = FALSE, pvalue = TRUE)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(confint(outmo_7)[3, 2], digits = 4, format = "f"))
expect_match(tmp, formatC(coef(outmo_5)[2], digits = 4, format = "f"))

tmp1 <- as_flextable(outmo_8, show_wvalues = FALSE, digits = 2, pvalue = TRUE, footnote = FALSE, show_path = FALSE, pcut = .400)
tmp <- to_html(tmp1)
expect_match(tmp, formatC(confint(outmo_8)[3, 2], digits = 2, format = "f"))
expect_false(grepl("Path", tmp))
expect_false(grepl("Note:", tmp))
tmp2 <- attr(outmo_8, "full_output")
class(tmp2) <- c("indirect_list", class(tmp2))
tmp3 <- indirect_effects_from_list(tmp2, pvalue = TRUE)
tmp4 <- formatC(tmp3$pvalue[3], digits = 3, format = "f")
tmp4 <- gsub("^0.", ".", tmp4)
expect_match(tmp, tmp4)

}
