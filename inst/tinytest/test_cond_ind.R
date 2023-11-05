# Need to check visually
if (FALSE) {

library(manymome)

# List of indirect effects

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
out_xmy_on_w

out_mome <- index_of_mome(x = "x",
                          y = "y",
                          m = "m",
                          w = "w",
                          fit = fit_lm,
                          boot_ci = TRUE,
                          boot_out = boot_out_lm)
out_mome

std_xmy_on_w <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "y",
                                      m = "m",
                                      fit = fit_lm,
                                      boot_ci = TRUE,
                                      boot_out = boot_out_lm,
                                      standardized_x = TRUE,
                                      standardized_y = TRUE)
std_xmy_on_w

tmp1 <- as_flextable(out_xmy_on_w,
                     var_labels = c(w = "Moderator"),
                     se = TRUE,
                     pvalue = TRUE,
                     digits = 4)
tmp2 <- as_flextable(out_xmy_on_w)
tmp3 <- as_flextable(std_xmy_on_w)
tmp4 <- as_flextable(std_xmy_on_w, indirect_raw = FALSE)

tmp1
tmp2
tmp3
tmp4

# From the tests of manymome

library(manymome)

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
fit_boot_out <- lm2boot_out(fit, R = 100, seed = 87415, progress = FALSE)
out_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_7 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_8 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)

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
# fit_boot_out <- lm2boot_out(fit, R = 100, seed = 87415)
outmo_6 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
outmo_7 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
outmo_8 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)

ft_1 <- as_flextable(out_1)
ft_2 <- as_flextable(out_2)
ft_3 <- as_flextable(out_3)
ft_4 <- as_flextable(out_4)
ft_5 <- as_flextable(out_5)
ft_6 <- as_flextable(out_6)
ft_7 <- as_flextable(out_7)
ft_7b <- as_flextable(out_7, pvalue = TRUE)
ft_8 <- as_flextable(out_8)
ft_8b <- as_flextable(out_8, show_indicators = TRUE)
ft_8c <- as_flextable(out_8, show_wvalues = FALSE)

ft_1
ft_2
ft_3
ft_4
ft_5
ft_6
ft_7
ft_7b
ft_8
ft_8b
ft_8c

ftmo_1 <- as_flextable(outmo_1, footnote = FALSE)
ftmo_2 <- as_flextable(outmo_2)
ftmo_3 <- as_flextable(outmo_3)
ftmo_4 <- as_flextable(outmo_4)
ftmo_5 <- as_flextable(outmo_5)
ftmo_6 <- as_flextable(outmo_6)
ftmo_7 <- as_flextable(outmo_7)
ftmo_8 <- as_flextable(outmo_8)

ftmo_1
ftmo_2
ftmo_3
ftmo_4
ftmo_5
ftmo_6
ftmo_7
ftmo_8

}

