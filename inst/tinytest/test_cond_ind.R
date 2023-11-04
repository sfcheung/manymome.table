# Need to check visually
if (FALSE) {

library(manymome)
suppressMessages(library(lavaan))

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
tmp4 <- as_flextable(stdx_boot, pvalue = TRUE)
tmp5 <- as_flextable(stdy_boot, pvalue = TRUE)
tmp6 <- as_flextable(std_boot, pvalue = TRUE)
tmp7 <- as_flextable(x1y2_boot)
tmp8 <- as_flextable(y2_boot)
tmp9 <- as_flextable(x2_boot)
tmp10 <- as_flextable(x2_boot, total_indirect = FALSE)
tmp11 <- as_flextable(x2_boot, footnote = FALSE)

tmp1
tmp2
tmp3
tmp4
tmp5
tmp6
tmp7
tmp8
tmp9
tmp10
tmp11

}

