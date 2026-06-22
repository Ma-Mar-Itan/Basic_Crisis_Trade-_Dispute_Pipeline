# =============================================================================
# FUNCTIONAL EQUIVALENCE HARNESS
# Runs the ORIGINAL inline logic and the REFACTORED functions on the REAL data
# and diffs every numeric output. Goal: try to prove the refactor WRONG.
# Run:  Rscript tools/equivalence_check.R
# =============================================================================

suppressMessages({
  library(readxl); library(plm); library(sandwich); library(lmtest)
  library(dplyr)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

PATH  <- "data/raw/Qatar_Data_V.1.0.xlsx"
SHEET <- "data_for_model"
FORM  <- ln_trade ~ ln_cti + ln_gdpc + distance + crises
GA    <- c("Saudi", "UAE", "Bahrain", "Egypt")

# Load refactored modules
renv <- new.env()
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) sys.source(f, envir = renv)

cmp <- function(label, o, r, tol = 0) {
  o <- as.numeric(o); r <- as.numeric(r)
  d <- suppressWarnings(max(abs(o - r), na.rm = TRUE))
  if (!is.finite(d)) d <- if (all(is.na(o) == is.na(r))) 0 else NaN
  data.frame(metric = label, original = o, refactored = r,
             difference = r - o, stringsAsFactors = FALSE)
}
results <- list(); add <- function(df) results[[length(results) + 1]] <<- df

# ---------------------------------------------------------------------------
# 1. ORIGINAL data path (verbatim inline logic)
# ---------------------------------------------------------------------------
o_data <- read_excel(PATH, sheet = SHEET)
o_df <- o_data
o_df$year   <- as.numeric(o_df$year)
o_df$crises <- as.numeric(o_df$crises)
o_pdata <- pdata.frame(o_df, index = c("country", "year"))
o_dfa <- o_pdata[o_pdata$country %in% GA, ]
o_dfb <- o_pdata[!o_pdata$country %in% GA, ]

# ---------------------------------------------------------------------------
# 2. REFACTORED data path (functions)
# ---------------------------------------------------------------------------
r_df    <- renv$load_panel(PATH, SHEET)
r_pdata <- renv$build_pdata(r_df)
r_g     <- renv$split_groups(r_pdata, GA)
r_dfa   <- r_g$A; r_dfb <- r_g$B

# ---------------------------------------------------------------------------
# 3. INTERMEDIATE DATA IDENTITY
# ---------------------------------------------------------------------------
cat("\n#### INTERMEDIATE DATASET IDENTITY ####\n")
chk <- function(lbl, a, b) {
  a <- as.data.frame(a); b <- as.data.frame(b)
  num <- intersect(names(a), names(b))
  ident <- isTRUE(all.equal(a[num], b[num], check.attributes = FALSE))
  cat(sprintf("%-28s rows %d/%d  identical=%s\n", lbl, nrow(a), nrow(b), ident))
  ident
}
chk("df vs load_panel", o_df, r_df)
chk("df_a", o_dfa, r_dfa)
chk("df_b", o_dfb, r_dfb)

# helper: fit a battery on a given (dfa, dfb) and return a named numeric vector
battery <- function(dfa, dfb) {
  ols_a <- lm(FORM, data = dfa);  ols_b <- lm(FORM, data = dfb)
  fe_a  <- plm(FORM, data = dfa, index = c("country","year"), model = "within")
  fe_b  <- plm(FORM, data = dfb, index = c("country","year"), model = "within")
  re_a  <- plm(FORM, data = dfa, model = "random", random.method = "walhus")
  re_b  <- plm(FORM, data = dfb, model = "random", random.method = "walhus")
  sfe_a <- summary(fe_a); sfe_b <- summary(fe_b)
  rob_a <- coeftest(fe_a, vcovSCC(fe_a, type = "HC1"))
  rob_b <- coeftest(fe_b, vcovSCC(fe_b, type = "HC1"))
  haus_a <- phtest(fe_a, re_a); haus_b <- phtest(fe_b, re_b)
  bp_a <- bptest(ols_a); bp_b <- bptest(ols_b)
  cd_a <- pcdtest(fe_a, test = "cd"); cd_b <- pcdtest(fe_b, test = "cd")
  list(
    ols_a_coef = coef(ols_a), ols_b_coef = coef(ols_b),
    fe_a_coef = coef(fe_a),   fe_b_coef = coef(fe_b),
    re_a_coef = coef(re_a),   re_b_coef = coef(re_b),
    fe_a_se = coef(sfe_a)[, "Std. Error"], fe_b_se = coef(sfe_b)[, "Std. Error"],
    fe_a_rob_se = rob_a[, "Std. Error"],   fe_b_rob_se = rob_b[, "Std. Error"],
    fe_a_rob_p  = rob_a[, "Pr(>|t|)"],     fe_b_rob_p  = rob_b[, "Pr(>|t|)"],
    fe_a_r2 = sfe_a$r.squared["rsq"], fe_b_r2 = sfe_b$r.squared["rsq"],
    fe_a_adjr2 = sfe_a$r.squared["adjrsq"], fe_b_adjr2 = sfe_b$r.squared["adjrsq"],
    ols_a_aic = AIC(ols_a), ols_b_aic = AIC(ols_b),
    ols_a_bic = BIC(ols_a), ols_b_bic = BIC(ols_b),
    haus_a_stat = haus_a$statistic, haus_b_stat = haus_b$statistic,
    haus_a_p = haus_a$p.value, haus_b_p = haus_b$p.value,
    bp_a_stat = bp_a$statistic, bp_b_stat = bp_b$statistic,
    bp_a_p = bp_a$p.value, bp_b_p = bp_b$p.value,
    cd_a_stat = cd_a$statistic, cd_b_stat = cd_b$statistic,
    cd_a_p = cd_a$p.value, cd_b_p = cd_b$p.value,
    desc_a_mean = mean(dfa$ln_trade, na.rm=TRUE), desc_b_mean = mean(dfb$ln_trade, na.rm=TRUE),
    desc_a_sd = sd(dfa$ln_trade, na.rm=TRUE), desc_b_sd = sd(dfb$ln_trade, na.rm=TRUE)
  )
}

cat("\n#### MODEL BATTERY (fitting on both data paths) ####\n")
O <- battery(o_dfa, o_dfb)
R <- battery(r_dfa, r_dfb)

# Build comparison rows for every scalar / vector element
for (nm in names(O)) {
  ov <- O[[nm]]; rv <- R[[nm]]
  if (length(ov) > 1) {
    for (i in seq_along(ov))
      add(cmp(paste0(nm, "[", names(ov)[i] %||% i, "]"), ov[i], rv[i]))
  } else {
    add(cmp(nm, ov, rv))
  }
}

# ---------------------------------------------------------------------------
# 4. FIRST DIFFERENCE  (original Stationary-nb2 logic vs add_first_difference)
# ---------------------------------------------------------------------------
cat("\n#### FIRST DIFFERENCE d_ln_gdpc ####\n")
o_diff <- o_df %>% arrange(country, year) %>% group_by(country) %>%
  mutate(d_ln_gdpc = ln_gdpc - dplyr::lag(ln_gdpc)) %>% ungroup()
r_diff <- renv$add_first_difference(o_df, "ln_gdpc")
o_dv <- o_diff$d_ln_gdpc; r_dv <- r_diff$d_ln_gdpc
diff_max <- suppressWarnings(max(abs(o_dv - r_dv), na.rm = TRUE))
na_match <- all(is.na(o_dv) == is.na(r_dv))
cat(sprintf("d_ln_gdpc max abs diff = %.3e | NA pattern identical = %s | n=%d\n",
            diff_max, na_match, length(o_dv)))
add(data.frame(metric="d_ln_gdpc (max abs diff)", original=0,
               refactored=diff_max, difference=diff_max))

# ---------------------------------------------------------------------------
# 5. UNIT-ROOT INPUT (original clean_panel_var vs refactored) + purtest
# ---------------------------------------------------------------------------
cat("\n#### UNIT-ROOT: clean + purtest ####\n")
o_clean_fun <- function(data, varname, min_T = 6) {
  data %>% mutate(country=as.factor(country), year=as.integer(year),
                  value=as.numeric(.data[[varname]])) %>%
    filter(!is.na(country), !is.na(year), is.finite(value)) %>%
    arrange(country, year) %>% distinct(country, year, .keep_all=TRUE) %>%
    group_by(country) %>% filter(n() >= min_T) %>% ungroup()
}
for (v in c("ln_trade","ln_cti","ln_gdpc")) {
  oc <- o_clean_fun(o_df, v); rc <- renv$clean_panel_var(o_df, v)
  val_diff <- suppressWarnings(max(abs(oc$value - rc$value), na.rm=TRUE))
  op <- purtest(pdata.frame(oc, index=c("country","year"))$value, test="ips", exo="intercept", lags=0)
  rp <- purtest(pdata.frame(rc, index=c("country","year"))$value, test="ips", exo="intercept", lags=0)
  os <- as.numeric(op$statistic$statistic); rs <- as.numeric(rp$statistic$statistic)
  opv <- as.numeric(op$statistic$p.value);  rpv <- as.numeric(rp$statistic$p.value)
  cat(sprintf("%-9s value-diff=%.2e  IPS stat O=%.5f R=%.5f  p O=%.5f R=%.5f\n",
              v, val_diff, os, rs, opv, rpv))
  add(cmp(paste0("IPS_stat[",v,"]"), os, rs))
  add(cmp(paste0("IPS_p[",v,"]"), opv, rpv))
}

# ---------------------------------------------------------------------------
# 6. FINAL TABLE
# ---------------------------------------------------------------------------
tab <- do.call(rbind, results)
tab$difference[!is.finite(tab$difference)] <- NA
maxdiff <- max(abs(tab$difference), na.rm = TRUE)
cat("\n#### COMPARISON TABLE (all metrics) ####\n")
print(within(tab, {
  original   <- signif(original, 7)
  refactored <- signif(refactored, 7)
  difference <- signif(difference, 3)
}), row.names = FALSE)

cat(sprintf("\n==== METRICS COMPARED: %d ====\n", nrow(tab)))
cat(sprintf("==== MAX ABSOLUTE DIFFERENCE ACROSS ALL METRICS: %.3e ====\n", maxdiff))
cat(sprintf("==== VERDICT: %s ====\n",
            if (maxdiff == 0) "BIT-IDENTICAL" else
            if (maxdiff < 1e-10) "IDENTICAL TO FLOATING-POINT TOLERANCE" else "DIFFERENCES FOUND"))
