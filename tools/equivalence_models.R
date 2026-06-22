# =============================================================================
# EQUIVALENCE: modeling layer (models.R / robust.R / criteria.R)
# Compares each clean function against the verbatim inline legacy call.
# =============================================================================
suppressMessages({ library(readxl); library(plm); library(sandwich); library(lmtest); library(dplyr) })

PATH <- "data/raw/Qatar_Data_V.1.0.xlsx"; SHEET <- "data_for_model"
FORM <- ln_trade ~ ln_cti + ln_gdpc + distance + crises
GA   <- c("Saudi","UAE","Bahrain","Egypt")

renv <- new.env(); for (f in list.files("R","[.]R$",full.names=TRUE)) sys.source(f, envir=renv)

o <- read_excel(PATH, sheet=SHEET); o$year <- as.numeric(o$year); o$crises <- as.numeric(o$crises)
op <- pdata.frame(o, index=c("country","year"))
dfa <- op[op$country %in% GA, ]; dfb <- op[!op$country %in% GA, ]

diffs <- c()
rec <- function(lbl, a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  d <- suppressWarnings(max(abs(a-b), na.rm=TRUE)); if(!is.finite(d)) d <- NA
  diffs[[lbl]] <<- d
  cat(sprintf("%-34s maxdiff = %.3e\n", lbl, d))
}

# ---- OLS ----
rec("OLS_A coef", coef(lm(FORM,data=dfa)),            coef(renv$fit_ols(dfa)))
rec("OLS_B coef", coef(lm(FORM,data=dfb)),            coef(renv$fit_ols(dfb)))
rec("OLS_A whiteHC1 SE",
    coeftest(lm(FORM,data=dfa), vcov=vcovHC(lm(FORM,data=dfa),type="HC1"))[,"Std. Error"],
    coeftest(renv$fit_ols(dfa), vcov=renv$white_hc(renv$fit_ols(dfa)))[,"Std. Error"])

# ---- FE ----
fe_o <- plm(FORM,data=dfa,index=c("country","year"),model="within"); fe_r <- renv$fit_fe(dfa)
rec("FE_A coef", coef(fe_o), coef(fe_r))
rec("FE_B coef", coef(plm(FORM,data=dfb,index=c("country","year"),model="within")), coef(renv$fit_fe(dfb)))
rec("FE_A DK-HC1 SE",
    coeftest(fe_o, vcovSCC(fe_o,type="HC1"))[,"Std. Error"],
    renv$robust_coeftest(fe_r, function(m) renv$driscoll_kraay(m,"HC1"))[,"Std. Error"])

# ---- RE (all three methods) ----
for (m in c("walhus","amemiya","nerlove")) {
  re_o <- plm(FORM,data=dfa,model="random",random.method=m)
  rec(paste0("RE_A[",m,"] coef"), coef(re_o), coef(renv$fit_re(dfa, method=m)))
}

# ---- CRE (Mundlak) ----
pc <- op
pc$mean_ln_cti<-ave(pc$ln_cti,pc$country); pc$mean_ln_gdpc<-ave(pc$ln_gdpc,pc$country)
pc$mean_distance<-ave(pc$distance,pc$country); pc$mean_crises<-ave(pc$crises,pc$country)
cre_o <- plm(ln_trade~ln_cti+ln_gdpc+distance+crises+mean_ln_cti+mean_ln_gdpc+mean_distance+mean_crises,
             data=pc, model="random")
cre_r <- renv$fit_cre(op)
rec("CRE coef", coef(cre_o), coef(cre_r))

# ---- Criteria (OLS) ----
ols_o <- lm(FORM,data=dfa)
HQ_legacy <- function(model){ ll<-as.numeric(logLik(model)); k<-attr(logLik(model),"df"); n<-nobs(model); -2*ll+2*k*log(log(n)) }
rec("OLS_A AIC", AIC(ols_o), renv$info_criteria(ols_o)$AIC)
rec("OLS_A BIC", BIC(ols_o), renv$info_criteria(ols_o)$BIC)
rec("OLS_A HQ",  HQ_legacy(ols_o), renv$hannan_quinn(ols_o))

# ---- Criteria (manual FE, LOG AIC BIC.R) ----
res<-residuals(fe_o); n<-length(res); s2<-mean(res^2); ll<- -n/2*(log(2*pi)+1+log(s2)); k<-length(coef(fe_o))
rec("FE_A manual AIC", -2*ll+2*k,          renv$fe_manual_criteria(fe_o)$AIC)
rec("FE_A manual BIC", -2*ll+k*log(n),     renv$fe_manual_criteria(fe_o)$BIC)
rec("FE_A manual HQC", -2*ll+2*k*log(log(n)), renv$fe_manual_criteria(fe_o)$HQC)

md <- max(unlist(diffs), na.rm=TRUE)
cat(sprintf("\n==== MODELS LAYER: %d checks, MAX DIFF = %.3e -> %s ====\n",
            length(diffs), md, if(md==0)"BIT-IDENTICAL" else if(md<1e-10)"IDENTICAL (fp tol)" else "DIFFERENCES"))
