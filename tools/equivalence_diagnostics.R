# =============================================================================
# EQUIVALENCE: diagnostics layer (diagnostics.R)
# =============================================================================
suppressMessages({ library(readxl); library(plm); library(sandwich); library(lmtest); library(dplyr) })
PATH <- "data/raw/Qatar_Data_V.1.0.xlsx"; SHEET <- "data_for_model"
FORM <- ln_trade ~ ln_cti + ln_gdpc + distance + crises
GA   <- c("Saudi","UAE","Bahrain","Egypt")
renv <- new.env(); for (f in list.files("R","[.]R$",full.names=TRUE)) sys.source(f, envir=renv)

o <- read_excel(PATH, sheet=SHEET); o$year<-as.numeric(o$year); o$crises<-as.numeric(o$crises)
op <- pdata.frame(o, index=c("country","year")); dfa <- op[op$country %in% GA, ]
fe <- plm(FORM,data=dfa,index=c("country","year"),model="within")
re <- plm(FORM,data=dfa,model="random",random.method="walhus")
ols <- lm(FORM, data=dfa)

diffs<-c(); rec<-function(l,a,b){a<-as.numeric(a);b<-as.numeric(b);d<-suppressWarnings(max(abs(a-b),na.rm=TRUE));if(!is.finite(d))d<-NA;diffs[[l]]<<-d;cat(sprintf("%-28s maxdiff=%.3e\n",l,d))}

# Hausman
ho<-phtest(fe,re); hr<-renv$hausman(fe,re)
rec("Hausman stat", ho$statistic, hr$statistic); rec("Hausman p", ho$p.value, hr$p.value)
# Breusch-Pagan
bo<-bptest(ols); br<-renv$bp_test(ols)
rec("BP stat", bo$statistic, br$statistic); rec("BP p", bo$p.value, br$p.value)
# Wooldridge
wo<-pwartest(fe); wr<-renv$wooldridge_serial(fe)
rec("Wooldridge stat", wo$statistic, wr$statistic); rec("Wooldridge p", wo$p.value, wr$p.value)
# Breusch-Godfrey (order 1,2)
for(k in 1:2){go<-bgtest(ols,order=k);gr<-renv$bg_test(ols,order=k)
  rec(paste0("BG[",k,"] stat"),go$statistic,gr$statistic); rec(paste0("BG[",k,"] p"),go$p.value,gr$p.value)}
# Durbin-Watson
do<-dwtest(ols); dr<-renv$durbin_watson(ols)
rec("DW stat", do$statistic, dr$statistic)
# Pesaran CD
co<-pcdtest(fe,test="cd"); cr<-renv$pesaran_cd(fe,"cd")
rec("CD stat", co$statistic, cr$statistic); rec("CD p", co$p.value, cr$p.value)
# VIF vs car::vif (if available)
if (requireNamespace("car", quietly=TRUE)) {
  rec("VIF (vs car)", car::vif(ols), renv$vif_values(ols))
} else cat("VIF                          (car not installed; manual formula used)\n")

# Hausman guard: must error on cross-group
fe_b <- plm(FORM, data=op[!op$country %in% GA,], index=c("country","year"), model="within")
guard <- tryCatch({renv$hausman(fe_b, re); "NO ERROR (bad)"}, error=function(e) "errors as intended")
cat("Cross-group hausman guard:", guard, "\n")

md<-max(unlist(diffs),na.rm=TRUE)
cat(sprintf("\n==== DIAGNOSTICS LAYER: %d checks, MAX DIFF=%.3e -> %s ====\n",
  length(diffs), md, if(md==0)"BIT-IDENTICAL" else if(md<1e-10)"IDENTICAL (fp tol)" else "DIFFERENCES"))
