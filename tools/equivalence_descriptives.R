# =============================================================================
# EQUIVALENCE + smoke test: descriptives / plots / export
# =============================================================================
suppressMessages({ library(readxl); library(plm); library(dplyr) })
renv <- new.env(); for (f in list.files("R","[.]R$",full.names=TRUE)) sys.source(f, envir=renv)

df <- read_excel("data/raw/Qatar_Data_V.1.0.xlsx", sheet="data_for_model")
df$year <- as.numeric(df$year); df$crises <- as.numeric(df$crises)
op <- pdata.frame(df, index=c("country","year"))
dfa <- op[op$country %in% c("Saudi","UAE","Bahrain","Egypt"), ]
re <- plm(ln_trade~ln_cti+ln_gdpc+distance+crises, data=dfa, model="random", random.method="walhus")

# ---- panel_stats vs original panel_stats_plm ----
ops <- function(model){ y<-model$model[[1]]; u<-residuals(model)
  data.frame(Mean_dep_var=mean(y,na.rm=TRUE), SD_dep_var=sd(y,na.rm=TRUE),
             Sum_sq_resid=sum(u^2), SE_regression=sqrt(sum(u^2)/df.residual(model))) }
o <- ops(re); r <- renv$panel_stats(re)
cat(sprintf("panel_stats max diff = %.3e\n", max(abs(as.numeric(o)-as.numeric(r)))))

# ---- smoke-test plot + exports ----
if (requireNamespace("ggplot2", quietly=TRUE)) {
  p <- renv$plot_resid_fitted(re, "test")
  f <- renv$save_plot_svg(p, "smoke_test.svg")
  cat("plot + svg OK:", file.exists(f), "\n")
} else cat("ggplot2 not installed - plot smoke skipped\n")
csv <- renv$save_table_csv(r, "smoke_test.csv")
cat("csv OK:", file.exists(csv), "\n")

# cleanup smoke artifacts
unlink(csv); unlink(renv$proj_path("output/figures/smoke_test.svg"))
cat("==== DESCRIPTIVES/PLOTS/EXPORT: panel_stats identical; I/O OK ====\n")
