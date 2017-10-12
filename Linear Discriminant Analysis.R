require(ISLR)
require(MASS)
require(boot)
require(ggplot2)
require(dplyr)
devtools::install_github("datadotworld/dwapi-r", build_vignettes = TRUE)

saved_cfg <- data.world::save_config("eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Omplbm5pZmVyY2h1bmciLCJpc3MiOiJhZ2VudDpqZW5uaWZlcmNodW5nOjozZTNiMTAzMy1kYzMxLTRmZTAtYTM4NS0yNDFjMjVmNzY2NTUiLCJpYXQiOjE0ODQ2OTcyNDUsInJvbGUiOlsidXNlcl9hcGlfd3JpdGUiLCJ1c2VyX2FwaV9yZWFkIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.wObx6AHihRFAOixo6S51tLFadtGEsOoJNndHSIPajvSdXshuRzzemuLUxuMvU4vSLjWu4g2PesjXPjPqggdRWA")
data.world::set_config(saved_cfg)

vignette("quickstart", package = "data.world")

require(data.world)

project <- "https://data.world/tommywilczek/f-17-eda-project-2"
BreastCancerDF <- data.world::query(
  data.world::qry_sql("SELECT * FROM BreastCancer"),
  dataset = project
)
View(BreastCancerDF)
summary(BreastCancerDF)

###############################################
######Linear Discriminant Analysis############# Jennifer
?lda

lda.fit=lda(pr_status~tumor + node,data=BreastCancerDF, subset=age_at_initial_pathologic_diagnosis<55)
lda.fit
plot(lda.fit)

BreastCancerDF.55=subset(BreastCancerDF,age_at_initial_pathologic_diagnosis>55)
lda.pred=predict(lda.fit,BreastCancerDF.55)

lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
lda.pred.df = data.frame(lda.pred)

ggplot(lda.pred.df) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class)
ggplot(lda.pred.df) + geom_boxplot(mapping = aes(x=class, y=LD1))
table(lda.pred$class,BreastCancerDF.55$pr_status)
mean(lda.pred$class==BreastCancerDF.55$pr_status)
