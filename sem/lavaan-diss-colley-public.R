# update to newest version
#devtools::install_github("alishinski/lavaanPlot")
#devtools::install_github("dr-JT/semoutput")


# Load necessary libraries
library(devtools)
library(rstudioapi) 
library(lavaan)
library(lavaanPlot)
library(DiagrammeRsvg)
library(rsvg)
library(semTable)
library(semoutput)
library(rcrossref)



# Source external R script
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")

# Set working directory to current script's directory
setwd(dirname(getActiveDocumentContext()$path))  





#### Data Cleaning ####

### was already done ####


combined <- read.csv(file = "dissertation_colley_combined_trust_all_core_pub.csv")
combined <- as.data.frame(combined)
names(combined)



# reporting statistics
nrow(combined)
report::report(combined)


#### Structural Equation Modeling (SEM) ####

# 
# model <- '
# # measurement model
# trust_combined =~ overallTiAUnderstanding + overallTiATrust
# eta1 =~ trust_combined + SA + tlx_mental + perceivedSafetyCombined
# # structural model
# SA ~ tlx_mental
# eta1 ~ trust_combined + SA + perceivedSafetyCombined
# '
# 
# 
# # (3) Analyze model with function  lavaan::sem()
# result <- lavaan::sem(model=model, data=combined, estimator="mlr")
# # (4) print results including fit measures and standardized parameters
# summary(result, fit.measures=TRUE, standardized=TRUE)
# 
# 
# 
# # significant standardized paths only
# #lavaanPlot(model = result, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "latent")
# 
# # new version since 0.7.0
# lavaanPlot2(model = result, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), include = c("covs"), coef_labels = TRUE, edge_options = list(color = "grey"), stars = "latent")
# 






model2 <- '
# measurement model
trust_combined =~ overallTiAUnderstanding + overallTiATrust
# structural model
trust_combined ~ SA + tlx_mental + perceivedSafetyCombined
'


result2 <- sem(model=model2, data=combined, estimator="mlr")
summary(result2, fit.measures=TRUE, standardized=TRUE)


semTable(result2, type = "latex", table.float = TRUE, caption ="Results for the SEM." , label = "tab:sem", )
sem_tables(result2)

# new version since 0.7.0
pl <- lavaanPlot2(result2, include = "covs", labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("regress"), coef_labels = TRUE)
pl

lavaanPlot::embed_plot_pdf(pl, "plot2.pdf")


effectsize::interpret(result2) |> xtable(caption = "\\gls{SEM} fit. All but \\gls{PNFI} are satisfactory.", label = "tab:fit")













model3 <- '
# structural model
overallTiATrust ~ SA + tlx_mental + perceivedSafetyCombined
# residual correlations
'

result3 <- sem(model=model3, data=combined, estimator="mlr")
summary(result3, fit.measures=TRUE, standardized=TRUE)


semTable(result3, type = "latex", table.float = TRUE, caption ="Results for the \\gls{SEM} for the subscale Trust." , label = "tab:sem_trust")
sem_tables(result3)

pl <- lavaanPlot2(result3, include = "covs", labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("regress"), coef_labels = TRUE)
pl


lavaanPlot::embed_plot_pdf(pl, "plot-trust.pdf")

effectsize::interpret(result3) |> xtable(caption = "Fit of the \\gls{SEM} for subscale trust. All but \\gls{PNFI} are satisfactory.", label = "tab:fit_trust")




model4 <- '
# structural model
overallTiAUnderstanding ~ SA + tlx_mental + perceivedSafetyCombined
# residual correlations
'

result4 <- sem(model=model4, data=combined, estimator="mlr")
summary(result4, fit.measures=TRUE, standardized=TRUE)


semTable(result4, type = "latex", table.float = TRUE, caption ="Results for the \\glsSEM} for the subscale Understanding." , label = "tab:sem_understanding")
sem_tables(result4)

pl <- lavaanPlot2(result4, include = "covs", labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("regress"), coef_labels = TRUE)
pl

lavaanPlot::embed_plot_pdf(pl, "plot-understanding.pdf")

effectsize::interpret(result4) |> xtable(caption = "Fit of the \\gls{SEM} for subscale Understanding. All but \\gls{PNFI} are satisfactory.", label = "tab:fit_understanding")




