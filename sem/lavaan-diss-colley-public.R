# update to newest version
# devtools::install_github("alishinski/lavaanPlot")
# devtools::install_github("dr-JT/semoutput")


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

#### References ####
# https://files.eric.ed.gov/fulltext/EJ1062693.pdf
# https://lavaan.ugent.be/tutorial/est.html



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



#### Visual Inspection ####


# First: Check your missings:
# Proportion of Missingness
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss = sum(is.na(x)),
      n = length(x),
      propmiss = sum(is.na(x)) / length(x)
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)], d[-ncol(d)])
  return(d[order(d$propmiss), ])
}

miss_vars <- propmiss(combined)
miss_vars_mean <- mean(miss_vars$propmiss)
miss_vars_ges <- miss_vars %>% arrange(desc(propmiss))

plot1 <- ggplot(miss_vars_ges, aes(x = reorder(variable, propmiss), y = propmiss * 100)) +
  geom_point(size = 3) +
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("Missingness per variable") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed")
  ) +
  ggtitle("Percentage of missingness")
plot1


md.pattern(combined, rotate.names = TRUE)





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







model2 <- "
# measurement model
trust_combined =~ overallTiAUnderstanding + overallTiATrust
# structural model
trust_combined ~ SA + tlx_mental + perceivedSafetyCombined
"








result2 <- sem(model = model2, data = combined, estimator = "mlr", missing = "fiml")
summary(result2, fit.measures = TRUE, standardized = TRUE)


semTable(result2, type = "latex", table.float = TRUE, caption = "Results for the SEM.", label = "tab:sem", )
sem_tables(result2)

# new version since 0.7.0
pl <- lavaanPlot2(result2, include = "covs", labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("regress"), coef_labels = TRUE)
pl

lavaanPlot::embed_plot_pdf(pl, "plot2.pdf")


effectsize::interpret(result2) |> xtable(caption = "\\gls{SEM} fit. All but \\gls{PNFI} are satisfactory.", label = "tab:fit")













model_trust <- "
# structural model
overallTiATrust ~ SA + tlx_mental + perceivedSafetyCombined
# residual correlations
"

result3 <- sem(model = model_trust, data = combined, estimator = "mlr", missing = "fiml")
summary(result3, fit.measures = TRUE, standardized = TRUE)


semTable(result3, type = "latex", table.float = TRUE, caption = "Results for the \\gls{SEM} for the subscale Trust.", label = "tab:sem_trust")
sem_tables(result3)

pl <- lavaanPlot2(result3, include = "covs", labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("regress"), coef_labels = TRUE)
pl


lavaanPlot::embed_plot_pdf(pl, "plot-trust.pdf")

effectsize::interpret(result3) |> xtable(caption = "Fit of the \\gls{SEM} for subscale trust. All but \\gls{PNFI} are satisfactory.", label = "tab:fit_trust")







model_understanding <- "
# structural model
overallTiAUnderstanding ~ SA + tlx_mental + perceivedSafetyCombined
# residual correlations
"

result4 <- sem(model = model_understanding, data = combined, estimator = "mlr", missing = "fiml")
summary(result4, fit.measures = TRUE, standardized = TRUE)


semTable(result4, type = "latex", table.float = TRUE, caption = "Results for the \\glsSEM} for the subscale Understanding.", label = "tab:sem_understanding")
sem_tables(result4)

pl <- lavaanPlot2(result4, include = "covs", labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("regress"), coef_labels = TRUE)
pl

lavaanPlot::embed_plot_pdf(pl, "plot-understanding.pdf")

effectsize::interpret(result4) |> xtable(caption = "Fit of the \\gls{SEM} for subscale Understanding. All but \\gls{PNFI} are satisfactory.", label = "tab:fit_understanding")










#### Future Work ####

### Imputation-Based Methods ###
#reference: https://statistics.ohlsen-web.de/multiple-imputation-with-mice/

library(mitools)
library(semTools)
library(mice)


# run lavaan and imputation in one step
out_trust <- runMI(model_trust,
  data = combined,
  m = 5,
  miPackage = "mice",
  fun = "sem",
  meanstructure = TRUE
)
summary(out_trust)
fitMeasures(out_trust, "chisq")



out_understanding <- runMI(model_understanding,
  data = combined,
  m = 5,
  miPackage = "mice",
  fun = "sem",
  meanstructure = TRUE
)
summary(out_understanding)
fitMeasures(out_understanding, "chisq")
