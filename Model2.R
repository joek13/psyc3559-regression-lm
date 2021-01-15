# Group 1 PSYC 3559 Final
# Model 2 - behavSum ~ polit + percv_sev
# Code: Joe Kerrigan
#

# 0. Import libraries
library(psych)
library(car)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(quantreg)
library(purrr)
library(effects)
library(WebPower)

# 0.5. Configure ggplot themeing
theme_update(plot.title = element_text(hjust = 0.5))

polit_colors <- c(
  "Democrat"="#43a2ca",
  "Republican"="#e34a33",
  "Other"="#31a354",
  "Prefer not to say"="#fa9fb5"
)

#
# DEPENDENT VARIABLE
#  behavSum - sum of protective behaviors
#  range: 0-12 inclusive
#  level of measurement: equal interval
#
# INDEPENDENT VARIABLES
# 
# polit - What is your political affiliation?
#  Democrat = 0
#  Republican = 1
#  Other = 2;
#  Prefer not to say = 3;
# percv_sev - On a scale of 0-10, if you were infected with coronavirus, how severe do you think it would be?
#  0 (not severe) - 10 (very severe)

# 1. Load data
allData <- read.csv("../data/KAPJTerm.csv", stringsAsFactors = TRUE, check.names = FALSE, na.strings = "", sep = ",")
data <- read.csv("../data/KAPJTerm.csv", stringsAsFactors = TRUE, check.names = FALSE, na.strings = "", sep = ",")

# Do some cleanup on the data
# Reorder factor levels for polit, so it displays nicely
data$polit <- factor(data$polit, levels=c("Democrat", "Republican", "Other", "Prefer not to say"))

# 2. Handle missingness
# NB: In general, it's a bad idea to simply "throw out" missing cases. But for our purposes, it's alright.
# That's exactly what we'll do.

countAndRemoveMissing <- function (colName) {
  # This function counts the number of missing observations in data for column colName.
  # Prints them out and removes them.
  missing <- subset(allData, is.na(allData[,colName]))
  nMissing <- nrow(missing)
  message("There are a total of ", nMissing, " observations for column ", colName, ". They have been removed.")
  data <<- subset(data, !is.na(data[,colName]))
}

countAndRemoveMissing("behavSum")
countAndRemoveMissing("polit")
countAndRemoveMissing("percv_sev")

nDeleted <- nrow(allData) - nrow(data)
message("In the end, we deleted ", nDeleted, " observations due to missingness in one or more variables.")
# In our case, this value is only 487. Maybe this is just a coincidence, but all of the cases that had missing percv_sev also had missing polit. Interesting!

# 3. Descriptive statistics & simple plots
# Dependent variable

summary(data$behavSum)
describe(data$behavSum)

# And a simple histogram
d <- ggplot(data, aes(x = behavSum))
d + geom_bar() + 
  labs(x = "# of Protective Behaviors", y = "Count") + 
  ggtitle("Histogram of # of Protective Behaviors")

# Independent variables
# polit
table(data$polit) # raw counts
table(data$polit) / sum(table(data$polit)) # proportions
# And a simple barchart for polit
d <- ggplot(data, aes(x=polit, fill=polit))
d + geom_bar() +
  scale_fill_manual(values=polit_colors) +
  geom_text(stat="count", aes(label=..count..), vjust=-0.75) + 
  ggtitle("Participants' Political Affiliations") +
  xlab("Political Affiliation") +
  ylab("Count") + 
  theme(legend.position = "none")

# percv_sev
summary(data$percv_sev)
describe(data$percv_sev)
# and another histogram/barchart for percv_sev
d <- ggplot(data, aes(x=percv_sev))
d + geom_bar() +
  ggtitle("Participants' Perceived Severity of COVID") +
  xlab("Perceived Severity of COVID") +
  ylab("Count")


# 4. Relationship plots 
# IV Interactions - are there any significant shifts we should know about?
# Let's plot perceived COVID severity, conditioned on political affiliations
# (We're approximating the conditional distribution of percv_sev | polit)
plotSevByPolit <- function(affiliation) {
  affiliated_subset <- subset(data, data$polit == affiliation)
  d <- ggplot(affiliated_subset, aes(x=percv_sev, fill=polit))
  d + geom_bar() +
    scale_fill_manual(values=polit_colors) +
    xlab("") +
    ylab("") +
    ggtitle(affiliation) + 
    theme(legend.position = "none")
}
plots <- map(levels(data$polit), plotSevByPolit)
combined_polit_figure <- ggarrange(plotlist=plots, ncol = 2, nrow = 2)
annotate_figure(combined_polit_figure, top = text_grob("Perceived Severity of COVID by Political Affiliation", size=14), left="Count", bottom="Perceived Severity of COVID")

# IV vs. DV interactions
#  Let's visualize the effect our IVs have on our DV
#  Protective behaviors by political affiliation
plotBehavByPolit <- function(affiliation) {
  affiliated_subset <- subset(data, data$polit == affiliation)
  d <- ggplot(affiliated_subset, aes(x=behavSum, fill=polit))
  d + geom_bar() +
    scale_fill_manual(values=polit_colors) +
    xlab("") +
    ylab("") +
    ggtitle(affiliation) +
    theme(legend.position = "none")
}
plots <- map(levels(data$polit), plotBehavByPolit)
combined_polit_figure <- ggarrange(plotlist=plots, ncol = 2, nrow = 2)
annotate_figure(combined_polit_figure, top = text_grob("# of Protective Behaviors by Political Affiliation", size=14), left="Count", bottom="# of Protective Behaviors")

#  Protective behaviors by perceived severity
d <- ggplot(data, aes(x=percv_sev, y=behavSum))
r2 <- cor(data$percv_sev, data$behavSum)
d + geom_jitter(width = 0.5, height = 0.5) + # plot is jittered for clarity
  xlab("Perceived Severity of COVID") +
  ylab("# of Protective Behaviors") +
  ggtitle("Jittered Plot of Perceived Severity of COVID vs. # of Protective Behaviors")
# 5. Assumptions
# First: what distribution does our dependent variable follow? Assess with some plots.
d <- ggplot(data, aes(x = behavSum))

distplot <- d + geom_bar() + 
  labs(x = "# of Protective Behaviors", y = "Count") + 
  ggtitle("Histogram of # of Protective Behaviors") + 
  stat_function(fun = function (x) { dnorm(x, mean = mean(data$behavSum), sd = sd(data$behavSum)) * length(data$behavSum) })

# Let's verify this with a Q-Q plot
d <- ggplot(data, aes(sample = behavSum))
qqplot <- d + geom_qq() +
  ggtitle("Q-Q Plot for # of Protective Behaviors") +
  labs(x="Theoretical Quantile", y = "Sample Quantile") +
  geom_qq_line()

ggarrange(distplot, qqplot, nrow=1, ncol=2)

# Looks normal enough!

# 5. Power Analysis
# assume a small effect - use Cohen's guidance
effect <- 0.02 # assumed small effect by Cohen
wp.regression(n = nrow(data), p1 = 3, f2 = effect) # power of 1! hot dog!

# ??. Define the model

# Recode polit so we can define appropriate contrasts
# First, we merge Other and Prefer not to say into a single group
data$politMerged <- Recode(data$polit, "'Democrat' = 'Democrat'; 'Republican' = 'Republican'; 'Prefer not to say' = 'Other/Prefer not to say'; 'Other' = 'Other/Prefer not to say';")

# Then, we reorder the factor so that our reference group is first
data$politMerged <- factor(data$politMerged, levels=c("Other/Prefer not to say", "Democrat", "Republican"))

# Use dummy (treatment) contrasts for our variable
# we are comparing each group to a reference in the unaffiliated group
contrasts(data$politMerged) <- contr.treatment(3)

# Even though perceived severity is a Likert scale, and is "truly" categorical,
# we're going to interpret it like it is numerical.
# Justification: a Likert scale with 11 values is almost more numerical than it is categorical.
# And if we were to create polynomial contrasts, we'd be fitting a 10th-degree polynomial.
# It'll increase our R^2, but it's prone to overfitting.

model <- lm(behavSum ~ politMerged + percv_sev, data=data)
summary(model)

# ??. Check assumptions of model

# Check linearity
par(mfrow=c(1,2))
plot(model, 1)
plot(model, 3)

par(mfrow=c(1,2))
plot(model, 4)
plot(model, 5)

par(mfrow=c(1,1))

plot(1:4998, resid(model))

res <- resid(model)
res_dataframe <- data.frame(res)
d <- ggplot(res_dataframe, aes(x=res))
resHist <- d + geom_histogram(aes(y=..density..), breaks=seq(-6,6, by=1), color="#000000") +
  stat_function(fun = function (x) { dnorm(x, mean = mean(res), sd = sd(res)) }) +
  ggtitle("Histogram of Model Residuals") +
  xlab("Residuals") +
  ylab("Probability")

d <- ggplot(res_dataframe, aes(sample=res))
resQq <- d + geom_qq() + 
  geom_qq_line() +
  xlab("Theoretical Quantile") +
  ylab("Sample Quantile") + 
  ggtitle("Q-Q Plot of Model Residuals")

d <- ggplot(res_dataframe, aes(y=res))
resBoxplot <- d + geom_boxplot() +
  ylab("Model Residuals") +
  ggtitle("Boxplot of Model Residuals") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())

d <- ggplot(res_dataframe, aes(y=res, x=1:nrow(res_dataframe)))
resPlot <- d + geom_point(shape = 21, color = "#000000") +
  ggtitle("Model Residuals") + 
  xlab("Observations") + 
  ylab("Model Residuals")

ggarrange(resHist, resQq, resBoxplot, resPlot, nrow=2, ncol=2)

# NB: data is symmetric, even if tails are heavy
# make sure to note that our procedures can be robust to that

# shapiro.test(res)
library(moments)
anscombe.test(res)

agostino.test(res) # no skewness, some kurtosis

# ??. Model comparisons
# Let's create a reference constant model
model.0 <- lm(behavSum ~ 1, data=data)
# And compare the AIC/BIC for the reference and target model
AIC(model.0, model)
BIC(model.0, model)

# Nested model comparison
#  Add parameters one at a time and compare AIC/BIC
model.politOnly <- lm(behavSum ~ politMerged, data=data)
model.percvSevOnly <- lm(behavSum ~ percv_sev, data=data)
AIC(model.0, model.politOnly, model.percvSevOnly, model)
BIC(model.0, model.politOnly, model.percvSevOnly, model)


# Plot the effects of our model
fx <- allEffects(model)

# Model results
summary(model) # which variables are significant?
df <- nrow(data) - 3 - 1 # k = num predictors = 3

# Outliers
cooksd <- cooks.distance(model)
cutoff <- 20 * mean(cooksd)

outliers <- keep(cooksd, function(x) {x > cutoff})

# ??. Post study power analysis
# Let's get our effect size
# Cohen's effect size, f2, = (R^2) / (1 - R^2)
R2 <- summary(model)$r.squared
f2 <- R2 / (1 - R2)
message("We calculated an effect size of ", f2)
# This effect size is... small
wp.regression(n = nrow(data), p1=3, f2 = f2)



