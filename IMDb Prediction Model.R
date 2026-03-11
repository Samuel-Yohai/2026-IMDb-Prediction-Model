
install.packages("e1071")
install.packages("corrplot")
install.packages("car") 
install.packages("lmtest")
install.packages("plm")
install.packages("psych")
install.packages("splines")
install.packages("stargazer")

#Loading data set. Please change the route to your own to test the code
IMDb_data <- read_csv("Desktop/Stats/IMDB_data_Winter_2026.csv")
test_data <- read_csv("Desktop/Stats/test_data_IMDB.csv")
data_dictionary <- read_csv("Desktop/Stats/data_dictionary_IMDB_Winter_2026.csv")
attach(IMDb_data)
attach(test_data)
attach(data_dictionary)

View(IMDb_data)




###############################################################################
###Part 1: Data Exploration
# ------------------------------------------------------------------------
##a) Getting the "feel for the data set"
str(IMDb_data)
dim(IMDb_data)
summary(IMDb_data)
#Count missing value for each column
colSums(is.na(IMDb_data))

# ------------------------------------------------------------------------
##b) Histogram & boxplots
#Select numerical variables
numeric_vars = IMDb_data[, c(
  "imdb_score",
  "movie_budget",
  "release_year",
  "duration",
  "aspect_ratio",
  "nb_news_articles",
  "actor1_star_meter",
  "actor2_star_meter",
  "actor3_star_meter",
  "nb_faces",
  "movie_meter_IMDBpro"
)]

#Plot All Histograms
par(mfrow = c(3, 4))  # 3x4 grid
for (var in names(numeric_vars)) {
  hist(numeric_vars[[var]],
       main = paste("Histogram of", var),
       col = "green",
       breaks = 20)
}

#Plot boxplots
par(mfrow = c(3, 4)) 
for (var in names(numeric_vars)) {
  boxplot(numeric_vars[[var]],
          main = paste("Boxplot of", var))
}

par(mfrow = c(1, 1)) #reset grid format

# ------------------------------------------------------------------------
##c) Checking skewness
library(e1071)
skew_values = sapply(numeric_vars, skewness, na.rm = TRUE)
skew_values

# ------------------------------------------------------------------------
##d) Correlation Matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
round(cor_matrix, 2)
library(corrplot)
corrplot(cor_matrix, 
         method = "color",
         col = colorRampPalette(c("blue", "white", "red"))(200))
#From the correlation matrix, find that "duration", "nb_news_articles"

# ------------------------------------------------------------------------
##e) Outliers
full_model = lm(imdb_score ~ ., data = numeric_vars)
library(car)
outlierTest(full_model)
#Checking outliers
numeric_vars[c(492,1806,316,1581,191,989,395), ]
#Here we notice tha all 7 outliers are legitimate data points so they should not be removed
#Result: no outlier removed

# ------------------------------------------------------------------------
##f) Simple regressions for all numerical variables
for (var in names(numeric_vars)) {
  if (var != "imdb_score") {
    formula = as.formula(paste("imdb_score ~", var))
    model = lm(formula, data = IMDb_data)
    print(summary(model))
  }
}
#Based on the results from simple regressions, we have observed that duration has the strongest R square (0.1686),
#, nb_news_articles the second strongest R square (0.05083), release_year the third (0.03796)
#with the other variables with small to zero R square
#Therefore we will focus on these three variables for now

# ------------------------------------------------------------------------
##g) Heteroskedasticity & Collinearity
#Heteroskedasicity
#Test with duration
library(lmtest)
library(car)
hreg = lm(imdb_score~duration, data = IMDb_data)
b0=coef(hreg)[1]
b1=coef(hreg)[2]
summary(hreg)
bptest(hreg)
#Funnel test
residualPlot(hreg, quadratic=FALSE)
#NCV test
ncvTest(hreg) #Note that p < 0.05 --> heteroskedasticity present 
#Heteroskedasticity-corrected model
require(lmtest)
library(sandwich)
coeftest(hreg, vcov=vcovHC(hreg, type="HC1"))

#Collinearity
require(psych)
vif(full_model) 
#Note that VIF for all variable are less than 4, there is no collinearity problem present
quantvars = IMDb_data[, c("imdb_score","duration","release_year","nb_news_articles","movie_budget")]
pairs.panels(quantvars,method = "pearson",hist.col = "lightgray",ellipses = TRUE)
###############################################################################



###############################################################################
###Part 2: Functional Form & Non-Linearity
# ------------------------------------------------------------------------
## a)Testing Polynomial Terms
quadratic_reg1=lm(imdb_score ~ poly(duration, 2 ,raw = TRUE), data = IMDb_data)  #Degree 2
cubic_reg1=lm(imdb_score~ poly(duration,3, raw = TRUE), data = IMDb_data)  #Degree 3
quartic_reg1=lm(imdb_score~ poly(duration,4,raw = TRUE), data = IMDb_data) #Degree 4
summary(hreg)$r.squared        
summary(quadratic_reg1)$r.squared 
summary(cubic_reg1)$r.squared
summary(quartic_reg1)$r.squared 
anova(hreg, quadratic_reg1, cubic_reg1, quartic_reg1)
#Based on the results, quadratic model perform the best

# ------------------------------------------------------------------------
##b) Testing Log Transformations
log_model_duration= lm(imdb_score ~ log(duration), data = IMDb_data)
summary(log_model_duration)

log_model_news = lm(imdb_score ~ log(nb_news_articles + 1), data = IMDb_data)
summary(log_model_news)

log_model_year = lm(imdb_score ~ log(release_year), data = IMDb_data)
summary(log_model_year)

train_data = IMDb_data[idx_train, ]
valid_data = IMDb_data[-idx_train, ]  # save this immediately as valid_data
test_data_holdout = read.csv('/Users/kevinsun/MGSC 401/Midterm Project/test_data_IMDB.csv')  # rename to avoid confusion

# ------------------------------------------------------------------------
##c) Testing splines
library(splines)
library(ggplot2)

plot=ggplot(IMDb_data, aes(y=imdb_score, x=duration))
scatter= geom_point(color="grey")
knot_points = as.numeric(quantile(IMDb_data$duration, probs = c(0.25, 0.50, 0.75)))
# B-splines with explicit knots (degree 1 and degree 2)
spline_1= geom_smooth(method = "lm", formula = y~bs(x,knots=knot_points, degree=1), aes(color="red"))
spline_2= geom_smooth(method = "lm", formula = y~bs(x,knots=knot_points, degree=2), aes(color="blue"))
spline_3=geom_smooth(method = "lm", formula = y~bs(x,knots=knot_points, degree=3), aes(color="green"))
spline_4=geom_smooth(method = "lm", formula = y~bs(x,knots=knot_points, degree=4), aes(color="purple") )
spline_plot=plot+scatter+spline_1+spline_2+spline_3+spline_4
spline_plot
#To make a legend
spline_plot+scale_color_identity(name = "Splines",
                                 breaks = c("red", "blue", "green", "purple"),
                                 labels = c("d=1", "d=2", "d=3", "d=4"),
                                 guide = "legend")
#With quantiles
k1= quantile(IMDb_data$duration,.20)
k2= quantile(IMDb_data$duration,.40)
k3= quantile(IMDb_data$duration,.60)
k4= quantile(IMDb_data$duration,.80)
reg_w_knots=lm(imdb_score~bs(duration,knots=c(k1,k2,k3, k4), degree=3), data = IMDb_data)
eq_spline= geom_smooth(method = "lm", formula = y~bs(x,knots=c(k1,k2,k3,k4), degree=3))
plot+scatter+eq_spline+geom_vline(xintercept=c(k1,k2,k3,k4), linetype="dotted")

# ------------------------------------------------------------------------
##d) Comparing R squared and performance
library(splines)
library(ggplot2)
set.seed(1)

n = nrow(IMDb_data)
idx_train = sample.int(n, size = floor(0.8 * n), replace = FALSE)

train_data = IMDb_data[idx_train, ]
test_data = IMDb_data[-idx_train, ]

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

#Choose knot locations from train
knot_points = as.numeric(quantile(train_data$duration, probs = c(0.25, 0.50, 0.75), na.rm = TRUE))

# Fit models
m_linear    <- lm(imdb_score ~ duration, data = train_data)
m_quad      <- lm(imdb_score ~ poly(duration, 2, raw = TRUE), data = train_data)
m_cubic     <- lm(imdb_score ~ poly(duration, 3, raw = TRUE), data = train_data)
m_quartic   <- lm(imdb_score ~ poly(duration, 4, raw = TRUE), data = train_data)
m_log_dur   <- lm(imdb_score ~ log(duration), data = train_data)
m_log_news  <- lm(imdb_score ~ log(nb_news_articles + 1), data = train_data)
m_spline_d1 <- lm(imdb_score ~ bs(duration, knots = knot_points, degree = 1), data = train_data)
m_spline_d2 <- lm(imdb_score ~ bs(duration, knots = knot_points, degree = 2), data = train_data)
m_spline_d3 <- lm(imdb_score ~ bs(duration, knots = knot_points, degree = 3), data = train_data)

# Collect R squared and RMSE
models <- list(
  linear      = m_linear,
  quadratic   = m_quad,
  cubic       = m_cubic,
  quartic     = m_quartic,
  log_duration= m_log_dur,
  log_news    = m_log_news,
  spline_d1   = m_spline_d1,
  spline_d2   = m_spline_d2,
  spline_d3   = m_spline_d3
)

results <- data.frame(
  model      = names(models),
  train_r2   = NA_real_,
  valid_rmse = NA_real_
)

for (i in seq_along(models)) {
  fit <- models[[i]]
  results$train_r2[i] <- summary(fit)$r.squared
  pred <- predict(fit, newdata = test_data)
  results$valid_rmse[i] <- rmse(test_data$imdb_score, pred)
}

# Sort by best validation performance (lowest RMSE)
results = results[order(results$valid_rmse), ]
print(results)

# ------------------------------------------------------------------------
## e)Decide linear V.S. non-linear model
best_model_name = results$model[1]
cat("\nBest model by validation RMSE:", best_model_name, "\n")
# Show the best model summary
cat("\nSummary of best model:\n")
print(summary(models[[best_model_name]]))
###############################################################################




###############################################################################
###Part 3: Build Model
# ------------------------------------------------------------------------
## a)Combining predictors into regression models
library(lmtest)
library(sandwich)
library(car)
library(stargazer)

#For Categorical Variables:
cat_vars = c("release_month","language","country","maturity_rating",
             "distributor","director","actor1","actor2","actor3",
             "colour_film","cinematographer","production_company")
for (v in cat_vars) {
  train_data[[v]] = as.factor(train_data[[v]])
  test_data[[v]] = as.factor(test_data[[v]])
}
for (v in cat_vars) {
  lv <- levels(train_data[[v]])
  test_data[[v]] = factor(test_data[[v]], levels = lv)
}

#For Numerical Variables:
num_base = c("movie_budget","release_day","release_year","aspect_ratio",
             "nb_news_articles","actor1_star_meter","actor2_star_meter","actor3_star_meter",
             "nb_faces","movie_meter_IMDBpro")

#For Genre Variables (Dummified):
genre_dummies = c("action","adventure","scifi","thriller","musical","romance","western",
                  "sport","horror","drama","war","animation","crime")

#Start with quadratic form of duration
train_data$duration2 = train_data$duration^2
test_data$duration2 = test_data$duration^2

#Baseline models
m1 = lm(imdb_score ~ duration + duration2, data = train_data)
m2 = lm(imdb_score ~ duration + duration2 +
          release_year + nb_news_articles +
          movie_budget + movie_meter_IMDBpro +
          actor1_star_meter + actor2_star_meter + actor3_star_meter +
          nb_faces + aspect_ratio + release_day,
        data = train_data)

#Model with genre dummies
m3 = lm(as.formula(paste(
  "imdb_score ~ duration + duration2 + release_year + nb_news_articles +",
  "movie_budget + movie_meter_IMDBpro + actor1_star_meter + actor2_star_meter + actor3_star_meter +",
  "nb_faces + aspect_ratio + release_day +",
  paste(genre_dummies, collapse = " + "))), data = train_data)

#Model with categorical predictors
m4 = lm(as.formula(paste(
  "imdb_score ~ duration + duration2 + release_year + nb_news_articles +",
  "movie_budget + movie_meter_IMDBpro + actor1_star_meter + actor2_star_meter + actor3_star_meter +",
  "nb_faces + aspect_ratio + release_day + release_month + language + country + maturity_rating + colour_film +",
  paste(genre_dummies, collapse = " + ")
)), data = train_data)

#Compare Models
models_base = list(m1=m1, m2=m2, m3=m3, m4=m4)

base_results = data.frame(
  model = names(models_base),
  train_r2 = NA_real_,
  valid_rmse = NA_real_
)

for (i in seq_along(models_base)) {
  fit = models_base[[i]]
  base_results$train_r2[i] = summary(fit)$r.squared
  pred = predict(fit, newdata = test_data)
  base_results$valid_rmse[i] = rmse(test_data$imdb_score, pred)
}

base_results = base_results[order(base_results$valid_rmse), ]
print(base_results)

# ------------------------------------------------------------------------
## b)Testing interactions & c)Handling dummy variables
#Side note: Rationale here: duration's effect on imdb score might varies depending on 
#           genre or rating, and news may matter more for newer films
m4_int1 = update(m4, . ~ . + duration:nb_news_articles)
m4_int2 = update(m4, . ~ . + duration:release_year)
m4_int4 = update(m4, . ~ . + nb_news_articles:release_year)
genre_int_terms = paste0("duration:", genre_dummies)
m4_int_genres = update(m4, as.formula(paste(". ~ . +", paste(genre_int_terms, collapse = " + "))))

# Compare interaction models
models_int = list(
  m4 = m4,
  int1 = m4_int1,
  int2 = m4_int2,
  int_genres = m4_int_genres,
  int4 = m4_int4
)
int_results = data.frame(
  model = names(models_int),
  train_r2 = NA_real_,
  valid_rmse = NA_real_
)
for (i in seq_along(models_int)) {
  fit = models_int[[i]]
  int_results$train_r2[i] = summary(fit)$r.squared
  pred = predict(fit, newdata = test_data)
  int_results$valid_rmse[i] = rmse(test_data$imdb_score, pred)
}
int_results = int_results[order(int_results$valid_rmse), ]
print(int_results)

best_int_name = int_results$model[1]
best_model = models_int[[best_int_name]]

cat("\nBest model after interaction testing (by validation RMSE):", best_int_name, "\n")
#from the code, we see that m4_int1 is the best model, in which we evaluate the interation between
#duration (quadratic) and number of news articles, and therefore influence the imdb_score

# ------------------------------------------------------------------------
## d)Running diagnostics
cat("Best Model Summary:")
print(summary(best_model))
# Standard diagnostic plots
par(mfrow=c(2,2))
plot(best_model)
par(mfrow=c(1,1))
# Outliers / influence diagnostics
cat("Outlier Test")
print(outlierTest(best_model))

# ------------------------------------------------------------------------
## e)Checking heteroskedasticity (White test, BP test)
#BP test
bp = bptest(best_model)
print(bp)

# White test
u2 = resid(best_model)^2
f = fitted(best_model)

white_proxy = lmtest::bptest(u2 ~ f + I(f^2))
print(white_proxy)

# ------------------------------------------------------------------------
##f)Checking VIF for collinearity
best_model_fixed = lm(
  imdb_score ~ duration + duration2 +
    nb_news_articles +
    duration:nb_news_articles +
    release_year +
    movie_budget +
    movie_meter_IMDBpro +
    actor1_star_meter +
    actor2_star_meter +
    actor3_star_meter +
    nb_faces +
    aspect_ratio +
    release_day +
    maturity_rating +
    colour_film +
    action + adventure + scifi + thriller +
    musical + romance + western + sport +
    horror + drama + war + animation + crime,
  data = train_data
)
car::vif(best_model_fixed)

# ------------------------------------------------------------------------
#The following section is for the final deliverable
#Stargazer tables
library(stargazer)
###stargazer(m1, m2, m3, m4,
#type = "text",
#title = "Regression Models (Core Build-Up)",
#digits = 3,
#omit.stat = c("f","ser"))

#Diagnostics summary
cat("Final model name:", best_int_name, "\n")
cat("Train R^2:", round(summary(best_model)$r.squared, 4), "\n")

pred_final <- predict(best_model, newdata = test_data)
cat("Validation RMSE:", round(rmse(test_data$imdb_score, pred_final), 4), "\n")

cat("\nBP test p-value:", format.pval(bp$p.value, digits=4), "\n")
cat("White proxy test p-value:", format.pval(white_proxy$p.value, digits=4), "\n")

cat("\nShapiro test (residual normality) p-value:\n")
print(shapiro.test(residuals(best_model))$p.value)

library(stargazer)

stargazer(m1, m2, m3, m4, best_model,
          type = "text",
          title = "Model Comparison: Build-Up to Final Model",
          column.labels = c("Quadratic", "Numeric", "With Genres", "With Categoricals", "Final (Interaction)"),
          dep.var.labels = "IMDb Score",
          digits = 3,
          omit.stat = c("f", "ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("country", "language", "director", "actor1", "actor2",
                   "actor3", "distributor", "cinematographer", "production_company"),
          add.lines = list(
            c("Country FE",       "No", "No", "No", "Yes", "Yes"),
            c("Language FE",      "No", "No", "No", "Yes", "Yes"),
            c("Interaction Term", "No", "No", "No", "No",  "Yes")
          ),
          notes = "High-cardinality categorical variables (country, language, etc.) omitted from display."
)


###############################################################################




###############################################################################
###Part 4: Predictions & Validations
# ------------------------------------------------------------------------
##a) K-fold or validation-set testing
# Candidate predictors
genre_dummies = c("action","adventure","scifi","thriller","musical","romance","western",
                  "sport","horror","drama","war","animation","crime")

num_terms = c("movie_budget","release_day","release_year","aspect_ratio",
              "nb_news_articles","actor1_star_meter","actor2_star_meter","actor3_star_meter",
              "nb_faces","movie_meter_IMDBpro")

# Candidate models
# 1) Simple quadratic on duration
f_m1 = imdb_score ~ duration + duration2

# 2) Numeric set + duration quadratic
f_m2 = imdb_score ~ duration + duration2 +
  release_year + nb_news_articles + movie_budget + movie_meter_IMDBpro +
  actor1_star_meter + actor2_star_meter + actor3_star_meter +
  nb_faces + aspect_ratio + release_day

# 3) With genre dummies
f_m3 = as.formula(paste(
  "imdb_score ~ duration + duration2 + release_year + nb_news_articles +",
  "movie_budget + movie_meter_IMDBpro + actor1_star_meter + actor2_star_meter + actor3_star_meter +",
  "nb_faces + aspect_ratio + release_day +",
  paste(genre_dummies, collapse=" + ")
))

# 4) With key categorical variables
f_m4 = as.formula(paste(
  "imdb_score ~ duration + duration2 + release_year + nb_news_articles +",
  "movie_budget + movie_meter_IMDBpro + actor1_star_meter + actor2_star_meter + actor3_star_meter +",
  "nb_faces + aspect_ratio + release_day +",
  "release_month + language + country + maturity_rating + colour_film +",
  paste(genre_dummies, collapse=" + ")
))

# 5) Centered best interaction idea: duration * nb_news_articles (with quadratic duration)
f_int_centered = as.formula(paste(
  "imdb_score ~ c_duration + c_duration2 + release_year + c_nb_news +",
  "movie_budget + movie_meter_IMDBpro + actor1_star_meter + actor2_star_meter + actor3_star_meter +",
  "nb_faces + aspect_ratio + release_day +",
  "release_month + language + country + maturity_rating + colour_film +",
  paste(genre_dummies, collapse=" + "),
  "+ c_duration:c_nb_news"
))

models_formulas = list(
  m1 = f_m1,
  m2 = f_m2,
  m3 = f_m3,
  m4 = f_m4,
  int_centered = f_int_centered
)

cat_vars = c("release_month","language","country","maturity_rating","colour_film")

mse = function(actual, predicted) {mean((actual - predicted)^2, na.rm = TRUE)}

collapse_rare = function(x, min_count = 10) {
  x = as.character(x)
  tab = table(x)
  rare = names(tab[tab < min_count])
  x[x %in% rare] = "Other"
  factor(x)
}

for (v in cat_vars) {
  train_data[[v]] = collapse_rare(train_data[[v]], min_count = 10)
}

# K-fold CV function
kfold_cv_mse = function(formula, data, cat_vars, K = 10, seed = 1) {
  set.seed(seed)
  n = nrow(data)
  folds = sample(rep(1:K, length.out = n))
  master_levels = list()
  for (v in cat_vars) {
    master_levels[[v]] = levels(as.factor(data[[v]]))
  }
  
  fold_mse = numeric(K)
  
  for (k in 1:K) {
    tr = data[folds != k, , drop = FALSE]
    te = data[folds == k, , drop = FALSE]
    for (v in cat_vars) {
      tr[[v]] = factor(as.character(tr[[v]]), levels = master_levels[[v]])
      te[[v]] = factor(as.character(te[[v]]), levels = master_levels[[v]])
    }
    #Create centered variables inside the fold
    mean_dur  = mean(tr$duration, na.rm = TRUE)
    mean_news = mean(tr$nb_news_articles, na.rm = TRUE)
    tr$c_duration = tr$duration - mean_dur
    te$c_duration = te$duration - mean_dur
    tr$c_duration2 = tr$c_duration^2
    te$c_duration2 = te$c_duration^2
    tr$c_nb_news = tr$nb_news_articles - mean_news
    te$c_nb_news = te$nb_news_articles - mean_news
    
    fit = lm(formula, data = tr)
    pred = predict(fit, newdata = te)
    
    fold_mse[k] = mse(te$imdb_score, pred)
  }
  
  mean(fold_mse)
}

#Running CV on the candidate models
K = 10
cv_table = data.frame(
  model = names(models_formulas),
  train_mse = NA_real_,
  cv_mse = NA_real_
)

train_data$c_duration  = train_data$duration - mean(train_data$duration, na.rm = TRUE)
train_data$c_duration2 = train_data$c_duration^2
train_data$c_nb_news   = train_data$nb_news_articles - mean(train_data$nb_news_articles, na.rm = TRUE)

for (i in seq_along(models_formulas)) {
  f = models_formulas[[i]]
  fit_train = lm(f, data = train_data)
  pred_train = predict(fit_train, newdata = train_data)
  cv_table$train_mse[i] = mse(train_data$imdb_score, pred_train)
  cv_table$cv_mse[i] = kfold_cv_mse(f, data = train_data, cat_vars = cat_vars, K = K, seed = 1)
}
cv_table = cv_table[order(cv_table$cv_mse), ]
print(cv_table)

# ------------------------------------------------------------------------
##b) Comparing models by out-of-sample MSE
# Ensure test_data's factor levels to be the same as train_data


if (!exists("cat_vars")) stop("cat_vars not found. It should be defined in section (a).")

for (v in cat_vars) {
  # Make sure both are character first (stable handling)
  train_data[[v]] = as.character(train_data[[v]])
  valid_data[[v]] = as.character(valid_data[[v]])
  lv = sort(unique(train_data[[v]]))
  if (!("Other" %in% lv)) lv = c(lv, "Other")
  train_data[[v]] = factor(train_data[[v]], levels = lv)
  x = valid_data[[v]]
  x[is.na(x)] = "Other"
  x[!(x %in% lv)] = "Other"
  valid_data[[v]] = factor(x, levels = lv)
}

# Build variables
train_data$duration2 = train_data$duration^2
valid_data$duration2 = valid_data$duration^2
mean_dur_train  = mean(train_data$duration, na.rm = TRUE)
mean_news_train = mean(train_data$nb_news_articles, na.rm = TRUE)
train_data$c_duration  = train_data$duration - mean_dur_train
valid_data$c_duration  = valid_data$duration - mean_dur_train
train_data$c_duration2 = train_data$c_duration^2
valid_data$c_duration2 = valid_data$c_duration^2
train_data$c_nb_news   = train_data$nb_news_articles - mean_news_train
valid_data$c_nb_news   = valid_data$nb_news_articles - mean_news_train

# Fit models on train_data
if (!exists("mse")) {
  mse = function(actual, predicted) mean((actual - predicted)^2, na.rm = TRUE)
}

oos_table = data.frame(
  model     = names(models_formulas),
  train_mse = cv_table$train_mse[match(names(models_formulas), cv_table$model)],
  cv_mse    = cv_table$cv_mse[match(names(models_formulas), cv_table$model)],
  oos_mse   = NA_real_
)

for (i in seq_along(models_formulas)) {
  f = models_formulas[[i]]
  fit = lm(f, data = train_data)
  pred_valid = predict(fit, newdata = valid_data)
  oos_table$oos_mse[i] = mse(valid_data$imdb_score, pred_valid)
}

# Rank models by out-of-sample MSE (lower is better)
oos_table = oos_table[order(oos_table$oos_mse), ]
print(oos_table)

# Identify best model by OOS MSE
best_oos_name = oos_table$model[1]
cat("Best model by out-of-sample MSE (evaluated on valid_data):", best_oos_name, "\n")

# ------------------------------------------------------------------------
##c) Simplifying if overfitting occurs
#Rationale: If the best model outperforms on train than on test, then we simplify
#           the model by dropping interation and/or high dimensional categorical terms
# Rebuild duration2 for all datasets

best_name = oos_table$model[1]
best_formula = models_formulas[[best_name]]

#Compute train vs validation MSE for the best model
fit_best = lm(best_formula, data = train_data)
pred_train_best = predict(fit_best, newdata = train_data)
pred_valid_best = predict(fit_best, newdata = valid_data)
train_mse_best = mse(train_data$imdb_score, pred_train_best)
valid_mse_best = mse(valid_data$imdb_score, pred_valid_best)
cat("\nCurrent best model:", best_name, "\n")
cat("Train MSE:", round(train_mse_best, 4), "\n")
cat("Valid  MSE:", round(valid_mse_best, 4), "\n")

# Overfitting flag rule: model will be classified as "likely overfitting" if 
#                        validation MSE is > 15% worse than train MSE
overfit_flag = (valid_mse_best > 1.15 * train_mse_best)
cat("Overfitting flag (Valid MSE > 1.15 * Train MSE):", overfit_flag, "\n")

# When overfitting problem present --> try simpler candidate models

if (overfit_flag) {
  cat("\nTrying simpler models because overfitting was flagged...\n")
  simplify_candidates = c("m1", "m2", "m3", "m4", "int_centered")
  simplify_candidates = simplify_candidates[simplify_candidates %in% names(models_formulas)]
  simplify_table = data.frame(
    model = simplify_candidates,
    train_mse = NA_real_,
    valid_mse = NA_real_
  )
  for (i in seq_along(simplify_candidates)) {
    nm = simplify_candidates[i]
    f  = models_formulas[[nm]]
    fit = lm(f, data = train_data)
    p_tr = predict(fit, newdata = train_data)
    p_va = predict(fit, newdata = valid_data)
    simplify_table$train_mse[i] = mse(train_data$imdb_score, p_tr)
    simplify_table$valid_mse[i] = mse(valid_data$imdb_score, p_va)
  }
  simplify_table = simplify_table[order(simplify_table$valid_mse), ]
  print(simplify_table)
  # Update the best model
  best_name    = simplify_table$model[1]
  best_formula = models_formulas[[best_name]]
  cat("\nChosen simplified model (best by validation MSE):", best_name, "\n")
} else {
  cat("\nNo simplification applied (overfitting not flagged).\n")
}

# ------------------------------------------------------------------------
##d) Generating final predictions for 12 movies
#Make sure categorical variables in train/test match
for (v in cat_vars) {
  train_data[[v]] = as.character(train_data[[v]])
  lv = sort(unique(train_data[[v]]))
  if (!("Other" %in% lv)) lv = c(lv, "Other")
  train_data[[v]] = factor(train_data[[v]], levels = lv)
  x = as.character(test_data[[v]])
  x[is.na(x)] = "Other"
  x[!(x %in% lv)] = "Other"
  test_data[[v]] = factor(x, levels = lv)
}

# Build needed numeric variables
# duration2
train_data$duration2 = train_data$duration^2
test_data$duration2  = test_data$duration^2
# Centering terms (MUST use train means)
mean_dur_train  = mean(train_data$duration, na.rm = TRUE)
mean_news_train = mean(train_data$nb_news_articles, na.rm = TRUE)
train_data$c_duration  = train_data$duration - mean_dur_train
test_data$c_duration   = test_data$duration  - mean_dur_train
train_data$c_duration2 = train_data$c_duration^2
test_data$c_duration2  = test_data$c_duration^2
train_data$c_nb_news = train_data$nb_news_articles - mean_news_train
test_data$c_nb_news  = test_data$nb_news_articles  - mean_news_train

# Fit final model
final_train = rbind(train_data, valid_data)
final_fit = lm(best_formula, data = final_train)

# Predict for the 12 movies
test_pred = predict(final_fit, newdata = test_data)

# Save predictions
predictions_12 = data.frame(
  movie_id = if ("movie_id" %in% names(test_data)) test_data$movie_id else NA,
  movie_title = if ("movie_title" %in% names(test_data)) test_data$movie_title else NA,
  pred_imdb_score = test_pred
)

print(predictions_12)

# ------------------------------------------------------------------------
##e) Calculating final model performance metrics
#Define metrics
rmse = function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm = TRUE))
mae  = function(actual, predicted) mean(abs(actual - predicted), na.rm = TRUE)
r2   = function(actual, predicted) {
  sse = sum((actual - predicted)^2, na.rm = TRUE)
  sst = sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  1 - sse/sst
}

# Predictions on train and valid using a model fitted on train only
fit_report = lm(best_formula, data = train_data)
pred_tr = predict(fit_report, newdata = train_data)
pred_va = predict(fit_report, newdata = valid_data)

# Create a performance table
perf_table = data.frame(
  dataset = c("train_data", "valid_data", "train+valid (refit)"),
  MSE  = NA_real_,
  RMSE = NA_real_,
  MAE  = NA_real_,
  R2   = NA_real_
)

# Train metrics
perf_table$MSE[1]  = mse(train_data$imdb_score, pred_tr)
perf_table$RMSE[1] = rmse(train_data$imdb_score, pred_tr)
perf_table$MAE[1]  = mae(train_data$imdb_score, pred_tr)
perf_table$R2[1]   = r2(train_data$imdb_score, pred_tr)

# Validation metrics
perf_table$MSE[2]  = mse(valid_data$imdb_score, pred_va)
perf_table$RMSE[2] = rmse(valid_data$imdb_score, pred_va)
perf_table$MAE[2]  = mae(valid_data$imdb_score, pred_va)
perf_table$R2[2]   = r2(valid_data$imdb_score, pred_va)

# Combined metrics (fit on final_train, predict on final_train)
pred_all = predict(final_fit, newdata = final_train)
perf_table$MSE[3]  = mse(final_train$imdb_score, pred_all)
perf_table$RMSE[3] = rmse(final_train$imdb_score, pred_all)
perf_table$MAE[3]  = mae(final_train$imdb_score, pred_all)
perf_table$R2[3]   = r2(final_train$imdb_score, pred_all)

#Print final performance
print(perf_table)
cat("\nFinal chosen model name:", best_name, "\n")
cat("Final chosen formula:\n")
print(best_formula)
cat("\nNote: test_data has no imdb_score, so performance metrics cannot be computed on test_data.\n")
###############################################################################

print(summary(final_fit))



#RESULT PREDICTIONS
#1 - file path
IMDb_data      <- read.csv('/Users/kevinsun/MGSC 401/Midterm Project/IMDB_data_Winter_2026.csv')
test_data_raw  <- read.csv('/Users/kevinsun/MGSC 401/Midterm Project/test_data_IMDB.csv')  # your 12-movie file

# -----------------------------------------------------------------------
# 2- re-run the train/valid split
set.seed(1)
n          <- nrow(IMDb_data)
idx_train  <- sample.int(n, size = floor(0.8 * n), replace = FALSE)
train_data <- IMDb_data[idx_train, ]
valid_data <- IMDb_data[-idx_train, ]

# -----------------------------------------------------------------------
# 3- define helper & categorical collapsing

cat_vars <- c("release_month", "language", "country", "maturity_rating", "colour_film")

collapse_rare <- function(x, min_count = 10) {
  x   <- as.character(x)
  tab <- table(x)
  x[x %in% names(tab[tab < min_count])] <- "Other"
  factor(x)
}

for (v in cat_vars) {
  train_data[[v]] <- collapse_rare(train_data[[v]], min_count = 10)
}

# -----------------------------------------------------------------------
# 4- align factor levels

test_data <- test_data_raw   # work on a copy

for (v in cat_vars) {
  train_data[[v]] <- as.character(train_data[[v]])
  lv              <- sort(unique(train_data[[v]]))
  if (!("Other" %in% lv)) lv <- c(lv, "Other")
  train_data[[v]] <- factor(train_data[[v]], levels = lv)
  
  x        <- as.character(test_data[[v]])
  x[is.na(x)]        <- "Other"
  x[!(x %in% lv)]    <- "Other"   # unseen levels → "Other"
  test_data[[v]]      <- factor(x, levels = lv)
}

# -----------------------------------------------------------------------
# 5- build engineered features

train_data$duration2  <- train_data$duration^2
test_data$duration2   <- test_data$duration^2

mean_dur_train  <- mean(train_data$duration,         na.rm = TRUE)
mean_news_train <- mean(train_data$nb_news_articles,  na.rm = TRUE)

train_data$c_duration  <- train_data$duration         - mean_dur_train
test_data$c_duration   <- test_data$duration          - mean_dur_train
train_data$c_duration2 <- train_data$c_duration^2
test_data$c_duration2  <- test_data$c_duration^2
train_data$c_nb_news   <- train_data$nb_news_articles - mean_news_train
test_data$c_nb_news    <- test_data$nb_news_articles  - mean_news_train

# Also apply to valid_data for the final combined fit
valid_data$duration2  <- valid_data$duration^2
valid_data$c_duration <- valid_data$duration         - mean_dur_train
valid_data$c_duration2<- valid_data$c_duration^2
valid_data$c_nb_news  <- valid_data$nb_news_articles  - mean_news_train

for (v in cat_vars) {
  lv <- levels(train_data[[v]])
  x  <- as.character(valid_data[[v]])
  x[is.na(x) | !(x %in% lv)] <- "Other"
  valid_data[[v]] <- factor(x, levels = lv)
}

# -----------------------------------------------------------------------
# 6- Define the best formula (int_centered)

genre_dummies <- c("action","adventure","scifi","thriller","musical","romance",
                   "western","sport","horror","drama","war","animation","crime")

best_formula <- as.formula(paste(
  "imdb_score ~ c_duration + c_duration2 + release_year + c_nb_news +",
  "movie_budget + movie_meter_IMDBpro + actor1_star_meter + actor2_star_meter + actor3_star_meter +",
  "nb_faces + aspect_ratio + release_day +",
  "release_month + language + country + maturity_rating + colour_film +",
  paste(genre_dummies, collapse = " + "),
  "+ c_duration:c_nb_news"
))

# -----------------------------------------------------------------------
# 7- fit the 'final' model on train + valid combined, then predict

final_train <- rbind(train_data, valid_data)
final_fit   <- lm(best_formula, data = final_train)

test_predictions <- predict(final_fit, newdata = test_data)

# -----------------------------------------------------------------------
# 8- results

predictions_df <- data.frame(
  movie_id        = test_data$movie_id,
  movie_title     = test_data$movie_title,
  pred_imdb_score = round(test_predictions, 2)
)

print(predictions_df)
write.csv(predictions_df, "imdb_predictions.csv", row.names = FALSE)






