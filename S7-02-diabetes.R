##########################################################################
# Jose Cajide - @jrcajide
# Classification models 
##########################################################################

library(tidymodels)
# rsample: for sample splitting (e.g. train/test or cross-validation)
# recipes: for pre-processing
# parsnip: for specifying the model
# yardstick: for evaluating the model

library(tidyverse)
library(workflows)
library(tune) # for parameter tuning procedure
library(mlbench) # for the data


data(PimaIndiansDiabetes)
?PimaIndiansDiabetes
diabetes_orig <- PimaIndiansDiabetes %>% 
  as_tibble()



# EDA AND DATA PREPARATION ------------------------------------------------



# there are more zeros in the data than expected
summary(diabetes_orig)

ggplot(diabetes_orig) +
  geom_histogram(aes(x = glucose))

# Check the other variables: glucose, pressure, insulin and mass 
ggplot(diabetes_orig) +
  geom_histogram(aes(x = pressure))

# Check all at the same time
diabetes_orig %>% 
  summarise(across(glucose:age , ~ sum(.x == 0)))

# Remplazamos los 0 por NA
diabetes_clean <- na_if(diabetes_orig, 0)

# Check again the previous steps


table(diabetes_clean$diabetes)

# For better interpretation (later for ROC curve plotting) we need to fix the reference level of our dependent variable “diabetes” to positive (pos) using the relevel( ) function
diabetes_clean$diabetes <- relevel(diabetes_clean$diabetes, ref = "pos")



# TRAIN AND VALIDATION SPLIT ----------------------------------------------


set.seed(1973)

# split the data into trainng (75%) and testing (25%)
diabetes_split <- initial_split(diabetes_clean, prop = 3/4, strata = diabetes)
diabetes_split

# extract training and testing sets
diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)

table(diabetes_train$diabetes)

# cross-validated version of the training for parameter tuning
# https://www.researchgate.net/figure/A-visualization-of-K-fold-cross-validation-The-blue-area-represents-the-training-data_fig2_323561095
# create CV object from training data
diabetes_cv <- vfold_cv(diabetes_train)



# DATA PREPROCESSING ------------------------------------------------------

# Define a recipe

# Specify the formula (recipe()): specify the outcome variable and predictor variables
# Specify pre-processing steps (step_zzz()): define the pre-processing steps, such as imputation, creating dummy variables, scaling, and more

# define the recipe
diabetes_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age, 
         data = diabetes_clean) %>% # Importante. Usamos diabetes_clean
  step_normalize(all_numeric()) %>%
  step_impute_knn(all_predictors())

# Otros ejemplos:
# step_rm(date)
# step_downsample(diabetes) %>% 
# step_dummy(all_nominal(), -all_outcomes()) 
# step_other(mass, threshold = 0.005)
# step_date(date, features = c("year")) 

diabetes_recipe


diabetes_train_preprocessed <- diabetes_recipe %>%
  # apply the recipe to the training data
  prep(diabetes_train) %>%
  # extract the pre-processed training dataset
  juice()
diabetes_train_preprocessed


# MODELLING ---------------------------------------------------------------


# Specify the model

#  model type: what kind of model you want to fit,
# arguments: the model parameter values 
# engine: the underlying package the model should come from 
# The mode: the type of prediction

# LOGISTIC REGRESSION

lr_model <- 
  # specify that the model is a logistic regression
  logistic_reg() %>%
  # select the engine/package that underlies the model
  set_engine("glm") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification") 


# set the workflow
lr_workflow <- workflow() %>%
  # add the recipe
  add_recipe(diabetes_recipe) %>%
  # add the model
  add_model(lr_model)

lr_fit <- lr_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(diabetes_split)

# Extract model coeffcients taking the exponent of the estimated coefficients
lr_workflow %>% 
  # finalize_workflow(lr_best) %>%
  fit(diabetes_train) %>%
  pull_workflow_fit() %>%
  tidy(exponentiate = TRUE) %>% 
  filter(p.value < 0.05)


# MODEL EVALUATION --------------------------------------------------------

lr_test_performance <- lr_fit %>% collect_metrics() %>% mutate(model = "Logistic Regression")
lr_test_performance

# extract the test set predictions
lr_test_predictions <- lr_fit %>% collect_predictions()
lr_test_predictions

# generate a confusion matrix
lr_test_predictions %>% 
  conf_mat(truth = diabetes, estimate = .pred_class) %>% 
  autoplot(type = "heatmap") # autoplot(type = "mosaic")

accuracy(lr_test_predictions, truth = diabetes,
         estimate = .pred_class)

# Sensitivity = TP / FN+TP
# The sensitivity of a classifier is the ratio between how much were correctly identified as positive to how much were actually positive.
sens(lr_test_predictions, truth = diabetes,
     estimate = .pred_class)

# Recall = TP / FN+TP (The same as before)
recall(lr_test_predictions, truth = diabetes,
       estimate = .pred_class)

# Specificity = TN/FP+TN.
# Specificity of a classifier is the ratio between how much were correctly classified as negative to how much was actually negative.
yardstick::spec(lr_test_predictions, truth = diabetes,
     estimate = .pred_class)

# Precision = TP/TP+FP
# How much were correctly classified as positive out of all positives?
precision(lr_test_predictions, truth = diabetes,
          estimate = .pred_class)

# F-measure
# F-measure is a weighted harmonic mean of precision and recall with the best score of 1 and the worst score of 0. F-measure score conveys the balance between precision and recall
f_meas(lr_test_predictions, truth = diabetes, estimate = .pred_class)
# the trained model has a classification strength of 82.7%

# Kappa can range from −1 to +1 and gives information on how much better a model over the random classifier
# https://es.wikipedia.org/wiki/Coeficiente_kappa_de_Cohen
kap(lr_test_predictions, truth = diabetes, estimate = .pred_class)

# Matthews Correlation Coefficient (MCC) is used as a measure of the quality of a binary classifier. The value ranges from −1 and +1.
mcc(lr_test_predictions, truth = diabetes, estimate = .pred_class)

# All at one
custom_metrics <- metric_set(accuracy, sens, yardstick::spec, precision, recall, f_meas, kap, mcc)
custom_metrics(lr_test_predictions,
               truth = diabetes,
               estimate = .pred_class)

# ROC Curve
lr_test_predictions %>% 
  # group_by(id) %>% # sólo en caso de KfoldCV
  roc_curve(diabetes , .pred_pos ) %>% 
  autoplot()

# Gain and Lift charts
lr_test_predictions %>%
  gain_curve(diabetes,  .pred_pos) %>% 
  autoplot()

lr_test_predictions %>%
  lift_curve(diabetes,  .pred_pos) %>% 
  autoplot()

# predict the diabetes status of a new woman

new_woman <- tribble(~pregnant, ~glucose, ~pressure, ~triceps, ~insulin, ~mass, ~pedigree, ~age,
                     2, 95, 70, 31, 102, 28.2, 0.67, 47)
new_woman

# Fitting and using the final model
lr_final_model <- fit(lr_workflow, diabetes_clean)

predict(lr_final_model, new_data = new_woman)
predict(lr_final_model, new_data = new_woman, type = 'prob')







# RANDOM FOREST -----------------------------------------------------------


# mtry is no of Variables randomly chosen at each split
# trees is the number of trees to build
# min_n is the number of observations needed to keep splitting nodes


rf_model <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # specify that the `mtry` parameter needs to be tuned
  set_args(mtry = tune(), trees = tune(), min_n=tune()) %>% 
  # select the engine/package that underlies the model
  set_engine("ranger", importance = "impurity") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification") 


# set the workflow
rf_workflow <- workflow() %>%
  # add the recipe
  add_recipe(diabetes_recipe) %>%
  # add the model
  add_model(rf_model)

# Tune the parameters
# specify which values eant to try

rf_grid <- expand.grid(mtry = c(3, 4, 5), trees = c(100, 500))
rf_grid <- expand.grid(mtry = c(3, 4, 5), trees = seq(100, 500, by = 50))
rf_grid <- expand.grid(mtry = c(3, 4, 5), trees = seq(100, 500, by = 50), min_n  = 2:8)

rf_grid <- grid_regular(
  mtry(range = c(3, 5)),
  trees(range = c(100, 500)),
  min_n(range = c(2, 8)),
  levels = 5
)

# install.packages('doParallel')
doParallel::registerDoParallel(cores = 4)
# extract results
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = diabetes_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )


# print results
rf_tune_results %>%
  collect_metrics()

rf_tune_results %>%
  collect_metrics() %>% 
  group_by(.metric) %>% 
  top_n(1, mean)


# Finalize the workflow

param_final <- rf_tune_results %>%
  select_best(metric = "roc_auc")
param_final

rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

# Evaluate the model on the test set

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(diabetes_split)

rf_test_performance <- rf_fit %>% collect_metrics() %>% mutate(model = "Random Forest")
rf_test_performance

# extract the test set predictions
rf_test_predictions <- rf_fit %>% collect_predictions()
rf_test_predictions

# generate a confusion matrix
rf_test_predictions %>% 
  conf_mat(truth = diabetes, estimate = .pred_class) %>% 
  autoplot(type = "heatmap") # autoplot(type = "mosaic")

rf_test_predictions %>% 
  # group_by(id) %>% # sólo en caso de KfoldCV
  roc_curve(diabetes , .pred_pos ) %>% 
  autoplot()

# Gain and Lift charts
rf_test_predictions %>%
  gain_curve(diabetes,  .pred_pos) %>% 
  autoplot()

rf_test_predictions %>%
  lift_curve(diabetes,  .pred_pos) %>% 
  autoplot()

# Plot predicted probability distributions for our two classes.
rf_test_predictions %>%
  ggplot() +
  geom_density(aes(x = .pred_pos, fill = diabetes), 
               alpha = 0.5)


# Fitting and using the final model
rf_final_model <- fit(rf_workflow, diabetes_clean)

predict(rf_final_model, new_data = new_woman)
predict(rf_final_model, new_data = new_woman, type = 'prob')

# ranger_obj <- pull_workflow_fit(final_model)$fit
# ranger_obj

# Variable importance
library(vip)
rf_final_model %>%
  extract_fit_parsnip() %>%
  vip(geom = "col") + 
  labs(title = "Random forest variable importance") 



# MODEL COMPARISON --------------------------------------------------------


# create dataframe with all models
model_compare <- bind_rows(
  rf_test_performance,
  lr_test_performance
) 

model_compare %>% 
  ggplot(aes(x=model, y=.estimate, fill=.metric)) + 
  geom_col() + 
  facet_wrap(vars(.metric)) + 
  ylim(0,1)

rf_test_predictions %>% mutate(model = "Random Forest") %>% 
  bind_rows(lr_test_predictions %>% mutate(model = "Logistic Regression")) %>% 
  group_by(model) %>% 
  roc_curve(diabetes , .pred_pos ) %>% 
  autoplot()


# SAVE MODEL FOR DEPLOYMENT -----------------------------------------------

dir.create('models')
saveRDS(lr_final_model, file = 'models/lr_model.RDS')
saveRDS(diabetes_recipe, file = 'models/diabetes_recipe.RDS')

# See: api.R & run.R

