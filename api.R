library(plumber)
library(dplyr)
library(parsnip)

#* Test model
#* @get /test
function() {
  
  final_model <- readRDS('models/lr_model.RDS')
  
  diabetes_recipe <- readRDS('models/diabetes_recipe.RDS')
  
  new_woman <- tribble(~pregnant, ~glucose, ~pressure, ~triceps, ~insulin, ~mass, ~pedigree, ~age,
                       2, 95, 70, 31, 102, 28.2, 0.67, 47)
  
  # predict(final_model, new_data = new_woman)
  
  return(predict(final_model, new_data = new_woman, type = 'prob'))
  
}

#* Predict diabetes using the following variables
#* @param pregnant
#* @param glucose
#* @param pressure
#* @param triceps
#* @param insulin
#* @param mass
#* @param pedigree
#* @param age
#* @get /prediction
function(pregnant, glucose, pressure, triceps, insulin, mass, pedigree, age) {
  
  
  final_model <- readRDS('models/lr_model.RDS')
  
  diabetes_recipe <- readRDS('models/diabetes_recipe.RDS')
  
  new_woman <- tribble(~pregnant, ~glucose, ~pressure, ~triceps, ~insulin, ~mass, ~pedigree, ~age,
                       pregnant, glucose, pressure, triceps, insulin, mass, pedigree, age)
  
  new_woman <- new_woman %>% 
    mutate_if(is.character, as.numeric)
  
  # predict(final_model, new_data = new_woman)
  
  return(predict(final_model, new_data = new_woman, type = 'prob'))
  
}


