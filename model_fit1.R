library(magrittr)
library(skimr)
library(tidyverse)
library(lme4)
library(janitor)
library(MuMIn)


#### --- Binary GLMM ---
skim(dataset_1)

# Recode Infection_Status to binary
dataset_1 <- dataset_1 %>%
  mutate(infection_status_bin = ifelse(infection_status == "Yes", 1, 0))

# Convert serial_no to factor for random effect
dataset_1$serial_no <- as.factor(dataset_1$serial_no)


# List all model variables
model_vars <- c("infection_status_bin", "serial_no", "climatic_z", "agro_eco_z", "terrain",
                "rice_variety", "stage_of_maturity", "soil_type", "rice_ecosystem",
                "topography", "flooded_y_n", "method_of_planting",
                "is_the_farmer_aware_of_mg_before", "anual_r_fmm")

# Check missing values counts for each
sapply(dataset_1[, model_vars], function(x) sum(is.na(x)))

model_vars <- c("infection_status_bin", "serial_no", "climatic_z", "agro_eco_z", "terrain",
                "rice_variety", "stage_of_maturity", "soil_type", "rice_ecosystem",
                "topography", "flooded_y_n", "method_of_planting",
                "is_the_farmer_aware_of_mg_before", "anual_r_fmm")

dataset_clean <- dataset_1 %>%
  filter(across(all_of(model_vars), ~ !is.na(.)))

glmm_model <- glmer(
  infection_status_bin ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + (1 | serial_no),
  data = dataset_clean,
  family = binomial
)

summary(glmm_model)

#### fit model with out rice variety

model2 <- glmer(
  infection_status_bin ~ climatic_z + agro_eco_z + terrain +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + (1 | serial_no),
  data = dataset_clean,
  family = binomial
)

summary(model2)


#### fit model with out rice variety

model3 <- glmer(
  infection_status_bin ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + (1 | serial_no),
  data = dataset_clean,
  family = binomial
)

summary(model3)


#### fit model without 

library(lme4)

# Convert response to binary (if not already)
dataset_clean$infection_status_bin <- as.numeric(dataset_clean$infection_status == "Yes")  # or 1/0

# Fit full model
model_full <- glmer(
  infection_status_bin ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + (1 | serial_no),
  data = dataset_clean,
  family = binomial,
)

# Stepwise backward elimination manually using drop1()
step_model <- model_full

repeat {
  drop_result <- drop1(step_model, test = "Chisq")
  print(drop_result)
  
  # Find the variable with the largest p-value
  pvalues <- drop_result$`Pr(Chi)`[-1]  # Remove intercept
  if (all(is.na(pvalues)) || min(pvalues, na.rm = TRUE) < 0.05) {
    break  # Stop if no non-significant term to remove
  }
  
  # Remove the least significant term
  worst <- names(pvalues)[which.max(pvalues)]
  cat("Dropping:", worst, "\n")
  
  # Update the model without that term
  formula_new <- update(formula(step_model), paste(". ~ . -", worst))
  step_model <- update(step_model, formula_new)
}

#### --- do not run this

library(lme4)

# Step 1: Fit the full model
model_full <- glmer(
  infection_status_bin ~ climatic_z + agro_eco_z + terrain + rice_variety + 
    stage_of_maturity + soil_type + rice_ecosystem + topography + 
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before + 
    anual_r_fmm + (1 | serial_no),
  data = dataset_1,
  family = binomial(link = "logit")
)

# Step 2: Backward Elimination Loop
library(lme4)

# Full model
full_model <- glmer(
  infection_status ~ climatic_z + agro_eco_z + terrain + anual_r_fmm +
    rice_variety + stage_of_maturity + soil_type + rice_ecosystem +
    topography + flooded_y_n + method_of_planting +
    is_the_farmer_aware_of_mg_before +
    (1 | serial_no),
  data = dataset_1,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(full_model)
# Backward selection
step_model <- full_model
step <- 1

repeat {
  cat("\n------------------- Step", step, "-------------------\n")
  drop_result <- drop1(step_model, test = "Chisq")
  print(drop_result)
  
  # Extract p-values excluding intercept
  pvalues <- drop_result$`Pr(Chi)`[-1]
  terms <- rownames(drop_result)[-1]
  
  # Identify terms that can be dropped (p > 0.05)
  removable <- which(pvalues > 0.05 & !is.na(pvalues))
  
  if (length(removable) == 0) {
    cat("\n✅ All remaining terms are significant (p < 0.05). Stopping backward selection.\n")
    break
  }
  
  # Drop the term with the largest p-value
  worst_term <- terms[removable[which.max(pvalues[removable])]]
  cat("🔻 Dropping:", worst_term, "| p-value =", round(pvalues[worst_term], 4), "\n")
  
  # Update formula and model
  new_formula <- update(formula(step_model), paste(". ~ . -", worst_term))
  step_model <- update(step_model, formula = new_formula)
  step <- step + 1
}

# Final selected model
cat("\n=================== Final Model ===================\n")
print(summary(step_model))

#### ---- logistic regression ---

# Define the model variables
model_vars <- c("infection_status_bin", "climatic_z", "agro_eco_z", "terrain",
                "rice_variety", "stage_of_maturity", "soil_type", "rice_ecosystem",
                "topography", "flooded_y_n", "method_of_planting",
                "is_the_farmer_aware_of_mg_before", "anual_r_fmm")

# Remove rows with missing values
dataset_clean_bin <- dataset_1 %>%
  filter(across(all_of(model_vars), ~ !is.na(.)))

# Fit the binomial GLM (no random effect)
glm_binomial <- glm(
  infection_status_bin ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm,
  data = dataset_clean,
  family = binomial
)

# Summary of the model
summary(glm_binomial)


#### --- selection steps

# Define all predictor variables
model_vars <- c("climatic_z", "agro_eco_z", "terrain", "rice_variety",
                "stage_of_maturity", "soil_type", "rice_ecosystem",
                "topography", "flooded_y_n", "method_of_planting",
                "is_the_farmer_aware_of_mg_before", "anual_r_fmm")

# Clean dataset: remove rows with NA in model variables or response
data_model <- dataset_1 %>%
  filter(across(c("infection_status_bin", all_of(model_vars)), ~ !is.na(.)))

# Fit the full model
glm_model_indep <- glm(
  infection_status_bin ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm,
  data = data_model,
  family = binomial
)

# ---- Stepwise Selection Procedures ----

# 1. Forward Selection (start with null model)
null_model <- glm(infection_status_bin ~ 1, data = data_model, family = binomial)

stepwise_forward <- step(null_model,
                         scope = formula(glm_model_indep),
                         direction = "forward")

summary(stepwise_forward)  # Final selected model

# 2. Stepwise Selection (both directions)
stepwise_both <- step(glm_model_indep, direction = "both")
summary(stepwise_both)

# 3. Backward Elimination
stepwise_backward <- step(glm_model_indep, direction = "backward")
summary(stepwise_backward)
