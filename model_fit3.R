
library(glmmTMB)

library(MASS)
library(car)

#### --- poisson glm model

# Define the model variables
model_vars <- c("gall_count", "climatic_z", "agro_eco_z", "terrain",
                "rice_variety", "stage_of_maturity", "soil_type", "rice_ecosystem",
                "topography", "flooded_y_n", "method_of_planting",
                "is_the_farmer_aware_of_mg_before", "anual_r_fmm")

# Remove rows with missing values
dataset_clean_pois <- dataset_1 %>%
  filter(across(all_of(model_vars), ~ !is.na(.)))

# Fit the Poisson GLM (no random effect)
glm_poisson <- glm(
  gall_count ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm,
  data = dataset_clean_pois,
  family = poisson
)


# Summary of the model
summary(glm_poisson)

#### over dispersion

# Calculate overdispersion statistic
overdispersion_stat <- sum(residuals(glm_poisson, type = "pearson")^2) / df.residual(glm_poisson)

# Print the overdispersion value
cat("Overdispersion statistic:", overdispersion_stat, "\n")

# Rule of thumb:
if (overdispersion_stat > 1.5) {
  cat("Warning: Evidence of overdispersion detected.\n")
} else {
  cat("No significant overdispersion detected.\n")
}


#### negative_binomial


glm_negbin <- glm.nb(
  gall_count ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm,
  data = dataset_clean
)

summary(glm_negbin)


### --- stepwise selection for negative binomial ---



# Full Negative Binomial model
full_nb_model <- glm.nb(
  gall_count ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm,
  data = dataset_clean
)

null_nb_model <- glm.nb(
  gall_count ~ 1,
  data = dataset_clean_pois
)

nb_forward <- step(
  null_nb_model,
  scope = list(lower = formula(null_nb_model), upper = formula(full_nb_model)),
  direction = "forward",
  trace = FALSE
)
summary(nb_forward)

nb_backward <- step(
  full_nb_model,
  direction = "backward",
  trace = FALSE
)
summary(nb_backward)

nb_stepwise <- step(
  full_nb_model,
  direction = "both",
  trace = FALSE
)
summary(nb_stepwise)

#### --- negative glmm ---



# Fit the Negative Binomial GLMM
glmm_nb <- glmmTMB(
  gall_count ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + (1 | serial_no),
  data = dataset_clean,
  family = nbinom2
)

# View summary
summary(glmm_nb)

### --- multicollienarity --- 

## drop unused factor levels

dataset_clean <- droplevels(dataset_clean)
# Load necessary libraries
library(glmmTMB)
library(forcats)
library(dplyr)

# Step 1: Drop unused levels
dataset_clean <- droplevels(dataset_clean)

# Step 2: Lump rare factor levels (<10 obs) into "Other"
dataset_clean$rice_variety <- fct_lump_min(dataset_clean$rice_variety, min = 10)
dataset_clean$stage_of_maturity <- fct_lump_min(dataset_clean$stage_of_maturity, min = 10)
dataset_clean$soil_type <- fct_lump_min(dataset_clean$soil_type, min = 10)

# Step 3: Scale the rainfall variable
dataset_clean$anual_r_fmm_scaled <- scale(dataset_clean$anual_r_fmm)

# Step 4: Fit Negative Binomial GLMM
nb_glmm_model <- glmmTMB(
  gall_count ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm_scaled + (1 | serial_no),
  data = dataset_clean,
  family = nbinom2(link = "log")
)

# Step 5: View model summary
summary(nb_glmm_model)
