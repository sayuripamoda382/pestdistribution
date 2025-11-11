# Define variables (same as before, just changing the response)
model_vars_pois <- c("gall_count", "serial_no", "climatic_z", "agro_eco_z", "terrain",
                     "rice_variety", "stage_of_maturity", "soil_type", "rice_ecosystem",
                     "topography", "flooded_y_n", "method_of_planting",
                     "is_the_farmer_aware_of_mg_before", "anual_r_fmm")

# Remove rows with NA in any of the model variables
dataset_clean_pois <- dataset_1 %>%
  filter(across(all_of(model_vars_pois), ~ !is.na(.)))

# Fit the Poisson GLMM
glmm_pois <- glmer(
  gall_count ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + (1 | serial_no),
  data = dataset_clean_pois,
  family = poisson
)

# View the model summary
summary(glmm_pois)

# checking overdispersion

# Function to check overdispersion in GLMM
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  cat("Pearson chi-square statistic:", round(Pearson.chisq, 2), "\n")
  cat("Degrees of freedom:", rdf, "\n")
  cat("Overdispersion ratio (chi²/df):", round(prat, 2), "\n")
  cat("p-value:", signif(pval, 4), "\n")
  if (prat > 1.5) {
    cat("⚠️ Warning: Evidence of overdispersion.\n")
  } else {
    cat("✅ No significant overdispersion.\n")
  }
}

# Apply the function to your model
overdisp_fun(glmm_pois)





#### --- backward selection ---

# Step 1: Clean data
model_vars_pois <- c("gall_count", "serial_no", "climatic_z", "agro_eco_z", "terrain",
                     "rice_variety", "stage_of_maturity", "soil_type", "rice_ecosystem",
                     "topography", "flooded_y_n", "method_of_planting",
                     "is_the_farmer_aware_of_mg_before", "anual_r_fmm")

dataset_clean_pois <- dataset_1 %>%
  filter(across(all_of(model_vars_pois), ~ !is.na(.)))

# Step 2: Start with full model
model_full <- glmer(
  gall_count ~ climatic_z + agro_eco_z + terrain + rice_variety +
    stage_of_maturity + soil_type + rice_ecosystem + topography +
    flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + (1 | serial_no),
  data = dataset_clean_pois,
  family = poisson
)



# Step 3: Create a list of fixed effect terms
terms_to_test <- c("climatic_z", "agro_eco_z", "terrain", "rice_variety",
                   "stage_of_maturity", "soil_type", "rice_ecosystem",
                   "topography", "flooded_y_n", "method_of_planting",
                   "is_the_farmer_aware_of_mg_before", "anual_r_fmm")

# Step 4: Perform backward selection based on AIC
current_model <- model_full
for (term in terms_to_test) {
  # Create formula without this term
  new_formula <- as.formula(
    paste("gall_count ~", paste(setdiff(terms_to_test, term), collapse = " +"), "+ (1 | serial_no)")
  )
  reduced_model <- glmer(new_formula, data = dataset_clean_pois, family = poisson)
  
  cat("\n--- Dropping:", term, "---\n")
  print(anova(current_model, reduced_model))
  
  # Keep reduced model only if AIC is lower
  if (AIC(reduced_model) < AIC(current_model)) {
    current_model <- reduced_model
    terms_to_test <- setdiff(terms_to_test, term)  # Update term list
    cat("✔ Dropped", term, "\n")
  } else {
    cat("✘ Kept", term, "\n")
  }
}

# Step 5: Final model
summary(current_model)



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

#### --- selection for poison model ---

#forward selection

