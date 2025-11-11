library(skimr)
library(dplyr)
library(magrittr)




# List of columns to convert
cat_vars <- c(
  "agro_eco_r", "climatic_z", "agro_eco_z", "terrain_adj", "rice_variety",
  "stage_of_maturity", "soil_type", "rice_ecosystem", "topography",
  "flooded_y_n", "method_of_planting", "is_the_farmer_aware_of_mg_before"
)

# Convert to factor
dataset_adj[cat_vars] <- lapply(dataset_adj[cat_vars], factor)
dataset_noNA <- na.omit(dataset_adj)


# fit a negative binomila model - full model
# If not installed yet
# install.packages("MASS")

library(MASS)





# multicollinearity check

# Install if not already installed
# install.packages("car")

library(car)

# Fit a linear model for VIF diagnostics using same predictors as your glm.nb model
vif_lm <- lm(
  gall_count ~ agro_eco_r + climatic_z + agro_eco_z + terrain_adj +
    rice_variety + stage_of_maturity + soil_type +
    rice_ecosystem + topography + flooded_y_n +
    method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + age_das,
  data = dataset_adj
)

# Calculate VIF values
vif(vif_lm)


alias(vif_lm)

##  Now go with terrain_adj


neg_binom_model <- glm.nb(
  gall_count ~ agro_eco_r + climatic_z + agro_eco_z + terrain_adj +
    rice_variety + stage_of_maturity + soil_type +
    rice_ecosystem + topography + flooded_y_n +
    method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + age_das,
  data = dataset_adj
)

# View model summary
summary(neg_binom_model)


## adjust alias
# drop terrain

neg_binom_model2 <- glm.nb(
  gall_count ~ agro_eco_r + climatic_z + agro_eco_z + 
    rice_variety + stage_of_maturity + soil_type +
    rice_ecosystem + topography + flooded_y_n +
    method_of_planting + is_the_farmer_aware_of_mg_before +
    anual_r_fmm + age_das,
  data = dataset_adj
)

# View model summary
summary(neg_binom_model2)

#### drop rain fmm
neg_binom_model3 <- glm.nb(
  gall_count ~ agro_eco_r + climatic_z + agro_eco_z + 
    rice_variety + stage_of_maturity + soil_type +
    rice_ecosystem + topography + flooded_y_n +
    method_of_planting + is_the_farmer_aware_of_mg_before +
   age_das,
  data = dataset_noNA
)

# View model summary
summary(neg_binom_model3)


###

table(dataset_noNA$climatic_z)
table(dataset_noNA$agro_eco_z)
table(dataset_noNA$stage_of_maturity)


###

neg_binom_model4 <- glm.nb(
  gall_count ~ agro_eco_r  + agro_eco_z + 
    rice_variety + stage_of_maturity + soil_type +
    rice_ecosystem + topography + flooded_y_n +
    method_of_planting + is_the_farmer_aware_of_mg_before +
    age_das,
  data = dataset_noNA
)

# View model summary
summary(neg_binom_model4)
###
library(dplyr)
library(forcats)

dataset_noNA <- dataset_noNA %>%
  mutate(
    stage_of_maturity = fct_collapse(stage_of_maturity,
                                     Seedling_Veg = c("Seedling", "Vegetative"),
                                     Ripening = "Ripening",
                                     Reproductive = "Reproductive")
  )

table(dataset_noNA$stage_of_maturity)

###
neg_binom_model5 <- glm.nb(
  gall_count ~ agro_eco_r + 
    rice_variety + stage_of_maturity + soil_type +
    rice_ecosystem + topography + flooded_y_n +
    method_of_planting + is_the_farmer_aware_of_mg_before +
    age_das,
  data = dataset_noNA
)

# View model summary
summary(neg_binom_model5)

###

table(dataset_noNA$stage_of_maturity)
table(dataset_noNA$rice_ecosystem)
# Example to lump rainfed_midland with rainfed lowland, if needed
library(dplyr)
library(forcats)

dataset_noNA <- dataset_noNA %>%
  mutate(
    rice_ecosystem = fct_collapse(rice_ecosystem,
                                  Rainfed = c("Rainfed lowland", "Rainfed Midland"),
                                  Irrigated = "Irrigated")
  )

table(dataset_noNA$rice_ecosystem)

###
neg_binom_model6 <- glm.nb(
  gall_count ~ agro_eco_r + 
    rice_variety + stage_of_maturity + soil_type +
    rice_ecosystem + topography + flooded_y_n +
    method_of_planting + is_the_farmer_aware_of_mg_before +
    age_das,
  data = dataset_noNA
)

# View model summary
summary(neg_binom_model6)

levels(dataset_noNA$stage_of_maturity)

## set refercne as reproductive
dataset_noNA$stage_of_maturity <- relevel(dataset_noNA$stage_of_maturity, ref = "Ripening")

### drop stage of maturity 
neg_binom_model7 <- glm.nb(
  gall_count ~ agro_eco_r + 
    rice_variety  + soil_type +
    rice_ecosystem + topography + flooded_y_n +
    method_of_planting + is_the_farmer_aware_of_mg_before +
    age_das,
  data = dataset_noNA
)

# View model summary
summary(neg_binom_model7)


###### vif

library(car)

vif_lm <- lm(
  gall_count ~ agro_eco_r + rice_variety + soil_type +
    rice_ecosystem + topography + flooded_y_n + method_of_planting +
    is_the_farmer_aware_of_mg_before + age_das,
  data = dataset_noNA
)

vif(vif_lm)

#### adjsut standar error problem
table(dataset_noNA$rice_variety)
table(dataset_noNA$agro_eco_r)

library(forcats)

dataset_noNA <- dataset_noNA %>%
  mutate(
    rice_variety = fct_lump_min(rice_variety, min = 50, other_level = "Other"),
    agro_eco_r = fct_lump_min(agro_eco_r, min = 50, other_level = "Other")
  )

table(dataset_noNA$rice_variety)
table(dataset_noNA$agro_eco_r)

library(MASS)

neg_binom_model_8 <- glm.nb(
  gall_count ~ agro_eco_r + rice_variety + soil_type + rice_ecosystem + 
    topography + flooded_y_n + method_of_planting + is_the_farmer_aware_of_mg_before + 
    age_das,
  data = dataset_noNA
)

summary(neg_binom_model_8)

library(car)
vif(neg_binom_model_8)
