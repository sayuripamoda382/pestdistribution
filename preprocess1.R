library(readxl)
library(skimr)
library(tidyverse)
library(magrittr)

stat_consultancy <- read_excel("stat_consultancy.xlsx")
data <- stat_consultancy

skim(data)
# Get unique rice varieties
sus <- unique(data$`Prediction_on suseptibility of rice variety`)

# Print unique varieties
print(sus)

soil_ty <- unique(data$Soil_Type)
print(soil_ty)

unique(data$Rice_ecosystem)
unique(data$Irrigation)
unique(data$Topography)
unique(data$`Flooded (Y/N)`)
unique(data$Method_of_planting)
unique(data$`Is_the farmer_aware_of_Mg_before`)
unique(data$rep_no)
unique(data$Gall_count)

###  make data set_ one row for each site
# summarize disease severity at the sampling site level

df_site_severity <- dataset_1 %>%
  group_by(serial_no) %>% 
  summarise(
    total_gall_count = sum(gall_count, na.rm = TRUE),
    infected_plant_count = sum(gall_count > 0, na.rm = TRUE),
    total_plants = n()
  )

#make data set with out site gall counts, replicates
df_without_gall <- dataset_1 %>%
  select(-"rep_no", -"gall_count")

skim(df_without_gall)

df_site_metadata <- df_without_gall %>%
  group_by(serial_no) %>%
  slice(1) %>%
  ungroup()

View(df_site_metadata)


#finalized dataset
df <- df_site_metadata %>%
  left_join(df_site_severity, by = "serial_no")

View(df)
skim(df)

as.numeric(df$date_of_maturity )    #date of maturit appear as character earlier

#create disease severity and disease incidence
df <- df %>%
  mutate(
    Disease_Incidence = (infected_plant_count / 30) * 100,
    Disease_Severity = (total_gall_count / 30)
  )

#Save the dataset
write.csv(df, "dataset_site.csv", row.names = FALSE)



View(df_regions_infection)


 df <- df[,-31]
#create a bew varible by considerng agro and cliamte zone
df$CLIMATE_AGRO_ECO_ZONE <- substr(df$AGRO_ECO_R, 1, 2)

write.csv(df, "updated_survey_data.csv", row.names = FALSE)
