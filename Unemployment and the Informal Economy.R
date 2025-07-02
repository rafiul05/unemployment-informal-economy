##################################################################################################
# ECO 530 - Introduction to Econometrics (Fall 2024) 
# Date: 11 October 2024
# Author: Rafiul Ahmed
# RESEARCH POSTER _ DRAFT 1
##################################################################################################

################################################################################
#Clear the space
rm(list=ls())

## Install/Load packages
library(modelsummary)
library(estimatr)
library(tidyverse)
library(vtable)
library(ggplot2)
library(dplyr)
library(fixest)
library (plm)
## Set working directory
datafolder <- "I:/My Drive/Graduate Studies/Fall 2024/ECO 530 Econometrics/Poster Presentation/Data"
scriptsfolder <- "I:/My Drive/Graduate Studies/Fall 2024/ECO 530 Econometrics/Poster Presentation/Script"
tablesfigures <- "I:/My Drive/Graduate Studies/Fall 2024/ECO 530 Econometrics/Poster Presentation/Tables & Figures"


#Load the data
library(readxl)
infrml_econ_unemp_database <- read_excel("G:/My Drive/Graduate Studies/Fall 2024/ECO 530 Econometrics/Poster Presentation/Data/infrml-econ_unemp database.xlsx")
View(infrml_econ_unemp_database)

# Specify the file path
file_path <- "G:/My Drive/Graduate Studies/Fall 2024/ECO 530 Econometrics/Poster Presentation/Data/infrml-econ_unemp database.xlsx"

# Get the names of all sheets
sheet_names <- excel_sheets("G:/My Drive/Graduate Studies/Fall 2024/ECO 530 Econometrics/Poster Presentation/Data/infrml-econ_unemp database.xlsx")

# Read all sheets into a list
all_sheets <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# Name the list elements with the sheet names
names(all_sheets) <- sheet_names

# to access the "COMPILED DATA" sheet:
compiled_data <- all_sheets[["COMPILED DATA"]]
########################################################################
#Table1
#Summary Table

# Load necessary libraries
library(dplyr)
library(knitr)
library(kableExtra)
library(readxl)

# Step 1: Load your dataset
file_path <- "G:/My Drive/Graduate Studies/Fall 2024/ECO 530 Econometrics/Poster Presentation/Data/infrml-econ_unemp database.xlsx"
df_combined <- read_excel(file_path, sheet = "COMPILED DATA")

# Step 2: Create a summary table with separate rows for each variable, including the number of observations
summary_stats <- df_combined %>%
  summarise(
    Unemployment_Mean = round(mean(UNEMP_p, na.rm = TRUE), 2),
    Unemployment_SD = round(sd(UNEMP_p, na.rm = TRUE), 2),
    Unemployment_Min = round(min(UNEMP_p, na.rm = TRUE), 2),
    Unemployment_Max = round(max(UNEMP_p, na.rm = TRUE), 2),
    Unemployment_N = sum(!is.na(UNEMP_p)),  # Count of observations for Unemployment
    
    DGE_Mean = round(mean(DGE_p, na.rm = TRUE), 2),
    DGE_SD = round(sd(DGE_p, na.rm = TRUE), 2),
    DGE_Min = round(min(DGE_p, na.rm = TRUE), 2),
    DGE_Max = round(max(DGE_p, na.rm = TRUE), 2),
    DGE_N = sum(!is.na(DGE_p)),  # Count of observations for DGE
    
    MIMIC_Mean = round(mean(MIMIC_p, na.rm = TRUE), 2),
    MIMIC_SD = round(sd(MIMIC_p, na.rm = TRUE), 2),
    MIMIC_Min = round(min(MIMIC_p, na.rm = TRUE), 2),
    MIMIC_Max = round(max(MIMIC_p, na.rm = TRUE), 2),
    MIMIC_N = sum(!is.na(MIMIC_p))  # Count of observations for MIMIC
  )

# Step 3: Reshape the summary table into long format (3 rows for 3 variables)
summary_table <- data.frame(
  Variable = c("Unemployment Rate", "DGE", "MIMIC"),
  Mean = c(summary_stats$Unemployment_Mean, summary_stats$DGE_Mean, summary_stats$MIMIC_Mean),
  SD = c(summary_stats$Unemployment_SD, summary_stats$DGE_SD, summary_stats$MIMIC_SD),
  Min = c(summary_stats$Unemployment_Min, summary_stats$DGE_Min, summary_stats$MIMIC_Min),
  Max = c(summary_stats$Unemployment_Max, summary_stats$DGE_Max, summary_stats$MIMIC_Max),
  N = c(summary_stats$Unemployment_N, summary_stats$DGE_N, summary_stats$MIMIC_N)  # Add number of observations
)

# Step 4: Display the summary table in a clean format using kable
summary_table %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  kableExtra::row_spec(0, font_size = 14, bold = TRUE, color = "black") %>%
  kableExtra::row_spec(1:nrow(summary_table), font_size = 12, color = "black")

########################################################################
#Figure 1
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)  # For formatting the graph


# Read each sheet for Least Developed, Developing, and Developed
least_developed <- read_excel(file_path, sheet = "Least Developed Country")
developing <- read_excel(file_path, sheet = "Develping Country")
developed <- read_excel(file_path, sheet = "Developed Country")

# Step 2: Add a classification column to each dataset
least_developed$Classification <- "Least Developed"
developing$Classification <- "Developing"
developed$Classification <- "Developed"

# Step 3: Combine all datasets into one
df_combined <- bind_rows(least_developed, developing, developed)

# Step 4: Calculate World Average for each year (across all countries)
world_avg <- df_combined %>%
  group_by(Year) %>%
  summarise(avg_unemp = mean(UNEMP_p, na.rm = TRUE),
            avg_dge = mean(DGE_p, na.rm = TRUE),
            avg_mimic = mean(MIMIC_p, na.rm = TRUE)) %>%
  mutate(Classification = "World")

# Step 5: Combine the World Average with the existing country group data
df_grouped <- df_combined %>%
  group_by(Classification, Year) %>%
  summarise(avg_unemp = mean(UNEMP_p, na.rm = TRUE),
            avg_dge = mean(DGE_p, na.rm = TRUE),
            avg_mimic = mean(MIMIC_p, na.rm = TRUE), 
            .groups = "drop")  %>%
  bind_rows(world_avg)  # Add world averages to the dataset

# Step 6: Create a visually appealing line plot for Unemployment Rate over time
ggplot(df_grouped, aes(x = Year)) +
  geom_line(aes(y = avg_unemp, color = Classification, linetype = Classification), size = 1.5) +
  scale_color_manual(values = c("Least Developed" = "#D55E00", 
                                "Developing" = "#E69F00", 
                                "Developed" = "#56B4E9", 
                                "World" = "#009E73")) +
  scale_linetype_manual(values = c("Least Developed" = "solid", 
                                   "Developing" = "solid", 
                                   "Developed" = "solid", 
                                   "World" = "solid")) +
  labs(title = "Unemployment Rate Trends Over Time (1993-2020)",
       x = "Year", y = "Average Unemployment Rate (% of total labor force)",
       color = "Country Group", linetype = "Country Group") +
  theme_minimal(base_size = 12) +  # Increase base size for posters
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom") +  # Center the title and move legend to bottom
  scale_y_continuous(labels = percent_format(scale = 1))  # Format Y-axis as percentage

# Step 7: Similarly, create the DGE and MIMIC line graphs
#DGE
ggplot(df_grouped, aes(x = Year)) +
  geom_line(aes(y = avg_dge, color = Classification, linetype = Classification), size = 1.5) +
  scale_color_manual(values = c("Least Developed" = "#D55E00", 
                                "Developing" = "#E69F00", 
                                "Developed" = "#56B4E9", 
                                "World" = "#009E73")) +
  scale_linetype_manual(values = c("Least Developed" = "solid", 
                                   "Developing" = "solid", 
                                   "Developed" = "solid", 
                                   "World" = "solid")) +
  labs(title = "DGE estimated informal output over time (1993-2020)",
       x = "Year", y = "Average DGE estimated value \n of informal economy (% of official GDP)",
       color = "Country Group", linetype = "Country Group") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")
#MIMIC
ggplot(df_grouped, aes(x = Year)) +
  geom_line(aes(y = avg_mimic, color = Classification, linetype = Classification), size = 1.5) +
  scale_color_manual(values = c("Least Developed" = "#D55E00", 
                                "Developing" = "#E69F00", 
                                "Developed" = "#56B4E9", 
                                "World" = "#009E73")) +
  scale_linetype_manual(values = c("Least Developed" = "solid", 
                                   "Developing" = "solid", 
                                   "Developed" = "solid", 
                                   "World" = "solid")) +
  labs(title = "MIMIC estimated informal output over time (1993-2020)",
       x = "Year", y = "Average MIMIC estimated value \n of informal output (% of official GDP)",
       color = "Country Group", linetype = "Country Group") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

#Figure 2
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)  # For reshaping the data

# Load region sheets (example: "East Asia & Pacific", "Europe & Central Asia", etc.)
east_asia_pacific <- read_excel(file_path, sheet = "East Asia & Pacific")
europe_central_asia <- read_excel(file_path, sheet = "Europe & Central Asia")
latin_america_caribbean <- read_excel(file_path, sheet = "Latin America & Caribbean")
mena <- read_excel(file_path, sheet = "Middle East & N Africa")
south_asia <- read_excel(file_path, sheet = "South Asia")
sub_saharan_africa <- read_excel(file_path, sheet = "Sub-Saharan Africa")
north_america <- read_excel(file_path, sheet = "North America")

# Step 2: Add region classification to each dataset
east_asia_pacific$Region <- "East Asia & Pacific"
europe_central_asia$Region <- "Europe & Central Asia"
latin_america_caribbean$Region <- "Latin America & Caribbean"
mena$Region <- "Middle East & N Africa"
south_asia$Region <- "South Asia"
sub_saharan_africa$Region <- "Sub-Saharan Africa"
north_america$Region <- "North America"

# Step 3: Combine all regions into a single dataset
df_combined <- bind_rows(east_asia_pacific, europe_central_asia, latin_america_caribbean, mena, south_asia, sub_saharan_africa, north_america)

# Step 4: Group by Region and calculate the average for Unemployment, DGE, and MIMIC
df_region_avg <- df_combined %>%
  group_by(Region) %>%
  summarise(avg_unemp = mean(UNEMP_p, na.rm = TRUE),
            avg_dge = mean(DGE_p, na.rm = TRUE),
            avg_mimic = mean(MIMIC_p, na.rm = TRUE))

# Step 5: Reshape the data to long format for easier plotting
df_long <- df_region_avg %>%
  pivot_longer(cols = c(avg_unemp, avg_dge, avg_mimic),
               names_to = "Metric",
               values_to = "Value")

# Step 6: Create a grouped bar chart with legend positioned below
ggplot(df_long, aes(x = Region, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("avg_unemp" = "#008080", "avg_dge" = "#DC143C", "avg_mimic" = "navyblue"),
                    labels = c("Unemployment Rate", "DGE Value", "MIMIC Value")) +
  labs(title = " Figure 2(c): Average Unemployment Rate, DGE based output, \n and MIMIC based output by Region",
       x = "Region", y = "Average Value (%)", fill = "Metric") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") 

#Regression Test (DGE)
#Least Developed

model_leastdeveloped <- lm_robust(UNEMP_p ~ DGE_p, data = least_developed)
modelsummary(model_leastdeveloped,
             title = "Regression Results: Least Developed Countries",
             stars = TRUE)  # Include significance levels


model_leastdevelopedplm <- plm(UNEMP_p ~ DGE_p, data = least_developed, model = "within")
modelsummary(model_leastdevelopedplm)




#Developing

model_developing <- lm_robust(UNEMP_p ~ DGE_p, data = developing)

modelsummary(model_developing,
             title = "Regression Results: Developing Countries",
             stars = TRUE)  # Include significance levels


#Developed
model_developed <- lm_robust(UNEMP_p ~ DGE_p, data = developed)

modelsummary(model_developed,
             title = "Regression Results: Developed Countries",
             stars = TRUE)  # Include significance levels



#Table for DGE
modelsummary(
  list(
    "Least Developed Countries" = model_leastdeveloped,
    "Developing Countries" = model_developing,
    "Developed Countries" = model_developed
  ),
  title = "Table 2: Comparison of DGE Coefficients Across Country Groups",
  gof_omit = "R2|Adj|F|AIC|BIC|Log|RMSE",
  stars = TRUE,
  output = "kableExtra"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2C3E50") %>% # Bold header
  row_spec(3, color = "#E74C3C")

#Regression Test (MIMIC)
#Least Developed

model_leastdeveloped <- lm(UNEMP_p ~ MIMIC_p, data = least_developed)

modelsummary(model_leastdeveloped,
             title = "Regression Results: Least Developed Countries",
             stars = TRUE)  # Include significance levels


#Developing

model_developing <- lm(UNEMP_p ~ MIMIC_p, data = developing)

modelsummary(model_developing,
             title = "Regression Results: Developing Countries",
             stars = TRUE)  # Include significance levels


#Developed
model_developed <- lm(UNEMP_p ~ MIMIC_p, data = developed)

modelsummary(model_developed,
             title = "Regression Results: Developed Countries",
             stars = TRUE)  # Include significance levels


#Combined Table
# Load required libraries
library(estimatr)   # For lm_robust
library(broom)      # For tidy output of models
library(kableExtra) # For beautiful tables

# Step 1: Run Robust OLS Models for DGE and MIMIC
# DGE Models
model_leastdeveloped_dge <- lm_robust(UNEMP_p ~ DGE_p, data = least_developed)
model_developing_dge <- lm_robust(UNEMP_p ~ DGE_p, data = developing)
model_developed_dge <- lm_robust(UNEMP_p ~ DGE_p, data = developed)

# MIMIC Models
model_leastdeveloped_mimic <- lm_robust(UNEMP_p ~ MIMIC_p, data = least_developed)
model_developing_mimic <- lm_robust(UNEMP_p ~ MIMIC_p, data = developing)
model_developed_mimic <- lm_robust(UNEMP_p ~ MIMIC_p, data = developed)

# Step 2: Extract Coefficients, Standard Errors, and Significance
extract_results <- function(model) {
  tidy_model <- tidy(model)
  coef <- tidy_model$estimate[2]  # Coefficient for DGE_p or MIMIC_p
  se <- tidy_model$std.error[2]   # Standard error for DGE_p or MIMIC_p
  pval <- tidy_model$p.value[2]   # p-value for DGE_p or MIMIC_p
  significance <- ifelse(pval < 0.001, "***", 
                         ifelse(pval < 0.01, "**", 
                                ifelse(pval < 0.05, "*", "")))
  return(c(coef, se, significance))
}

# Extract results for all models
results <- data.frame(
  Group = c("Least Developed", "Developing", "Developed"),
  DGE_Coefficient = sapply(list(model_leastdeveloped_dge, model_developing_dge, model_developed_dge), 
                           function(x) extract_results(x)[1]),
  DGE_SE = sapply(list(model_leastdeveloped_dge, model_developing_dge, model_developed_dge), 
                  function(x) extract_results(x)[2]),
  DGE_Sig = sapply(list(model_leastdeveloped_dge, model_developing_dge, model_developed_dge), 
                   function(x) extract_results(x)[3]),
  MIMIC_Coefficient = sapply(list(model_leastdeveloped_mimic, model_developing_mimic, model_developed_mimic), 
                             function(x) extract_results(x)[1]),
  MIMIC_SE = sapply(list(model_leastdeveloped_mimic, model_developing_mimic, model_developed_mimic), 
                    function(x) extract_results(x)[2]),
  MIMIC_Sig = sapply(list(model_leastdeveloped_mimic, model_developing_mimic, model_developed_mimic), 
                     function(x) extract_results(x)[3])
)

# Round the results for presentation
results$DGE_Coefficient <- round(as.numeric(results$DGE_Coefficient), 3)
results$DGE_SE <- round(as.numeric(results$DGE_SE), 3)
results$MIMIC_Coefficient <- round(as.numeric(results$MIMIC_Coefficient), 3)
results$MIMIC_SE <- round(as.numeric(results$MIMIC_SE), 3)

# Step 3: Create the Final Table with Significance Levels
results %>%
  mutate(
    DGE = paste0(DGE_Coefficient, " ", DGE_Sig, " (", DGE_SE, ")"),
    MIMIC = paste0(MIMIC_Coefficient, " ", MIMIC_Sig, " (", MIMIC_SE, ")")
  ) %>%
  select(Group, DGE, MIMIC) %>%
  kable(
    col.names = c("Country Group", "DGE Coefficient (S.E.)", "MIMIC Coefficient (S.E.)"),
    caption = "Table 2: Comparison of DGE and MIMIC estimated Coefficients Across Country Groups",
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "DGE Model" = 1, "MIMIC Model" = 1))



#Using FE 
#DGE
# For least developed countries
model_leastdeveloped_c <- feols(UNEMP_p ~ DGE_p | Economy, data = least_developed)
model_leastdeveloped_cy <- feols(UNEMP_p ~ DGE_p | Economy + Year, data = least_developed)

# For developing countries
model_developing_c <- feols(UNEMP_p ~ DGE_p | Economy, data = developing)
model_developing_cy <- feols(UNEMP_p ~ DGE_p | Economy + Year, data = developing)

# For developed countries
model_developed_c <- feols(UNEMP_p ~ DGE_p | Economy, data = developed)
model_developed_cy <- feols(UNEMP_p ~ DGE_p | Economy + Year, data = developed)

#MIMIC
# For least developed countries
model_leastdeveloped_cm <- feols(UNEMP_p ~ MIMIC_p | Economy, data = least_developed)
model_leastdeveloped_cym <- feols(UNEMP_p ~ MIMIC_p | Economy + Year, data = least_developed)

# For developing countries
model_developing_cm <- feols(UNEMP_p ~ MIMIC_p | Economy, data = developing)
model_developing_cym <- feols(UNEMP_p ~ MIMIC_p | Economy + Year, data = developing)

# For developed countries
model_developed_cm <- feols(UNEMP_p ~ MIMIC_p | Economy, data = developed)
model_developed_cym <- feols(UNEMP_p ~ MIMIC_p | Economy + Year, data = developed)

###DGE Table 3
# List of DGE Models
dge_models <- list(
  "Least Developed (FE by Country)" = model_leastdeveloped_c,
  "Least Developed (FE by Country + Year)" = model_leastdeveloped_cy,
  "Developing (FE by Country)" = model_developing_c,
  "Developing (FE by Country + Year)" = model_developing_cy,
  "Developed (FE by Country)" = model_developed_c,
  "Developed (FE by Country + Year)" = model_developed_cy
)

#DGE Table
dge_table <- modelsummary(
  dge_models,
  stars = TRUE,
  coef_map = c("DGE_p" = "DGE Coefficient"),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|R2|R2 Adj.|R2 Within|R2 Within Adj.|Std.Errors",
  title = "DGE Fixed Effects Regression: Impact of Informal Economy on Unemployment"
)
# Optionally format with kableExtra
dge_table


###MIMIC Table 4
# List of MIMIC Models
mimic_models <- list(
  "Least Developed (FE by Country)" = model_leastdeveloped_cm,
  "Least Developed (FE by Country + Year)" = model_leastdeveloped_cym,
  "Developing (FE by Country)" = model_developing_cm,
  "Developing (FE by Country + Year)" = model_developing_cym,
  "Developed (FE by Country)" = model_developed_cm,
  "Developed (FE by Country + Year)" = model_developed_cym
)

#MIMIC Table
mimic_table <- modelsummary(
  mimic_models,
  stars = TRUE,
  coef_map = c("MIMIC_p" = "MIMIC Coefficient"),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|R2|R2 Adj.|R2 Within|R2 Within Adj.|Std.Errors",
  title = "MIMIC Fixed Effects Regression: Impact of Informal Economy on Unemployment"
)
# Optionally format with kableExtra
mimic_table

