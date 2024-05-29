rm(list=ls())

library(readxl)
library(tableone)
library(tidyverse)

# Table 1 #----
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration_meta$Antibody <- factor(DB_integration_meta$Antibody, levels = c("Control", "AChR+", "MuSK+", "SNMG"))

## Vector of variables to summarize
myVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender", "Visiting_age",	'Onset_age',	'Duration',	'ADL_all',	'QOL_all',	'MGC_all',	'QMG_all')

## Vector of categorical variables that need transformation
catVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender")

## Create a TableOne object
tab2 <- CreateTableOne(data = DB_integration_meta, vars = myVars, factorVars = catVars)


#tab3 <- CreateTableOne(data = DB_integration_meta, vars = myVars, factorVars = catVars, strata = "Antibody")
tab3 <- CreateTableOne(data = DB_integration_meta, vars = myVars, factorVars = catVars, strata = "Category_1")


tab3Mat <- print(tab3, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab3Mat, file = "myTable.csv")

# Table 2#----
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration_meta.MG <- data.frame(DB_integration_meta[DB_integration_meta$Category_1 != "Control",]) %>%as_tibble()
#DB_integration_meta.MG$Antibody <- factor(DB_integration_meta.MG$Antibody, levels = c( "AChR+", "MuSK+", "SNMG"))

## Vector of variables to summarize
myVars <- c("ClinicalType", 'Antibody',"Immunosuppression_before", "Thymoma", "Thymectomy", "Gender", "Visiting_age",	'Onset_age',	'Duration',	'ADL_all',	'QOL_all',	'MGC_all',	'QMG_all')

## Vector of categorical variables that need transformation
catVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender")

## Create a TableOne object
tab3 <- CreateTableOne(data = DB_integration_meta.MG, vars = myVars, factorVars = catVars, strata = "leiden")

tab3Mat <- print(tab3, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab3Mat, file = "myTable.csv")



# Table 3 #----
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration_meta.MG <- data.frame(DB_integration_meta[DB_integration_meta$Category_1 != "Control",]) %>%as_tibble()

DB_integration_meta.MG$Antibody <- factor(DB_integration_meta.MG$Antibody, levels = c( "AChR+", "MuSK+", "SNMG"))

## Vector of variables to summarize
myVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender", "Visiting_age",	'Onset_age',	'Duration',	'ADL_all',	'QOL_all',	'MGC_all',	'QMG_all')

## Vector of categorical variables that need transformation
catVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender")

## Create a TableOne object
tab3 <- CreateTableOne(data = DB_integration_meta.MG, vars = myVars, factorVars = catVars, strata = "Antibody")

tab3Mat <- print(tab3, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab3Mat, file = "myTable.csv")




# Table 4 #----
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration_meta.naive <- data.frame(DB_integration_meta[DB_integration_meta$Category_2 == "Naïve AChR+ MG",]) %>%as_tibble()

## Vector of variables to summarize
myVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender", "Visiting_age",	'Onset_age',	'Duration',	'ADL_all',	'QOL_all',	'MGC_all',	'QMG_all')

## Vector of categorical variables that need transformation
catVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender")

## Create a TableOne object
tab3 <- CreateTableOne(data = DB_integration_meta.naive, vars = myVars, factorVars = catVars, strata = "Steroid_response")

tab3Mat <- print(tab3, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab3Mat, file = "myTable.csv")


# Table S Age_range#----
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
#DB_integration_meta.MG <- data.frame(DB_integration_meta[DB_integration_meta$Category_1 == "MG",]) %>%as_tibble()

DB_integration_meta <- DB_integration_meta %>%
  mutate(Age_range = case_when(
    Visiting_age < 35 ~ "10-35",
    Visiting_age > 65 ~ "65-90",
    TRUE ~ "35-65"  # This line is optional if you want to catch any other cases
  ))



## Vector of variables to summarize
myVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender", "Visiting_age",	'Onset_age',	'Duration',	'ADL_all',	'QOL_all',	'MGC_all',	'QMG_all')

## Vector of categorical variables that need transformation
catVars <- c("ClinicalType", "Immunosuppression_before", "Thymoma", "Thymectomy", "Gender")

## Create a TableOne object
tab3 <- CreateTableOne(data = DB_integration_meta, vars = myVars, factorVars = catVars, strata =c("Category_1", "Age_range"))

tab3Mat <- print(tab3, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
tab3Mat

## Save to a CSV file
write.csv(tab3Mat, file = "myTable.csv")