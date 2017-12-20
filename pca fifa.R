library(ggplot2)
library(readr)
library(devtools)
library(tidyverse)  # for data wrangling
library(stringr)    # for string manipulations
library(ggbiplot)   # pca biplot with ggplot
library(Rtsne)      # implements the t-SNE algorithm
library(kohonen)    # implements self organizing maps
library(hrbrthemes) # nice themes for ggplot
library(GGally)     # to produce scatterplot matrices
install.packages("sqldf")
library(sqldf)

# Load the dataset
fifa_tbl <- readr::read_csv("~/Desktop/cleaned.csv")

# Remove GK Stats
fifa_tbl <- fifa_tbl %>%
  select(Acceleration:Finishing,`Heading accuracy`:Volleys, `Preferred Positions`)

# Create position vector, group it
fifa_tbl <- fifa_tbl %>%
  mutate(position = word(`Preferred Positions`,1)) %>% 
  mutate(position = factor(position,
                           levels = c("GK","CB","RB","LB","RWB","LWB","CDM",
                                      "CM","RM","LM","CAM",
                                      "CF","RW","LW","ST"))) %>% 
  mutate(position = case_when(
    position == 'RW'|position == 'LW'|position == 'RM'|position == 'LM' ~ 'Wing',
    position == 'RB'|position == 'LB' ~ 'Full Back',
    position == 'ST'|position == 'CF' ~ 'Forward',
    position == 'CAM' ~ 'CAM',
    position == 'CM' ~ 'CM',
    position == 'CDM' ~ 'CDM',
    position == 'CB' ~ 'CB', 
    TRUE ~ 'Other'
  )) %>%
  
  # Filter out defenders 
  filter(position != 'Other',
         position != 'CB',
         position != 'Full Back') %>%
  select(-`Preferred Positions`) %>%
  na.omit()

# Remove position and run pca 
fifa_pca <- fifa_tbl %>% 
  select(-position) %>%
  prcomp(center=TRUE,scale.=TRUE)

#plot PCA in biplot
ggbiplot(fifa_pca, 
         obs.scale = 1, 
         var.scale = 1, 
         groups = fifa_tbl$position, 
         varname.size = 4, 
         varname.adjust = 2,
         loadings=TRUE
         )

#plot PCA in autoplot
autoplot(fifa_pca,
         data=fifa_tbl,
         colour='position',
         loadings=TRUE,
         loadings.label=TRUE)
