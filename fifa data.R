install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE, force=TRUE)

download.file("https://github.com/ricardo-bion/ggtech/blob/master/Circular%20Air-Light%203.46.45%20PM.ttf", "~/Circular Air-Light 3.46.45 PM.ttf", method="curl")
extrafont::font_import(pattern = 'Circular', prompt=FALSE)

extrafont::font_import(pattern = 'Circular', prompt = FALSE)
install.packages("memisc")
library(memisc)
library(ggplot2)
library(tidyverse)
library(readr)
library(tibble)
library(scales)
library(ggradar)
library(ggiraph)
library(ggfortify)
library(devtools)
library(ggbiplot)

fifa <- readr::read_csv("~/Desktop/CompleteDataset.csv")

#clustering
fifa3 <- fifa %>%
  filter(
           `Preferred Positions` == 'RB'
         | `Preferred Positions` == 'LB'
         | `Preferred Positions` == 'CB'
         | `Preferred Positions` == 'CAM'
         | `Preferred Positions` == 'CM'
         | `Preferred Positions` == 'CDM'
         | `Preferred Positions` == 'LM'
         | `Preferred Positions` == 'LW'
         | `Preferred Positions` == 'RW'
         | `Preferred Positions` == 'RM'
         | `Preferred Positions` == 'CF'
         | `Preferred Positions` == 'ST'
         )%>%
  
  #filter(  Nationality=='Portugal'
  #       | Nationality=='Spain'
  #      | Nationality=='Brazil'
  #      | Nationality=='Germany')%>%
  
  
  mutate(position = case_when(
    `Preferred Positions` == 'RW'|`Preferred Positions` == 'LW' ~ 'Wing',
    `Preferred Positions` == 'RM'|`Preferred Positions` == 'LM' ~ 'Flank',
    `Preferred Positions` == 'RB'|`Preferred Positions` == 'LB' ~ 'Full Back',
    `Preferred Positions` == 'ST'|`Preferred Positions` == 'CF' ~ 'Forward',
    `Preferred Positions` == 'CAM' | `Preferred Positions` == 'CM' | `Preferred Positions` == 'CDM' ~ 'Mid',
    `Preferred Positions` == 'CB' ~ 'CB', 
    TRUE ~ 'Other'
    ))%>%
  group_by(position) %>%
  summarise(
    finish = mean(as.numeric((substr(Finishing,1,2)))),
    strength = mean(as.numeric((substr(Strength,1,2)))),
    speed = mean(as.numeric((substr(`Sprint speed`,1,2)))),
    jump = mean(as.numeric((substr(Jumping,1,2)))),
    tackling = mean(as.numeric((substr(`Standing tackle`,1,2))))
    )

  ggradar(fifa3,grid.max = 80, grid.min = 20) 
  
##  PCA
  cleanedForCaleb <- fifa %>%
    filter(
      `Preferred Positions` == 'RB'
      | `Preferred Positions` == 'LB'
      | `Preferred Positions` == 'CB'
      | `Preferred Positions` == 'CAM'
      | `Preferred Positions` == 'CM'
      | `Preferred Positions` == 'CDM'
      | `Preferred Positions` == 'LM'
      | `Preferred Positions` == 'LW'
      | `Preferred Positions` == 'RW'
      | `Preferred Positions` == 'RM'
      | `Preferred Positions` == 'CF'
      | `Preferred Positions` == 'ST'
    )%>%
    mutate(position = case_when(
      `Preferred Positions` == 'RW'|`Preferred Positions` == 'LW' ~ 'Wing',
      `Preferred Positions` == 'RM'|`Preferred Positions` == 'LM' ~ 'Flank',
      `Preferred Positions` == 'RB'|`Preferred Positions` == 'LB' ~ 'Full Back',
      `Preferred Positions` == 'ST'|`Preferred Positions` == 'CF' ~ 'Forward',
      `Preferred Positions` == 'CAM' ~ 'CAM',
      `Preferred Positions` == 'CM' ~ 'CM',
      `Preferred Positions` == 'CDM' ~ 'CDM',
      `Preferred Positions` == 'CB' ~ 'CB', 
      TRUE ~ 'Other'
    )) %>%
    na.omit()
  
  calebHead <- na.omit(head(cleanedForCaleb, 1000))
  
  forColor <- subset(
    calebHead,
    select = c(76,
               as.numeric(substr(14,1,2)),
               as.numeric(substr(15,1,2)),
               as.numeric(substr(16,1,2)),
               as.numeric(substr(17,1,2)),
               as.numeric(substr(18,1,2)),
               as.numeric(substr(19,1,2)),
               as.numeric(substr(20,1,2)),
               as.numeric(substr(21,1,2)),
               as.numeric(substr(22,1,2)),
               as.numeric(substr(23,1,2)),
               as.numeric(substr(24,1,2))
               )
    ) %>%
    na.omit()
  
  #### new 
  fifa.start <- na.omit(data.matrix(forColor[,2:12]))
  fifa.positions <- na.omit(forColor[,1])
  fifa.pca <- prcomp(fifa.start,
                     center = TRUE,
                     scale. = TRUE) 
  
  str(fifa.start)
  
  g <- ggbiplot(fifa.pca, obs.scale = 1, var.scale = 1)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  print(g)
  
  
  
  
  
  
  #### old 
  forpca <- data.matrix(
    subset(
      forColor, 
      select = c(
        2,3,4,5,6,7,8,9,10,11,12
      )
    )
  )%>%
    na.omit()
  
  autoplot(prcomp(forpca), data = calebHead, colour = 'position')
  
  write.csv(cleanedForCaleb, "cleanedforcaleb.csv")

  df <- iris[c(1, 2, 3, 4)]
