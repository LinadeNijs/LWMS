# ------------------------------------------------------------------------------------------------- #   
#                                LMWS analyse testreizen
# ------------------------------------------------------------------------------------------------- #
# 
#   Lina de Nijs
# 
#   13/10/2022 
# 
# ------------------------------------------------------------------------------------------------- #

# TIP ####
# title #### = sort of index = main heading
#### title #### = subtitle
# you can see the index if you click on the top right corner of your script (the sign with some horizontal stripes)

# LOAD NECESARRY PACKAGES ####
    library(tidyverse) # library(tidymodels)
    if(!require(easypackages)) install.packages("easypackages")
    easypackages::packages(c("tidyverse", 'dbplyr','readxl','ggplot2')) # here you can put all the packages you will need
    

# NECESARRY FUNCTIONS 
    r2 <-function(x, y) summary(lm(y~x))$r.squared # function to calculate R2
    
    lowcase <- function(df) {
      names(df) <- tolower(names(df)) %>% gsub("\\?|\\s+|\\.+|_+|\\(|\\)","",.) 
      df
    }

#- Set location
datafolder <- "D:/R/LWMS analyse/data"
myfile <- "SCH123 2022 template - length and bio.xlsx"

# setwd("~/") # set to the working directory you stored the file in 
setwd("D:/R/LWMS analyse")

# LOAD DATA ####
# make sure you close the xlsx file before loading it into R
sheet_names <- excel_sheets("data/SCH123 2022 template - length and bio.xlsx")           # Get sheet names, just for your idea/ overview

path <- ("data/SCH123 2022 template - length and bio.xlsx")
data <- path %>%   excel_sheets() %>%   set_names() %>%   map(read_excel, path = path)
        # this loads all excel sheets into 1 dataframe


# ------------------------------------------------------------------------------------------------- #
#                                       Validatie deel 
# ------------------------------------------------------------------------------------------------- #

# DATA ANALYSIS ####
# raw dataset bio haul
    df <-
      readxl::read_excel(path = file.path(datafolder, myfile),
                         sheet = 'bio haul',
                         col_names = TRUE,
                         col_types = "text", 
              .name_repair =  ~make.names(., unique = TRUE)) %>% 
     lowcase() %>%
    
      mutate_at(c("lengthcm", "weightg", "weightlmws", "lengthlmws"), as.numeric) %>%
      mutate_at(c("fishid"), as.integer) %>%
      mutate(date = as.Date(as.numeric(date), origin = "1899-12-30") ) 
          # Leest dus alleen sheet 'bio haul' in, als ik 'bio merk' wil hebben moet ik dit deel kopieren en specificeren naar 'bio merk'!
          # Inlezen als text, om fouten te voorkomen. Daarna specifieke kolommen omvormen tot getallen (al dan niet met komma) en data. 
          # Lowcase functie haalt alle spaties en streeptjes etc weg uit de kolomnamen



                                      #### Plotjes #### 
# ------------------------------------------------------------------------------------------------- #
#- CORRELATION LENGTH
    df %>% 
      filter(!is.na(species)) %>%
      ggplot(aes(x=lengthlmws, y=lengthcm))+
        geom_point() +
        geom_smooth(method='lm') +
        theme_bw() +
        labs(title="Correlation", x="Length LMWS", y="Length cm") +
        # facet_wrap(~ paste(haul,species), scales = "free")
        # Of per species
         facet_wrap(~ species, scales = "free")
  
#- CORRELATION WEIGHT
    df %>% 
      filter(!is.na(species)) %>%
      ggplot(aes(x=weightlmws, y=weightg))+
      geom_point() +
      geom_smooth(method='lm') +
      theme_bw() +
      labs(title="Correlation", x="Weight LMWS", y="Weight gr") +
      # facet_wrap(~ paste(haul,species), scales = "free")
      # Of per species
      facet_wrap(~ species, scales = "free")
  
#- R^2 
    BioHaul             <-     data$`bio haul`
    specific_species    <-    '...' # Vul hier HER/MAC/HOM in
    subset_BioHaul      <-     BioHaul %>% filter(species == specific_species)  
        
    r2Her <-    r2(subset_BioHaul$lengthlmws, subset_BioHaul$lengthcm)
    r2Mac <-    r2(subset_BioHaul$lengthlmws, subset_BioHaul$lengthcm)
    r2Hom <-    r2(subset_BioHaul$lengthlmws, subset_BioHaul$lengthcm)  
  
    r2Her <-    r2(subset_BioHaul$weightlmws, subset_BioHaul$weightg)
    r2Mac <-    r2(subset_BioHaul$weigthlmws, subset_BioHaul$weightg)
    r2Hom <-    r2(subset_BioHaul$weigthlmws, subset_BioHaul$weightg)

#- LENGTH COMPARISON
t <-
  df %>% 
  dplyr::select(vessel, trip, haul, species, lengthcm, lengthlmws, fishid) %>% 
  filter(!is.na(species)) %>% 
  tidyr::pivot_longer(names_to = "variable", values_to = "data", c(lengthcm, lengthlmws))
  # t wordt dataframe waarin de geselecteerde kolommen weergegeven worden, waar de no species (regel 556 tot 600) niet meedoen en dan dan door pivot onder elkaar ipv naast elkaar. 

  # Per species/per haul
        t2 <-
          t %>% 
          group_by(vessel, trip, haul, species, variable) %>% 
          summarise(
            gem = mean(data, na.rm=TRUE),
            x   = min(fishid))
        # t2 gaat verder met t en maakt groupen op basis van haul en species. Daarna berekent ie gemiddeldes en maakt ie x om later als waarde voor x-as te kunnen gebruiken. 

        t %>% 
          ggplot(aes(x=fishid, y=data, group=haul)) +
          theme_bw() +
          geom_point(aes(colour=variable)) +
          geom_hline(data=t2, aes(yintercept=gem, colour=variable)) +
          #   geom_text(data=t2, aes(y=gem, x=x, label= gem, colour=variable)) +      ## zet berekende waardes van t2 in de plotjes, maar nog niet optimaal/overzichtelijk
          #   geom_path(aes(colour=variable)) +                                       ## verbind de verschillende punten, niet heel nuttig met veel datapunten
          labs(title="Length", x="", y="cm") +
          facet_wrap(~ paste(haul,species), scales = "free")

  # Per species
        t2 <-
          t %>% 
          group_by(vessel, trip, species, variable) %>% 
          summarise(
            gem = mean(data, na.rm=TRUE),
            x   = min(fishid))
        # t2 gaat verder met t en maakt groupen op basis van haul en species. Daarna berekent ie gemiddeldes en maakt ie x om later als waarde voor x-as te kunnen gebruiken. 
        
        t %>% 
          ggplot(aes(x=fishid, y=data, group=haul)) +
          theme_bw() +
          geom_point(aes(colour=variable)) +
          geom_hline(data=t2, aes(yintercept=gem, colour=variable)) +
          #   geom_text(data=t2, aes(y=gem, x=x, label= gem, colour=variable)) +      ## zet berekende waardes van t2 in de plotjes, maar nog niet optimaal/overzichtelijk
          #   geom_path(aes(colour=variable)) +                                       ## verbind de verschillende punten, niet heel nuttig met veel datapunten
          labs(title="Length", x="fishid", y="cm") +
          facet_wrap(~ species, scales = "free")
    
      
#- WEIGHT COMPARISON
w <-
  df %>% 
  dplyr::select(vessel, trip, haul, species, weightg, weightlmws, fishid) %>% 
  filter(!is.na(species)) %>% 
  tidyr::pivot_longer(names_to = "variable", values_to = "data", c(weightcm, weigthlmws))
  # t wordt dataframe waarin de geselecteerde kolommen weergegeven worden, waar de no species (regel 556 tot 600) niet meedoen en dan dan door pivot onder elkaar ipv naast elkaar. 
   
  # Per species/per haul
        w2 <-
          w %>% 
          group_by(vessel, trip, haul, species, variable) %>% 
          summarise(
            gem = mean(data, na.rm=TRUE),
            x   = min(fishid))
        # w2 gaat verder met t en maakt groupen op basis van haul en species. Daarna berekent ie gemiddeldes en maakt ie x om later als waarde voor x-as te kunnen gebruiken. 
        
        w %>% 
          ggplot(aes(x=fishid, y=data, group=haul)) +
          theme_bw() +
          geom_point(aes(colour=variable)) +
          geom_hline(data=w2, aes(yintercept=gem, colour=variable)) +
          #   geom_text(data=w2, aes(y=gem, x=x, label= gem, colour=variable)) +      ## zet berekende waardes van t2 in de plotjes, maar nog niet optimaal/overzichtelijk
          #   geom_path(aes(colour=variable)) +                                       ## verbind de verschillende punten, niet heel nuttig met veel datapunten
          labs(title="Weight", x="Fish ID", y="Gr") +
          facet_wrap(~ paste(haul,species), scales = "free")

  # Per species
        w2 <-
          w %>% 
          group_by(vessel, trip, species, variable) %>% 
          summarise(
            gem = mean(data, na.rm=TRUE),
            x   = min(fishid))
        # w2 gaat verder met t en maakt groupen op basis van haul en species. Daarna berekent ie gemiddeldes en maakt ie x om later als waarde voor x-as te kunnen gebruiken. 
        
        w %>% 
          ggplot(aes(x=fishid, y=data, group=haul)) +
          theme_bw() +
          geom_point(aes(colour=variable)) +
          geom_hline(data=w2, aes(yintercept=gem, colour=variable)) +
          #   geom_text(data=w2, aes(y=gem, x=x, label= gem, colour=variable)) +      ## zet berekende waardes van t2 in de plotjes, maar nog niet optimaal/overzichtelijk
          #   geom_path(aes(colour=variable)) +                                       ## verbind de verschillende punten, niet heel nuttig met veel datapunten
          labs(title="Weight", x="Fish ID", y="Gr") +
          facet_wrap(~ species, scales = "free")


#- RELATIVE LENGTH DIFFERENCE
  df %>% 
    mutate(differenceL = lengthlmws-lengthcm) %>% 
    filter(!is.na(species)) %>%
    group_by(species) %>%                             #species, haul (als je scherper overzicht wil hebben per trek, dan ook de andere facet_wrap gebruiken)
    mutate(avg = mean(differenceL, na.rm=TRUE)) %>% 
    
    ggplot(aes(x=fishid, y=differenceL)) +
    theme_bw() +
    geom_point() +
    #geom_line() +
    geom_hline(aes(yintercept = avg), colour="blue") +
    #geom_text(aes(label=format(avg, format="0.01"), y=avg, x=20), colour="blue") +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(title="Relative length difference", x="Fish ID", y="cm") +
    #facet_wrap( ~ paste(haul,species), scales = "free")
    facet_wrap(~ species, scales = "free")

#- RELATIVE WEIGHT DIFFERENCE
  df %>% 
    mutate(differenceW = weightlmws-weightg) %>% 
    filter(!is.na(species)) %>%
    group_by(species) %>%                             #species, haul (als je scherper overzicht wil hebben per trek, dan ook de andere facet_wrap gebruiken)
    mutate(avg = mean(differenceW, na.rm=TRUE)) %>% 
    
    ggplot(aes(x=fishid, y=differenceW)) +
    theme_bw() +
    geom_point() +
    #geom_line() +
    geom_hline(aes(yintercept = avg), colour="blue") +
    #geom_text(aes(label=format(avg, format="0.01"), y=avg, x=20), colour="blue") +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(title="Relative weight difference", x="Fish ID", y="gr") +
    #facet_wrap( ~ paste(haul,species), scales = "free")
    facet_wrap(~ species, scales = "free")

  
  
# ------------------------------------------------------------------------------------------------- #
#                                       Doormeet deel 
# ------------------------------------------------------------------------------------------------- #

# DATA ANALYSIS ####
# raw dataset bio merk
    dfm <-
      readxl::read_excel(path = file.path(datafolder, myfile),
                         sheet = 'bio merk',
                         col_names = TRUE,
                         col_types = "text", 
                         .name_repair =  ~make.names(., unique = TRUE)) %>% 
      lowcase() %>%
      
      mutate_at(c("lengthcm", "weightg", "weightlmws", "lengthlmws"), as.numeric) %>%
      mutate_at(c("fishid"), as.integer) %>%
      mutate(date = as.Date(as.numeric(date), origin = "1899-12-30") ) 


  
                                      #### Plotjes #### 
# ------------------------------------------------------------------------------------------------- #

    























# LENGTH FREQUENCY - WERKT NOG NIET
#     LF <- df %>% 
#       tidyr::pivot_longer(names_to = "variable", values_to = "data", c(lengthcm, lengthlmws)) %>% 
#       mutate(data = floor(data)) %>% 
#       group_by(haul, species, variable, data, fishid) %>% 
#       summarise(n = n()) %>% 
#       ggplot(aes(x=data, y=n)) +
#       theme_bw() +
#       geom_point(aes(colour=variable)) +
#       geom_line(aes(colour=variable)) +
#       # geom_bar(aes(fill=variable), stat="identity", position=position_dodge2(preserve="single")) +
#       labs(title="length-frequency", x="length", y="N") +
#       facet_wrap( ~ paste(haul, species))


# Relatie SL and TL
#      df %>% 
#        mutate(TL = floor(TL), SL = floor(SL)) %>% 
#        tidyr::pivot_longer(names_to = "variable", values_to = "data", c(SL, TL)) %>% 
#        group_by(Species, variable, data) %>% 
#        summarise(n = n()) %>% 
#        
#        ggplot(aes(x=data, y=n)) +
#        theme_publication() +
#        geom_point(aes(colour=variable)) +
#        geom_line(aes(colour=variable)) +
#        labs(title="length-frequency", x="length", y="N") +
#        facet_wrap( ~ Species, scales = "free_y")


