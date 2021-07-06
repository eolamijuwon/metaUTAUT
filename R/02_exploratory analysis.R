

################################################################################
#
#' ========================> Sexuality Education in the Digital Age:
#' Modelling the predictors of acceptance and behavioural intention
#' to access and interact with sexuality information on social media.
#
# EXPLORATORY ANALYSIS
#
################################################################################

healthInfo_survey <- readRDS("Data/HISurvey_Africa.rds") %>% 
                     filter (order_attempt == 1)

questions <- read.csv("Data/questions.csv")

## Demographic Characteristics ---------------------------------

demographics <-   descrTable(healthInfo_survey %>% 
                               select(age, sex,
                                      country, education,
                                      relationship),
                             show.n = FALSE)

export2html(demographics,
            file = "Output/Table 1 - Demographics.html")
## -------------------------------------------------------------



          healthInfo_survey %>% 
          select(PE01:BII03) %>% 
          gather("Variable", "value") %>% 
          group_by(Variable) %>% 
          summarise(Mean=mean(value, na.rm=TRUE), 
                    SD=sd(value, na.rm=TRUE), 
                    min=min(value, na.rm=TRUE), 
                    max=max(value, na.rm=TRUE), 
                    '% Missing'=100*length(which(is.na(value)))/n())
  
# Loading/installing required packages
          pc3 <- principal(healthInfo_survey %>% select(PE01:BII03), nfactors=7, rotate="varimax")
          print.psych(pc3, cut = 0.4, sort = TRUE)
          
          
          fit <- factanal(healthInfo_survey %>% select(PE01:BII03), 7, rotation="varimax")
          print(fit, digits=2, cutoff=.5, sort=TRUE)
          
          fit_pro <- factanal(healthInfo_survey %>% select(PE01:BII03), 7, rotation="promax")
          print(fit_pro, digits=2, cutoff=.5, sort=TRUE)


          


showtext_auto()

if ("BarlowCondensed-Medium.otf" %in% list.files("C:\\Windows\\Fonts")) {

  
  font_add("BarlowCondensed-Medium", "BarlowCondensed-Medium.otf")
  Font <- "BarlowCondensed-Medium"
  
} else {
  
  font_add("ARIALN", "ARIALN.ttf")
  Font <- "ARIALN"
  
}

if ("BarlowCondensed-LightItalic.otf" %in% list.files("C:\\Windows\\Fonts")) {
  
  
  font_add("BarlowCondensed-LightItalic", "BarlowCondensed-LightItalic.otf")
  Text <- "BarlowCondensed-LightItalic"
  
} else {
  
  font_add("ARIALN", "ARIALN.ttf")
  Text <- "ARIALN"
  
}


## Social Media Preference :: Digital 
pref  <-  healthInfo_survey %>% count(sexEd_pref) %>% 
          mutate (perc = ((n/sum(n))) %>% round(3)) %>% 
          rename (Preference = sexEd_pref) %>% 
          filter (!is.na(Preference)) %>% 
          arrange (desc(perc)) %>% 
          mutate(pos = ifelse((perc > .03),
                              (perc - .03),
                              (perc + .03)),
                 color = ifelse((perc > .05),
                                "inside",
                                "outside"))



        pref %>% 
        ggplot(aes(x = perc, y = reorder(Preference, perc), fill = Preference)) +
        geom_bar(stat="identity") +
        geom_text(aes(x = pos, y = Preference, label = paste0(perc*100,"%"),
                      color = color), size = 10.5,
                  family = Font) +
        scale_x_continuous(limits = c(0, 0.5),
                           breaks = c(0, .1, .2,
                                      .3, .4, .5),
                           labels = percent_format()) +
        theme_bw(base_family = Font,
                 base_size = 45) + 
        scale_fill_manual(values = c("#7A6E6E", "#333030", "#2B71AD",
                                     "#EB5EC1", "#8AC7E6", "#23CFBB")) +  
        scale_color_manual(values = c("#FFFFFF", "#000000")) +
        labs(x = "Percentages") +
        theme(axis.title.y = element_blank(),
              legend.position = "none",
              panel.grid = element_blank())
        
        
        ## Save Plot
        ggsave("Output/present_choice.png", 
               dpi=250, height= 6, width= 9.5)  
        



## Acceptability of Social Media :: Digital 
accept <- healthInfo_survey %>% count(sexEd_acc) %>% 
          mutate (perc = ((n/sum(n))) %>% round(3)) %>% 
          rename (Acceptability = sexEd_acc) %>% 
          filter (!is.na(Acceptability)) %>% 
          mutate (colour = ifelse((Acceptability == "Acceptable" |
                                  Acceptability == "Perfectly Acceptable" |
                                   Acceptability == "Slightly acceptable"),
                                  "#2B71AD",
                                  "#000000")) %>% 
          mutate(Acceptability = derivedFactor("Perfectly \nAcceptable" = (Acceptability == "Perfectly Acceptable"),
                                               "Acceptable" = (Acceptability == "Acceptable"),
                                               "Slightly \nAcceptable" = (Acceptability == "Slightly acceptable"),
                                               "Neutral" = (Acceptability == "Neutral"),
                                               "Unacceptable" = (Acceptability == "Unacceptable"),
                                               "Slightly \nUnacceptable" = (Acceptability == "Slightly Unacceptable"),
                                               "Totally \nUnacceptable" = (Acceptability == "Totally Unacceptable"),
                                               
                                               .default = NA)) %>% 
          mutate(pos = ifelse((perc > .05),
                              (perc - .05),
                              (perc + .03)),
                 color = ifelse((perc > .05),
                                "inside",
                                "outside"))




          accept %>% 
          ggplot(aes(y = perc, x = Acceptability, fill = colour)) +
          geom_bar(stat="identity") +
          geom_text(aes(y = pos, x = Acceptability, 
                        label = paste0(perc*100,"%"), color = color),
                    size = 10.5, family = Font) +
          scale_y_continuous(limits = c(0, 0.5),
                             breaks = c(0, .1, .2,
                                        .3, .4, .5),
                             labels = percent_format()) +
          theme_bw(base_family = Font,
                   base_size = 40) + 
          scale_fill_manual(values = c("#000000",
                                       "#2B71AD")) +
          scale_color_manual(values = c("#FFFFFF", "#000000")) +
          labs(x = "Percentages") +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(lineheight = unit(0.4, "pt")),
                legend.position = "none",
                panel.grid = element_blank())

          
          ## Save Plot
          ggsave("Output/present_Acceptance.png", 
                 dpi=250, height= 6, width= 9)  
          

