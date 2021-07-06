################################################################################
#
#' ========================> Sexuality Education in the Digital Age:
#' Modelling the predictors of acceptance and behavioural intention
#' to access and interact with sexuality information on social media.
#
# SUPPLEMENTARY ANALYSIS
#
################################################################################


#' Fit a logit model to assess significant socio-demographic 
#' differences between attentive and non-attentive 
#' participants and to create Non-Response Weights based
#' on the four strata - Age - Sex - Education - Country
#' 
      non_resp <- glm(attention ~ age + sex + 
                        country + education, 
                      family = binomial(link = "cloglog"),
                      data = healthInfo_survey)  
      summary(non_resp)

      
      probabilities <-  non_resp %>%
                        predict(type = "response")
      ### ===================================================      
      attention_survey_data <-  cbind(healthInfo_survey, probabilities) %>% 
                                mutate(non_response = 1/probabilities,
                                       ID = row_number()) %>% 
                                filter(attention == 1)
      

## Declare Survey Mode - svydesign
svy_weight <- svydesign(id =~ 1,
                        weights =~ non_response,
                        strata =~ sex+age,
                        data = attention_survey_data)



#' Evaluate/Compare Demographic Distribution in
#' Gross Sample: Full Eligible Sample (n = 936)
#' Net Sample (Weighted): Eligible Sample Weighted for Non-Response (n = 585)
#' Net Sample (Unweighted): Unweighted Eligible Sample (n = 585)

    rbind(
          (attention_survey_data %>% 
          count(sex) %>% 
            rename("Gross Sample" = n,
                   "Characteristics" = sex) %>% 
            mutate("Unweighted Percentage \nn = 585" = round(prop.table(table(attention_survey_data$sex))*100, 2)) %>% 
            mutate("Weighted Percentage \nn = 585" = round(svytable ( ~ sex, svy_weight, Ntotal=TRUE) *100, digits = 2))),
          
          
          (attention_survey_data %>% 
          count(country) %>% 
            rename("Gross Sample" = n,
                   "Characteristics" = country) %>% 
            mutate("Unweighted Percentage \nn = 585" = round(prop.table(table(attention_survey_data$country))*100, 2)) %>% 
            mutate("Weighted Percentage \nn = 585" = round(svytable ( ~ country, svy_weight, Ntotal=TRUE) *100, digits = 2))),

            
          (attention_survey_data %>% 
          count(education) %>% 
            rename("Gross Sample" = n,
                   "Characteristics" = education) %>% 
            mutate("Unweighted Percentage \nn = 585" = round(prop.table(table(attention_survey_data$education))*100, 2)) %>% 
            mutate("Weighted Percentage \nn = 585" = round(svytable ( ~ education, svy_weight, Ntotal=TRUE) *100, digits = 2))),
            
          attention_survey_data %>% 
          count(relationship) %>% 
            rename("Gross Sample" = n,
                   "Characteristics" = relationship) %>% 
            mutate("Unweighted Percentage \nn = 585" = round(prop.table(table(attention_survey_data$relationship))*100, 2)) %>% 
            mutate("Weighted Percentage \nn = 585" = round(svytable ( ~ relationship, svy_weight, Ntotal=TRUE) *100, digits = 2))
          ) %>% 
      
        kable(caption = "", align = "l", booktabs = T, position = "left") %>%
        kable_styling("striped", full_width = F) %>% 
        kableExtra::group_rows("Sex", 1, 2) %>%
        kableExtra::group_rows("Country", 3, 5) %>%
        kableExtra::group_rows("Education", 6, 7) %>%
        kableExtra::group_rows("Relationship Status", 8, 10) %>% 
        save_kable(file = "Output/Table S1 - Demographics.html", self_contained = T, bs_theme = "simplex")
      
            
            
## Model Re-Specification
          
    utaut.SEM <- '
              # ## Latent Variable
              attitude =~ AT01 + AT02 + AT03
              effort_expect =~ EE01 + EE02
              performance_expect =~ PE01 + PE02 + PE03
              social_influence =~ SI01 + SI02 + SI03 + SI04
              intent_use =~ BIU01 + BIU02
              intent_interact =~ BII01 + BII02 + BII03


              ## Mediating Path
              
              effort_expect         ~   FC02 + social_influence

              attitude              ~   performance_expect + 
                                        social_influence + 
                                        effort_expect
  
              ## Endogenous Path  
              intent_use            ~   performance_expect + social_influence + 
                                        effort_expect + FC02 + attitude + 
                                        age + gender_male + relationship_yes +
                                        education_tertiary + 
                                        country_ke
  
              intent_interact       ~   performance_expect + social_influence + 
                                        effort_expect + FC02 + attitude +
                                        age + gender_male + relationship_yes +
                                        education_tertiary + 
                                        country_ke
  
              ## Co-Variances 
              SI03 ~~ SI04
              SI01 ~~ SI02
              BIU02 ~~ BII01
              AT01 ~~ AT03

              ##
              performance_expect    ~~	FC02
              social_influence      ~~  FC02
              effort_expect	        ~~  performance_expect

              '
## Model Fitting ========================
    
        ## Gross Sample
        utaut.GrossModel   <-     sem(utaut.SEM,
                                      data=healthInfo_survey,
                                      estimator = "MLMVS",
                                      se = "robust",
                                      auto.var = TRUE,
                                      fixed.x = FALSE)
        
        ## Net Sample (Unweighted)
        utaut_UnweightedModel  <- sem(utaut.SEM,
                                      data=attention_survey_data,
                                      estimator = "MLMVS",
                                      se = "robust",
                                      auto.var = TRUE,
                                      fixed.x = FALSE)
        
        ## Net Sample (Weighted)
        utaut_WeightedModel   <-      lavaan.survey(utaut_UnweightedModel,
                                                    svy_weight,
                                                    estimator = "MLMVS")
        #' We are aware of a warning about covariance matrix the covariance matrix of 
        #' the residuals of the observed variables (theta) is not positive definite.
        #' Given that this warning is only related to `lavaan.survey` and no similar
        #' warnings for the other models [gross and unweighted net sample],
        #' we suspect that the warning may in fact be more related to the package
        #' estimation approach than an indication of model misfit.
        #' 
        #' See here: https://stats.stackexchange.com/questions/134348/when-a-cfa-model-has-a-covariance-matrix-was-not-positive-definite-problem-is

## ===================================
        
    summary(utaut_UnweightedModel, fit.measures = TRUE, 
            standardize = TRUE, rsquare = TRUE)
    
        fitmeasures(utaut.GrossModel,
                    c('chisq', 'df', 'pvalue',
                      'rmsea', 'tli', 'cfi',
                      'nfi', 'pnfi', 'gfi'))
    

## Model Fit-Indicies for all Samples ============

    (fit   <-    cbind(
                  (fitmeasures(utaut_WeightedModel,
                               c('chisq', 'df', 'pvalue', 'rmsea',
                                 'srmr', 'tli', 'cfi', 'pnfi')) %>% 
                     data.frame () %>% 
                     rownames_to_column ("Fit statistics") %>%
                     rename ("value" = ".") %>% 
                     rbind(c("chisq/df", .$value[1]/.$value[2])) %>% 
                     rename ("Weighted Net \nSample (n = 585)" = "value") %>% 
                     mutate (`Weighted Net \nSample (n = 585)` = as.numeric(`Weighted Net \nSample (n = 585)`, 2) %>% round(3)) %>% 
                     mutate (`Recommended Value` = c("", "", ">=0.05",
                                                     "<=0.05", "<=0.08", ">=0.95", ">=0.95",
                                                     ">=0.60", "<=3.00")) %>% 
                     select("Fit statistics", "Recommended Value", "Weighted Net \nSample (n = 585)")),
                  
                  (fitmeasures(utaut_UnweightedModel,
                               c('chisq', 'df', 'pvalue', 'rmsea',
                                 'srmr', 'tli', 'cfi', 'pnfi')) %>% 
                     data.frame () %>% 
                     remove_rownames() %>%
                     rename ("value" = ".") %>% 
                     rbind(.$value[1]/.$value[2]) %>% 
                     rename ("Unweighted Net \nSample (n = 585)" = "value") %>% 
                     mutate (`Unweighted Net \nSample (n = 585)` = as.numeric(`Unweighted Net \nSample (n = 585)`, 2) %>% round(3))),
                  
                  
                  (fitmeasures(utaut.GrossModel,
                               c('chisq', 'df', 'pvalue', 'rmsea',
                                 'srmr', 'tli', 'cfi', 'pnfi')) %>% 
                     data.frame () %>% 
                     remove_rownames() %>%
                     rename ("value" = ".") %>% 
                     rbind(.$value[1]/.$value[2]) %>% 
                     rename ("Gross Sample \n(n = 936)" = "value") %>% 
                     mutate (`Gross Sample \n(n = 936)` = as.numeric(`Gross Sample \n(n = 936)`, 2) %>% round(3)))))
                     
    
    fit   <-  fit[c(1,2,9, 3:8),] %>% 
      mutate(`Fit statistics` = derivedFactor("Chi-Square (X^2)" = (`Fit statistics` == "chisq"),
                                              "Degree of Freedom (DF)" = (`Fit statistics` == "df"),
                                              "Chi-Square (X^2)/Degree of Freedom (DF)" = (`Fit statistics` == "chisq/df"),
                                              "Root Mean Square Error Approximation (RMSEA)" = (`Fit statistics` == "rmsea"),
                                              "Tucker-Lewis Index (TLI)" = (`Fit statistics` == "tli"),
                                              "Comparative Fit Index (CFI)" = (`Fit statistics` == "cfi"),
                                              "Goodness of Fit Index (GFI)" = (`Fit statistics` == "gfi"),
                                              "Normaed Fit Index (AGFI)" = (`Fit statistics` == "nfi"),
                                              "Penalized Normed Fit Index (PNFI)" = (`Fit statistics` == "pnfi"),
                                              "Standardized Root Mean Squared Residual (SRMR)" = (`Fit statistics` == "srmr"),
                                              "Probability Value (p-value)" = (`Fit statistics` == "pvalue"),
                                              .default = NA))
    fit %>% 
      kable(align = "l", digits=3, 
            booktabs = T, position = "left") %>%
      kable_styling("striped", full_width = F) %>%
      print() %>% 
      save_kable(file = "Output/Table S2 - Model Fit.html",
                 self_contained = T, bs_theme = "simplex")
### ===========================================

     
## Model Output [Gross Sample] ================
result_Unweighted   <-  parameterEstimates(utaut_WeightedModel, 
                                    standardized = TRUE)[c(23:42),] %>% 
                    mutate(est = format(round(est, 2), nsmall = 2),
                           pvalue = round(pvalue, 3),
                           ci.lower = format(round(ci.lower, 2), nsmall = 2),
                           ci.upper = format(round(ci.upper, 2), nsmall = 2)) %>% 
                    mutate(std.all = format(round(std.all, 2), nsmall = 2)) %>% 
                    mutate(label = paste0("[β=", std.all, ", 95%CI = ", ci.lower, "; ", ci.upper, "]")) %>% 
                    mutate(std.all = as.numeric(std.all),
                           ci.lower = as.numeric(ci.lower),
                           ci.upper = as.numeric(ci.upper)) %>% 
      
                    select(-c(se, z, std.lv, std.nox)) %>% 
                    
                   filter(rhs != "age" &
                            rhs != "gender_male" &
                            rhs != "relationship_yes" &
                            rhs != "education_tertiary" &
                            rhs != "country_ke") %>% 
                   # select(lhs, op, rhs, pvalue) %>% 
                   mutate(Result = ifelse((pvalue < 0.05),
                                          "Supported",
                                          "Not Supported")) %>% 
                   mutate(rhs = derivedFactor("Performance Expectancy" = (rhs == "performance_expect"),
                                              "Social Influence" = (rhs == "social_influence"),
                                              "Effort Expectancy" = (rhs == "effort_expect"),
                                              "Facilitating Condition" = (rhs == "FC02"),
                                              "Attitude" = (rhs == "attitude"),
                                              .default = NA),
                          lhs = derivedFactor("Intention to Interact" = (lhs == "intent_interact"),
                                              "Intention to Use" = (lhs == "intent_use"),
                                              "Effort Expectancy" = (lhs == "effort_expect"),
                                              "Attitude" = (lhs == "attitude"),
                                              .default = NA)) %>% 
                  select(lhs, rhs, pvalue, ci.lower, ci.upper, std.all, label) %>% 
                  mutate (label = replace(label, which (label == "[ß= 0.10, 95%CI =  0.00; 0.19]"),
                                          "[ß= 0.10, 95%CI =  0.001; 0.19]"),
                          label = replace(label, which (label == "[ß= 0.10, 95%CI =  0.00; 0.23]"),
                                          "[ß= 0.10, 95%CI =  0.001; 0.23]"))
    
    
## Model Output [Net Sample - Weighted] ==================
result_Weighted <-  parameterEstimates(utaut_UnweightedModel, 
                                     standardized = TRUE)[c(23:42),] %>% 
                    mutate(est = format(round(est, 2), nsmall = 2),
                           pvalue = round(pvalue, 3),
                           ci.lower = round(ci.lower, 2),
                           ci.upper = round(ci.upper, 2)) %>% 
                    mutate(std.all = format(round(std.all, 2), nsmall = 2)) %>% 
                    mutate(label = paste0("[β=", std.all, ", 95%CI = ", ci.lower, "; ", ci.upper, "]")) %>% 
                    mutate(std.all = as.numeric(std.all)) %>% 
      
                    select(-c(se, z, std.lv, std.nox)) %>% 
                    
                    filter(rhs != "age" &
                             rhs != "gender_male" &
                             rhs != "relationship_yes" &
                             rhs != "education_tertiary" &
                             rhs != "country_ke") %>% 
                    # select(lhs, op, rhs, pvalue) %>% 
                    mutate(Result = ifelse((pvalue < 0.05),
                                           "Supported",
                                           "Not Supported")) %>% 
                    mutate(rhs = derivedFactor("Performance Expectancy" = (rhs == "performance_expect"),
                                               "Social Influence" = (rhs == "social_influence"),
                                               "Effort Expectancy" = (rhs == "effort_expect"),
                                               "Facilitating Condition" = (rhs == "FC02"),
                                               "Attitude" = (rhs == "attitude"),
                                               .default = NA),
                           lhs = derivedFactor("Intention to Interact" = (lhs == "intent_interact"),
                                               "Intention to Use" = (lhs == "intent_use"),
                                               "Effort Expectancy" = (lhs == "effort_expect"),
                                               "Attitude" = (lhs == "attitude"),
                                               .default = NA)) %>% 
                    select(lhs, rhs, pvalue, ci.lower, ci.upper, std.all, label) %>% 
                    mutate (label = replace(label, which (label == "[ß= 0.10, 95%CI = 0; 0.19]"),
                                            "[ß= 0.10, 95%CI = 0.001; 0.19]"),
                            label = replace(label, which (label == "[ß= 0.11, 95%CI = 0; 0.23]"),
                                            "[ß= 0.11, 95%CI = 0.001; 0.23]"))
    
                  
###       Plotting of Regression Model for the Gross Sample    
          result_Unweighted %>% 
          mutate (label = as.factor (label)) %>% 
          mutate (lhs = as.character(result_Unweighted$lhs) %>% as.factor()) %>% 
            
          ggplot(aes(y = rhs, x = std.all,
                     xmin = ci.lower,
                     xmax = ci.upper)) +
          
          geom_errorbar(aes(xmin=ci.lower, xmax=ci.upper,
                            col=factor(lhs,
                                       levels = c("Intention to Use",
                                                  "Intention to Interact"))),
                        width=0.7,cex=1.2, 
                        position=position_dodge2(padding=0.8)) +
          
          geom_vline(xintercept =0, linetype=1) +
            
          geom_point(aes(y = rhs,
                         x = std.all,
                         col = factor(lhs,
                                      levels = c("Intention to Use",
                                                 "Intention to Interact"))),
                     position=position_dodge2(width = 0.7,
                                              padding=0.8),
                     shape=18, size = 3.0) +
          geom_text(aes(y = rhs, label = label),
                    x = 0.6, hjust = 0, vjust = 0.5,
                    family = Text, size = 7,
                    position=position_dodge2(width = 0.8, padding=0.8)) +
          
          scale_color_manual(values = c("#000000", "#CFA602")) +
          
          scale_x_continuous(breaks=seq(-0.8, 1.6, 0.4),
                             limits=c(-0.8, 1.6),
                             labels=seq(-0.8, 1.6, 0.4) %>% round(2)) +
          xlab("Standardized Coefficients (95% Confidence Interval)") +
            
          theme_minimal(base_family = Font,
                        base_size = 28) +
          labs (title = "Unweighted Net Sample  (n = 585)") +
          theme(plot.title=element_text(size=32, 
                                          face="bold", hjust = 0.5),
                plot.margin = margin(0.0,1.0,0.0,0.5, unit = "cm"),
                
                axis.title.y=element_blank(),
                axis.text.y=element_text(size = 17),
                axis.title.x=element_text(size = 18,
                                          margin = margin(25,0,50,0)),
                axis.text.x=element_text(size = 17),
                
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.key = element_rect(size = 5, colour = "white"),
                legend.key.width = unit(1.5,"cm"),
                legend.text = element_text(size = 20,
                                           margin = margin(0,100,0,0)),
    
                panel.grid.major.y = element_line(colour = "#D2D2D2",
                                                  linetype = "dashed",
                                                  size = 0.3),
                panel.grid.major.x = element_line(colour = "#D2D2D2",
                                                  linetype = "dashed",
                                                  size = 0.3),
    
                panel.grid.minor = element_blank(),
## ===================================================  Plot Graph
                panel.background = element_blank()) ->  unweighted_plot
        
    
    
            
###       Plotting of Regression Model for the Gross Sample    
          result_Weighted %>% 
          ggplot(aes(y = rhs, x = std.all,
                     xmin = ci.lower, xmax = ci.upper)) +
            
          geom_point(aes(y = rhs,
                         x = std.all,
                         col = factor(lhs,
                                      levels = c("Intention to Use",
                                                 "Intention to Interact"))),
                     position=position_dodge2(width = 0.7,
                                              padding=0.8),
                     shape=18, size = 3.0) +
            

            geom_errorbar(aes(xmin=ci.lower, xmax=ci.upper,
                              col=factor(lhs,
                                         levels = c("Intention to Use",
                                                    "Intention to Interact"))),
                          width=0.7,cex=1.2, 
                          position=position_dodge2(padding=0.8)) +
            
          geom_vline(xintercept =0, linetype=1) +
          xlab("Standardized Coefficients (95% Confidence Interval)") +
          
          geom_text(aes(y = rhs, label = label),
                    x = 0.6, hjust = 0, vjust = 0.5,
                    family = Text, size = 7,
                    position=position_dodge2(width = 0.8, padding=0.8)) +
          
          scale_color_manual(values = c("#000000", "#CFA602")) +
          
          scale_x_continuous(breaks=seq(-0.8, 1.6, 0.4),
                             limits=c(-0.8, 1.6),
                             labels=seq(-0.8, 1.6, 0.4) %>% round(2)) +
          theme_minimal(base_family = Font,
                        base_size = 28) +
          labs (title = "Weighted Net Sample (n = 585)") +
          theme(plot.title=element_text(size=32, 
                                        face="bold", hjust = 0.5),
                plot.margin = margin(0.0,0.0,0.0,0.0, unit = "cm"),
                
                # axis.text.y=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_text(size = 17),
                
                axis.title.x=element_text(size = 18,
                                          margin = margin(25,0,50,0)),
                axis.text.x=element_text(size = 17),
    
                legend.position = "bottom",
                legend.key.width = unit(5.0,"cm"),
                panel.grid.major.y = element_line(colour = "#D2D2D2",
                                                  linetype = "dashed",
                                                  size = 0.3),
                panel.grid.major.x = element_line(colour = "#D2D2D2",
                                                  linetype = "dashed",
                                                  size = 0.25),
                panel.grid.minor = element_blank(),
## ===================================================  Plot Graph
                panel.background = element_blank()) -> weighted_plot
        
    

    
    
## Combine Plot    
 ggarrange(unweighted_plot, weighted_plot,
           widths = c(1.1,1), align = "h",
           # labels = c("Unweighted Net Sample  (n = 585)",
           #            "Weighted Net Sample (n = 585)"),
           common.legend = TRUE, legend = "bottom")   
     

    ggsave("Output/Fig 4 -- Intention_toInteract.svg", 
           dpi=250, height= 12, width= 20)  
    
    