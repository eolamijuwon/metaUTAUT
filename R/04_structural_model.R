################################################################################
#
#' ========================> Sexuality Education in the Digital Age:
#' Modelling the predictors of acceptance and behavioural intention
#' to access and interact with sexuality information on social media.
#
# REGRESSION MODEL
#
################################################################################

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




## Practical Approaches to Dealing with Nonnormal and Categorical Variables
## https://pdfs.semanticscholar.org/94fc/1b1b3dd3159218413c97e65c3017bf5bcaa5.pdf
## ================================
utaut.model   <-      sem(utaut.SEM,
                          data=healthInfo_survey,
                          estimator = "MLMVS",
                          # se = "robust",
                          auto.var = TRUE,
                          fixed.x = FALSE)


                      summary(utaut.model, 
                              fit.measures = TRUE,
                              standardize = TRUE,
                              rsquare = TRUE)


                      fitmeasures(utaut.model,
                                  c('chisq', 'cfi',
                                    'tli','srmr',
                                    'rmsea', 'rmsea.ci.upper',
                                    'rmsea.ci.lower'))
                      
                      (modificationIndices(utaut.model) %>%
                          as_data_frame() %>%
                          arrange(-mi) %>%
                          filter(mi > 10) %>%
                          dplyr::select(lhs, op, rhs, mi, epc) -> mi)

                      

(fit   <-    fitmeasures(utaut.model,
            c('chisq', 'df', 'pvalue', 'rmsea',
              'srmr', 'tli', 'cfi', 'pnfi')) %>% 
            data.frame () %>% 
            rownames_to_column ("Fit statistics") %>%
            rename ("value" = ".") %>% 
            rbind(c("chisq/df", .$value[1]/.$value[2])) %>% 
            rename ("Model value" = "value") %>% 
            mutate (`Model value` = as.numeric(`Model value`, 2) %>% round(2)) %>% 
            mutate (`Recommended Value` = c("", "", ">=0.05",
                                            "<=0.05", "<=0.08", ">=0.95", ">=0.95",
                                            ">=0.60", "<=3.00")) %>% 
            select("Fit statistics", "Recommended Value", "Model value"))

fit   <-  fit[c(1,2,9, 3:8),] %>% 
          mutate(`Fit statistics` = derivedFactor("Chi-Square (X^2)" = (`Fit statistics` == "chisq"),
                                                  "Degree of Freedom (DF)" = (`Fit statistics` == "df"),
                                                  "Chi-Square (X^2)/Degree of Freedom (DF)" = (`Fit statistics` == "chisq/df"),
                                                  "Root Mean Square Error Approximation (RMSEA)" = (`Fit statistics` == "rmsea"),
                                                  "Tucker-Lewis Index (TLI)" = (`Fit statistics` == "tli"),
                                                  "Comparative Fit Index (CFI)" = (`Fit statistics` == "cfi"),
                                                  "Goodness of Fit Index (GFI)" = (`Fit statistics` == "gfi"),
                                                  "Penalized Normed Fit Index (PNFI)" = (`Fit statistics` == "pnfi"),
                                                  "Standardized Root Mean Squared Residual (SRMR)" = (`Fit statistics` == "srmr"),
                                                  "Probability Value (p-value)" = (`Fit statistics` == "pvalue"),
                                                  .default = NA))
fit %>% 
  kable(align = "l", digits=2, 
        booktabs = T, position = "left") %>%
  kable_styling("striped", full_width = F) %>%
  print() %>% 
  save_kable(file = "Output/Table 4 - Model Fit.html",
             self_contained = T, bs_theme = "simplex")


## ------------------------------------------------------------------                      

result      <-        parameterEstimates(utaut.model, 
                                         standardized = TRUE)[c(18:42),] %>% 
                                mutate(est = round(est, 2),
                                       pvalue = round(pvalue, 3),
                                       ci.lower = round(ci.lower, 3),
                                       ci.upper = round(ci.upper, 3)) %>% 
                                mutate(ci = paste0("[", ci.lower, "; ", ci.upper, "]")) %>% 
                                select(-c(se, z, ci.lower, ci.upper)) %>% 
                                mutate(std.all = round(std.all, 2))
                      
                      
                      intent_use  = result %>%  filter(lhs == "intent_use") %>% 
                                                select(rhs, est = std.all, pvalue, ci) %>% 
                                                mutate(Characteristics = derivedFactor("Attitude" = (rhs == "attitude"),
                                                                                       "Effort Expectancy" = (rhs == "effort_expect"),
                                                                                       "Facilitating Condition" = (rhs == "FC02"),
                                                                                       "Performance Expectancy" = (rhs == "performance_expect"),
                                                                                       "Social Influence" = (rhs == "social_influence"),
                                                                                       "Age" = (rhs == "age"),
                                                                                       "Gender = Male" = (rhs == "gender_male"),
                                                                                       "Relationship Status = Yes" = (rhs == "relationship_yes"),
                                                                                       "Education = Tertiary" = (rhs == "education_tertiary"),
                                                                                       "Country = Kenya" = (rhs == "country_ke"),
                                                                                       .default = NA))
                            
                      intent_interact  = result %>%  filter(lhs == "intent_interact") %>% 
                                                select(rhs, est = std.all, pvalue, ci)
                      
                      attitude    = result %>%  filter(lhs == "attitude") %>% 
                                                select(rhs, est = std.all, pvalue, ci)

                      effort      = result %>%  filter(lhs == "effort_expect") %>% 
                                                select(rhs, est = std.all, pvalue, ci)

                      
results_combined <- intent_use %>% 
                    left_join(., intent_interact,
                              by = "rhs") %>% 
                    left_join(., attitude,
                              by = "rhs") %>% 
                    left_join(., effort,
                              by = "rhs") %>% 
                    as.matrix() %>% 
                    replace_na("")

colnames(results_combined)[c(2, 6, 9, 12)] <- "Estimate"
colnames(results_combined)[c(3, 7, 10, 13)] <- "pvalue"
colnames(results_combined)[c(4, 8, 11, 14)] <- "Confidence Interval"


results_combined[,c(5,2:4,6:14)] %>% 
kable(align = "l", digits=2, 
      booktabs = T, position = "left") %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" ", "Intention to Use" = 3, "Intention to Interact" = 3,
                     "Attitude" = 3, "Effort Expectancy" = 3)) %>% 
  print() %>% 
  save_kable(file = "Output/Table 5 - Model Output.html",
             self_contained = T, bs_theme = "simplex")

  




## Summary of Hypothesis --------------
result %>% 
  filter(rhs != "age" &
           rhs != "gender_male" &
           rhs != "relationship_yes" &
           rhs != "education_tertiary" &
           rhs != "country_ke") %>% 
  select(lhs, op, rhs, pvalue) %>% 
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
  mutate(Hypothesis = paste0(lhs, " <= ", rhs)) %>% 
  select(Hypothesis, Result) %>% 
  kable(align = "l", digits=2, 
        booktabs = T, position = "left") %>%
  kable_styling("striped", full_width = F) %>%
  print() %>% 
  save_kable(file = "Output/Table 6 - Hypothesis summary.html",
             self_contained = T, bs_theme = "simplex")


