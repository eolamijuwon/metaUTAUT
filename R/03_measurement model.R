
################################################################################
#
#' ========================> Sexuality Education in the Digital Age:
#' Modelling the predictors of acceptance and behavioural intention
#' to access and interact with sexuality information on social media.
#
# MEASUREMENT MODEL
#
################################################################################


## SPECIFY ALL MODELS --------

## Summary Statistics for Measurement
measurement <- healthInfo_survey %>% 
                select(PE01:BII03) %>% 
                gather("Item", "value") %>% 
                group_by(Item) %>% 
                summarise(Mean=mean(value) %>% round(2), 
                          SD=sd(value) %>% round(2), 
                          min=min(value) %>% round(2), 
                          max=max(value) %>% round(2), 
                          '% Missing'=100*length(which(is.na(value)))/n()) %>%
                  left_join(questions, by = c(Item = "item")) %>% select(-c(var.name)) %>% 
                  select(Item, question, Mean, SD, min, max)

                  ## Re-ordering Measures
                  measurement <- measurement[c(12:14,
                                               1:3,
                                               9:10,
                                               15:18,
                                               11, 7:8,
                                               4:6),]


## Summary Statistics for Factor ---
factor_summary  <-    healthInfo_survey %>% 
                      select(PE01:BII03) %>% 
                      pivot_longer(cols = PE01:BII03,
                                   names_to = "item",
                                   values_to = "values") %>% 
                      mutate(construct = derivedFactor("attitude" = (item == "AT01" | 
                                                                       item == "AX01" | 
                                                                       item == "AT04"),
                                                       "effort_expect" = (item == "EE01" | 
                                                                            item == "EE02"),
                                                       "performance_expect" = (item == "PE01" | 
                                                                                 item == "PE02" | 
                                                                                 item == "PE03"),
                                                       "social_influence" = (item == "SI01" |
                                                                            item == "SI02" | 
                                                                            item == "SI03" | 
                                                                            item == "SI04"),
                                                       "intent_interact" = (item == "BII01" | 
                                                                                  item == "BII02" | 
                                                                                  item == "BII03"),
                                                       "intent_use" = (item == "BIU01" | 
                                                                                  item == "BIU02"),
                                                       .default = NA)) %>% 
                      filter(!is.na(construct)) %>% 
                      group_by(construct) %>% 
                      summarise(mean = mean(values),
                                sd = sd(values),
                                kt = kurtosi(values),
                                sk = skew(values))
                  

                      factor_summary[2:5] <-  factor_summary[2:5] %>%
                                              round(3)
                  
                  
                  
## Confirmatory Factor Analysis ---
utaut.SEM <- '
              # ## Latent Variable
              attitude =~ AT01 + AT02 + AT03
              effort_expect =~ EE01 + EE02
              performance_expect =~ PE01 + PE02 + PE03
              social_influence =~ SI01 + SI02 + SI03 + SI04
              intent_use =~ BIU01 + BIU02
              intent_interact =~ BII01 + BII02 + BII03

              ## Co-Variances 
              
              SI03 ~~ SI04
              SI01 ~~ SI02
              BIU02 ~~ BII01
              AT01 ~~ AT03

              '


utaut.model <-  lavaan::cfa(utaut.SEM, 
                            data=healthInfo_survey,
                            estimator = "MLMVS",
                            std.lv=T)

                ## Diagnostics - Model Fit
                summary(utaut.model, 
                        fit.measures = TRUE,
                        standardize = TRUE,
                        rsquare = TRUE)
                
                
                fitmeasures(utaut.model,
                            c('chisq', 'df', 'pvalue', 'rmsea',
                              'srmr', 'tli', 'cfi', 'pnfi'))

                modificationIndices(utaut.model) %>%
                  data.frame() %>%
                  arrange(-mi) %>%
                  filter(mi > 10) %>%
                  dplyr::select(lhs, op, rhs, mi, epc) 
                
## ----------------------------------

                
                
alpha <-   inspect(utaut.model, what="std")$lambda %>% 
           data.frame() %>% 
           rownames_to_column("item") %>% 
           # mutate(Total = select(., )) 
           mutate(lambda = rowSums(select(., attitude:intent_interact)) %>% 
                           round(2)) %>% 
           select(item, 
                  "Standard Loading" = lambda) %>% 
           print()

            rsquare <-  inspect(utaut.model, what="rsquare") %>%
                        data.frame() %>% rownames_to_column() %>% 
                        rename("R-squared" = ".",
                               "Item" = "rowname") %>% 
                        mutate(`R-squared` = round(`R-squared`, 2))
            ## Output 
            measurement %>% 
            left_join(., alpha,
                      by = c(Item = "item")) %>% 
            left_join(., rsquare,
                      by = "Item") %>% 
            select(Item, Question = question,
                   "Standard Loading",
                   "R-squared", Mean,
                   SD, Min = min,
                   Max = max) %>% 
            kable(align = "l", digits=2, 
                booktabs = T, position = "left") %>%
            kable_styling("striped", full_width = F) %>%
            kableExtra::group_rows("Performance Expectancy", 1, 3) %>%
            kableExtra::group_rows("Attitude", 4, 6) %>%
            kableExtra::group_rows("Effort Expectancy", 7, 8) %>%
            kableExtra::group_rows("Social Influence", 9, 12) %>%
            kableExtra::group_rows("Facilitating Condition", 13, 13) %>%
            kableExtra::group_rows("Behavioural Intention to Use", 14, 15) %>%
            kableExtra::group_rows("Behavioural Intention to Interact", 16, 18) %>%
            print() %>% 
            save_kable(file = "Output/Table 2 - Descriptive Measurement.html", self_contained = T, bs_theme = "simplex")
          

                # parameterEstimates(utaut.model, 
                #                    standardized = TRUE)
                

# Convergent validity (AVE > 0.5) - 
# Discriminant Validity (omega > .7)            
# specific construct should converge 
# or share a high proportion of variance in common
## See here https://researchhub.org/common-issues-in-structural-equation-modelling-sem-and-their-solutions/
## -------------------------------
(fit <- semTools::reliability(utaut.model))
                
reliability  <-   as.data.frame(t(fit)) %>%
                  select(alpha, omega, avevar) %>% 
                  rownames_to_column() %>% 
                  rename(factor = rowname) %>% 
                  mutate(ave_sqrt = sqrt(avevar) %>% round(3),
                         alpha = round(alpha, 2),
                         omega  = round(omega, 2),
                         avevar = round(avevar, 2)) %>% 
                  filter(factor != "total") %>% 
                  print()


        

corr_test =   inspect(utaut.model, what="std")$psi %>%
              as.data.frame() %>% round(3) %>% print

              diag(corr_test) = reliability$ave_sqrt 
              corr_test

disc_test <-  cbind(corr_test, ave_sqrt = reliability$ave_sqrt) %>% 
              mutate(discriminant = ifelse((ave_sqrt >= attitude &
                                 ave_sqrt >= effort_expect &
                                 ave_sqrt >= performance_expect &
                                 ave_sqrt >= social_influence &
                                 ave_sqrt >= intent_use &
                                 ave_sqrt >= intent_interact),
                               "Established",
                               "Not Established"))

corr_test[upper.tri(corr_test)] <- ""    


(dt <- cbind(reliability[2:4], corr_test, disc_test= disc_test[,8])) %>%
        rownames_to_column("construct") %>% 
        rename(Constructs = construct,
               `Cronbach Alpha` = alpha,
               `Composite Reliability (CR)` = omega,
               AVE = avevar,
               Attitude = attitude,
               `Effort Expectancy` = effort_expect,
               `Performance Expectancy` = performance_expect,
               `Social Influence` = social_influence,
               `Intention to Interact` = intent_interact,
               `Intention to Use` = intent_use,
               `Discriminant Validity` = disc_test) %>%
        mutate(Constructs = derivedFactor(Attitude = (Constructs == "attitude"),
                                          `Effort Expectancy` = (Constructs == "effort_expect"),
                                          `Performance Expectancy` = (Constructs == "performance_expect"),
                                          `Social Influence` = (Constructs == "social_influence"),
                                          `Intention to Interact` = (Constructs == "intent_interact"),
                                          `Intention to Use` = (Constructs == "intent_use"),
                                          .default = NA),
               `Convergent Validity` = "Established") %>%
        kable(align = "l", booktabs = T, position = "left") %>%
        kable_styling("striped", full_width = F) %>%
        print() %>% 
        save_kable(file = "Output/Table 3 - Validity Correlations.html",
                   self_contained = T, bs_theme = "simplex")

