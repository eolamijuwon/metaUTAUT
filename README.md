## **Sexuality Education in the Digital Age: Modelling the predictors of acceptance and behavioural intention to access and interact with sexuality information on social media**

### **Abstract**

**Introduction:** Diverse literature on sexual health promotion using social media suggests that increasing information reach and interaction are crucial for success. This study integrated the unified theory of acceptance and use of technology (UTAUT) to model the predictors of young adults' behavioural intention to use and interact with sexuality education on social media.

**Methods:** A structural equation model was fitted on a sample of 936 young adults in Kenya, Nigeria, and South Africa to identify the demographics and theoretical attributes associated with the intention to use and interact with sexuality information on social media. All the participants had access to the internet and Facebook and were recruited via Facebook's advertising platform between 22 May 2020 – 8 June 2020.

**Results:** About 84% of the young adults who had access to the internet consider social media an appropriate medium for sexual health communication, with Facebook being the most preferred (40%) digital platform for sexual health promotion. Results from the structural equation model showed that performance expectancy *(β =0.18, P \< 0.001)*, social influence *(β =0.09, P = 0.047)*, effort expectancy *(β =0.25, P \< 0.001)*, facilitating condition *(β =0.33, P \< 0.001)*, and attitude *(β =0.10, P = 0.039)* were significantly associated with the intention to access sexuality education on social media. These factors (except attitude) were also significantly associated with the intention to interact with sexual health information on social media.

**Conclusion:** Young people with internet access are amenable to receiving and interacting with sexuality information on social media. The use of social media for sexuality education is associated with whether such use is free of effort, endorsed by society, align with their engagements with other messages, and helps them achieve improvement in their sexual and reproductive health. Policy Implications: Strategies to increase access and interaction with sexuality information on social media help young people make an informed decision about their sexuality. Such use should also be free of effort, align with the way they interact with other information on social media, and supported by the society.

#### **How to replicate the analysis**

-   Download the analysis pack *[see structure below]*

    .
    ├── Data
    │   └── ...
    ├── Output
    │   └── ...
    ├── R
    │   ├── 00_master.R
    │   ├── 01_packages_install.R
    │   ├── 02_exploratory analysis.R
    │   ├── 03_measurement model.R
    │   ├── 04_structural_model.R
    │   └── 05_supplementary_analysis.R
    ├── README.md
    └── metaUTAUT.Rproj

-   Using RStudio open "metaUTAUT.Rproj" file in the main project directory.

-   Run the master file in "R/00_master.R".

    The analysis is split into four parts, which is reflected in the structure of R scripts.

    -   Step 1: Installation of Packages --------------------------

        `source ("R/01_packages_install.R")`

    -   Step 2: Data Wrangling/Exploratory and Analysis -------

        `source ("R/02_data wrangling.R")`

    -   Step 3: Measurement Model -----------------------------

        `source ("R/03_measurement model.R")`

    -   Step 4: Regression Models -------------------------------

        `source ("R/04_structural_model.R")`

    -   Step 5: Supplementary Analysis --------------------------

        `source ("R/05_supplementary_analysis.R")`

-   Outputs from the exploratory analysis, graphs and models are saved in the [output folder](output/).
