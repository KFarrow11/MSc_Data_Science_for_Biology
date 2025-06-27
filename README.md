# MSc_Data_Science_for_Biology

## 📊 BIO-7056A: Statistics for Biologists

Used **R** to apply statistical techniques to ecological field data, focusing on godwit foraging behaviour across estuarine habitats. Analyses were structured around two central research aims:

### [Black-Tailed Godwit Foraging Study](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/tree/main/Stats_for_biologists/black_tailed_godwits)

**Aim 1: Habitat Predictors of Foraging Presence**
- Modelled **godwit occurrence (presence/absence)** using binomial GLMs with 12 candidate predictors (e.g. elevation, birch cover, pool cover, water table depth).
- Conducted **stepwise simplification** using likelihood ratio tests to isolate ecologically meaningful predictors.
- Identified:
  - *Birch cover*, *elevation*, and *number of sedge pools* as strong positive predictors of godwit presence.
  - *Water table depth* and *pool cover* as negative predictors, indicating avoidance of deeply submerged or overly wet areas.
- Converted log-odds outputs into **predicted probabilities**, generating visualisations of probability curves stratified by habitat features.
- Validated model via **VIF scores**, **binned residuals**, and `performance::check_model()` to ensure robust inferences.

**Aim 2: Foraging Density and Environmental Gradients**
- Filtered dataset to include only sites with **confirmed godwit presence** to model foraging intensity (pecks per minute).
- Performed **principal component analysis (PCA)** on 11 environmental variables, producing four interpretable ecological gradients:
  - **PC1 (Wetness gradient)**: from dry, elevated sites to sedge-rich wet habitats.
  - **PC2 (Vegetation openness)**: contrasting birch/hayfield structure with more open marshes.
  - **PC3 (Willow-dominant wetlands)** and **PC4 (Anthropogenic modifications)**.
- Regressed foraging density against PC1–PC4 and found that:
  - Higher densities occurred in **wetter**, Juncus-rich environments (low PC1).
  - Open vegetation (PC2) supported more frequent foraging than densely wooded or managed sites.
- Produced **custom PCA biplots**, **density-tiered scatterplots**, and contribution charts to visualise ecological drivers of godwit behaviour.

**Skills developed across both aims:**
- End-to-end application of **GLMs, PCA, model diagnostics, and data visualisation**
- Ecological interpretation of multivariate patterns and behavioural responses
- Structuring analyses around **testable hypotheses** and grounded environmental theory
- Translating statistical findings into **evidence-based conservation recommendations**
  

## CMP-7010A: Introduction to Python for computer scientists
Using Jupyter Notebooks
- Conduct statistical analysis of ocean temperatures simulated by the GFDL-CM4 model.
  

## BIO-7051B: Data Science and Bioinformatics
Using a combination of bash scripting to access HPC and R to run bioinformatics projects


## CMP-7023B: Data Mining
Applied **data mining techniques** in Python using Jupyter Notebooks, with a focus on building interpretable, fair, and evidence-based models.

**Summative highlights:**

- [Preliminary Data Analysis](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/tree/main/data%20mining/Preliminary%20Data%20Analysis):  
  Explored employability and salary outcomes of 26,000+ data science graduates. Developed key capabilities in:
  - **Data audit & ethical exclusion**: Removed biased or redundant variables (e.g. race, surplus), reinforcing fairness in predictive modelling.
  - **Cleaning & wrangling**: Standardised case formats, handled missing data, and applied IQR filtering for outlier control.
  - **Visual storytelling**: Created KDEs, grouped boxplots, and formatted summary tables to uncover salary-age dynamics and gender disparities.
  - **Documentation**: Built a [data dictionary](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/blob/main/data%20mining/Preliminary%20Data%20Analysis/100318577_sum1_data_dictionary.ipynb) and [salary visuals](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/blob/main/data%20mining/Preliminary%20Data%20Analysis/images2/salary.png) to support interpretability and reproducibility.

- [Advanced Data Analysis](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/tree/main/data%20mining/Advanced%20Data%20Analysis):  
  Delivered a full machine learning workflow using a labelled insurance dataset (5,521 × 83), including:
  - **Feature engineering**: Categorised and encoded behavioural, demographic, and economic variables. Designed *Purchasing Power* as a meta-variable to simplify model reasoning.
  - **Unsupervised learning**: Applied PCA-driven hierarchical clustering and DBSCAN to reveal 5 behavioural customer segments. Silhouette scoring supported model evaluation.
  - **Supervised learning**: Trained and evaluated decision trees, XGBoost, and random forests. Best performance: **94% test accuracy**, **AUC 0.9939** (Random Forest).
  - **Transparent modelling**: Prioritised explainability using Gini feature importance and visualised classifier trees. Business insights were translated into actionable customer strategies.
  - Delivered a comprehensive [report](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/blob/main/data%20mining/Advanced%20Data%20Analysis/PDFs/100318577_Advanced_data_analysis_on_insurance_customer_types.pdf) and supporting [data dictionary](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/blob/main/data%20mining/Advanced%20Data%20Analysis/Python_scripts_and_data/1_data_dictionary.ipynb).

**Key skills developed:**
- Advanced proficiency in **data cleaning, encoding, and imputation**
- Application of both **unsupervised and supervised ML algorithms**
- Deepened understanding of **model evaluation metrics and fairness trade-offs**
- Commitment to **transparency, interpretability, and stakeholder-ready communication**


## CMP-7022B: Information Visualisation
Using a combination of JavaScript and R Quarto to make a series of interactive dashboards to illustrate a series of datasets in a visual format. 
- JavaScript was used in IT workshops
- Summative work was made using R Quarto 

