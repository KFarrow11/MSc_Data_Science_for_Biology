# MSc Data Science for Biology

## 📊 BIO-7056A: Statistics for Biologists

Applied a wide range of statistical techniques in **R** to explore ecological field data and behavioural outcomes in conservation contexts. Across two summative projects — *Kooki Bird Productivity* and *Black-Tailed Godwit Foraging Behaviour* — I developed and demonstrated advanced proficiency in:

- **Data wrangling & cleaning** using `tidyverse`, `janitor`, and `dplyr` to restructure and prepare long-format ecological data
- Constructing and simplifying **GLMs and linear regression models**, including interaction terms and categorical predictors
- Diagnosing and addressing **non-normality and heteroscedasticity**, and selecting appropriate **non-parametric tests**
- Applying **dimensionality reduction techniques** such as PCA and interpreting multivariate ecological gradients
- Validating models through **AIC, residual analysis, variance inflation factors (VIF)**, and leverage checks
- Translating quantitative results into **evidence-based ecological insights** using GT tables, violin plots, PCA biplots, and smoothed regression curves
- Structuring reproducible workflows grounded in **testable hypotheses and stakeholder-facing communication**

### 🐤 Kooki Bird Summative Analysis

Used a five-question framework to compare chick productivity, habitat preference, and environmental drivers across *wild and captive Kooki birds*.

- **Q1 & Q1b**: Assessed chick productivity using **distributional diagnostics**, log transformation, and **Wilcoxon rank-sum test** to compare breeding origin effects (wild vs. captive). Demonstrated:
  - Expertise in **assumption testing and data transformation**
  - Application of **non-parametric alternatives** when parametric models were unsuitable

- **Q2**: Fitted a **GLM with breeding group × habitat interaction** to explore behavioural preferences. Captured divergent space use between wild and captive birds and showcased:
  - Interpretation of **multi-level interaction terms**
  - Violin plot visualisation to highlight behavioural divergence

- **Q3 & Q4**: Built and simplified **separate GLMs** for wild and captive abundance using rainfall, temperature, and fruit availability. Demonstrated:
  - Selection of **ecologically meaningful predictors**
  - Clear communication of covariate effects with summary tables and diagnostics

- **Q5**: Modelled full-year abundance using **season × breeding group interaction** to reveal seasonal cycles in wild vs. captive activity. Communicated findings through:
  - Grouped bar plots and statistical interpretation of interaction coefficients
  - Integration of **temporal, ecological, and behavioural patterns**

### 🐦 Black-Tailed Godwit Foraging Study

Investigated foraging site selection and intensity in estuarine quadrats through two complementary aims, grounded in both **habitat modelling** and **multivariate analysis**.

#### Aim 1: Habitat Predictors of Foraging Presence

- Fitted a **binomial GLM** across 12 environmental predictors, simplified via likelihood ratio testing and AIC.
- Identified:
  - Positive predictors: **elevation**, **birch cover**, **sedge pool abundance**
  - Negative predictors: **water table depth**, **pool cover**
- Converted log-odds to **predicted probabilities**, stratified by sedge pool count and habitat features.
- Created high-quality **visual diagnostics**, residual plots, and [GT model summaries](../gt_table1.png), demonstrating:
  - End-to-end proficiency in **ecological logistic regression**
  - Translation of statistical estimates into **habitat suitability metrics**

#### Aim 2: Foraging Density and Environmental Gradients

- Applied **PCA** to 11 environmental variables, capturing four axes of ecological structure:
  - **PC1**: Wetness gradient (elevation, water depth, Juncus, sedge pools)
  - **PC2**: Vegetation openness (hayfield, birch)
  - PC3: Willow-dominated wetlands  
  - PC4: Anthropogenic influence
- Regressed foraging density on PCs, finding **PC1** and **PC2** significant. Higher foraging observed in:
  - **Wet, Juncus-rich areas (low PC1)**
  - **Open marshland with minimal woody vegetation (low PC2)**
- Delivered:
  - **Custom PCA biplots**, stratified density plots, and scatterplots
  - A full model validation workflow with assumption checks

This module reflects my capability to:
- Move fluidly between **hypothesis testing**, **predictive modelling**, and **dimensionality reduction**
- Tailor model complexity to data structure and interpretability
- Generate outputs that are simultaneously **ecologically grounded and methodologically transparent**

## 🐍 CMP-7010A: Introduction to Python for Computer Scientists

Used Python and scientific computing libraries to conduct a full-scale analysis of historical (1950–2000) and projected (2050–2100) ocean temperature trends in the North Atlantic using the GFDL-CM4 climate model. Applied structured programming, statistical testing, and geospatial visualisation to explore depth-specific warming, temporal variability, and seasonal signals.

### [Climate Model Analysis – Summative Project](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/blob/main/introduction_to_python/Summative_tasks_100318577.ipynb)

### 🧠 Skills Demonstrated

- Structured programming with reusable functions and annotated Jupyter cells
- Data wrangling and slicing using `xarray` and NumPy arrays across 4D `.nc` files
- Statistical testing using annual mean comparisons (two-sample t-tests with `scipy.stats`)
- Weighting calculations using cosine(latitude) for spatial averaging
- Seasonal filtering and resampling to compare boreal summer vs. winter warming
- Geospatial visualisation using `matplotlib`, `cartopy`, and significance-based hatch overlays
- Scientific communication through panel plots, annotated summaries, and interpretable colour maps

### 🗂 Task Breakdown

#### **Task 1: Annual & Monthly Warming Patterns**
- Computed spatially weighted average temperatures by year/month.
- Compared temperature changes between the historical and future periods at 2.5 m and 2500 m.
- Visualised:
  - Monthly mean temperature differences with ±1 SD shading
  - Decadal warming trends, highlighting seasonal patterns
- Found:
  - Surface warming ~3 °C
  - Deep ocean warming ~0.2 °C but consistent

#### **Task 2: Spatial Distribution of Warming**
- Plotted 2D difference maps using `pcolormesh()` and diverging `RdBu_r` palette.
- Created continent-aware maps with `cartopy`, overlayed land boundaries and axis labels.
- Interpreted spatial patterns and warming hotspots along the Gulf Stream and mid-latitude basins.

#### **Task 3: Seasonal Warming Analysis**
- Isolated DJF (winter) and JJA (summer) periods via datetime filtering.
- Generated seasonal anomaly maps for each depth.
- Found stronger surface warming in summer, with polar amplification near Greenland.

#### **Task 4: Significance Testing of Warming**
- Conducted cell-wise t-tests using annual means (1950–2000 vs. 2050–2100).
- Visualised areas of **statistically significant change** (p < 0.05) via stippling.
- The majority of surface grid cells showed significant warming; deep ocean changes were subtler but consistent.

#### **Task 5: Summarising & Communicating Findings**
- Integrated plots into figure panels with consistent colourmaps, titles, and scale bars.
- Summarised implications for future ocean stratification and climate resilience.
- Framed results using interpretable metrics for stakeholders, linking ocean warming to broader environmental trends.
  

## BIO-7051B: Data Science and Bioinformatics
Using a combination of bash scripting to access HPC and R to run bioinformatics projects

## CMP-7023B: Data Mining
Applied **data mining techniques** in Python using Jupyter Notebooks, with a focus on building interpretable, fair, and evidence-based models.

**Summative highlights:**

- [Preliminary Data Analysis](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/tree/main/data%20mining/Preliminary%20Data%20Analysis):  
  Explored the employability and salary outcomes of 26,000+ data science graduates. Developed key capabilities in:
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

## 📺 CMP-7027B: Information Visualisation

Designed and implemented an interactive, stakeholder-ready **measles surveillance dashboard** using **R**, `quarto`, `plotly`, and `leaflet`. The project translated multi-decade, multi-jurisdictional datasets into actionable visual narratives for outbreak tracking, programme evaluation, and public health communication.

### [Measles Watch Dashboard](https://github.com/KFarrow11/MSc_Data_Science_for_Biology/blob/main/information_visualisation/measles_db_final.qmd)

### 🎯 Purpose

Measles Watch addresses gaps in global surveillance by offering a unified visual platform for analysing historical trends, regional inequalities, and vaccination uptake from 1980 to 2021.

The dashboard was built to support:

- **Situational awareness** across three spatial scales (global, UK, US)
- **Demographic equity monitoring** using age, sex, and vaccine coverage overlays
- **Health communication** through accessible, animated storytelling
- **Rapid decision-making** with interactive summaries and burden-ranking metrics

It was designed as a **scalable, modular system**, capable of adapting to other disease contexts (e.g. influenza, Zika, Ebola) using reproducible code and harmonised data standards.

### 🛠️ Features & Functionality

- **Animated choropleth mapping (1980–2021):**  
  Built global maps of cases and deaths with `plotly::plot_geo()` and animated year sliders. Applied custom palettes, tooltips, and dynamic country labels for interpretability.

- **Continental analytics panels:**  
  Included value boxes for key milestones (e.g. most improved countries, peak burden year), animated bar charts by continent, and narrative summaries that update based on user inputs.

- **Vaccination coverage overlays (MCV1, MCV2):**  
  Merged WHO data into dashboard timelines and layered plots. Created animated scatterplots and diverging bubble maps to explore change over time and surface coverage gaps.

- **Demographic stratification & hesitancy modules:**  
  Linked measles incidence to age/sex groups. Built regional age pyramids and belief summaries using `leaflet`, hover markers, and visual encodings adapted for health literacy.

- **Subnational resolution (UK & USA):**  
  Used ONS topojsons and CDC metadata to create dropdown-selectable regional breakdowns. Added state-level doughnut charts, trend plots, and animated indicators for local surveillance.
  
### 🧠 Skills Demonstrated

- **Geospatial & multiscale visualisation:**  
  Developed hover-enabled choropleths, multi-tab dashboards, and linked time-series components with consistent coordinate mapping.

- **Data integration & harmonisation:**  
  Joined and cleaned datasets across different naming schemas and levels of granularity. Created custom mapping files to resolve country–continent inconsistencies and streamline joins.

- **Interface design & accessibility:**  
  Styled all dashboard elements for clarity using CSS overrides, scalable icons, and high-contrast thematic elements in line with NHS/WHO visual best practices.

- **Narrative communication:**  
  Used value boxes, context-aware highlight cards, and animation pacing to guide users through epidemiological stories. Framed data in actionable language suitable for both technical and public-facing reports.

