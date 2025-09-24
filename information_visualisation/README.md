# Measles Watch Dashboard

## Overview

**Measles Watch** is an epidemiological surveillance dashboard designed specifically for public health professionals who need to track, monitor, and respond to measles outbreaks locally, nationally, and globally. This interactive dashboard provides real-time insights into measles case distributions, vaccination coverage patterns, and outbreak dynamics to support evidence-based public health decision-making.

### Target Audience

This dashboard is designed for: - **Public Health Officials** tracking disease surveillance - **Epidemiologists** analyzing outbreak patterns - **Vaccination Program Managers** monitoring coverage gaps - **Healthcare Administrators** planning outbreak responses - **Researchers** studying infectious disease trends

## Key Features

### 🌍 Global Surveillance

-   **Interactive world maps** showing measles cases and deaths (1980-2021)
-   **Continental breakdowns** with time-series analysis
-   **Vaccination coverage tracking** (MCV1/MCV2) by country
-   **Real-time case monitoring** across 195+ countries

### United Kingdom Focus

-   **Regional case distribution** across England and Wales (2012-2024)
-   **Age-stratified analysis** revealing vulnerable populations
-   **Vaccination coverage trends** showing the impact of vaccine hesitancy
-   **Parental attitude tracking** toward MMR vaccination

### United States Analysis

-   **State-level outbreak mapping** with current hotspots
-   **Historical trend analysis** (1985-2025)
-   **Vaccination status breakdown** of confirmed cases
-   **Public perception monitoring** of vaccine-autism beliefs

### 📊 Advanced Analytics

-   **Animated visualisations** showing disease progression over time
-   **Interactive filtering** by region, age group, and time period
-   **Outbreak prediction indicators** based on vaccination coverage gaps
-   **Risk assessment tools** for identifying vulnerable populations

## Dashboard Structure

### Navigation Tabs

1.  **Global** - Worldwide measles surveillance and vaccination data
2.  **UK** - Detailed United Kingdom regional and demographic analysis
3.  **USA** - United States state-level tracking and trend analysis
4.  **About Measles** - Comprehensive disease information and public health guidance
5.  **Data Sources** - Methodology and data quality documentation

### Key Visualisations

#### 📈 Time Series Analysis

-   Animated line and bar charts showing case trends
-   Continental comparison of outbreak patterns
-   Vaccination coverage progression over time

#### 🗺️ Geographic Mapping

-   Choropleth maps with colour-coded case densities
-   Interactive zoom and pan functionality
-   Country/state-level detail on hover

#### 📊 Demographic Breakdowns

-   Age-stratified case distributions
-   Vaccination status of confirmed cases
-   Gender and socioeconomic analysis where available

#### 🎯 Risk Assessment

-   Coverage gap identification
-   Herd immunity threshold monitoring
-   Outbreak probability indicators

## Technical Implementation

### Built With

-   **R/Quarto** - Dashboard framework and statistical computing
-   **Plotly** - Interactive visualizations
-   **Leaflet** - Geographic mapping
-   **DT** - Interactive data tables
-   **Bootstrap** - Responsive design framework

### Data Sources

-   **WHO/UNICEF** - Global vaccination estimates
-   **CDC** - US National Notifiable Diseases Surveillance System
-   **UKHSA** - UK Health Security Agency surveillance
-   **Our World in Data** - Comprehensive global tracking
-   **Global Burden of Disease Study** - Mortality estimates
-   **Statista** - Statistical data compilation and analysis
-   **ONS Visual** - UK geographic data

### Performance Features

-   **Responsive design** for desktop and mobile access
-   **Fast loading** with optimized data processing
-   **Real-time updates** when new surveillance data is available
-   **Offline capability** for field work scenarios

## Use Cases

### 🚨 Outbreak Response

-   **Rapid case identification** and mapping
-   **Contact tracing support** with geographic clustering
-   **Resource allocation** based on case density
-   **Communication planning** for affected communities

### 📋 Surveillance Monitoring

-   **Weekly/monthly reporting** with automated insights
-   **Coverage gap identification** for targeted interventions
-   **Trend analysis** for early outbreak detection
-   **Performance metrics** for vaccination programs

### 🎯 Program Planning

-   **Vaccination campaign targeting** of high-risk areas
-   **Resource needs assessment** based on population vulnerability
-   **Policy impact evaluation** of intervention strategies
-   **Budget planning** with cost-effectiveness analysis

### 📚 Training and Education

-   **Public health training** with real outbreak scenarios
-   **Medical education** on disease patterns and prevention
-   **Community engagement** with visual storytelling
-   **Policy briefings** with executive-level summaries

## Adaptability for Other Diseases

### 🦠 Framework Flexibility

This dashboard framework is designed to be **easily adaptable** for monitoring other infectious diseases, including:

#### Respiratory Diseases

-   **Influenza** - Seasonal and pandemic strain tracking
-   **COVID-19** - Variant surveillance and vaccination monitoring
-   **RSV** - Pediatric outbreak management
-   **Pertussis** - Whooping cough surveillance

#### Vaccine-Preventable Diseases

-   **Polio** - Eradication program monitoring
-   **Rubella** - Congenital rubella syndrome prevention
-   **Mumps** - University outbreak tracking
-   **Varicella** - Chickenpox surveillance

#### Emerging Threats

-   **Mpox** - Contact tracing and vaccination
-   **H5N1** - Avian influenza preparedness
-   **Disease X** - Pandemic preparedness template

### 🔄 Adaptation Process

To adapt this dashboard for other diseases:

1.  **Data Structure Mapping** - Align disease-specific variables
2.  **Visualization Customization** - Adjust charts for disease characteristics
3.  **Epidemiological Parameters** - Update transmission and immunity metrics
4.  **Reference Materials** - Replace disease-specific guidance content
5.  **Color Schemes** - Apply disease-appropriate visual branding

## Installation and Setup

### Prerequisites

``` r
# Required R packages
install.packages(c(
  "quarto", "tidyverse", "plotly", "DT", "leaflet", "sf", "readxl", "RColorBrewer"
))
```

### References

1.  **R** R Core Team (2024). *R: A Language and Environment for Statistical Computing*. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org\
2.  **RStudio (version 2025.04.1)** Posit team (2025). *RStudio: Integrated Development Environment for R*. Posit Software, PBC, Boston, MA. https://posit.co\
3.  **Quarto (version 1.7.31)** Posit Software, PBC (2025). *Quarto: Scientific and Technical Publishing System*. Version 1.7.31. https://quarto.org\
4.  **tidyverse** Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., Grolemund, G., et al. (2019). *Welcome to the tidyverse*. *Journal of Open Source Software*, 4(43), 1686. https://doi.org/10.21105/joss.01686\
5.  **plotly** Sievert, C. (2020). *Interactive Web-Based Data Visualization with R, plotly, and shiny*. Chapman and Hall/CRC. https://plotly-r.com\
6.  **readr** Wickham, H., Hester, J., & Bryan, J. (2024). *readr: Read Rectangular Text Data* (R package version 2.1.5). https://readr.tidyverse.org\
7.  **readxl** Wickham, H., & Bryan, J. (2025). *readxl: Read Excel Files* (R package version 1.4.5). https://readxl.tidyverse.org\
8.  **DT** Xie, Y., Cheng, J., & Tan, X. (2025). *DT: A Wrapper of the JavaScript Library 'DataTables'* (R package version 0.33.3). https://github.com/rstudio/DT\
9.  **maps** Becker, R. A., Wilks, A. R., Brownrigg, R., Minka, T. P., & Deckmyn, A. (2023). *maps: Draw Geographical Maps* (R package version 3.4.1). https://CRAN.R-project.org/package=maps\
10. **sf** Pebesma, E. (2018). *Simple Features for R: Standardized Support for Spatial Vector Data*. *The R Journal*, 10(1), 439–446. https://doi.org/10.32614/RJ-2018-009\
11. **htmlwidgets** Vaidyanathan, R., Xie, Y., Allaire, J., Cheng, J., Sievert, C., & Russell, K. (2023). *htmlwidgets: HTML Widgets for R* (R package version 1.6.4). https://CRAN.R-project.org/package=htmlwidgets\
12. **leaflet** Cheng, J., Karambelkar, B., & Xie, Y. (2023). *leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library* (R package version 2.2.1). https://CRAN.R-project.org/package=leaflet\
13. **RColorBrewer** Neuwirth, E. (2022). *RColorBrewer: ColorBrewer Palettes* (R package version 1.1-3). https://CRAN.R-project.org/package=RColorBrewer\
14. **crosstalk** Cheng, J., & Sievert, C. (2023). *crosstalk: Inter-Widget Interactivity for HTML Widgets* (R package version 1.2.1). https://CRAN.R-project.org/package=crosstalk

### Quick Start

``` bash
# Clone the repository
git clone https://github.com/KFarrow11/100318577_Measles_Watch_Dashboard

# Navigate to project directory
cd 100318577_Measles_Watch_Dashboard

# Render the dashboard
quarto render measles_db_final.qmd
```

### Data Update Process

1.  **Download** the latest surveillance data from partner organisations
2.  **Process** data files using provided R scripts
3.  **Validate** data quality and completeness
4.  **Re-render** dashboard with updated visualizations

## Data Quality and Limitations

### 📊 Data Reliability

-   **Primary sources** from national surveillance systems
-   **International validation** through WHO/UNICEF processes
-   **Quality indicators** displayed with each dataset
-   **Update frequency** varies by data source (weekly to annual)

### ⚠️ Known Limitations

-   **Underreporting** in regions with limited surveillance capacity
-   **Reporting delays** affecting real-time analysis
-   **Denominator uncertainties** in vaccination coverage estimates
-   **Diagnostic variations** across healthcare systems

### 🔍 Quality Assurance

-   **Cross-validation** between multiple data sources
-   **Outlier detection** and investigation protocols
-   **Missing data handling** with transparent methodologies
-   **Regular audits** of data processing pipelines

## Contributing

### 🤝 Collaboration Welcome

We encourage contributions from the global public health community:

-   **Data Partners** - Share surveillance data from your jurisdiction
-   **Technical Contributors** - Improve visualizations and analytics
-   **Subject Matter Experts** - Enhance epidemiological content
-   **User Feedback** - Report bugs and suggest improvements

### 📝 Contribution Guidelines

1.  **Fork** the repository
2.  **Create** a feature branch
3.  **Document** changes thoroughly
4.  **Test** with sample data
5.  **Submit** a pull request with a detailed description

## Comprehensive Data Sources and Citations

### 🌍 Global Data Sources

#### Global Cases and Deaths

-   **File**: `global_cases_deaths.csv`
-   **Source**: Global Burden of Disease Study 2021 (GBD 2021) Results
-   **Institution**: Institute for Health Metrics and Evaluation (IHME), Seattle, United States
-   **Year**: 2022
-   **Citation**: Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2021 (GBD 2021) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022. Available from https://vizhub.healthdata.org/gbd-results/

#### Global Cases and Deaths by Demographics

-   **File**: `global_cases_sex_age.csv`
-   **Source**: Global Burden of Disease Study 2021 (GBD 2021) Results
-   **Institution**: Institute for Health Metrics and Evaluation (IHME), Seattle, United States
-   **Year**: 2022
-   **Citation**: Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2021 (GBD 2021) Results. Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022. Available from https://vizhub.healthdata.org/gbd-results/

#### Global Vaccination Coverage - First Dose

-   **File**: `1dose_share-of-children-vaccinated-against-measles.csv`
-   **Source**: [Share of one-year-olds vaccinated against measles, 2023](https://ourworldindata.org/grapher/share-of-children-vaccinated-against-measles)
-   **Institution**: Our World in Data
-   **Coverage**: Global vaccination rates for measles first dose (MCV1)

#### Global Vaccination Coverage - Second Dose

-   **File**: `2_doses_share-of-children-vaccinated-with-mcv.csv`
-   **Source**: [Share of children fully vaccinated against measles, 2023](https://ourworldindata.org/grapher/share-of-children-vaccinated-with-mcv2)
-   **Institution**: Our World in Data
-   **Coverage**: Global vaccination rates for the second dose (MCV2)

### 🇬🇧 United Kingdom Data Sources

#### Geographic Data

-   **Source**: UK Regions Topojson Data
-   **URL**: https://onsvisual.github.io/uk-topojson/
-   **Institution**: ONS Visual (Office for National Statistics)
-   **Usage**: Interactive mapping of UK regions for case distribution

#### UK Cases by Region and Age

-   **File**: `cases_region_2012_2024.xlsx`
-   **Source**: [Confirmed cases of measles in England and Wales by region and age 2012-2014](https://www.gov.uk/government/publications/measles-confirmed-cases/confirmed-cases-of-measles-in-england-and-wales-by-region-and-age-2012-to-2014)
-   **Institution**: UK Government / UK Health Security Agency (UKHSA)
-   **Coverage**: Regional breakdown of confirmed measles cases from 2012-2024
-   **Demographics**: Age-stratified data for epidemiological analysis

#### UK Vaccination Coverage - Primary Immunisation

-   **File**: `measles_vac_primary_england_2003_2024.xlsx`
-   **Source**: MMR primary immunisation in England 2024
-   **Institution**: Statista (compiled from NHS England data)
-   **Coverage**: Primary MMR vaccination rates across England (2003-2024)
-   **Purpose**: Tracking first dose vaccination coverage trends

#### UK Vaccination Coverage - Complete Immunisation

-   **File**: `measles_vac_complete_england_2003_2024.xlsx`
-   **Source**: MMR immunization England 2024
-   **Institution**: Statista (compiled from NHS England data)
-   **Coverage**: Complete MMR vaccination rates across England (2003-2024)
-   **Purpose**: Monitoring full vaccination series completion

#### UK Parental Attitudes Survey

-   **File**: `attitudes-of-parents-towards-vaccines-in-england-in-2022.xlsx`
-   **Source**: Attitudes of parents towards vaccines in England 2022
-   **Institution**: Statista (survey data)
-   **Coverage**: Public perception and attitudes toward MMR vaccination
-   **Purpose**: Understanding vaccine hesitancy patterns

### 🇺🇸 United States Data Sources

#### State Codes Reference

-   **Source**: US State Abbreviations
-   **URL**: https://www.50states.com/abbreviations.htm
-   **Usage**: Standardised state coding for mapping and data processing

#### US Historical Cases Trend

-   **File**: `new-cases-of-measles-in-the-us-1985-2025.xlsx`
-   **Source**: [New cases of measles in the US since 1950](https://www.statista.com/statistics/186678/new-cases-of-measles-in-the-us-since-1950/)
-   **Institution**: Statista (compiled from CDC data)
-   **Coverage**: Long-term trend analysis from 1985-2025
-   **Purpose**: Historical context and outbreak pattern analysis

#### US Cases by Age Group (2024)

-   **File**: `cases_by_age_2024.xlsx`
-   **Source**: [Number of measles cases by age U.S. 2020-2024](https://www.statista.com/statistics/1469710/number-measles-cases-in-the-us-by-age/)
-   **Institution**: Statista (CDC surveillance data)
-   **Coverage**: Age-stratified case distribution for 2024
-   **Purpose**: Identifying vulnerable age groups

#### US Cases by Age Group (2025)

-   **File**: `cases_by_age_2025.xlsx`
-   **Source**: [Number of measles cases by age U.S. 2024-2025](https://www.statista.com/statistics/1560807/number-measles-cases-by-age/)
-   **Institution**: Statista (CDC surveillance data)
-   **Coverage**: Current age-stratified case distribution
-   **Purpose**: Real-time epidemiological monitoring

#### US Cases by State (2024)

-   **File**: `cases_state_2024.xlsx`
-   **Source**: Number of measles by state U.S. 2024
-   **Institution**: Statista (CDC surveillance data)
-   **Coverage**: State-level case distribution for 2024
-   **Purpose**: Geographic outbreak mapping

#### US Cases by State (2025)

-   **File**: `cases_state_2025.xlsx`
-   **Source**: Number of measles by state U.S. 2025
-   **Institution**: Statista (CDC surveillance data)
-   **Coverage**: Current state-level case distribution
-   **Purpose**: Real-time outbreak monitoring

#### US Vaccination Status Analysis

-   **Files**:
    -   `vac_2024.xlsx` - Number of measles cases by vaccination status U.S. 2020-2024
    -   `vac_2025.xlsx` - Distribution of measles cases by vaccination status U.S. 2024-2025
    -   `vac_20_25.xlsx` - Combined vaccination status data 2020-2025
-   **Institution**: Statista (CDC surveillance data)
-   **Coverage**: Vaccination status of confirmed measles cases
-   **Purpose**: Vaccine effectiveness monitoring and breakthrough case analysis

#### US Public Perception Survey

-   **File**: `us-adults-who-believed-vaccines-cause-autism-2015-2024.xlsx`
-   **Source**: Opinions on whether vaccines cause autism U.S. 2024
-   **Institution**: Statista (survey research)
-   **Coverage**: Public beliefs about MMR-autism connection (2015-2024)
-   **Purpose**: Understanding vaccine hesitancy drivers

### 📊 Data Integration and Quality Control

#### Cross-Validation Methods

-   **Multi-source verification** between WHO, CDC, and national surveillance systems
-   **Temporal consistency checks** across historical datasets
-   **Geographic boundary validation** for mapping accuracy
-   **Demographic category standardization** across different data sources

#### Update Frequencies

-   **Global data**: Annual updates from WHO/UNICEF and GBD study
-   **US surveillance**: Weekly updates during outbreak periods, monthly otherwise
-   **UK surveillance**: Weekly updates from UKHSA
-   **Survey data**: Annual or bi-annual updates from Statista compilations

#### Quality Indicators

-   **Completeness scores** for each dataset
-   **Timeliness metrics** showing data recency
-   **Accuracy assessments** based on source reliability
-   **Coverage gaps** identified and documented

## License and Citation

### 📄 Open Source License

This dashboard is released under the **MIT License**, encouraging: - Free use for public health purposes - Modification and adaptation for local needs - Distribution with proper attribution - Commercial use with acknowledgement

### 📚 Citation

If using this dashboard for research or official reports:

```         
Measles Watch Dashboard (2024). Epidemiological surveillance 
for measles outbreak monitoring and vaccination coverage analysis. 
Available at: https://github.com/KFarrow11/100318577_Measles_Watch_Dashboard/README.md
```

### 📚 Data Citation Requirements

When using specific datasets from this dashboard, please cite the sources as listed above. For academic publications, include the dashboard and relevant primary data source citations.

