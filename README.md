# The international trade network and its determinants: An analysis of the impact of the COVID-19 pandemic from a relational data perspective 

The ZIP file contains the databases and rStudio codes needed to reproduce or replicate the results obtained in the article.
The file consists of two folders, titled: 

## 1). Data: This folder contains four databases: 
- Initial_data_all_countrys: Contains bilateral import and export data for 2018, 2020, and 2022 extracted from WITS.
- P_Data_Extract_From_World_Development_Indicators: Contains data on the nodal attributes used in the study for the years 2018, 2020, and 2022 extracted from the World Bank's Development Indicators database.
- Cualitative_variables: Contains data on the nodal attributes used in the study for the years 2018, 2020, and 2022 extracted from the CEPII database.
- country_codes: Contains the ISO3 codes of countries recognized by the UN.

## 2). Codes: This folder contains the rStudio codes used in the article; to reproduce and replicate the results, run the scripts in the order shown below: 
- Data_statistics_networks_2018_2020_2022: This script performs the data cleaning and manipulation necessary for the subsequent execution of the models and methodologies. Additionally, this script contains the codes used to calculate network statistics and construct graphs.
- ERGM_model_networks_2018_2020_2022: Contains the code used to adjust the ERGM model. Similarly, this script stores the codes used for simulations and the calculation of goodness of fit.
- Stochastic_Blocks_Model_2018_2020_2022: Contains the code used to adjust the stochastic blocks model (SBM).

## Abstract

The globalization processes of the last century have posed a challenge for modern economies, particularly for emerging and undeveloped economies, given that these economies face technical and technological barriers that hinder their production and consumption processes, asymmetries in information that limit their responses to growing demands, and unequal power relations that restrict their influence and bargaining power. Therefore, it is imperative for developing economies to understand the complex web of relationships that currently determines the demand for goods and services in the global market. Thus, the study of the global trade network and its determinants has become an essential mechanism for decision-making and policy-making. With this in mind, the objective of this document is to provide an analysis of the global trade network and its theoretical determinants for the years 2018, 2020, and 2022 through the characterization of the trade network, an Exponential Random Graph Model (ERGM), and a Stochastic Block Model. The results show the existence of a set of non-structural nodal characteristics, persistent over time and statistically significant, which are established as the main determinants of international trade for the years 2018, 2020, and 2022. Additionally, there is evidence of a structural pattern in which countries differ in terms of their degree of integration and relevance in the global trade network. Finally, there are no structural changes resulting from the COVID-19 pandemic.

<p align="center">
  <img width="442" height="460" alt="Captura de pantalla 2025-11-26 160900"
       src="https://github.com/user-attachments/assets/92a28921-4bbb-46ca-9109-8361d8fcdb7f" />
</p>
