# Auxiliar files

Auxiliar files for the manuscript "A statistical model for forecasting 
probabilistic epidemic bands for dengue cases in Brazil" by Freitas et al. 

## Getting data

In [0 getting_data.r](code/0_getting_data.r) we organize the crude data for probable 
cases from [DATASUS](https://datasus.saude.gov.br/transferencia-de-arquivos/) and 
suspected cases downloaded from [InfoDengue](https://info.dengue.mat.br/) project 
using a [Mosqlimate](https://mosqlimate.org/) API.

The file containing the cases probable and suspected dengue cases by municipality [cases.csv.gz](data/cases.csv.gz) 
with the following variables:

* data_iniSE - Start of the week (Sunday) for the symptoms onset.
* municipio_geocodigo - IBGE 7-digit code for the municipality
* ID_MN_RESI - IBGE 6-digit code for the municipality
* casos - Suspected dengue cases provided by the InfoDengue system, all reported cases.
* casos_prov - Probable cases from SINAN open data, all reported cases excluding descarted cases. 

In [spatial.tbl.csv](data/spatial.tbl.csv) there is a correspondence table for health districts (n=118), health regions (n=450) and all IBGE 7-digit code municipalities (n=5570).

## Running models

In [1_running_models.r](code/1_running_models.r) we fit INLA model to each health district then predict for the next season. This is done for seasons 2022-2023, 2023-2024 and 2024-2025. 
We then save the Monte Carlo samples for the predictions for probable and suspected cases for each season, output is available at [samples](samples/)

## Tables and plots

In [2_replication_analysis.r](code/2_replication_analysis.r) we provide the code to replicate the tables and figures of the manuscript.