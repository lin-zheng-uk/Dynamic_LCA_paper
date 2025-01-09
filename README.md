# Dynamic_LCA_paper

This repository is established for the systematic literature selection and analysis conducted for the paper "A Review of Buildings Dynamic Life Cycle Studies by Bibliometric Methods".

## Repository Overview

The repository includes various folders and scripts, each serving a specific purpose in the analysis and manuscript preparation process.

### Folders

#### 1. **Article_selection_Wos_Scopus**
This folder contains all the articles selected and exported from the two databases, WoS (Web of Science) and Scopus. The files are available in multiple formats, including `.bib`, `.csv`, and `.xls`.

#### 2. **Generated_Data_table**
This folder contains the data tables generated during the bibliometric analysis. These tables are used in the manuscript, such as the "Table of the Highly Cited Papers Summary."

#### 3. **Figures**
This folder includes all the figures used in the manuscript. The raw figures are presented in the paper but are not directly included in this repository. However, they can be regenerated easily by running the provided scripts.

#### 4. **Sensitivity_Analysis**
This folder contains all raw data used for sensitivity analysis. The data includes:
- All articles in CSV format.
- Annual growth rate data in CSV format.
- Folder, include all formats of selected articles.

### Scripts

The following scripts are included in the repository:

#### 1. **Bibliometrics_analysis_paper_EB.R**
- This script handles the process of importing the selected articles.
- It uses the `bibliometrix` package to conduct bibliometric analysis.
- The script generates key results such as:
  - Annual growth rate of publications.
  - Total number of publications.
  - Citation analysis.
  - Country collaborations.
  - Highly cited papers.

#### 2. **Data_visualization_EB.R**
- This script provides statistical summaries, such as mean results.
- It focuses on data visualization for figures used in the paper.

#### 3. **Bibliometrics_analysis_paper_EB_SensitivityAnalysis.R**
- This script is dedicated to the sensitivity analysis.
- It processes and visualizes data for sensitivity analysis.

## Usage

1. Clone the repository to your local system.
2. Explore the folders for articles, data tables, figures, and raw sensitivity analysis data.
3. Run the R scripts in the order specified to reproduce the analysis and visualizations. Ensure that all required R packages (e.g., `bibliometrix`) are installed before running the scripts.

## Requirements

- R programming environment.
- Required R packages:
  - `bibliometrix` for bibliometric analysis.
  - Additional packages for data manipulation and visualization (e.g., `ggplot2`).

## Acknowledgments

This repository and the associated work were developed for the paper "A Review of Buildings Dynamic Life Cycle Studies by Bibliometric Methods." The systematic literature selection and bibliometric analysis aim to provide insights into the current state and trends in the field of dynamic life cycle studies of buildings.

---
For any questions or further clarification, don't hesitate to contact me.


