# Code and Data for "Global Health Injustice From Climate Change Driven By Consumption"

## Overview

This repository provides the data and R code to reproduce the main figures (Figures 1-4) presented in the manuscript, "Global Health Injustice From Climate Change Driven By Consumption".

The code is provided as a single R script (`code.R`) and the data is supplied in the corresponding CSV and Excel files.

## Citation

### Citing the Manuscript

If you use the findings from the paper, please cite the original publication.

*(Placeholder; update upon publication)*

### Citing the Code/Data

If you use the code or data from this repository, please cite it using the Zenodo DOI. This ensures the dataset is archived and citable.

*(A Zenodo DOI placeholder)*

## System Requirements

The figures were generated using the following environment:
*   **R version:** 4.3.2
*   **R packages:**
    *   `tidyverse`: version 2.0.0
    *   `rworldmap`: version 1.3-8
    *   `sf`: version 1.0-19
    *   `ggtext`: version 0.1.2
    *   `patchwork`: version 1.3.0
    *   `ggsci`: version 3.2.0
    *   `ggrepel`: version 0.9.5
    *   `ggpp`: version 0.5.8-1
    *   `janitor`: version 2.2.0

## Repository Contents

*   `code.R`: The R script that generates all four main figures.
*   `README.md`: This file.
*   **Data Files:**
    *   `area_graph_inducer_updated.xlsx`
    *   `area_graph_receptor_updated.xlsx`
    *   `damage_factors_per_100000.csv`
    *   `disparity_map_updated.xlsx`
    *   `figure_country_names.csv`
    *   `income_graph_figure.csv`

## Instructions to Reproduce Figures

Follow these steps to generate the figures on your local machine.

### 1. Download the Repository

Download the contents of this repository as a ZIP file.
*   Click the green **`< > Code`** button on the main GitHub repository page.
*   Select **`Download ZIP`**.
*   Save the file to your computer.

### 2. Unzip the File

Unzip the downloaded file (`climate-health-injustice-main.zip`). This will create a folder containing all the necessary files (the `code.R` script and the data files).

### 3. Run the R Script

1.  **Open R or RStudio.**

2.  **Set your working directory** to the folder you unzipped in the previous step. You can do this by running the following command in the R console, replacing `"path/to/your/folder"` with the actual file path.
    ```r
    setwd("path/to/your/folder")
    ```

3.  **Open the `code.R` script.**

4.  **Install the required packages.** The first part of the script contains the necessary command. Run this section first if you do not have these packages installed.
    ```r
    # Install packages (skip if already installed)
    install.packages(c("tidyverse", "rworldmap", "sf", "ggtext",
                       "patchwork", "ggsci", "ggrepel", "ggpp",
                       "janitor"))
    ```

5.  **Run the entire script.** After the packages are installed, you can run the whole `code.R` script from top to bottom.

### 4. Check the Output

The script will execute and save the four figures as PNG files in your working directory:
*   `main_Figure1.png`
*   `main_Figure2.png`
*   `main_Figure3.png`
*   `main_Figure4.png`

## License

This project is licensed under the MIT License.
