# PPI_Visualization

## Introduction

Interactive app for visualizing cofactor/transcription factor protein-protein interactions.

## Dependencies

NOTE: The default versions of the required packages available on the BU SCC should be correct for all packages except shiny. See below for instructions on updating shiny.

This app was built in R 3.6.0 using the following packages:

|    library   | version |
|--------------|---------|
|     plyr     |  1.8.4  |
|   tidyverse  |  1.2.1  |
|   ggplot2*   |  3.3.0  |
|    purrr*    |  0.3.2  |
|    dplyr*    |  0.8.5  |
|    tidyr*    |  0.8.3  |
|   stringr*   |  1.4.0  |
|    igraph    | 1.2.4.1 |
|    ggraph    |  1.0.2  |
|     shiny    |  1.5.0  |
|    shinyjs   |   1.0   |
| colourpicker |   1.0   |
|      DT      |   0.6   |

Packages marked with an asterisk are attached along with the tidyverse package.

To update to the most recent version of shiny, run the following commands to start an R session on the SCC:

```
module load R/3.6.0
R
```

Once in the R session, run the following commands to update and check your shiny version:

```
install.packages("shiny")
library("shiny")
sessionInfo()
```

The output of the `sessionInfo()` command will include a list of all attached packages. Check for "shiny_1.5.0" in the section "other attached packages." The screenshot below shows what the output of the `sessionInfo()` command should look like after all the required packages are attached when launching the app.

![output of sessionInfo() command after running app](screenshots/session_info.png)

## App Overview

The app can be launched from RStudio by clicking the "Run App" button at the top of the source file pane when "app.R" is open. It can also be launched by using the command `runApp("path/to/app.R")`.

### Selecting and Filtering a Dataset

### Plotting Heatmaps

### Plotting Networks

