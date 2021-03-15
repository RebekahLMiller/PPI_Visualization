# PPI_Visualization

## Introduction

Interactive app for visualizing cofactor/transcription factor protein-protein interactions.

## Running RStudio on the SCC

The simplest way to access RStudio on the SCC is through [SCC OnDemand](https://scc-ondemand2.bu.edu/pun/sys/dashboard/). After logging in with your BU credentials, click on "Interactive Apps" at the top of the screen and then select "RStudio Server" from the list. This will take you to a page where you can set up your RStudio session. I recommend selecting R version 3.6.0 since that is the version the app was built with, but later versions of R should work as well. The default options for the other options should be fine. When you have finished configuring the options, hit the "Launch" button at the bottom of the page to submit your job to the queue. This will take you to a page showing all of your active jobs. Within a few minutes, your RStudio Server job should start and a "Connect to RStudio Server" button will appear. Click that button and it will open your RStudio Server session in a new tab.

Another option for running RStudio on the SCC is to open a terminal window and type the command `ssh username@scc1.bu.edu -L XXXX:localhost:8787`, replacing `username` with your BU login and `XXXX` with any four numbers. Enter your password when prompted, then open a browser window and navigate to `http://localhost:XXXX/`, again replacing `XXXX` with the same four numbers. Enter your BU login when prompted, and you should then see an RStudio session running in your browser. This option has the advantage of launching immediately and not having a time limit, but you cannot select which R version to use (3.6.0 is the only option).

## Installation

Open a terminal window, navigate to the directory in which you want to install the app, and run `git clone https://github.com/RebekahLMiller/PPI_Visualization.git` to make a copy of the repository in that directory. Then start an RStudio session and navigate to the new repository directory and open the "PPIVisualization.Rproj" file. A pop-up will ask you to confirm that you want to open that project. Click "Yes" and the RStudio session will reload. Then run the following commands to load the correct versions of all the required packages. This may take a while the first time.

```
install.packages("renv")    # You only need to run this once
renv::restore()
devtools::load_all()
```

After running these three commands, the app can be launched with the command `runPPIApp()`.
