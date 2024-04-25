#Authors: Jacob Dayton, Avalon Owens
#Manuscript: iLAM: Imaging locomotor Activity Monitor
#Date: 4/25/2024

#To aid in the processing and analysis of images captured by the iLAM system, customizable wrapper functions are accessible 
#within the iLAMtools package. This can be downloaded via:

library(devtools)
devtools::install_github("daytonjn/iLAMtools", force=TRUE)
library(iLAMtools)


#ALTERNATIVELY, as a resource, the iLAM wrapper functions are provided herein. Download these functions and place them in a "scripts" folder #accessible from your project directory. To access the functions in your workflow, source these functions with a command like:

for (f in list.files("./scripts", pattern="*.R",
                     full.names = TRUE)) {
  source(f)
}

#Make sure you have the required libraries loaded
library(dplyr)
library(imager)
library(lubridate)
library(magrittr)
library(readr)
library(reshape2)
library(stringr)
library(tibble)
library(tidyr)
library(plotrix)




 