#Script to process sample data and verify proper installation of iLAMtools functions
library(devtools)
devtools::install_github("iLAMtools/iLAMtools", force=TRUE)
library(iLAMtools)

#Alternatively: Download a zipped folder containing iLAMtools functions from https://ilamtools.github.io/ilam/post-process-image-segmentation.html 
#Save these files in a "scripts" folder that is accessible from the current project directory
#Source  these functions via:
# for (f in list.files("./scripts", pattern="*.R",
#                      full.names = TRUE)) {
#   source(f)
# }
# library(dplyr)
# library(imager)
# library(lubridate)
# library(magrittr)
# library(readr)
# library(reshape2)
# library(stringr)
# library(tibble)
# library(tidyr)
# library(plotrix)

#Download image files from link provided at https://ilamtools.github.io/post-process-image-segmentation.html

###
#change following values for every cage
pi_sub_folder <- "ilam01"
sex <- "male" #cage/project-specific
x_left <- 450 #cage-specific, depending on camera arrangement
x_right <- 2250 #cage-specific, depending on camera arrangement
y_bot <- 100 #cage-specific, depending on camera arrangement
y_top <- 1700 #cage-specific, depending on camera arrangement

#change following values for every experiment
out_file_name = "Pgreeni_test" #project-specific
n_thr = 0.999 #species-specific, depending on IR reflectance/contrast with background
n_cln = 10 #species-specific, depending on IR reflectance
n_max = 75000 #species-specific, pixel differences above this value will be considered as noise
start_photophase = 5 #project-specific
end_photophase = 21 #project-specific
genus = "photinus"
species = "greeni"
###
#creates a vector of .jpg image file names
file_names <- list.files(pi_sub_folder,
                         pattern= "*.jpg",
                         full.names = TRUE)

#finds all movements by image subtraction, global thresholding, and blob detection
out <- find_movements(files = file_names, # list of file names 
                      n_thr = n_thr,      # threshold value (0.992 == "0.8%")
                      n_cln = n_cln,      # value for cleaning (number of pixels)
                      n_grw = 1.5,        # multiplier for n_cln (shrink vs. grow)
                      n_blr = 3,          # blur radius
                      n_max = 75000,      # upper cut-off for # pixel differences
                      x_left = x_left,    # value for crop on x min
                      x_right = x_right,  # value for crop on x max
                      y_bot = y_bot,      # value for crop on y min
                      y_top = y_top,      # value for crop on y max
                      find_thr = F,       # ***NOTE! Typically, you would input T here to identify the proper threshold cutoff for your data***
                      type_thr = 0.037002818606312, # ***Note! Similarly, you would input "absolute" here to find your threshold cutoff***
                      p_sample = 0.05,    # percent of images to sample
                      channel = "grayscale",
                      animal = "black")

#adds additional columns to dataframe
out$ID <- paste0(n_thr*100,"%_", "s", n_cln, "g", 1.5*n_cln)
out$sex <- sex
out$genus <- genus
out$species <- species

#saves output containing all identified blobs, their size and location, into .csv in current wd
if (file.exists(paste0(out_file_name,".csv"))){
  write.table(out, file = paste0(out_file_name,".csv"),
              append = TRUE, quote = TRUE, sep = ",",
              row.names = FALSE, col.names = FALSE)
} else{
  write.csv(out, file = paste0(out_file_name,".csv"),
            col.names = TRUE, row.names = FALSE)
  
}
rm(out)

#parses output .csv 
by_change <- parse_movements(file_mvmnts = paste0(out_file_name,".csv"),
                             start_photophase = start_photophase,
                             end_photophase = end_photophase)

#plots a detected blobs onto a subset of images
#circles in bottom left corner denote standards of sizes: 12.8k px, 3.2k px, 800 px, 200 px, 50 px
plot_movements(file_names,
               pi_sub_folder,
               by_change,
               x_left, x_right,
               y_bot, y_top,
               n_max)

# Make a gif from plotted image jpegs
#library(magick)
make_gif(out_file_name,
         pi_sub_folder)

by_frame_Pg <- 
  by_change %>% ungroup() %>% 
  group_by(pi, ID, time, treatment) %>%
  summarize(n = length(s[!is.na(s)]), #number of blobs of size (s) != NA
            s = sum(s, na.rm = TRUE)) %>% #sum of blob sizes, NA removed
  mutate(n = ifelse(s == 0, 0, n)) %>% #if sum of blobs=0, then n<-0 (otherwise it'd be 1)
  distinct(pi, ID, time, treatment, n, s) #sanity check to remove any duplicates


by_frame_Pg <-
  by_frame_Pg %>% ungroup() %>%
  mutate(s = replace(s, s==2000000, NA),
         n = replace(n, is.na(s), NA))

by_frame_Pg <-
  by_frame_Pg %>% group_by(pi) %>%
  mutate(s = round((na.locf0(s, fromLast = TRUE) + na.locf0(s, fromLast = FALSE))/2,0),
         n = round((na.locf0(n, fromLast = TRUE) + na.locf0(n, fromLast = FALSE))/2,0)) %>% ungroup()

by_frame_Pg <-
  by_frame_Pg %>% ungroup() %>%
  mutate(s = replace(s, is.na(s), 0),
         n = replace(n, is.na(n), 0))

ilam_Pg = make_dam_file(by_frame_Pg, variable_name = "s") 
