# Code to calculate the lat-lon's along each bearing
# for PSSS volunteer survey
#----------------------------------------------------
# 2018 June     Original coding - based on dist_r.R   J. Jannot
#----------------------------------------------------
rm(list = ls())

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# set the libraries etc.
  source("intro_file.R", echo = T)
# -------------------------------

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Call and Prep the Bird data
# -- The data
  birdat <- read.csv(file = "C:/Users/Banksiola/Documents/Seattle Audobon/Data/PSSS 2017-18 data_May 2018.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)

# -- Create a uniqe ID for every line, just in case...
  birdat$UniqueID = c(1:nrow(birdat))

# -- Clean up bearing
# waiting on Eric - JJ 18 May 2018
# birdat$cln.bearing <- as.numeric(gsub("(0*)([0-9]*)", "\\2", gsub("(R*|L*)([0-9]*)", "\\2", birdat$bearing)))

# -- Make lat-long fields decimals
# lat
  lat_deg = as.numeric(str_extract(gsub("(N)(*)", "\\2", birdat$position), "(^[0-9]{2})" ))
  lat_mindec =  as.numeric(str_extract(birdat$position, "[0-9]{2}\\.[0-9]+$"))/60
  birdat$lat_dec = lat_deg +lat_mindec
# long
  lon_deg <- -as.numeric(gsub("(N[0-9]+ [0-9]{1,3}\\.[0-9]{1,5} W)([0-9]{1,3}\\b)( [0-9]{1,3}\\.[0-9]{1,5})", "\\2", birdat$position))
  lon_mindec =  as.numeric(gsub("(N[0-9]+ [0-9]{1,3}\\.[0-9]{1,5} W[0-9]{1,3} )([0-9]{1,3}\\.[0-9]{1,5})", "\\2", birdat$position))/60
  birdat$lon_dec = lon_deg +lon_mindec

# -- Create Year field for subsetting
  birdat$Year = str_extract(birdat$survey_date, "([0-9]{4})")

# -- Create lat-long input data
 bird.in = as_tibble(birdat)%>%
            filter(Year == 2017,
                   !is.na(lon_dec),
                   !is.na(lat_dec),
                   bearing != "")%>%
            mutate(bearing = as.numeric(bearing))%>%
            group_by(lon_dec, lat_dec)%>%
            dplyr::summarise(minB = min(bearing, na.rm = T),
                             maxB = max(bearing, na.rm = T))%>%
            ungroup()%>%
            dplyr::select(X  = lon_dec, Y = lat_dec, minB, maxB)%>%
            distinct(X, Y, minB, maxB)%>%
            data.frame(., stringsAsFactors = FALSE)
 # -- NOTE: All of  Puget Sound/Study area is within UTM Zone 10
 attr(bird.in, "zone") = 10
 attr(bird.in, "projection") = "LL"
 bird.in = convUL(bird.in)

# --------------------------------------

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Prep Map data
# -- The nepacLL high res map -- this could be swapped with something more high-res (EW)
  data(nepacLLhigh)

# -- Identify polygons in our region of interest, and only retain those
# --  JJ note: I changed the polygons to match the birdat, hopefully reducing the search space
  pid = group_by(nepacLLhigh, PID) %>%
        summarize(n = length(which(X < -(min(uni(lon_deg))) &
                                 X > -(max(uni(lon_deg)))   &
                                 Y > min(uni(lat_deg))      &
                                 Y < max(uni(lat_deg)))))  %>%
    filter(n > 0)
  nepacLLhigh = as_tibble(nepacLLhigh) %>%
                filter(PID %in% pid$PID)
# -- NOTE: All of  Puget Sound/Study area is within UTM Zone 10
  attr(nepacLLhigh, "zone") = 10
# convert this filtered nepacLL to UTM
  nepacLLutm = convUL(nepacLLhigh)
# --------------------------------------





