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
 bearSites = as_tibble(birdat)%>%
            filter(Year == 2017,
                   !is.na(lon_dec),
                   !is.na(lat_dec),
                   bearing != "")%>%
            mutate(bearing = as.numeric(bearing))%>%
            group_by(site_name, lon_dec, lat_dec)%>%
            dplyr::summarise(minB = min(bearing, na.rm = T),
                             maxB = max(bearing, na.rm = T))%>%
            ungroup()%>%
            mutate(X  = lon_dec,
                   Y = lat_dec)%>%
            distinct(site_name, lon_dec, X, lat_dec, Y, minB, maxB)%>%
            data.frame(., stringsAsFactors = FALSE)
 # -- NOTE: All of  Puget Sound/Study area is within UTM Zone 10
 attr(bearSites, "zone") = 10
 attr(bearSites, "projection") = "LL"
 bearSites = convUL(bearSites)

# --------------------------------------

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Prep Map data
# -- The nepacLL high res map -- this could be swapped with something more high-res (EW)
  data(nepacLLhigh)

# -- Identify polygons in our region of interest, and only retain those
# --  JJ note: I changed the polygons to match the birdat, hopefully reducing the search space
  pid = filter(nepacLLhigh, X < max(uni(lon_deg)),
               X > min(uni(lon_deg)),
               Y > min(uni(lat_deg)),
               Y < max(uni(lat_deg)))%>%
        group_by(PID) %>%
        summarize(n = length(.))%>%
        filter(n > 0)%>%
        ungroup()%>%
        dplyr::select(PID)%>%
        distinct(PID)%>%
        unlist(use.names = FALSE)

  nepacLLhigh = as_tibble(nepacLLhigh) %>%
                filter(PID %in% pid)
# -- NOTE: All of  Puget Sound/Study area is within UTM Zone 10
  attr(nepacLLhigh, "zone") = 10
# convert this filtered nepacLL to UTM
  nepacLLutm = convUL(nepacLLhigh)
# --------------------------------------

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  #Eric's original example to be added to test data
  # JJ- According to Eric this is in PID == 1
  # EW- Pick a random spot -- this is near Carkeek,
  this_lat = 47.711406
  this_lon = -122.380194

  df = data.frame("X"=this_lon, "Y"=this_lat, minB = 0, maxB = 360)
  attr(df, "zone")       = 10
  attr(df, "projection") = "LL"
  dfutm = convUL(df)

  #JJ - Erics original parameters
  pts_per_km = 10
  max_dist_k = 12

  #JJ - Toy example including Eric's original example
  pt_df <- bird.in

  tst <- sapply(1:nrow(pt_df), function(i){
    # EW - pythagorean theorem to draw a line
    cosrad = cos((seq(pt_df$minB[i], pt_df$maxB[i], by = 1))*pi/180)
    sinrad = sin((seq(pt_df$minB[i], pt_df$maxB[i], by = 1))*pi/180)

    # EW - these are equally spaced distances along the hypotenuse --
    # EW - need to calculate lat-lon coords of each
    z = seq(0, max_dist_k, length.out = length(cosrad))
    delta_lat = z * cosrad
    delta_lon = z * sinrad
    x_coords = pt_df$X[i] + delta_lon
    y_coords = pt_df$Y[i] + delta_lat

    inpoly = rep(0, length(x_coords)) # vector, 0 or 1 if in polygon

    # EW - group by polygon id, for each summarize whether this coord is in a polygon
    g = group_by(nepacLLutm, PID) %>%
      dplyr::summarise(inp = point.in.polygon(point.x=x_coords[i], point.y=y_coords[i],
                                              pol.x=X, pol.y=Y))
    # EW - record total -- should just be 0 or 1
    inpoly[i] = sum(g$inp)

    dist_to_land    = rep(0, length(inpoly))
    # EW - these arguments - 0.02, 0.7 are somewhat arbitrary and meant to catch cases looking at land
    # JJ - Im not sure I completely understand this part.  I changed the % of values used in the
    #       threshold mean calculation from 0.02 ==> 0.2 because most values ended up NA with 0.02
    # JJ - Also note that I added a small value when z[which(inpoly==1)] == 0 because in those cases,
    #      it's hard to tell that a "value" of z was found (since all other entries are 0)
    dist_to_land[i] = ifelse(mean(inpoly[[i]][1:round(0.2*length(inpoly[[i]]))], na.rm = T) > 0.7,
                             ifelse(z[[i]][which(inpoly[[i]]==1)] == 0,
                                    0.001,
                                    z[[i]][which(inpoly[[i]]==1)]),
                             NA)
    return(dist_to_land)})


  # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # Output
  # Test to see if each site has only a *single* lat-lon
  latlonCount <- as_tibble(birdat)%>%
                  filter(Year>2013)%>%
                  distinct(Year, site_name, lat_dec,lon_dec)%>%
                  group_by(Year, site_name)%>%
                  dplyr::summarise(n = n())%>%
                  data.frame(., stringsAsFactors = F)
  any(latlonCount$n >1) #FALSE




