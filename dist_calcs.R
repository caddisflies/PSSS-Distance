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
  lat_mindec =  as.numeric(str_extract(str_extract(birdat$position, "(N)([0-9]){2} ([0-9]){2}\\.([0-9]){1,100}"),"([0-9]){2}\\.([0-9]){1,100}") )/60
  birdat$lat_dec = lat_deg +lat_mindec
# long
  lon_deg = as.numeric(gsub("(N[0-9]+ [0-9]{1,3}\\.[0-9]{1,5} W)([0-9]{1,3}\\b)( [0-9]{1,3}\\.[0-9]{1,5})", "\\2", birdat$position))
  lon_mindec =  as.numeric(gsub("(N[0-9]+ [0-9]{1,3}\\.[0-9]{1,5} W[0-9]{1,3} )([0-9]{1,3}\\.[0-9]{1,5})", "\\2", birdat$position))/60
  birdat$lon_dec = -(lon_deg +lon_mindec)
  lon_deg = birdat$lon_dec

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
 # attr(bearSites, "zone") = 10
 # attr(bearSites, "projection") = "LL"
 # bearSites = convUL(bearSites)

# --------------------------------------

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Prep Map data
# -- The nepacLL high res map -- this could be swapped with something more high-res (EW)
  data(nepacLLhigh)

# -- Identify polygons in our region of interest, and only retain those
# --  JJ note: I changed the polygons to match the birdat, hopefully reducing the search space
  pid =  nepacLLhigh %>%
    mutate(ind = ifelse( (X <= max(uni(lon_deg)) &
                           X >= min(uni(lon_deg)) &
                           Y >= min(uni(lat_deg)) &
                           Y <= max(uni(lat_deg))), 1, 0))%>%
             group_by(PID) %>%
             summarize(tot = ifelse(sum(ind) > 0, 1, 0)) %>%
             filter(tot == 1)%>%
             data.frame(., stringsAsFactors = F)

  nepacLLhigh2 = as_tibble(nepacLLhigh)%>%
                 filter(PID %in% pid$PID,
                        (X <= max(uni(lon_deg)) &
                            X >= min(uni(lon_deg)) &
                            Y >= min(uni(lat_deg)) &
                            Y <= max(uni(lat_deg))))%>%
                  data.frame(., stringsAsFactors = FALSE)

# -- NOTE: All of  Puget Sound/Study area is within UTM Zone 10
  attr(nepacLLhigh2, "zone") = 10
  attr(nepacLLhigh2, "projection") = "LL"
# # convert this filtered nepacLL to UTM
#   nepacLLutm = convUL(nepacLLhigh2)
# --------------------------------------

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Create X,Y coordinates for points along a "line" for each site & bearing

  # -- Set the resolution
  # pts_per_km here really sets the resolution and I think we'd want that to be more like ~ 100-200?
  max_dist_k = 12
  pts_per_km = 30 # this enables z to be same length as bearAll, which just makes everything easier...
  # these are equally spaced distances along the hypotenuse --
  # need to calculate lat-lon coords of each
  z = seq(0, max_dist_k, length.out = max_dist_k * pts_per_km)

  bearAll   = seq(1, 360, by = 1)
  # pythagorean theorem to draw a line
  sinrad    = sin(bearAll)*pi/180
  cosrad    = cos(bearAll)*pi/180

  # -- Multiply every bearing (1:360) by every delta
  deltaLL = do.call(rbind,
                    Map(function(i){data.frame(cbind(    bear  = bearAll[i],
                                                         z = z[i],
                                                         delta_lon = z[i] * sinrad,
                                                         delta_lat = z[i] * cosrad))}, 1:length(sinrad)))
  dim(deltaLL)
  deltaLL = deltaLL[complete.cases(deltaLL) , ]
  dim(deltaLL)

  # -- Create a data frame of X, Y, and site_name

  # - Use a subset of the data for example
  pt_df <- bearSites[1:5, ]

  xycoords = do.call(rbind,
                     Map(function(i){
                               cbind(site_name = pt_df$site_name[i], bearing = bearAll,
                               data.frame(X = pt_df$X[i] + deltaLL$delta_lon),
                               data.frame(Y = pt_df$Y[i] + deltaLL$delta_lat), stringsAsFactors = FALSE)
                       },
                       1:nrow(pt_df)))

  inpoly = rep(0, nrow(xycoords)) # vector, 0 or 1 if in polygo
  # this loop is slow - can be sped up in dplyr/plyr
  for(i in 1:length(xycoords)) { # loop over coordinates
    # group by polygon id, for each summarize whether this coord is in a polygon nepacLLhigh2
    g = group_by(nepacLLhigh2, PID) %>%
      dplyr::summarise(inpoly = point.in.polygon(point.x = xycoords$X[i],
                                          point.y = xycoords$Y[i],
                                          pol.x = X, pol.y = Y))
    # record total -- should just be 0 or 1
    inpoly[i] = sum(g$inpoly)
  }

  # -- Subset for mapping example
  d = as_tibble(xycoords)%>%
            filter(site_name %in% unique(site_name)[1:5],
                   bearing == 270)%>%
            data.frame(., stringsAsFactors = FALSE)

  attr(d, "zone") = 10
  attr(d, "projection") = "LL"
  #d2 = convUL(d)

  # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # Mapping Example

  pdf("test_map.pdf", onefile = TRUE)
  for(i in 1:length(unique(d$site_name))){

    din = as_tibble(d)%>%
      filter(site_name == unique(.$site_name)[i])%>%
      data.frame(., stringsAsFactors = FALSE)

    minLon = min(din$X) + 0.1
    maxLon = max(din$X) - 0.1
    minLat = min(din$Y) - 0.1
    maxLat = max(din$Y) + 0.1
    pltitle = paste0(uni(din$site_name), " bearing: ", uni(din$bearing))

    g = ggplot(din, aes(x = X, y = Y)) +
      geom_polygon(data = nepacLLhigh2, aes(x = X, y = Y, group = PID), fill = 8, color = "black") +
      geom_point()+
      labs(x = "Longitude", y = "Latitude", title = pltitle) +
      theme_bw() +
      #coord_map() +
      coord_map(xlim = c(minLon, maxLon), ylim = c(minLat, maxLat)) +
      theme(panel.border     = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line        = element_line(colour = "black"))


    print(g)}

  dev.off()



