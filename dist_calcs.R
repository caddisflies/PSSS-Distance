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

  ##############################
  # function to find nearest distance to land
  ##############################
  calc_distance = function(polygons, dat, pts_per_km = 360, max_dist_k = 1) {

    outFinal = ldply(1:length(unique(dat$site_name)), function(i){
      # -- Subset to site
      d = as_tibble(dat)%>%
        filter(site_name %in% unique(site_name)[i])%>%
        #filter(site_name %in% unique(site_name)[1])%>%
        data.frame(., stringsAsFactors = FALSE)

      # -- Get distance to land for every bearing
      outB <- ldply(d$minB:d$maxB, function(j){
        bearing = j
        #bearing = 60
        # pythagorean theorem to draw a line
        rad = bearing * pi / 180
        #delta_lat = max_dist * cos(rad)
        #delta_lon = max_dist * sin(rad)

        # these are equally spaced distances along the hypotenuse --
        # need to calculate lat-lon coords of each
        z = seq(0, max_dist_k, length.out = max_dist_k * pts_per_km)
        delta_lat = z * cos(rad)
        delta_lon = z * sin(rad)

        x_coords = d$X + delta_lon
        y_coords = d$Y + delta_lat
        inpoly = rep(0, length(x_coords)) # vector, 0 or 1 if in polygon

        # this loop is slow - can be sped up in dplyr/plyr
        for(i in 1:length(x_coords)) { # loop over coordinates
          # group by polygon id, for each summarize whether this coord is in a polygon
          g = group_by(nepacLLhigh2, PID) %>%
            #g = group_by(polygons, PID) %>%
            summarize(inpoly = point.in.polygon(point.x=x_coords[i], point.y=y_coords[i], pol.x=X, pol.y=Y))
          # record total -- should just be 0 or 1
          inpoly[i] = sum(g$inpoly)
        }

        # these arguments - 0.02, 0.7 are somewhat arbitrary and meant to catch cases looking at land
        #if(mean(inpoly[1:round(0.02*length(inpoly))]) > 0.7) {
        # this is probably a piece of land
        #  dist_to_land = NA
        #  X = NA
        #  Y = NA
        #} else {
        # # find first location that is on the polygon
        dtl1 = which(diff(inpoly) ==  1) + 1
        # # dtl2 = ifelse(which(diff(inpoly) == -1) - 1 == 0, 1, which(diff(inpoly) == -1) - 1)
        # dist_to_land = z[dtl1]#ifelse(length(dtl1)>0, z[dtl1], z[dtl2])
        X = x_coords[dtl1]#ifelse(length(dtl1)>0, x_coords[dtl1], x_coords[dtl2])
        Y = y_coords[dtl1]#ifelse(length(dtl1)>0, y_coords[dtl1], y_coords[dtl2])
        # #  }
        #

        # these arguments - 0.02, 0.7 are somewhat arbitrary and meant to catch cases looking at land
        if(mean(inpoly[1:round(0.02*length(inpoly))]) > 0.7) {
          # this is probably a piece of land
          dist_to_land = NA
        } else {
          # find first location that is on the polygon
          dist_to_land = z[which(diff(inpoly)==1) + 1]
        }


        out1 = cbind(X, Y, bearing , dist_to_land)
        return(out1)
      })

      cbind(site_name = d$site_name,
            site_lat_dec   = d$lat_dec,
            site_lon_dec   = d$lon_dec,
            outB)
    })

    return(outFinal)
  }


  tst = calc_distance(polygons = nepacLLhigh2, dat = bearSites[1:2, ], pts_per_km = 360, max_dist_k = 1)

  # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # Mapping the max points?

  din = as_tibble(tst)%>%
    filter(dist_to_land>0, !is.na(dist_to_land))%>%
    data.frame(., stringsAsFactors = FALSE)

  pdf("test_map.pdf", onefile = TRUE)
  for(i in 1:length(unique(din$site_name))){

    din2 = as_tibble(din)%>%
      filter(site_name == unique(.$site_name)[i])%>%
      data.frame(., stringsAsFactors = FALSE)

    minLon = min(din2$X) + 0.1
    maxLon = max(din2$X) - 0.1
    minLat = min(din2$Y) - 0.1
    maxLat = max(din2$Y) + 0.1
    pltitle = paste0(uni(din2$site_name), " bearing: ", uni(din2$bearing))

    g = ggplot(data = din2) +
      geom_polygon(data = nepacLLhigh2, aes(x = X, y = Y, group = PID), fill = 8, color = "black") +
      geom_point(data = din2, aes(x = X, y = Y))+
      geom_point(aes(x = uni(site_lon_dec), y = uni(site_lat_dec)), color = "blue", shape = 2) +
      labs(x = "Longitude", y = "Latitude", title = pltitle) +
      theme_bw() +
      #coord_map() +
      coord_fixed(xlim = c(minLon, maxLon), ylim = c(minLat, maxLat)) +
      theme(panel.border     = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line        = element_line(colour = "black"))


    print(g)

  }

  dev.off()

