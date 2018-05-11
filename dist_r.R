# Code to calculate the lat-lon's along each bearing
# for PSSS volunteer survey
#----------------------------------------------------
# 2017         Original Coding     Eric Ward
# 2018-May     Improvements        J. Jannot
#----------------------------------------------------
rm(list = ls())
library(PBSmapping)
library(dplyr)
library(sp)

# Pick a random spot -- this is near Carkeek,
this_lat = 47.711406
this_lon = -122.380194

# Convert to UTM
df = data.frame("PID"=1, "POS"=1, "X"=this_lon, "Y"=this_lat)
attr(df, "zone")=10
attr(df, "projection") = "LL"
dfutm = convUL(df)

# pull in the nepacLL high res map -- this could be swapped with something more high-res
data(nepacLLhigh)

# use dplyr to identify polygons in our region of interest, and only retain those
pid = group_by(nepacLLhigh, PID) %>%
summarize(n = length(which(X < -121 & X > -126 & Y > 47 & Y < 49.5))) %>%
filter(n > 0)
nepacLLhigh = nepacLLhigh[which(nepacLLhigh$PID %in% pid$PID),]
attr(nepacLLhigh, "zone")=10
# convert this filtered nepacLL to UTM
nepacLLutm = convUL(nepacLLhigh)

##############################
# function to find nearest distance to land
##############################
calc_distance = function(polygons, pt_df, bearing = 270, pts_per_km = 10, max_dist_k = 12) {
	# pythagorean theorem to draw a line
	rad = bearing * pi / 180
	#delta_lat = max_dist * cos(rad)
	#delta_lon = max_dist * sin(rad)

	# these are equally spaced distances along the hypotenuse --
	# need to calculate lat-lon coords of each
	z = seq(0, max_dist_k, length.out = max_dist_k * pts_per_km)
	delta_lat = z * cos(rad)
	delta_lon = z * sin(rad)

	x_coords = pt_df$X + delta_lon
	y_coords = pt_df$Y + delta_lat
	inpoly = rep(0, length(x_coords)) # vector, 0 or 1 if in polygon

	# this loop is slow - can be sped up in dplyr/plyr
	for(i in 1:length(x_coords)) { # loop over coordinates
		# group by polygon id, for each summarize whether this coord is in a polygon
		g = group_by(polygons, PID) %>%
		summarize(inpoly = point.in.polygon(point.x=x_coords[i], point.y=y_coords[i], pol.x=X, pol.y=Y))
		# record total -- should just be 0 or 1
		inpoly[i] = sum(g$inpoly)
	}

    # these arguments - 0.02, 0.7 are somewhat arbitrary and meant to catch cases looking at land
    if(mean(inpoly[1:round(0.02*length(inpoly))]) > 0.7) {
    	# this is probably a piece of land
    	dist_to_land = NA
    } else {
       # find first location that is on the polygon
	   dist_to_land = z[which(diff(inpoly)==1) + 1]
    }
    return(dist_to_land)

}

# Example of using for a single site -- location on E side of puget sound
dist = rep(0, 360)
for(b in 240:360) {
	# pts_per_km here really sets the resolution and I think we'd want that to be more like ~ 100-200?
	dist[b] = calc_distance(polygons = nepacLLutm, pt_df = dfutm, bearing = b, pts_per_km = 30)
}

birdat <- read.csv(file = "C:/Users/Banksiola/Documents/Seattle Audobon/Data/PSSS 2017-18 data_May 2018.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)





