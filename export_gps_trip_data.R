
# Merge data.frames with trips details in - extended and main period

# Load extended file
trips_ex <- read.csv("trips_details_2016_01_29_extended.csv", header = TRUE)
names(trips_ex)[64] <- "stage"

# Load main period
trips_main <- read.csv("trips_details_2016_01_29_detailed.csv", header = TRUE)


# Merge these/ add extended to other (remove trips present in main period file)
duplicated.trip <- trips_ex$trip_id %in% trips_main$trip_id
# summary(duplicated.trip)
# 
# summary(trips_ex$period[!duplicated.trip])

library(plyr)
trips_combined <- rbind.fill(trips_ex[!duplicated.trip,], trips_main)


# Output this new dataframe ------
write.csv(trips_combined, file = "trips_details_2016_01_29_detailed_extended.csv")
