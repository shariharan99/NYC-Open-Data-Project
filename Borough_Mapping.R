library(raster)
library(dplyr)
library(rgeos)
library(kernlab)

#Load in Geo-Coded data frame from previous step#
load("final_df.Rdata")

#Subset only the Borough, longitude, and latitude#
final.df <- final.df[,c("Borough","longitude","latitude")]

#Transform the Borough column into factors#
#This is needed for support vector machine function#
final.df$Borough <- as.factor(final.df$Borough)

#Randomize the rows of the final data frame for SVM function#
final.df <- final.df[sample(nrow(final.df)),]

#Generate raster object based on NYC minimum and maximum longitude and latitude#
r = raster(nrows=500, ncols=500, 
           xmn=-74.3, xmx=-73.71, 
           ymn=40.49, ymx=40.92)

#Initialize raster object as NA#
r[] = NA

#Take 30k records from each Borough#
MN <- final.df[final.df[,"Borough"] == "MANHATTAN",][1:30000,]
QN <- final.df[final.df[,"Borough"] == "QUEENS",][1:30000,]
SI <- final.df[final.df[,"Borough"] == "STATEN ISLAND",][1:30000,]
BX <- final.df[final.df[,"Borough"] == "BRONX",][1:30000,]
BR <- final.df[final.df[,"Borough"] == "BROOKLYN",][1:30000,]

#Bind all the Boroughs to a 150k record long data frame, with 30k records
#from each Borough#
final.df <- rbind(MN, QN, SI, BX, BR)

#Create prediction data frame of all longitudes and latitudes in raster object#
pred_locs <- data.frame(xyFromCell(r, 1:250000))

#Name prediction data frame as "longitude" and "latitude"#
names(pred_locs) <- c("longitude", "latitude")

#Round the longitude/latitude to 3 digits and cbind with the first column#
final.df.poly <- cbind(final.df[,1],round(final.df[,-1], 3))

#Round the prediction data frame to 3 digits as well#
pred_locs_round <- round(pred_locs,3)

#Perform an ANTI_JOIN between the predictiond data frame and final Borough
#data frame to find all longitudes and latitudes NOT in the data (in the Ocean)#
otherPoints <- anti_join(pred_locs_round, final.df.poly)

#Randomize the rows of this created Data Frame#
otherPoints <- otherPoints[sample(nrow(otherPoints)),]

#Add column to classify points in this data frame as "Other"#
Borough <- rep("Other", nrow(otherPoints))
otherPoints <- cbind(Borough, otherPoints )

#Rbind all Borough Data with "Other" data to get full classification data frame#
full.df <- rbind(final.df.poly, otherPoints)

#First 150k records are from the Boroughs, and rest are "Other". Only take 50k
#"Other" points for the sake of computational speed. This seemed to work well#
#Note that the following 3 script lines take about 45 minutes total#

#Run 'kvsm' function using first 200k points, as explained above#
svm.object <- ksvm(Borough ~ ., data = full.df[1:200000,])

#Predict all coordinates based on svm object and save to raster object#
r[] <- predict(svm.object, pred_locs)

#Transform raster to a Spatial Polygon Object, and dissolve lines within a polygon#
poly <- rasterToPolygons(r,dissolve=TRUE)

#Set name of the data to "Name" for wercker test#
names(poly@data) = "Name"

#Set names based on which Polygons correspond to which Borough and "Other"#
poly@data$Name = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island", "Other")

#Source the 'write_geojson.R' script#
source("write_geojson.R")

#Write the resulting polygon as a .json file to the current working directory#
write_geojson(poly,"boroughs.json")

#Save the polygon to file for the write-up#
save("poly.png")
plot(poly)
dev.off()