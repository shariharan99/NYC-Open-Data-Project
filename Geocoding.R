library(data.table)
library(dplyr)
library(stringr)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)

#Read 5 GB Data File using 'fread' from data.table package#
nyc = fread("/home/vis/cr173/Sta523/data/nyc/nyc_311.csv") %>% tbl_df()

#Load interesections and pluto data for Geocoding#
load("/home/vis/cr173/Sta523/data/nyc/intersections/intersections.Rdata")
load("/home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata")

#Re-name intersections data#
intersections <- data

#Remove original data frame to conserve memory#
rm(data)
gc()

#Re-name mispelled 'Stree1' column to 'Street1'#
colnames(intersections)[which(names(intersections) == "Stree1")] <- "Street1"

#Select the geographical variables we need#
nyc_min <- nyc %>%
  select(Unique.Key, Incident.Address, Borough, Complaint.Type)

#Replace all Borough names in pluto with the full name for future JOIN#
pluto$Borough = pluto$Borough %>%
  str_replace_all("BK", "BROOKLYN") %>%
  str_replace_all("MN", "MANHATTAN") %>%
  str_replace_all("QN", "QUEENS") %>%
  str_replace_all("BX", "BRONX") %>%
  str_replace_all("SI", "STATEN ISLAND")


#For each pluto address, compute the average longtitude and latitude into the new pluto data frame.#
pluto <- pluto %>%
  group_by(Borough, Address) %>%
  summarize("longitude"= mean(x), "latitude"= mean(y), 
            "ZipCode"= first(ZipCode))

##Intersection Geo-Coding## 

#Note: We decided not to Geo-code the intersections due to the fact that it was
#computationally expensive, but we are leaving the code below in case anyone is 
#curious.


#Replace all abbreviations in intersections 'Street 1' column with the full name.#
#intersections$Street1 = intersections$Street1 %>% 
#  str_replace_all("AVE", "AVENUE") %>%
#  str_replace_all("ST", "STREET") %>%
#  str_replace_all("RD", "ROAD") %>%
#  str_replace_all("BLVD", "BOULEVARD") %>%
#  str_replace_all("PL", "PLACE") %>%
#  str_replace_all("CT", "COURT") %>%
#  str_replace_all("LN","LANE") %>%
#  str_replace_all("PKWY","PARKWAY") %>%
#  str_replace_all("EXPY","EXPRESSWAY")

#Repeat above task for 'Street 2' column#
#intersections$Street2 = intersections$Street2 %>% 
#  str_replace_all("AVE", "AVENUE") %>%
#  str_replace_all("ST", "STREET") %>%
#  str_replace_all("RD", "ROAD") %>%
#  str_replace_all("BLVD", "BOULEVARD") %>%
#  str_replace_all("PL", "PLACE") %>%
#  str_replace_all("CT", "COURT") %>%
#  str_replace_all("LN","LANE") %>%
#  str_replace_all("PKWY","PARKWAY") %>%
#  str_replace_all("EXPY","EXPRESSWAY")

#Inner Join our NYC Data with Intersections data using 'Cross Street.1'/'Street 1' and 'Cross.Street.2'/'Street2'#
#intersec.output1 <- inner_join(nyc_min, intersections, by = c("Cross.Street.1" = "Street1", "Cross.Street.2" = "Street2")) %>%
#                    group_by(Cross.Street.1, Cross.Street.2) %>%
#                    summarize("longitude" = mean(longitude), "latitude" = mean(latitude), "Borough" = first(Borough), 
#                              "Intersection.Street.1" = first(Intersection.Street.1), "Intersection.Street.2" = first(Intersection.Street.2), 
#                              "Incident.Address" = first(Incident.Address))

#Note that Cross Streets could be represented both ways#
#So Inner Join our NYC Data with Intersections data using 'Cross Street.2'/'Street 1' and 'Cross.Street.1'/'Street2'#
#intersec.output2 <- inner_join(nyc_min, intersections, by = c("Cross.Street.2" = "Street1", "Cross.Street.1" = "Street2")) %>%
#                    group_by(Cross.Street.1, Cross.Street.2) %>%
#                    summarize("longitude" = mean(longitude), "latitude" = mean(latitude), "Borough" = first(Borough), 
#                    "Intersection.Street.1" = first(Intersection.Street.1), "Intersection.Street.2" = first(Intersection.Street.2), 
#                    "Incident.Address" = first(Incident.Address))

#Inner Join our NYC Data with Intersections data using 'Intersection.Street.1/Street1' and 'Intersection.Street.2/Street2'#
#intersec.output3 <- inner_join(nyc_min, intersections, by = c("Intersection.Street.1" = "Street1", "Intersection.Street.2" = "Street2")) %>%
#                    group_by(Intersection.Street.1, Intersection.Street.2) %>%
#                    summarize("longitude" = mean(longitude), "latitude" = mean(latitude), "Borough" = first(Borough), 
#                    "Cross.Street.1" = first(Cross.Street.1), "Cross.Street.2" = first(Cross.Street.2), 
#                    "Incident.Address" = first(Incident.Address))

#Note that Cross Streets could be represented both ways#
#So Inner Join our NYC Data with Intersections data using 'Intersection.Street.2/'Street 1' and 'Intersection.Street.1'/'Street2'#
#intersec.output4 <- inner_join(nyc_min, intersections, by = c("Intersection.Street.2" = "Street1", "Intersection.Street.1" = "Street2")) %>%
#                    group_by(Intersection.Street.1, Intersection.Street.2) %>%
#                    summarize("longitude" = mean(longitude), "latitude" = mean(latitude), "Borough" = first(Borough), 
#                    "Cross.Street.1" = first(Cross.Street.1), "Cross.Street.2" = first(Cross.Street.2), 
#                    "Incident.Address" = first(Incident.Address))

#
#Re-order the columns for a future 'rbind' with the pluto joined data#
#intersec.output1 <- intersec.output1[,c("Incident.Address","Cross.Street.1","Cross.Street.2",
#                                        "Intersection.Street.1","Intersection.Street.2",
#                                        "Borough","longitude","latitude")]
#intersec.output2 <- intersec.output2[,c("Incident.Address","Cross.Street.1","Cross.Street.2",
#                                        "Intersection.Street.1","Intersection.Street.2",
#                                        "Borough","longitude","latitude")]
#intersec.output3 <- intersec.output3[,c("Incident.Address","Cross.Street.1","Cross.Street.2",
#                                        "Intersection.Street.1","Intersection.Street.2",
#                                        "Borough","longitude","latitude")]
#intersec.output4 <- intersec.output4[,c("Incident.Address","Cross.Street.1","Cross.Street.2",
#                                        "Intersection.Street.1","Intersection.Street.2",
#                                        "Borough","longitude","latitude")]

#RBind all the intersection joined data and remove duplicate rows#
#intersections_min <- distinct(rbind(intersec.output1, intersec.output2, intersec.output3, intersec.output4))

##Pluto Geocoding##

#Inner Join NYC and Pluto Data by the physical Address and the Borough, to prevent duplicate addresses
#across Boroughs#
pluto.output <- inner_join(nyc_min, pluto, by = c("Incident.Address" = "Address", "Borough" = "Borough"))

#Remove Duplicate rows#
pluto.output <- distinct(pluto.output)

##Final Data Frame##

#Select a certain subset of columns within the pluto joined data frame#
final.df <- pluto.output %>%
             select(Incident.Address, Borough, longitude, latitude, Complaint.Type)

#Save final data frame to current working directory. Note this includes Complaint Type
#which is needed in our final visualization#
save(final.df, file = "final_df.Rdata")
