library (plyr)
library (dplyr)
library (rgdal)
library (sp)
library (compare)

#################################################################
#                   eBird Record Stripper                       #
#                                                               #
# This script pre-processes eBird records file and india        #
# shape files to create RDS files for uploading to shiny        #
#################################################################

#######################Configurations###########################
#Name of the eBird quarterly archive WITHOUT .zip
# ebd_file_name <- 'ebd_IN_relAug-2016'
ebd_file_name <- 'ebd_US-WI_reevir1_201906_201906_relDec-2020'

#Name of the India region shape file archive WITHOUT .zip
# india_shape_file <- 'indiama-editedSQ'
# state <- 'IN-KL'
india_shape_file <- 'ebirdfiltersfinalfeb2017'
state <- 'US-WI'
################################################################

#Unzip and read eBird records
ebd_zip <- paste0('./data/', ebd_file_name, '.zip')
ebd_file <- paste0(ebd_file_name, '.txt')
ebd <- read.delim(unz(ebd_zip, ebd_file), quote = "", as.is = TRUE,
  na.strings = c("NA", "", "null"))

#Add unique list identifier for removing duplicates
ebd <- within (ebd, UNIQUE_SAMPLING_ID <-  ifelse(is.na(GROUP.IDENTIFIER),SAMPLING.EVENT.IDENTIFIER,GROUP.IDENTIFIER))

#If subspecies, copy subspecies common name
ebd <- within (ebd, COMMON.NAME <-  ifelse(CATEGORY=='issf',SUBSPECIES.COMMON.NAME,COMMON.NAME))

#Remove entries from shared lists
ebd_records   <- ebd[!duplicated(ebd[c("UNIQUE_SAMPLING_ID","COMMON.NAME")]),]

#Create species list by removing duplicate species entries
ebd_species   <- ebd_records[!duplicated(ebd_records$TAXONOMIC.ORDER),]

#Create state list by removing duplicate state entries
ebd_states    <- ebd_records[!duplicated(ebd_records$STATE.CODE),]

#Create district list by removing duplicate district entries
### ebd_districts is not used in the app -- remove at somepoint but keeping
### until removed from global.R
ebd_districts <- ebd_records[!duplicated(ebd_records$COUNTY.CODE),]

#Create unique lists by removing duplicate lists
ebd_lists     <- ebd_records[!duplicated(ebd_records$UNIQUE_SAMPLING_ID),]

# Strip unwanted columns from eBird records
ebd_records <- subset(ebd_records, select = c("TAXONOMIC.ORDER", "OBSERVATION.COUNT", "UNIQUE_SAMPLING_ID"))

# Strip unwanted columns from eBird species
ebd_species   <- subset(ebd_species, select = c("TAXONOMIC.ORDER", "COMMON.NAME"))

# Strip unwanted columns from eBird states
ebd_states    <- subset(ebd_states, select = c("STATE.CODE", "STATE"))

# Strip unwanted columns from eBird districts
ebd_districts <- subset(ebd_districts, select = c("COUNTY.CODE", "COUNTY"))

# Strip unwanted columns from eBird lists
ebd_lists <- subset(ebd_lists, select = c("STATE.CODE", "COUNTY.CODE", "OBSERVATION.DATE", "DURATION.MINUTES", "LONGITUDE", "LATITUDE", "UNIQUE_SAMPLING_ID", "ALL.SPECIES.REPORTED"))

#Unzip and open the shape file
shp_zip <- paste0('./data/', india_shape_file, '.zip')
shp_tmp <- tempfile()
unzip(shp_zip, exdir = shp_tmp)
indiamap <- readOGR(shp_tmp, india_shape_file)

sp::coordinates(ebd_lists) <- ~LONGITUDE+LATITUDE

# Map the CRS
# assume data from eBird are in WGS84
wgs84 <- CRS("+init=epsg:4326")
proj4string(ebd_lists) <- wgs84

# put ebd_lists in the same CRS as indiamap
ebd_lists <- spTransform(ebd_lists, CRS(proj4string(indiamap)))

# visual error check to see if list locations are within filter area
plot(ebd_lists)
plot(indiamap, add = TRUE)

ebd_filters <- data.frame(FILTER=character(),
                 stringsAsFactors=FALSE)

ebd_lists_with_filter <- NULL

for (filterindex in 1:nrow(indiamap@data))
{
  # Store filter metadata in another dataframe
  ebd_filters [filterindex, ] <- as.character(indiamap$FILTER[filterindex])
  head(ebd_filters,20)

  # Filter lists according to set filter polygons 
  india_selected  <- indiamap[filterindex, ]
  rgl_ebd_lists   <- ebd_lists
  rgl_ebd_lists$FILTER  <- 0;
  
  # special subset using polygon india_selected
  rgl_ebd_lists   <- rgl_ebd_lists[india_selected, ]
  
  # For all filtered lists, assign the filter_index
  rgl_ebd_lists$FILTER  <- filterindex;

  if(!is.null(ebd_lists_with_filter))
  {
    ebd_lists_with_filter <- rbind (ebd_lists_with_filter, rgl_ebd_lists)
  } else {
    ebd_lists_with_filter <- rgl_ebd_lists
  }
  
  filterindex  <- filterindex + 1
}

# Strip the list before joining
ebd_lists_with_filter <- subset(as.data.frame(ebd_lists_with_filter), select = c("UNIQUE_SAMPLING_ID", "FILTER"))

ebd_lists <- as.data.frame (ebd_lists)

# Join the filter assigned lists to the full lists. Remaining expected to be filter=0
ebd_lists <- join (ebd_lists, ebd_lists_with_filter, by = 'UNIQUE_SAMPLING_ID')

ebd_lists$FILTER[is.na(ebd_lists$FILTER)] <- 0

# Bug. Why join has 2 more than actual lists -- not seeing this in WI data

# Write to RDS file with compression
saveRDS(ebd_records,    './data/ebd_records.rds')
saveRDS(ebd_species,    './data/ebd_species.rds')
saveRDS(ebd_states,     './data/ebd_states.rds')
saveRDS(ebd_districts,  './data/ebd_districts.rds')
saveRDS(ebd_filters,    './data/ebd_filters.rds')
saveRDS(ebd_lists,      './data/ebd_lists.rds')

#Remove temp files
unlink(shp_tmp, recursive = TRUE)
