## Match up column headers, keeping ALL columns instead of just matching ones (fills added columns with NAs)
# SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
  ## Example use of rbind.all.columns function:
    # Create a list of all dataframes you want to stack
    # 'Reduce' iterates through list and merges with previous dataframe in the list
      #all_data <- Reduce(rbind.all.columns, file_dfs_list)
rbind.all.columns <- function(x, y) {
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}



# read in species list
drive_download("IMLS_sp_list_joined.csv",type="csv",overwrite=T) # download from Google Drive
sp_list <- read.csv("IMLS_sp_list_joined.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
sp_names <- sp_list[,2] # create list of target species names

# pull occurrence data
occur <- occ(query = sp_names, from = c("gbif","bison","ecoengine","idigbio"), limit = 2, has_coords = TRUE)

# create dataframe of gbif data
gbif_data_ls <- occur$gbif[[2]] # list of gbif occurrence data frames, one for each species
gbif_data <- Reduce(rbind.all.columns, gbif_data_ls) # 'Reduce' iterates through list and merges with previous element in list
  nrow(gbif_data) #
  colnames(gbif_data)
  unique(gbif_data$name) # species with data
# create dataframe of bison data
bison_data_ls <- occur$bison[[2]] # list of gbif occurrence data frames, one for each species
bison_data <- Reduce(rbind.all.columns, bison_data_ls) # 'Reduce' iterates through list and merges with previous element in list
  nrow(bison_data) #
  colnames(bison_data)
  unique(bison_data$name) # species with data
# create dataframe of ecoengine data
eco_data_ls <- occur$eco[[2]] # list of gbif occurrence data frames, one for each species
eco_data <- Reduce(rbind.all.columns, eco_data_ls) # 'Reduce' iterates through list and merges with previous element in list
  nrow(eco_data) #
  colnames(eco_data)
  unique(eco_data$name) # species with data
# create dataframe of ecoengine data
dig_data_ls <- occur$idigbio[[2]] # list of gbif occurrence data frames, one for each species
dig_data <- Reduce(rbind.all.columns, dig_data_ls) # 'Reduce' iterates through list and merges with previous element in list
  nrow(dig_data) #
  colnames(dig_data)
  unique(dig_data$name) # species with data

### ?? make a function or loop to do the steps above ??






###
##### D. BISON
###

# read in species list
sp_list <- read.csv("sp_list_joined.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
sp_names <- sp_list[,2] # create list of target species names

# pull occurrence data using small limit, just to see which species have data (otherwise will take too long)
occur <- occ(query = sp_names, from = "bison", limit = 5)

# list of data frames, one for each species
bison_data_ls <- occur$bison$data; length(bison_data_ls)

# create list of species that have BISON data
have_data <- NA
place <- 1
  for(i in 1:length(bison_data_ls)){
    if(typeof(bison_data_ls[[i]]$name) == "character"){
      have_data[place] <- bison_data_ls[[i]]$name
      place <- place+1
    }
  }; str(have_data)

# pull all occurrence data for species with BISON data
for(i in 1:length)
occur2 <- occ(query = have_data[1], from = "bison", limit = 14049)

bison_data_ls2 <- occur2$bison$data; str(bison_data_ls2)


# Function to match up column headers, keeping all columns, not just matching ones (fills added columns with NAs)
    # SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- ""
    y[, c(as.character(x.diff))] <- ""
    return(rbind(x, y))
}

#bison_data <- Reduce(rbind.all.columns, bison_data_ls) # 'Reduce' iterates through list and merges with previous element in list
  nrow(bison_data) #
  colnames(bison_data)
  unique(bison_data$name) # species with data

length(bison_data_ls)
length(have_data)
