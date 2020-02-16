library(raster)


# Import phenologicla data
dat_pheno <- read.csv("./Data/herbarium/occurrences.csv")

# Keep only georeferenced records
dat_pheno <-  dat_pheno[complete.cases(dat_pheno[,c("decimalLatitude","decimalLongitude")]), ]


#
# MERGE SEASONAL VARIABLES
#

# Note: we can expand this simply by adding more names to the var_Seasonal vector





var_seasonal <- c("springtmean","winterppt")


# Loop through each seasonal variable
for(var in var_seasonal){
  
# Create list of file names to import rasters
raster_list <- data.frame(matrix(nrow = length(1896:2018), ncol = 2))
raster_list[,1] <- 1896:2018
raster_list[,2] <- paste(paste(var,"_",1896:2018,sep = ""), ".gri",sep = "")
colnames(raster_list) <- c("year","file_name")

# Loop through each year in pheno dataset
for (i in dat_pheno$year){
  
  # If year in pheno dataset is out of bounds, go to next year
  if(i>2014|i<1895) {next}
  
  # Import raster corresponding to variable var and year i
  raster_var_i <- raster(paste(getwd(),"/","Data/prism_data/seasonal/",var,"/",raster_list[raster_list$year==i,"file_name"],sep = "") )
  
  # Extract value of seasonal variable from raster and store in correspnding rows if pheno dataset
  dat_pheno[dat_pheno$year==i, var] <- extract(x=raster_var_i, y= dat_pheno[dat_pheno$year==i,c("decimalLongitude","decimalLatitude")])
  
} # Close year loop

} # Close variable loop





#
# MERGE ANNUAL VARIABLES  
#


var_annual <- c("tmax", "tmean","tmin","ppt")

for(var in var_annual){
  
  # PPT data changes from M2 to M3 data in 1981, so all file names change. 
  # This conditional statement breaks the raster_list into two periods to produce the right file names.
  
  if(var=="ppt"){
    
    # Create list of file names to import rasters
    raster_list <- data.frame(matrix(nrow = length(1895:2014), ncol = 3))
    raster_list[,1] <- 1895:2014
    raster_list[1:length(1895:1980),2] <- paste("PRISM_",var,"_stable_","4kmM2_",1895:1980,"_","bil", sep="")
    raster_list[length(1895:1981):length(1895:2014),2] <- paste("PRISM_",var,"_stable_","4kmM3_",1981:2014,"_","bil", sep="")
    raster_list[,3] <- paste(raster_list[,2],".bil",sep="")
    colnames(raster_list) <- c("year","folder","file_name")
    
  } else{
  
  # Create list of file names to import rasters
  raster_list <- data.frame(matrix(nrow = length(1895:2014), ncol = 3))
  raster_list[,1] <- 1895:2014
  raster_list[,2] <- paste("PRISM_",var,"_stable_","4kmM3_",1895:2014,"_","bil", sep="")
  raster_list[,3] <- paste(raster_list[,2],".bil",sep="")
  colnames(raster_list) <- c("year","folder","file_name")
  
  }
  
  for(i in dat_pheno$year){
    
    # If year in pheno dataset is out of bounds, go to next year
    if(i>2014|i<1895) {next}
    
    raster_var_i <- raster(paste(getwd(),"/Data/","prism_data/","annual/", var,"/",
                                 raster_list[raster_list$year==i,"folder"],"/",
                                 raster_list[raster_list$year==i,"file_name"], sep = ""))
                           
    dat_pheno[dat_pheno$year==i,paste(var,"_annual",sep="")] <- extract(x=raster_var_i, y= dat_pheno[dat_pheno$year==i,c("decimalLongitude","decimalLatitude")])

    
    
  }
  
}


# Check that values make sense
View(dat_pheno[,c("year","tmax_annual","tmean_annual","tmin_annual","springtmean","winterppt")])






