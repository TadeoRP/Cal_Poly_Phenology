library(raster)
library(lubridate)

# Import phenologicla data
dat_pheno <- read.csv("./Data/herbarium/occurrences.csv")

# Keep only georeferenced records with DOY and YEAR
dat_pheno <-  dat_pheno[complete.cases(dat_pheno[,c("decimalLatitude","decimalLongitude","startDayOfYear","year")]), ]


#
# MERGE SEASONAL VARIABLES
#

# Note: we can expand this simply by adding more names to the var_Seasonal vector





var_seasonal <- c("winterppt","wintertmin","wintertmean","wintertmax","springppt","springtmin","springtmean","springtmax")


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







#
# MERGE VARIABLES IN MONTH OF COLLECTION (MOC)
#


# Create lists to match DOY with a month for leap and non-leap years
month_leap <- data.frame(matrix(NA, nrow=366, ncol = 2))
month_nonleap <- data.frame(matrix(NA, nrow=365, ncol = 2))

colnames(month_leap) = colnames(month_nonleap) = c("DOY","month")

month_leap$DOY <- 1:366
month_nonleap$DOY <- 1:365


month_leap$month <-    c(rep("01", 31),rep("02", 29),rep("03", 31),
                         rep("04", 30),rep("05", 31),rep("06", 30),
                         rep("07", 31),rep("08", 31),rep("09", 30),
                         rep("10", 31),rep("11", 30),rep("12", 31))

month_nonleap$month <-    c(rep("01", 31),rep("02", 28),rep("03", 31),
                             rep("04", 30),rep("05", 31),rep("06", 30),
                             rep("07", 31),rep("08", 31),rep("09", 30),
                             rep("10", 31),rep("11", 30),rep("12", 31))








var_monthly <- c("ppt","tmin","tmax","tmean")

for(var in var_monthly){
  
  if(var == "ppt"){
    
 # Create list of raster folders and files
 # data frame
 raster_list <- data.frame(matrix(NA, nrow = length(1895:2014)*12, ncol = 4))
 # Fill in years
 raster_list[,1] <- rep(1895:2014,each=12)
 # Fill in months
 raster_list[,2] <- rep(c("01","02","03","04","05","06","07","08","09","10","11","12"), length(1895:2014))
 
 # Fill in folder names
 
 # 1895:1980: M2
 raster_list[1:max(which(raster_list[,1]==1980)),3] <- 
   
   paste("PRISM_",var,"_stable_4kmM2_",
          as.vector(t(outer(1895:1980, c("01","02","03","04","05","06","07","08","09","10","11","12"), paste, sep=""))),
         "_bil",sep = "")

 # 1981:2014: M3
 raster_list[min(which(raster_list[,1]==1981)):nrow(raster_list),3] <-
   
   paste("PRISM_",var,"_stable_4kmM3_",
         as.vector(t(outer(1981:2014, c("01","02","03","04","05","06","07","08","09","10","11","12"), paste, sep=""))),
         "_bil",sep = "")
 
 # Fill in raster file names
 raster_list[,4] <- paste(raster_list[,3],".bil",sep="")
 
 colnames(raster_list) <- c("year","month","folder","file_name")
  
  } 
  
  else { 
    raster_list <- data.frame(matrix(NA, nrow = length(1895:2014)*12, ncol = 4))
    raster_list[,1] <- rep(1895:2014,each=12)
    raster_list[,2] <- rep(c("01","02","03","04","05","06","07","08","09","10","11","12"), length(1895:2014))
    raster_list[,3] <- 
      
      paste("PRISM_",var,"_stable_4kmM3_",
            as.vector(t(outer(1895:2014, c("01","02","03","04","05","06","07","08","09","10","11","12"), paste, sep=""))),
            "_bil",sep = "")

    raster_list[,4] <- paste(raster_list[,3],".bil",sep="")
    
    colnames(raster_list) <- c("year","month","folder","file_name")
    
    }
  
  

for(i in 1:nrow(dat_pheno)){
  
  if(leap_year(dat_pheno[i,"year"])==TRUE){month_i <- month_leap[month_leap$DOY==dat_pheno[i,"startDayOfYear"], "month"]} 
  
  else{month_i <- month_nonleap[month_nonleap$DOY==dat_pheno[i,"startDayOfYear"], "month"]}
  
  if(dat_pheno[i,"year"]>2014|dat_pheno[i,"year"]<1895) {next}
  
  raster_var_i <- raster(paste(getwd(),"/Data/","prism_data/","monthly/", var,"/",
                               raster_list[raster_list$year==dat_pheno[i,"year"] & raster_list$month == month_i,"folder"],"/",
                               raster_list[raster_list$year==dat_pheno[i,"year"] & raster_list$month == month_i,"file_name"], sep = ""))
  
  
  dat_pheno[i,paste(var,"MOC",sep = "_")] <- extract(x=raster_var_i, y= dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  
}

}


# Format seasonal variables as "var"_"season"
colnames(dat_pheno)[which(colnames(dat_pheno) %in% var_seasonal)] <- 
  c("ppt_wt","tmin_wt","tmean_wt","tmax_wt","ppt_sp","tmin_sp","tmean_sp","tmax_sp")








#  
# MERGE NORMALS
# 

#
# Annual/seasonal
#


for(i in 1:nrow(dat_pheno)){
  
  #
  # Annual
  #
  
  ppt_normal_annual <- raster(paste(getwd(),"/Data/prism_data/normals/ppt/ppt_normal_annual",sep = ""))
  tmin_normal_annual <- raster(paste(getwd(),"/Data/prism_data/normals/tmin/tmin_normal_annual",sep = ""))
  tmean_normal_annual <- raster(paste(getwd(),"/Data/prism_data/normals/tmean/tmean_normal_annual",sep = ""))
  tmax_normal_annual <- raster(paste(getwd(),"/Data/prism_data/normals/tmax/tmax_normal_annual",sep = ""))
  
  dat_pheno[i,"ppt_norm_annual"] <- extract(x=ppt_normal_annual, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmin_norm_annual"] <- extract(x=tmin_normal_annual, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmean_norm_annual"] <- extract(x=tmean_normal_annual, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmax_norm_annual"] <- extract(x=tmax_normal_annual, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  
  #
  # Seasonal
  #
  
  # winter
  ppt_normal_wt <- raster(paste(getwd(),"/Data/prism_data/normals/ppt/winterppt_normal_seasonal",sep = ""))
  tmin_normal_wt <- raster(paste(getwd(),"/Data/prism_data/normals/tmin/wintertmin_normal_seasonal",sep = ""))
  tmean_normal_wt <- raster(paste(getwd(),"/Data/prism_data/normals/tmean/wintertmean_normal_seasonal",sep = ""))
  tmax_normal_wt <- raster(paste(getwd(),"/Data/prism_data/normals/tmax/wintertmax_normal_seasonal",sep = ""))
  
  dat_pheno[i,"ppt_norm_wt"] <- extract(x=ppt_normal_wt, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmin_norm_wt"] <- extract(x=tmin_normal_wt, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmean_norm_wt"] <- extract(x=tmean_normal_wt, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmax_norm_wt"] <- extract(x=tmax_normal_wt, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  
  
  # Spring
  ppt_normal_sp <- raster(paste(getwd(),"/Data/prism_data/normals/ppt/springppt_normal_seasonal",sep = ""))
  tmin_normal_sp <- raster(paste(getwd(),"/Data/prism_data/normals/tmin/springtmin_normal_seasonal",sep = ""))
  tmean_normal_sp <- raster(paste(getwd(),"/Data/prism_data/normals/tmean/springtmean_normal_seasonal",sep = ""))
  tmax_normal_sp <- raster(paste(getwd(),"/Data/prism_data/normals/tmax/springtmax_normal_seasonal",sep = ""))
  
  dat_pheno[i,"ppt_norm_sp"] <- extract(x=ppt_normal_sp, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmin_norm_sp"] <- extract(x=tmin_normal_sp, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmean_norm_sp"] <- extract(x=tmean_normal_sp, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  dat_pheno[i,"tmax_norm_sp"] <- extract(x=tmax_normal_sp, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  
  }
  
  
#
# Month of Collection (MOC)  
#

vars <- c("ppt","tmin","tmean","tmax")

for(var in vars){
  
  raster_list <- data.frame(matrix(NA, nrow=12, ncol=3))
  raster_list[,1] <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  raster_list[,2] <- paste(getwd(),"/Data/prism_data/normals/",var,"/",var,"_normal",raster_list[,1],sep = "")
  
  colnames(raster_list) <- c("month","file_name")
  


for(i in 1:nrow(dat_pheno)){
  
  if(leap_year(dat_pheno[i,"year"])==TRUE){month_i <- month_leap[month_leap$DOY==dat_pheno[i,"startDayOfYear"], "month"]} 
  
  else{month_i <- month_nonleap[month_nonleap$DOY==dat_pheno[i,"startDayOfYear"], "month"]}
  
  raster_i <- raster(raster_list$file_name[raster_list$month==month_i])
    
  dat_pheno[i,paste(var,"_norm_MOC",sep = "")] <- extract(x = raster_i, y = dat_pheno[i,c("decimalLongitude","decimalLatitude")])
  
  
  
}  
}










#
# COMPUTE ANOMALIES: vars - norms
#








# New variables
dat_pheno[, paste(c("ppt_wt","tmin_wt","tmean_wt","tmax_wt","ppt_sp","tmin_sp","tmean_sp","tmax_sp", 
      "ppt_annual","tmin_annual","tmean_annual","tmax_annual",
      "ppt_MOC","tmin_MOC","tmean_MOC","tmax_MOC"), "_anom",sep = "")] <-
  
  
  # Vars in focal year, season, month
  dat_pheno[,c("ppt_wt","tmin_wt","tmean_wt","tmax_wt","ppt_sp","tmin_sp","tmean_sp","tmax_sp", 
            "ppt_annual","tmin_annual","tmean_annual","tmax_annual",
            "ppt_MOC","tmin_MOC","tmean_MOC","tmax_MOC")] - # minus
  
  # Norms
  dat_pheno[,c("ppt_norm_wt","tmin_norm_wt","tmean_norm_wt","tmax_norm_wt","ppt_norm_sp","tmin_norm_sp","tmean_norm_sp","tmax_norm_sp",
               "ppt_norm_annual","tmin_norm_annual","tmean_norm_annual","tmax_norm_annual",
               "ppt_norm_MOC","tmin_norm_MOC","tmean_norm_MOC","tmax_norm_MOC")]





#
# View pheno data with all new variables
#

View(dat_pheno[,c("year","startDayOfYear","ppt_wt","tmin_wt","tmean_wt","tmax_wt","ppt_sp","tmin_sp","tmean_sp","tmax_sp", 
                  "ppt_annual","tmin_annual","tmean_annual","tmax_annual",
                  "ppt_MOC","tmin_MOC","tmean_MOC","tmax_MOC", "ppt_norm_annual","tmin_norm_annual","tmean_norm_annual",
                  "tmax_norm_annual","ppt_norm_wt","tmin_norm_wt","tmean_norm_wt","tmax_norm_wt",
                  "ppt_norm_sp","tmin_norm_sp","tmean_norm_sp","tmax_norm_sp",
                  "ppt_norm_MOC","tmin_norm_MOC","tmean_norm_MOC","tmax_norm_MOC",
                  paste(c("ppt_wt","tmin_wt","tmean_wt","tmax_wt","ppt_sp","tmin_sp","tmean_sp","tmax_sp", 
                           "ppt_annual","tmin_annual","tmean_annual","tmax_annual",
                            "ppt_MOC","tmin_MOC","tmean_MOC","tmax_MOC"), "_anom",sep = ""))])



#
# Write csv file with phenological and climate data to use later
#

write.csv(dat_pheno, "phenoclim data.csv")

