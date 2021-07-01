##Random forest 250621

library(rgdal)
library(randomForest)
require(caTools)
library(raster)
library(rasterVis)
library(mapview)
library(caret)

path = "C:/Users/Elizabeth.Wiley/OneDrive - Essex County Council/Data accelerator_Geospatial data/rasters_v2"
setwd(path)

raster_ag <- raster( 'raster_ag.tif')
raster_ag[is.na(raster_ag)] = 0
rast_fig2GI <- raster(path, 'raster_fig2.tif')
rast_fig2GI[is.na(rast_fig2GI)] = 0
rast_majorsites <- raster('raster_majorsitesfordevelopment.tif')
rast_majorsites[is.na(rast_majorsites)] = 0
sssi_raster <- raster('raster_sssi.tif')
sssi_raster[is.na(sssi_raster)] = 0
watercourse_raster <- raster('raster_watercourselink.tif')
watercourse_raster[is.na(watercourse_raster)] = 0
labels <- raster( 'labels.tif')
labels[is.na(labels)] = 0

summary(raster_ag)
raster_ag <- as.matrix(raster_ag, xy = TRUE)
rast_fig2GI <- as.matrix(raster_ag, xy = TRUE)
rast_majorsites <- as.matrix(raster_ag, xy = TRUE)
sssi_raster <- as.matrix(raster_ag, xy = TRUE)
watercourse_raster <- as.matrix(raster_ag, xy = TRUE)
labels <- as.matrix(raster_ag, xy = TRUE)

# there are better ways of doing this I'm just out of time!!!
first = TRUE          
for(r in 1:nrow(raster_ag)){
  print(r)
  for(c in 1:ncol(raster_ag)){
    vector_array <- as.array(cbind(raster_ag[r,c], rast_fig2GI[r,c], rast_majorsites[r,c], sssi_raster[r,c], watercourse_raster[r,c], labels[r,c]))
    label_array <- as.array(labels[r,c])
    if(first==TRUE){
      first <- FALSE
      final_vector_array = vector_array
      final_label_array = label_array
    }else{
      final_vector_array = rbind(final_vector_array, vector_array)
      final_label_array = rbind(final_label_array, label_array)
    }
    
  }
}

#
nat_green_df = as.data.frame(final_vector_array)
#
colnames(nat_green_df) <- c("raster_ag","rast_fig2GI", "rast_majorsites", "sssi_raster", "watercourse_raster", "labels")#

#turn labels column into binary
nat_green_df$labels<-ifelse(nat_green_df$labels=="0",0,1)

summary (nat_green_df$labels)
#
set.seed(42)
#Subset data into train and test. Since the draw is unfair, we must attach specific probabilities 
#to the values 0 and 1 with a fourth argument, prob = c(0.2, 0.8) 
ind = sample(2, nrow(nat_green_df), replace=TRUE, prob=c(0.05,0.95))
#
trainData = nat_green_df[ind==1,]
#
testData = nat_green_df[ind==2,]

#rf regression
nat_green_rf = randomForest(labels~., data=trainData, ntree=10, proximity=T)


#
table(predict(nat_green_rf), trainData$labels)

#random forest
X = final_vector_array
y = final_label_array

X = nat_green_df
y=table(predict(nat_green_rf), trainData$labels)
#
set.seed(42)
test_inds = createDataPartition(y = 1:length(y), p = 0.2, list = F) #

# Split data into test/train using indices
X_test = X[test_inds, ]; y_test = y[test_inds] 
X_train = X[-test_inds, ]; y_train = y[-test_inds]

rfc <- randomForest(nat_green_df,  ntree=500)
rfc
plot(rfc)

#random code
lidar_dem <- raster(x = "data/week-03/BLDR_LeeHill/pre-flood/lidar/pre_DTM.tif")

#subsetting training data - if vector too large (> 20000)
rf1 <- randomForest(labels ~ ., data =trainData[1:14063,], ntree=10, proximity=T)
rf2 <- randomForest(labels ~ ., data =trainData[14064:28126,], ntree=10, proximity=T)
nat_green_rf <- combine(rf1,rf2)

