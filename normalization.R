library("dplyr")
library(sva)
library(janitor)
library(ggplot2)
source("common.R")
require(graphics)


###############################################################

files <- list.files(path="features/dnaDamage/", pattern="*._ch-0.txt", full.names=TRUE, recursive=FALSE)
for (ch0_file in files){
  if (!exists("dataset")){
    dataset <- read.table(ch0_file, header=TRUE, sep="\t", stringsAsFactors=FALSE)
  }

  if (exists("dataset")){
    temp_dataset <-read.table(ch0_file, header=TRUE, sep="\t", stringsAsFactors=FALSE,strip.white=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

#dataset = read.table(file='test-data/dnaDamage-test.txt', sep='\t', header = TRUE,strip.white=TRUE,stringsAsFactors=FALSE)


# split original features matrix into two matrix, metadata and features

metadata <- dataset %>% select (imageID,Control,screen,plate,tp,ch)
features <- dataset %>% select (-imageID,-Control,-screen,-tp,-ch,-plate)

# function remove zero variance column
removeZeroVar <-function(features){
  features[, sapply(features,var) > 1e-5]  
}

features <- removeZeroVar(features)

# remove columns with more than 5% NAs
features <- features[, which(colMeans(!is.na(features))>0.05)]

# Standardise features
features <-as.matrix(scale(features))


# batch variable
batch<- as.factor(metadata$plate)

# Create the model for combat,the data should have samples (i.e. images) in rows and features in columns
mod = model.matrix(~1,data=data.frame(features))


# apply combat function
combat_data = ComBat(dat=t(features),batch=batch,mod=mod,par.prior=TRUE)

# add metadata back
combat_data <- data.frame(metadata,t(combat_data))

# extract controls
ctrl <- combat_data[!(combat_data$Control=="0"),]
nctrl <- combat_data[!(combat_data$Control=="-"),]

ctrl_features <-ctrl %>% select(-imageID,-Control,-screen,-tp,-ch,-plate)
ctrl_metadata <-ctrl %>% select(imageID,Control,screen,tp,ch,plate)

nctrl_features <-nctrl %>% select(-imageID,-Control,-screen,-tp,-ch,-plate)


# PCA on controls
pca_ctrl <- prcomp(t(ctrl_features),scale.=T, center=T)
pca_eigenvalues <- pca_ctrl$sdev^2
no_of_selected_features <- get_elbow_ofDistribution(1:length(pca_eigenvalues),pca_eigenvalues)



screeplot(pca_ctrl)

pca_nctrl <- prcomp(t(nctrl_features),scale.=T, center=T)

#predict(pca_ctrl,newdata = nctrl_features)


# variance explained, first PC 21.5%, second 8%
# summary(pca)$importance[2,]
# var_explained <-pca$sdev^2/sum(pca$sdev^2)
# var_explained [1:10]
# pca$center[1:5]
# pca$x[1:5,1:5]

# plot using first 2 components for all
# pca$x %>% 
#   as.data.frame %>%
#   ggplot(aes(x=PC1,y=PC2)) + geom_point(size=4) +
#   theme_bw(base_size=32) + 
#   labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
#        y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
#   theme(legend.position="top")


#write.table(combat_data,file="combat_features.txt", sep="\t", row.names = FALSE)






###non-controls



