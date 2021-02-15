library("dplyr")
library(sva)



###############################################################

# files <- list.files(path="test-data/dnaDamage-1/", pattern="*._ch-0.txt", full.names=TRUE, recursive=FALSE)
# for (ch0_file in files){
#   if (!exists("dataset")){
#     dataset <- read.table(ch0_file, header=TRUE, sep="\t", stringsAsFactors=FALSE)
#   }
# 
#   if (exists("dataset")){
#     temp_dataset <-read.table(ch0_file, header=TRUE, sep="\t", stringsAsFactors=FALSE,strip.white=TRUE)
#     dataset<-rbind(dataset, temp_dataset)
#     rm(temp_dataset)
#   }
# }

dataset = read.table(file='test-data/dnaDamage-test.txt', sep='\t', header = TRUE,strip.white=TRUE,stringsAsFactors=FALSE)


#split original features matrix into two matrix, metadata and features

metadata <- dataset %>% select (imageID,Control,screen,plate,tp,ch)
features <- dataset %>% select (-imageID,-Control,-screen,-tp,-ch,-plate)

#scale features matrix
features_m <-as.matrix(scale(features))

#batch variable vector
batch<- metadata %>% select(plate)

#print(batch)

#features matrix used for combat function.
sva_features <- t(features_m)

batch = as.factor(batch$plate)

#bind plate back to features matrix
with_plate <- cbind(as.data.frame(features_m),batch)

#assign plate as name
#rownames(with_plate)[nrow(with_plate)] <- "batch"

mod = model.matrix(~1,data=with_plate)


#apply combat function
combat_data = ComBat(dat=with_plate,batch=batch,mod=mod,par.prior=TRUE)

#export comabat_data
#combat_data <- as.data.frame(t(combat_data)) %>% select (-batch)

#write.table(cbind(metadata,combat_data),file="combat_features.txt", sep="\t", row.names = FALSE)









