rm(list=ls())

library(pander)
library(data.table)
rawdata <- data.table(read.csv('/Users/pongnick/Desktop/database.csv'))
data<-rawdata

data[,Record.ID:=NULL]

data[,Agency.Code:=NULL]
data[,Agency.Name:=NULL]
data[,City:=NULL]
data[,Incident:=NULL]

library(descr)
freq(data$Perpetrator.Sex)
table(data$Perpetrator.Sex,data$Crime.Solved)
data[,Perpetrator.Sex:=NULL]
data[,Perpetrator.Age:=NULL]
data[,Perpetrator.Race:=NULL]
data[,Perpetrator.Ethnicity:=NULL]
data[,Relationship:=NULL]

data<-data[,Perpetrator.Count:=NULL]
data<-data[Victim.Count>0,]
data<-data[Victim.Age<99]

pander(summary(data))

library(ggplot2)
ggplot(data, aes(Agency.Type)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,40000,2500)) + 
  ggtitle("Distribution of Agency Types") +
  labs(x="Agency Type",y="Count") +
  theme_bw()

ggplot(data, aes(State)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,8500,500)) + 
  ggtitle("Incidents Per State") +
  labs(x="State",y="Count") +
  theme_bw()

ggplot(data, aes(Year)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,2000,200)) +
  scale_x_continuous(breaks=c(80:14)) +
  ggtitle("Incidents Per Year") +
  labs(x="Year",y="Count") +
  theme_bw()

ggplot(data, aes(Month)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,5000,500)) +
  ggtitle("Incidents Per Month") +
  labs(x="Month",y="Count") +
  theme_bw()

ggplot(data, aes(Crime.Type)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,52000,5000)) +
  ggtitle("Type of Crime") +
  labs(x="Type of Crime",y="Count") +
  theme_bw()

ggplot(data, aes(Victim.Sex)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,35000,5000)) +
  ggtitle("Sex of Victim") +
  labs(x="Sex of Victim",y="Count") +
  theme_bw()

ggplot(data, aes(Victim.Age)) +
  geom_histogram(bins=20,color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,10000,500)) +
  scale_x_continuous(breaks=seq(0,100,5)) +
  ggtitle("Age of Victim") +
  labs(x="Age of Victim (0 = 0-4, etc.)",y="Count") +
  theme_bw()

ggplot(data, aes(Victim.Race)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,32000,5000)) +
  ggtitle("Race of Victim") +
  labs(x="Race of Victim",y="Count") +
  theme_bw()

ggplot(data, aes(Victim.Ethnicity)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,32000,10000)) +
  ggtitle("Ethinicity of Victim") +
  labs(x="Ethnicity of Victim",y="Count") +
  theme_bw()

ggplot(data, aes(Weapon)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,30000,5000)) +
  ggtitle("Weapon Used in Incident") +
  labs(x="Weapon",y="Count") +
  theme_bw()

ggplot(data, aes(Victim.Count)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,40000,5000)) +
  scale_x_continuous(breaks=c(0:10)) +
  ggtitle("Victim Count Per Incident") +
  labs(x="Victim Count",y="Count") +
  theme_bw()

ggplot(data, aes(Record.Source)) +
  geom_bar(color='white',fill='darkgreen') + 
  scale_y_continuous(breaks=seq(0,52000,5000)) +
  ggtitle("Source of Record") +
  labs(x="Source",y="Count") +
  theme_bw()

library(h2o)

h2o.init(nthreads = 6) # Utilizing 6 cores of my processor
h2o.removeAll()

df<-data
h2o_d<-as.h2o(df) #Upload dataframe to h20

h2o_sd<- h2o.splitFrame(h2o_d, ratios = c(.6, 0.2),seed=4321) # Split the dataframe into training, validation and test sets
names(h2o_sd) <- c('train', 'valid', 'test')

#Random Forest
h2o.rm("RF") # Remove anything already 'RF'

RF <- h2o.grid(
  algorithm = "randomForest", 
  grid_id = "RF", # Establishing 'RF' as name
  hyper_params = list(max_depth=c(15,20,25),mtries=c(3,5,7)), # 15,20,25 -> Number of levels, 3,5,7 -> Number of predictors per tree
  training_frame = h2o_sd$train, # What is the training set
  validation_frame = h2o_sd$valid, # What is the validation set
  x=colnames(h2o_sd$train), # Which predictors to use
  y="Crime.Solved", # Prediction variable
  seed=4321,
  ntrees=100,
  nfolds=5 # Cross-Validation
)

RFm<-h2o.getGrid( # List of Random Forests sorted by AUC
  grid_id = "RF", 
  sort_by = "AUC",
  decreasing = TRUE
)

RFm
RFb<- h2o.getModel(RFm@model_ids[[1]]) # Get best model
RFb@parameters$max_depth # best parameter
RFb@parameters$mtries # best parameter

RFp<-h2o.performance(RFb,valid=T) # Extract performance
h2o.auc(RFp) # Get AUC
RFrp<-cbind(h2o.fpr(RFp),h2o.tpr(RFp)$tpr) # Get false/true positive rates
colnames(RFrp)[3]<-"tpr" # change variable name 
RFt<-h2o.performance(RFb,newdata = h2o_sd$test) # Same for test set 
RFrt<-cbind(h2o.fpr(RFt),h2o.tpr(RFt)$tpr)
h2o.auc(RFt)
colnames(RFrt)[3]<-"tpr"

RFev<-ggplot(h2o.F1(RFp)) +
  geom_line(aes(x=threshold,y=f1,color=threshold),size=1) +
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5) +
  labs(x="Threshold",y="F1 Metric")

RFet<-ggplot(h2o.F1(RFt)) + 
  geom_line(aes(x=threshold,y=f1,color=threshold),size=1) +
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5) +
  labs(x="Threshold",y="F1 Metric")
  
RFav<-ggplot(RFrp,aes(x=fpr,y=tpr)) +
  geom_line(aes(col=threshold),size=1) +
  labs(x="False Positive Rate",y="True Positive Rate") +
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5) +
  geom_segment(x=0,y=0,xend=1,yend=1,size=1,col="#00BFC4")

RFat<-ggplot(RFrt,aes(x=fpr,y=tpr)) +
  geom_line(aes(col=threshold),size=1) + 
  labs(x="False Positive Rate",y="True Positive Rate") +
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=1,col="#00BFC4")


grid.arrange(RFev,RFav,ncol=1)
grid.arrange(RFet,RFat,ncol=1)

ggplot(data.table(cbind(h2o.varimp(RFb)$variable[1:10],h2o.varimp(RFb)$scaled_importance[1:10],h2o.varimp(RFb)$relative_importance[1:10]))) +
  geom_col(aes(x=V1,y=as.numeric(V2),fill=as.numeric(V3))) +
  coord_flip() +
  scale_x_discrete(limits=rev(h2o.varimp(RFb)$variable[1:10])) +
  scale_y_continuous(breaks=seq(0,1,0.25)) +
  theme(axis.ticks=element_blank()) +
  labs(x="Variable",y="Relative Importance") +
  scale_fill_distiller(palette="Spectral",guide=F)

h2o.confusionMatrix(RFp,metric="min_per_class_accuracy")[,1:3]
h2o.confusionMatrix(RFt,metric="min_per_class_accuracy")[,1:3]

# GBM
h2o.rm("GBM") # Remove anything already GBM

GBM <- h2o.grid(
  algorithm = "gbm", 
  grid_id = "GBM",
  hyper_params = list(max_depth=c(4,5,6),learn_rate=c(0.06,0.07,0.08,0.09,0.1)), # 
  training_frame = h2o_sd$train,
  validation_frame = h2o_sd$valid,
  x=colnames(h2o_sd$train),
  y="Crime.Solved",
  seed=4321,
  ntrees=100,
  nfolds=5
)

GBMm<-h2o.getGrid(
  grid_id = "GBM", 
  sort_by = "AUC",
  decreasing = TRUE
)

GBMm
GBMb<- h2o.getModel(GBMm@model_ids[[1]])
GBMb@parameters$learn_rate
GBMb@parameters$max_depth

GBMp<-h2o.performance(GBMb,valid=T)
h2o.auc(GBMp)
GBMrp<-cbind(h2o.fpr(GBMp),h2o.tpr(GBMp)$tpr)
colnames(GBMrp)[3]<-"tpr"
GBMt<-h2o.performance(GBMb,newdata = h2o_sd$test)
GBMrt<-cbind(h2o.fpr(GBMt),h2o.tpr(GBMt)$tpr)
h2o.auc(GBMt)
colnames(GBMrt)[3]<-"tpr"

GBMev<-ggplot(h2o.F2(GBMp)) +
  geom_line(aes(x=threshold,y=f2,color=threshold),size=1) +
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5) +
  labs(x="Threshold",y="F2 Metric")

GBMet<-ggplot(h2o.F2(GBMt)) +
  geom_line(aes(x=threshold,y=f2,color=threshold),size=1) +
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5) +
  labs(x="Threshold",y="F2 Metric")

GBMav<-ggplot(GBMrp,aes(x=fpr,y=tpr)) +
  geom_line(aes(col=threshold),size=1) +
  labs(x="False Positive Rate",y="True Positive Rate") +
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5)+
  geom_segment(x=0,y=0,xend=1,yend=1,size=1,col="#00BFC4")

GBMat<-ggplot(GBMrt,aes(x=fpr,y=tpr)) +
  geom_line(aes(col=threshold),size=1) +
  labs(x="False Positive Rate",y="True Positive Rate") +
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5) +
  geom_segment(x=0,y=0,xend=1,yend=1,size=1,col="#00BFC4")


grid.arrange(GBMev,GBMav,ncol=1)
grid.arrange(GBMet,GBMat,ncol=1)

ggplot(data.table(cbind(h2o.varimp(GBMb)$variable[1:10],h2o.varimp(GBMb)$scaled_importance[1:10],h2o.varimp(GBMb)$relative_importance[1:10])))+
  geom_col(aes(x=V1,y=as.numeric(V2),fill=as.numeric(V3))) +
  coord_flip() +
  scale_x_discrete(limits=rev(h2o.varimp(GBMb)$variable[1:10])) +
  scale_y_continuous(breaks=seq(0,1,0.25)) +
  theme(axis.ticks=element_blank()) +
  labs(x="Variable",y="Relative Importance") +
  scale_fill_distiller(palette="Spectral",guide=F)

h2o.confusionMatrix(GBMp)[,1:3]
h2o.confusionMatrix(GBMt)[,1:3]

h2o.shutdown()

















library(randomForest)
set.seed(123)
N<-nrow(data)
id<-sample(1:N,0.7*N)
d_train<-data[id]
d_test<-data[-id]

md <- randomForest(Crime.Solved ~ ., data = d_train, ntree = 300, importance = TRUE)
varImpPlot(md, type = 2)
plot(md)
table(predict(md,d_test),d_test$Crime.Solved)
phat<-predict(md,d_test,type="prob")[,"Yes"]
library(ROCR)
library(ggplot2)

rocr_obj <- prediction(phat, d_test$Crime.Solved)
plot(performance(rocr_obj, "err"))    
performance(rocr_obj, "auc")@y.values[[1]]
plot(performance(rocr_obj, "tpr", "fpr"), colorize=TRUE) 
