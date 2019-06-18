#########################################################################
### Prediction of House Prices with Advanced Regression Techniques ######
#########################################################################  
#########################################################################

#Installing Packages

for (i in c('caret', 'pROC', 'randomForest', 'plotly', 'ggthemes','plm', 'party','caTools', 'rcompanion', 'MASS','modelr','glmnet','gbm', 'scales', 'randomForestExplainer','ROCR','outliers', 'ggplot2', 'ggcorrplot', 'reshape2', 'Hmisc', 'stringr', 'mosaic',  'MLmetrics' ,'tidyverse')){
  if (!require(i, character.only=TRUE)) install.packages(i, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

#---------------------#
# Reading in the data #
#---------------------#

house_train<- read.csv('C:/Users/dkrystallidou/Documents/Statistical Machine Learning/house-prices-advanced-regression-techniques/train.csv')
house_test<- read.csv('C:/Users/dkrystallidou/Documents/Statistical Machine Learning/house-prices-advanced-regression-techniques/test.csv')

#inspecting the data: structure, existence of missing values
str(house_train)
summary(house_train)

#check missing values per column
sapply(house_train, function(x) sum(is.na(x)))
# 19 variables contain missing values 

#inspecting the target variable to determine skeweness
plotNormalHistogram(house_train$SalePrice)

#Since the data are strictly positive and has a minimul value of 0, it is log normally distributed
#Hence, we adjust for the skeweness by transforming data using the logarithmic function.
house_train$SalePrice_Log<- log(house_train$SalePrice)
plotNormalHistogram(house_train$SalePrice_Log)

#--------------------#
# Data Visualisation #
#--------------------#


# Defining functions for visualisation: Bar plot/Density plot/ multiplot functions
#to be used in the univariate analysis.


# Bar plot function

plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}


## Multiple Bar plot function

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


# Multiple plot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
  
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
      
    }
  }
}

#---------------------#
# Univariate Analysis #
#---------------------#

# Inspecting Data to identify trends & outliers

x<- house_train$MSSubClass
ggplot(house_train, aes(x=LotArea, y=SalePrice)) + 
  geom_point()


# Focusing on property size/area.  Expecting a positive relationship with Sale Price

# GrLivArea - SalePrice
p1 <- subset(house_train, !is.na(GrLivArea))
p1 <- ggplot(p1, aes(GrLivArea, SalePrice)) + geom_point(color = 'blue') +scale_y_continuous(labels=dollar_format())+ theme_bw()
p1

# LotArea - SalePrice
p2 <- subset(house_train, !is.na(LotArea))
p2 <- ggplot(p2, aes(LotArea, SalePrice)) + geom_point(color = 'blue') +scale_y_continuous(labels=dollar_format())+ theme_bw()
p2

# LotFrontage - SalePrice
p3 <- subset(house_train, !is.na(LotFrontage))
p3 <- ggplot(p3, aes(LotFrontage, SalePrice)) + geom_point(color = 'blue') +scale_y_continuous(labels=dollar_format())+ theme_bw()
p3

# GarageArea - SalePrice
p4 <- subset(house_train, !is.na(GarageArea))
p4 <- ggplot(p4, aes(GarageArea, SalePrice)) + geom_point(color = 'blue') +scale_y_continuous(labels=dollar_format())+ theme_bw()
p4

# Plotting all the variables together for comparison.
multiplot(p1, p2, p3, p4, cols=2)


#Plotting: Property Size/Area vs Price Per Neighborhood

ggplot(house_train, aes(GrLivArea, SalePrice)) + geom_point(aes(color = Neighborhood)) + 
  scale_x_continuous("GrLivArea") +
  scale_y_continuous(labels=dollar_format())+
  theme_bw() + facet_wrap( ~ Neighborhood) + theme(legend.position="none")


#Price Per BuildYear Relationship, Is price increasing with year_built?

house_train[!is.na(house_train$SalePrice),] %>% 
  group_by(YearBuilt) %>% 
  summarize(mean_build_price=mean(SalePrice)) %>%
  ggplot(aes(x=YearBuilt, y=mean_build_price))+
  geom_point(color="darkred",size=3)+
  geom_smooth(method="lm",color='darkgrey')+
  labs(x="Year",y="Mean Price",title="Price per Year relationship")+
  scale_y_continuous(labels=dollar_format())+
  theme_minimal()


#Data Visualization 

cat_var <- names(house_train)[which(sapply(house_train, is.factor))]
numeric_var <- names(house_train)[which(sapply(house_train, is.numeric))]


## Creating one training dataset with categorical variable and one with numeric variable. We will use this for data visualization.

train1_cat<-house_train[cat_var]
train1_num<-house_train[numeric_var]

## Barplots for the categorical features

doPlots(train1_cat, fun = plotHist, ii = 1:4, ncol = 2)
doPlots(train1_cat, fun = plotHist, ii  = 5:8, ncol = 2)
doPlots(train1_cat, fun = plotHist, ii = 9:12, ncol = 2)
doPlots(train1_cat, fun = plotHist, ii = 13:18, ncol = 2)
doPlots(train1_cat, fun = plotHist, ii = 19:22, ncol = 2)


#BoxPlot Neighborhood Vs Sales Price

ggplot(house_train, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

#The boxplot between the neighboorhoods and sale price shows that BrookSide and South & West of Iowa State University 
# have less expensive houses. Conversely, Heights, StoneBr & NoRidge are
# rich neighborhoods with several outliers in terms of price.

#Histograms for the numeric variables

doPlots(train1_num, fun = plotHist, ii = 18:23, ncol = 2)

#MO Sold Analysis

analysedata <- house_train %>% group_by(MoSold) %>% summarise(Count = n())

analysedata$MoSold <- as.factor(analysedata$MoSold)

ggplot(data=analysedata, aes(x=MoSold, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

# There are more number of sales in April, May, June and July, which may indicate some seasonality pattern.
# Conversely, in September, December, January and February there are less house sales.


#MS Sub Analysis

analysedata <- house_train %>% group_by(MSSubClass) %>% summarise(Count = n())
analysedata$MSSubClass <- as.factor(analysedata$MSSubClass)
ggplot(data=analysedata, aes(x=MSSubClass, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

#----------------------#
# Multivariate Analysis#
#----------------------#

#Correlation heatmap: observe correlations between variables 

#checking which variables from the train dataset are numeric
numeric<- unlist(lapply(house_train, is.numeric))  

# subsetting dataset: including numeric variables
numeric_vars<-house_train[,numeric]

# remove missing values to calculate correlations
numeric_vars<-remove_missing(numeric_vars)

#compute correlations between numeric variables
corr<- round(cor(numeric_vars), 2)
corr

#ggcorrplot(corr, hc.order = TRUE, type = "lower",
#           outline.col = "white")

# Computing Correlation Matrix: identify correlations between variables
library(ggplot2)
library(reshape2)
ggplot(melt(corr), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red") +
  coord_equal()+
  theme(axis.text.x = element_text(color = "grey20", size = 7, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 7, angle = 0, hjust = 1, vjust = 0, face = "plain"))
      
# test correlations insignificant: check the p values.
rcorr<-rcorr(as.matrix(numeric_vars), type= 'pearson')

# From the correlation matrix and the results of the rcorr function, 
# we can make the following observations:
# 1. there are many predictor variables that correlate. YearBuilt-OverallQual(0.59, p=), YearBuilt-YearRemodAdd (0.62), BsmtFullBath-BsmtFinSF1(0.65), TotalBsmtSF-X1stFlrSF .84, X2ndFlrSF-GrLivArea .69
#We need to correct for that, as this can give rise to multicollinearity problems. 
# 2. There are variables that correlate with Saleprice, indicating that they will have 
# important predictive power: eg. OverallQual-SalePrice r=.80, p<.001/TotalBsmtSF-Saleprice r=.62, p<.001
#                              GrLivArea - Salepric r=.71, p<.001.


#-----------------------#
# Handling Missing Data #
#-----------------------#

###  Numeric variables  ###

#missing values of target
null_val<-sum(which(house_train$SalePrice == 'NA'))
print(null_val)

# The target variable has no missing values

#Adjusting NAs for numeric variables: replace by the mean

house_train$LotFrontage[is.na(house_train$LotFrontage)] <- round(mean(house_train$LotFrontage, na.rm=TRUE))

house_train$MasVnrArea[is.na(house_train$MasVnrArea)]<- round(mean(house_train$MasVnrArea, na.rm=TRUE))

house_train$GarageYrBlt[is.na(house_train$GarageYrBlt)]<- round(mean(house_train$MasVnrArea, na.rm=TRUE))

###  Character variables  ###

# creating function to compute mode
Mode <- function(x, na.rm=TRUE) {
   x =x[!is.na(x)]
  level <- unique(x)
  return(level[which.max(tabulate(match(x, level)))])
}


#converting variables into factors  
unique(house_train$SaleCondition)

#CONVERTING INTO CHARACTER

# check distribution across different groups of the variable  ??? - maybe not good
summary(house_train$Alley)

house_train$Alley<- as.character(house_train$Alley)
house_train$Alley[is.na(house_train$Alley)]<- Mode(house_train$Alley)
house_train$Alley<- as.factor(house_train$Alley)
summary(house_train$Alley)

#Fence variable

# check factor levels
unique(house_train$Fence)

# convert to character & assign mode to the missing values
house_train$Fence<- as.character(house_train$Fence)
house_train$Fence[is.na(house_train$Fence)]<- Mode(house_train$Fence)

# convert back to factor
house_train$Fence<- as.factor(house_train$Fence)
summary(house_train$Fence)

# For the NA values, mode imputation was chosen as there is a clear mode.

#MiscFeature

summary(house_train$MiscFeature)
house_train$MiscFeature<- as.character(house_train$MiscFeature)
house_train$MiscFeature[is.na(house_train$MiscFeature)]<- Mode(house_train$MiscFeature)
house_train$MiscFeature<- as.factor(house_train$MiscFeature)


#MasVnrType

summary(house_train$MasVnrType)
house_train$MasVnrType<- as.character(house_train$MasVnrType)
house_train$MasVnrType[is.na(house_train$MasVnrType)]<- Mode(house_train$MasVnrType)
house_train$MasVnrType<- as.factor(house_train$MasVnrType)

# For the NA values, mode imputation was chosen as there is a clear mode.

#--------#
# PoolQC #
#--------#

# inspect the distribution across the levels of the variable
summary(house_train$PoolQC)
house_train$PoolQC<- as.character(house_train$PoolQC)

# check pool area variable: if area is 0, then assign 'no pool' in quality
house_train[which(house_train$PoolArea ==0), 'PoolQC']<- 'No POol'

#convert back to factor
house_train$PoolQC<- as.factor(house_train$PoolQC)
summary(house_train$PoolQC)


#house_train[which(house_train$GarageArea==0 & is.na(house_train$GarageYrBlt)),'GarageArea']<- 0

#------------#
# GarageQual #
#------------#

summary(house_train$GarageQual)
house_train$GarageQual<- as.character(house_train$GarageQual)
house_train$GarageQual[is.na(house_train$GarageQual)]<- Mode(house_train$GarageQual)
house_train$GarageQual<- as.factor(house_train$GarageQual)
summary(house_train$GarageQual)

# For the NA values, mode imputation was chosen as there is a clear mode.

#------------#
# GarageCond #
#------------#

summary(house_train$GarageCond)
house_train$GarageCond<- as.character(house_train$GarageCond)
house_train$GarageCond[is.na(house_train$GarageCond)]<- Mode(house_train$GarageCond)
house_train$GarageCond<- as.factor(house_train$GarageCond)
summary(house_train$GarageCond)

#------------#
# GarageType #
#------------#

summary(house_train$GarageType)
house_train$GarageType<- as.character(house_train$GarageType)
house_train$GarageType[is.na(house_train$GarageType)]<- Mode(house_train$GarageType)
house_train$GarageType<- as.factor(house_train$GarageType)
summary(house_train$GarageType)


#--------------#
# GarageFinish #
#--------------#

summary(house_train$GarageFinish)
house_train$GarageFinish<- as.character(house_train$GarageFinish)
house_train$GarageFinish[is.na(house_train$GarageFinish)]<- Mode(house_train$GarageFinish)
house_train$GarageFinish<- as.factor(house_train$GarageFinish)
summary(house_train$GarageFinish)


#-------------#
# FireplaceQu #
#-------------#

#house_train[which(house_train$GarageArea==0 & is.na(house_train$GarageYrBlt)),'GarageFinish']<- 'Unf'

summary(house_train$FireplaceQu)
house_train$FireplaceQu<- as.character(house_train$FireplaceQu)

# check fireplace area variable: if area is 0, then assign 'no fireplace' in quality

house_train[which(house_train$Fireplaces ==0), 'FireplaceQu']<- 'No Fireplace'
#house_train$FireplaceQu[is.na(house_train$FireplaceQu)]<- 'No Fireplace'
house_train$FireplaceQu<- as.factor(house_train$FireplaceQu)
summary(house_train$FireplaceQu)

#------------#
# Electrical #
#------------#

summary(house_train$Electrical)
house_train$Electrical<- as.character(house_train$Electrical)
house_train$Electrical[is.na(house_train$Electrical)]<- Mode(house_train$Electrical)
house_train$Electrical<- as.factor(house_train$Electrical)
summary(house_train$Electrical)

#--------------#
# BsmtFinType2 #
#--------------#

summary(house_train$BsmtFinType2)
house_train$BsmtFinType2<- as.character(house_train$BsmtFinType2)
house_train$BsmtFinType2[is.na(house_train$BsmtFinType2)]<- Mode(house_train$BsmtFinType2)
house_train$BsmtFinType2<- as.factor(house_train$BsmtFinType2)
summary(house_train$BsmtFinType2)

#----------#
# BsmtQual #
#----------#

summary(house_train$BsmtQual)
house_train$BsmtQual<- as.character(house_train$BsmtQual)
house_train$BsmtQual[is.na(house_train$BsmtQual)]<- Mode(house_train$BsmtQual)
house_train$BsmtQual<- as.factor(house_train$BsmtQual)
summary(house_train$BsmtQual)

#----------#
# BsmtCond #
#----------#
summary(house_train$BsmtCond)
house_train$BsmtCond<- as.character(house_train$BsmtCond)
house_train$BsmtCond[is.na(house_train$BsmtCond)]<- Mode(house_train$BsmtCond)
house_train$BsmtCond<- as.factor(house_train$BsmtCond)
summary(house_train$BsmtCond)

#--------------#
# BsmtExposure #
#--------------#

summary(house_train$BsmtExposure)
house_train$BsmtExposure<- as.character(house_train$BsmtExposure)
house_train$BsmtExposure[is.na(house_train$BsmtExposure)]<- Mode(house_train$BsmtExposure)
house_train$BsmtExposure<- as.factor(house_train$BsmtExposure)
summary(house_train$BsmtExposure)

#--------------#
# BsmtFinType1 #
#--------------#

summary(house_train$BsmtFinType1)
house_train$BsmtFinType1<- as.character(house_train$BsmtFinType1)
house_train$BsmtFinType1[is.na(house_train$BsmtFinType1)]<- Mode(house_train$BsmtFinType1)
house_train$BsmtFinType1<- as.factor(house_train$BsmtFinType1)
summary(house_train$BsmtFinType1)



#-------------------------------------#
# Standardisation & Outlier Detection #
#-------------------------------------#


# Getting a list of numeric variables, for which outliers need to be checked:

numeric_vars<-house_train %>% select_if(is.numeric)
colnames(numeric_vars)


# The following variables were discounted for the analysis: 'Id', 'MSSubClass', 'OverallQual', 'OverallCond'
# and variables including years. This was done as this variables, are ordinal variables. 
# Outlier correction is not meaningful in this case.


#-------------#
# LotFrontage #
#-------------#

# checking lower and upper bounds to determine outliers
lower <- ((mean(house_train$LotFrontage))-3*sd(house_train$LotFrontage))
upper <- ((mean(house_train$LotFrontage))+3*sd(house_train$LotFrontage))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = LotFrontage, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$LotFrontage<- rm.outlier(house_train$LotFrontage, fill = TRUE)


#-------------#
# LotFrontage #
#-------------#

# checking lower and upper bounds to determine outliers
lower <- ((mean(house_train$LotFrontage))-3*sd(house_train$LotFrontage))
upper <- ((mean(house_train$LotFrontage))+3*sd(house_train$LotFrontage))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = LotFrontage, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$LotFrontage<- rm.outlier(house_train$LotFrontage, fill = TRUE)

#---------#
# LotArea #
#---------#

# checking lower and upper bounds to determine outliers
lower <- ((mean(house_train$LotArea))-3*sd(house_train$LotArea))
upper <- ((mean(house_train$LotArea))+3*sd(house_train$LotArea))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = LotArea, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$LotArea<- rm.outlier(house_train$LotArea, fill = TRUE)
out_plot<-qplot(data = house_train, x = LotArea, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

#------------#
# MasVnrArea #
#------------#

# checking lower and upper bounds to determine outliers
lower <- ((mean(house_train$MasVnrArea))-3*sd(house_train$MasVnrArea))
upper <- ((mean(house_train$MasVnrArea))+3*sd(house_train$MasVnrArea))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = MasVnrArea, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$MasVnrArea<- rm.outlier(house_train$MasVnrArea, fill = TRUE)
out_plot<-qplot(data = house_train, x = MasVnrArea, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

#------------#
# BsmtFinSF1 #
#------------#

# checking lower and upper bounds to determine outliers
lower <- ((mean(house_train$BsmtFinSF1))-3*sd(house_train$BsmtFinSF1))
upper <- ((mean(house_train$BsmtFinSF1))+3*sd(house_train$BsmtFinSF1))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = BsmtFinSF1, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$BsmtFinSF1<- rm.outlier(house_train$BsmtFinSF1, fill = TRUE)
out_plot<-qplot(data = house_train, x = BsmtFinSF1, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

#------------#
# BsmtFinSF2 #
#------------#

mean_bfsf2<-(mean(house_train$BsmtFinSF2))
mean_bfsf2

# checking lower and upper bounds to determine outliers
lower <- ((mean(house_train$BsmtFinSF2))-3*sd(house_train$BsmtFinSF2))
upper <- ((mean(house_train$BsmtFinSF2))+3*sd(house_train$BsmtFinSF2))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = BsmtFinSF2, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$BsmtFinSF2<- rm.outlier(house_train$BsmtFinSF2, fill = TRUE)
out_plot<-qplot(data = house_train, x = BsmtFinSF2, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

mean_bfsf2_cor<-(mean(house_train$BsmtFinSF2))
mean_bfsf2_cor

#------------#
# BsmtUnfSF #
#------------#

#check mean, lower & upper  bounds to determine outliers
mean_busf<-(mean(house_train$BsmtUnfSF))
mean_busf
lower <- (mean_busf-3*sd(house_train$BsmtUnfSF))
upper <- (mean_busf+3*sd(house_train$BsmtUnfSF))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = BsmtUnfSF, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$BsmtUnfSF<- rm.outlier(house_train$BsmtUnfSF, fill = TRUE)
out_plot<-qplot(data = house_train, x = BsmtUnfSF, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_busf_cor<-(mean(house_train$BsmtUnfSF))
mean_busf_cor

#-------------#
# TotalBsmtSF #
#-------------#

#check mean, lower & upper  bounds to determine outliers
mean_tbsf<-(mean(house_train$TotalBsmtSF))
mean_tbsf
lower <- (mean_tbsf-3*sd(house_train$TotalBsmtSF))
upper <- (mean_tbsf+3*sd(house_train$TotalBsmtSF))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = TotalBsmtSF, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$TotalBsmtSF<- rm.outlier(house_train$TotalBsmtSF, fill = TRUE)
out_plot<-qplot(data = house_train, x = TotalBsmtSF, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_tbsf_cor<-(mean(house_train$TotalBsmtSF))
mean_tbsf_cor

#-----------#
# X1stFlrSF #
#-----------#

#check mean, lower & upper  bounds to determine outliers
mean_x1fsf<-(mean(house_train$X1stFlrSF))
mean_x1fsf
lower <- (mean_x1fsf-3*sd(house_train$X1stFlrSF))
upper <- (mean_x1fsf+3*sd(house_train$X1stFlrSF))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = X1stFlrSF, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$X1stFlrSF<- rm.outlier(house_train$X1stFlrSF, fill = TRUE)
out_plot<-qplot(data = house_train, x = X1stFlrSF, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_x1fsf_cor<-(mean(house_train$X1stFlrSF))
mean_x1fsf_cor

#-----------#
# X2ndFlrSF #
#-----------#

#check mean, lower & upper  bounds to determine outliers
mean_x2fsf<-(mean(house_train$X2ndFlrSF, na.rm=TRUE))
mean_x2fsf
lower <- (mean_x2fsf-3*sd(house_train$X2ndFlrSF))
upper <- (mean_x2fsf+3*sd(house_train$X2ndFlrSF))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = X2ndFlrSF, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$X2ndFlrSF<- rm.outlier(house_train$X2ndFlrSF, fill = TRUE)
out_plot<-qplot(data = house_train, x = X2ndFlrSF, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_x2fsf_cor<-(mean(house_train$X2ndFlrSF))
mean_x2fsf_cor

#-----------#
# GrLivArea #
#-----------#

#check mean, lower & upper  bounds to determine outliers
mean_gla<-(mean(house_train$GrLivArea, na.rm=TRUE))
mean_gla
lower <- (mean_gla-3*sd(house_train$GrLivArea))
upper <- (mean_gla+3*sd(house_train$GrLivArea))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = GrLivArea, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$GrLivArea<- rm.outlier(house_train$GrLivArea, fill = TRUE)
out_plot<-qplot(data = house_train, x = GrLivArea, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_gla_cor<-(mean(house_train$GrLivArea))
mean_gla_cor

#-----------#
# GrLivArea #
#-----------#

#check mean, lower & upper  bounds to determine outliers
mean_gla<-(mean(house_train$GrLivArea, na.rm=TRUE))
mean_gla
lower <- (mean_gla-3*sd(house_train$GrLivArea))
upper <- (mean_gla+3*sd(house_train$GrLivArea))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = GrLivArea, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$GrLivArea<- rm.outlier(house_train$GrLivArea, fill = TRUE)
out_plot<-qplot(data = house_train, x = GrLivArea, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_gla_cor<-(mean(house_train$GrLivArea))
mean_gla_cor

#------------#
# GarageArea #
#------------#

#check mean, lower & upper  bounds to determine outliers
mean_ga<-(mean(house_train$GarageArea))
mean_ga
lower <- (mean_ga-3*sd(house_train$GarageArea))
upper <- (mean_ga+3*sd(house_train$GarageArea))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = GarageArea, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$GarageArea<- rm.outlier(house_train$GarageArea, fill = TRUE)
out_plot<-qplot(data = house_train, x = GarageArea, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_ga_cor<-(mean(house_train$GarageArea))
mean_ga_cor

#------------#
# WoodDeckSF #
#------------#

#check mean, lower & upper  bounds to determine outliers
mean_wdsf<-(mean(house_train$WoodDeckSF))
mean_wdsf
lower <- (mean_wdsf-3*sd(house_train$WoodDeckSF))
upper <- (mean_wdsf+3*sd(house_train$WoodDeckSF))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = WoodDeckSF, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$WoodDeckSF<- rm.outlier(house_train$WoodDeckSF, fill = TRUE)
out_plot<-qplot(data = house_train, x = WoodDeckSF, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_wdsf_cor<-(mean(house_train$WoodDeckSF))
mean_wdsf_cor

#-------------#
# OpenPorchSF #
#-------------#

#check mean, lower & upper  bounds to determine outliers
mean_opsf<-(mean(house_train$OpenPorchSF))
mean_opsf
lower <- (mean_opsf-3*sd(house_train$OpenPorchSF))
upper <- (mean_opsf+3*sd(house_train$OpenPorchSF))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = OpenPorchSF, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$OpenPorchSF<- rm.outlier(house_train$OpenPorchSF, fill = TRUE)
out_plot<-qplot(data = house_train, x = OpenPorchSF, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_opsf_cor<-(mean(house_train$OpenPorchSF))
mean_opsf_cor

#---------------#
# EnclosedPorch #
#---------------#

#check mean, lower & upper  bounds to determine outliers
mean_ep<-(mean(house_train$EnclosedPorch))
mean_ep
lower <- (mean_ep-3*sd(house_train$EnclosedPorch))
upper <- (mean_ep+3*sd(house_train$EnclosedPorch))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = EnclosedPorch, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$EnclosedPorch<- rm.outlier(house_train$EnclosedPorch, fill = TRUE)
out_plot<-qplot(data = house_train, x = EnclosedPorch, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_ep_cor<-(mean(house_train$EnclosedPorch))
mean_ep_cor


#-------------#
# ScreenPorch #
#-------------#

#check mean, lower & upper  bounds to determine outliers
mean_sp<-(mean(house_train$ScreenPorch))
mean_sp
lower <- (mean_sp-3*sd(house_train$ScreenPorch))
upper <- (mean_sp+3*sd(house_train$ScreenPorch))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = ScreenPorch, bins=25) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$ScreenPorch<- rm.outlier(house_train$ScreenPorch, fill = TRUE)
out_plot<-qplot(data = house_train, x = ScreenPorch, bins=20) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_sp_cor<-(mean(house_train$ScreenPorch))
mean_sp_cor

#----------#
# PoolArea #
#----------#

#check mean, lower & upper  bounds to determine outliers
mean_pa<-(mean(house_train$PoolArea))
mean_pa
lower <- (mean_pa-3*sd(house_train$PoolArea))
upper <- (mean_pa+3*sd(house_train$PoolArea))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = PoolArea, bins=10) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$PoolArea<- rm.outlier(house_train$PoolArea, fill = TRUE)
out_plot<-qplot(data = house_train, x = PoolArea, bins=5) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_pa_cor<-(mean(house_train$PoolArea))
mean_pa_cor

#Not sure whether it is good practice to remove outliers in PoolArea variable given the prevelance of 0s.

#---------#
# MiscVal #
#---------#

#check mean, lower & upper  bounds to determine outliers
mean_mv<-(mean(house_train$MiscVal))
mean_mv
lower <- (mean_mv-3*sd(house_train$MiscVal))
upper <- (mean_mv+3*sd(house_train$MiscVal))

# Plotting Lot Frontage to detect the outliers 
out_plot<-qplot(data = house_train, x = MiscVal, bins=10) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Replacing outliers with the mean
house_train$MiscVal<- rm.outlier(house_train$MiscVal, fill = TRUE)
out_plot<-qplot(data = house_train, x = MiscVal, bins=5) + ylab("")
out_plot+ geom_vline(xintercept=c(upper, lower))

# Validate mean correction
mean_mv_cor<-(mean(house_train$MiscVal))
mean_mv_cor


#Outliers target variable?
#zscores<-zscore(house_train$LotFrontage,na.rm = getOption("na.rm", FALSE) )
#house_train$LotFrontage[which(zscores>3 | zscores< (-3))]


#---------------------------#
# Creation of New Variables #
#---------------------------#


###Feature Engineering: New Variables

# Creation of total area variables that might prove to be potentially useful
#numeric features.

#Total area of property

house_train$TotalArea <- house_train$LotFrontage + house_train$LotArea + house_train$MasVnrArea + house_train$BsmtFinSF1 + 
  house_train$BsmtFinSF2 + house_train$BsmtUnfSF + house_train$TotalBsmtSF + house_train$X1stFlrSF + 
  house_train$X2ndFlrSF + house_train$GrLivArea + house_train$GarageArea + house_train$WoodDeckSF +
  house_train$OpenPorchSF + house_train$EnclosedPorch + house_train$X3SsnPorch + 
  house_train$ScreenPorch + house_train$LowQualFinSF + house_train$PoolArea

#Total area of 1st and 2nd floor

house_train$TotalArea1st2nd <- house_train$X1stFlrSF + house_train$X2ndFlrSF


#IsHotMonth: indicates whether the house has been sold in one of the peak months in terms of sales. (as seen in the graphs)

house_train['IsHotMonth'] = recode(house_train$MoSold,"1"=0,"2"=0,"3"=0,"4"=1,"5"=1, "6"=1, "7"=1, "8"=0, "9"=0, "10"=0, "11"=0, "12"=0)


# Is Popular Dwelling/MS Sub Class

house_train['IsPopDwelling'] = recode(house_train$MSSubClass,"20"=1,"30"=0,"40"=0,"45"=0,"50"=1, "60"=1, "70"=0, "75"=0, "80"=0, "85"=0, "90"=0, "120"=1, "160"=0, "180"=0, "190"=0)


# HasBeenRemodeled: indicates whether the house has been remodeled: If the 
#YearBuilt is different than the remodel year 1, else 0.

house_train['HasBeenRemodeled'] <- ifelse(house_train$YearRemodAdd == house_train$YearBuilt, 0, 1)


# HasBeenRecentlyRemodeled: indicates whether the house has been remodelled
# after the year it was sold.

house_train['HasBeenRecentlyRemodeled'] <- ifelse(house_train$YearRemodAdd == house_train$YrSold, 0, 1) 

# IsNewHouse: indicates whether the house was sold the year it was built

house_train['IsNewHouse'] <-ifelse(house_train$YearBuilt == house_train$YrSold, 1, 0) 

# Age: indicates: How old the house is

house_train['Age'] <- as.numeric(house_train$YrSold - house_train$YearBuilt)


#----------------------------#
# Subsetting into Train-Test #
#----------------------------#


### split train and train datasets STRATIFY
set.seed(101)
#
#library(caTools)
#sample = sample.split(house_train$SalePrice, SplitRatio = .60)
train = subset(house_train, sample == TRUE)
#test  = subset(house_train, sample == FALSE)


#-----------------------------------------------------#
# Performing Shrinkage Methods for variable selection #
#-----------------------------------------------------#

library(glmnet)

# creating an x matrix and y vector
x= model.matrix(SalePrice ~ . ,data= house_train )[,-1]
y= house_train$SalePrice[which(!is.na(house_train$SalePrice))]

#split data into train & test
set.seed(101)
train_rows = sample(1:nrow(x), nrow(x)/2)
test = (-train_rows)
y.test=y[-train_rows]

#defining grid (lambdas value to try, in 10-fold cross-validation)
grid= 10^seq(10, -2, length=100)

#-----------------------------------------#
# Ridge Regression for variable selection #
#-----------------------------------------#

#Implement ridge regression on train dataset: setting alpha=0
ridge.mod=glmnet(x[train_rows,],y[train_rows], lambda = grid, alpha=0, family= 'gaussian')

#perform cross-validation
set.seed(1)
cv.out= cv.glmnet(x[train_rows,], y[train_rows], alpha=0, nfolds=10, family= 'gaussian')
plot(cv.out)
# the plot shows that the log of the optimal value of lambda which minimises the MSE is 10.

#identify optimal cross-validated lambda
bestlambda=cv.out$lambda.min
bestlambda_se=cv.out$lambda.1se
bestlambda
bestlambda_se
plot(ridge.mod, xvar = "lambda")
#legend("bottomright", lwd = 1, col = 1:10, legend = colnames(x), cex = .7)

#The large lambda value suggests that a lot of parameters need to removed, as their coefficients converge to 0.
#From this graph we can see, that increasing the Lambda > 10 shrinks the coefficients to 0

#compute test error
ridge.pred=predict(ridge.mod, s=bestlambda, newx=x[test,])
mse_error=mean((ridge.pred-y.test)^2)
print(mse_error)

out= glmnet(x[train_rows,], y[train_rows], alpha=0, lambda=grid)
ridge.coef = predict(out, type = 'coefficients', s =bestlambda)[1:89]
ridge.coef

# check number of variables to keep 
inds<-which(ridge.coef!=0)
colnames(house_train[inds])
length(colnames(house_train[inds]))

# The resulting model contains 84 variables.


#------------------------------------------#
#  Lasso Regression for variable selection #
#------------------------------------------#

#Implement lasso regression on train dataset: setting alpha=1
lasso.mod=glmnet(x[train_rows,],y[train_rows], alpha=1, lambda = grid, family= 'gaussian')

#perform cross-validation
set.seed(1)
cv.out= cv.glmnet(x[train_rows,], y[train_rows], alpha=1, nfolds=10, family= 'gaussian')
plot(cv.out)
# the plot shows that the log of the optimal value of lambda which minimises the MSE is 6.

#identify optimal cross-validated lambda
bestlambda=cv.out$lambda.min
bestlambda
bestlambda_se=cv.out$lambda.1se
bestlambda_se
plot(lasso.mod, xvar = "lambda")
#legend("bottomright", lwd = 1, col = 1:10, legend = colnames(x), cex = .7)

#The large lambda value suggests that a lot of parameters need to removed, as their coefficients converge to 0.
#From this graph we can see, that increasing the Lambda > 10 shrinks the coefficients to 0

#compute test error
lasso.pred=predict(lasso.mod, s=bestlambda, newx=x[test,], family= 'gaussian')
mse_error=mean((lasso.pred-y.test)^2)
print(mse_error)

#Checking the shrinked coefficients 
out= glmnet(x[train_rows,], y[train_rows], alpha=1, lambda=grid)
lasso.coef = predict(out, type = 'coefficients', s =bestlambda)[1:89]
lasso.coef

# check number of variables to keep
inds<-which(lasso.coef!=0)
colnames(house_train[inds])
length(colnames(house_train[inds]))

# The resulting model contains 46 variables.

#---------------------------------#
# Comparison of Shirnkage methods #
#---------------------------------#

# Given the high correlation among predictor variables and the smallest resulting MSE of the lasso method, 
# The lasso procedure was chosen as it encourages simple, sparse models (i.e. models with fewer parameters).
# Given the high multicollinearity observed in the correlation matrix, this seems the more appropriate shrinkage method.


#--------------------#
# Subsetting Dataset #
#--------------------#

# using the significant lasso coefficients to create an index & subset to include only the important variables
inds<-which(lasso.coef!=0)

#dataset: target -> SalePrice
house_train_sh<-house_train[,c(inds,81)]

#split shrinked data into train & test, stratifying over the target
set.seed(101)
sample = sample.split(house_train$SalePrice, SplitRatio = .60)
train = subset(house_train_sh, sample == TRUE)
test  = subset(house_train_sh, sample == FALSE)

#dataset: target -> SalePrice_Log
house_train_sh_log<-house_train[,c(inds,82)]

#split shrinked data into train & test, stratifying over the target
set.seed(101)
sample = sample.split(house_train$SalePrice_Log, SplitRatio = .60)
train_log = subset(house_train_sh_log, sample == TRUE)
test_log  = subset(house_train_sh_log, sample == FALSE) 


#train_rows <- sample(1:nrow(x), .70*nrow(x))
#train <- house_train[train_rows,]
#test<- house_train[-train_rows,]
#x.test <- house_train[-train_rows, c(1:80, 82:83) ]
#y.train <- house_train[train_rows, 'SalePrice']
#y.test <- house_train[-train_rows, 'SalePrice']


#####################
#-------------------#
# Regression Models #
#-------------------#
#####################

# Defining evaluation Metrics for Assessment
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

RMSE= RMSE(y.boost.house_prices_log, test_log$SalePrice_Log)
RMSE

r_sq <- function (x, y) cor(x, y) ^ 2


#-------------------------#
# Linear Regression Model #
#-------------------------# 

### split train and test datasets, stratifying the target: SalePrice
set.seed(101)
sample = sample.split(house_train$SalePrice, SplitRatio = .60)
train = subset(house_train, sample == TRUE)
train = train[, -82]
test  = subset(house_train, sample == FALSE)
test = test[, -82]

# applying the model
lg <- lm(SalePrice~.,data=train)
summary(lg)

# The adjusted R-squared value of our model is 0.933.

# # From the summary of the model, we are checking the significant features 
# and building a new model by including only these features.

fit_select<- lm(SalePrice~MSZoning+I(LotArea^2)+Street+
              
              LotConfig+LandSlope+Neighborhood+Condition1+OverallQual+OverallCond+
              
              YearBuilt+RoofStyle+RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +
              
              BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+
              
              ScreenPorch+PoolArea+PoolQC, train)

summary(fit_select)

### split train and test datasets,  stratifying the target: SalePrice_Log
sample = sample.split(house_train$SalePrice_Log, SplitRatio = .60)
train_log = subset(house_train, sample == TRUE)
train_log = train_log[, -81]
test_log  = subset(house_train, sample == FALSE)
test_log = test_log[, -81]

# applying the model to the log trasformed target variable
lg <- lm(SalePrice_Log~.,data=train_log)
summary(lg)

# From the summary of the model, we are checking the significant features 
# and building a new model by including only these features.

fit_1 <- lm(SalePrice_Log~MSZoning+LotArea+
              LotConfig+LandSlope+Neighborhood+Condition1+OverallCond+
              YearBuilt+ExterQual+BsmtExposure+ BsmtCond + BsmtFinSF2+ BsmtFinSF1+ BsmtFinType2+ BsmtFinType1+
              +X2ndFlrSF+KitchenQual+ X3SsnPorch+ EnclosedPorch+ GarageType +Fireplaces+ Functional+ HalfBath+ GrLivArea
              + BsmtFinType2+ ExterCond+ MasVnrType+ Exterior2nd+BldgType + Condition2
              + Heating+ TotalBsmtSF +LowQualFinSF+ScreenPorch+SaleType+ PoolQC +MiscFeature,train_log)

summary(fit_1)

# The adjusted R square is 0.920 indicating a very good model fit.

#by only including the previously important (in terms of statistical significance) variables
# we are reducing the number of predictors while having a high adjusted R2 value.
# we achieve a good model fit while at the same time reducing model complexity.


plot(fit_1$fit,fit_1$residuals)

abline(h=0,col="blue")

prediction <- predict(fit_1,test)

length(test$SalePrice)

length(prediction)


#-----------------------------------------#
# Linear Regression Model with shrinkage  #
#-----------------------------------------# 

lg <- lm(SalePrice_Log~.,data=house_train_sh_log)
summary(lg)
length(coefficients(lg))
#157
RSS <- c(crossprod(lg$residuals))
RSS
 #17.58124
MSE <- RSS / length(lg$residuals)
MSE
RMSE <- sqrt(MSE)
RMSE
#0.1097358

#----------------#
# Boosting Model #
#----------------# 

# APPLY BOOSTING MODEL ON SALEPRICE
set.seed(1)
boost.house_prices <- gbm(SalePrice ~ ., data=train, distribution="gaussian", n.trees=1000, shrinkage=0.01)

#check variable importance
summary(boost.house_prices)

# Produce partial dependence plots for the variables with highest predictive power on the train dataset
par(mfrow=c(1,2))
plot(boost.house_prices, i='GrLivArea')
plot(boost.house_prices, i='TotalBsmtSF')
plot(boost.house_prices, i='ExterQual')
plot(boost.house_prices, i='KitchenQual')
plot(boost.house_prices, i='BsmtFinSF1')
plot(boost.house_prices, i='YearBuilt')

# apply boosting model to create predictions for the test dataset 

y.boost.house_prices=predict(boost.house_prices, newdata=test, type= 'response')

predict.gbm <- function (object, newdata, n.trees, type = "link", single.tree = FALSE, ...) {
  if (missing(n.trees)) {
    if (object$train.fraction < 1) {
      n.trees <- gbm.perf(object, method = "test", plot.it = FALSE)
    }
    else if (!is.null(object$cv.error)) {
      n.trees <- gbm.perf(object, method = "cv", plot.it = FALSE)
    }
    else {
      n.trees <- length(object$train.error)
    }
    cat(paste("Using", n.trees, "trees...\n"))
    gbm::predict.gbm(object, newdata, n.trees, type, single.tree, ...)
  }
}
y.boost.house_prices=predict.gbm(boost.house_prices, newdata=test)



# check accuracy of the model
MSE=round(mean((y.boost.house_prices-test$SalePrice)^2),2) #CHECK VALUE
print(MSE)
MAPE=MAPE(y_pred = y.boost.house_prices, y_true = test$SalePrice)
print(MAPE)
R2= rsquare(model =boost.house_prices , test)
print(R2)
RMSE= rmse(model =boost.house_prices, test)
RMSE


install.packages('rsq')
library(rsq)

# r_squared
r.squared(boost.house_prices)

rsq(boost.house_prices, test)
r_sq(boost.house_prices, test)
#----------------#
# Boosting Model - Log #
#----------------# 

# APPLY BOOSTING MODEL ON SALEPRICE_LOG
set.seed(1)
boost.house_prices_log <- gbm(SalePrice_Log ~ ., data=train_log, distribution="gaussian", n.trees=1000, shrinkage=0.01)

#check variable importance
summary(boost.house_prices)

# Produce partial dependence plots for the variables with highest predictive power on the train dataset
par(mfrow=c(1,2))
plot(boost.house_prices, i='GrLivArea')
plot(boost.house_prices, i='ExterQual')
plot(boost.house_prices, i='TotalBsmtSF')
plot(boost.house_prices, i='BsmtFinSF1')
plot(boost.house_prices, i='KitchenQual')
plot(boost.house_prices, i='YearBuilt')

# apply boosting model to create predictions for the test dataset 

y.boost.house_prices_log=predict(boost.house_prices_log, newdata=test_log)
MSE_log=round(mean((y.boost.house_prices_log-test_log$SalePrice_Log)^2),2)
print(MSE_log)
MAPE_log=MAPE(y_pred = y.boost.house_prices_log, y_true = test_log$SalePrice_Log)
print(MAPE_log)
R2= rsquare(model =boost.house_prices_log , test_log)
R2

r2=r_sq(y.boost.house_prices_log, test_log$SalePrice_Log)
r2

r2=r_sq(y.boost.house_prices_log, test_log$SalePrice_Log)
r2




#---------------------#
# Random Forest Model #
#---------------------# 



library(caTools)
set.seed(101)
sample = sample.split(house_train$SalePrice, SplitRatio = .60)
train_ = subset(house_train, sample == TRUE)
test_ = subset(house_train, sample == FALSE)


# Train the model based on the selected numeric variables above - se
# modelrf, selected random forest due to large p (variable number)

set.seed(2017)
target<- train$SalePrice 
target_test<- test$SalePrice 
modelrf <- randomForest(SalePrice ~., data=train_, importance=TRUE,na.action=na.roughfix, ntree=100, maxnodes = 10, cutoff = c(0.1, 0.9), type = 'regression')
modelrf2 <- randomForest(target ~., data=train_, localImp =TRUE, importance=TRUE,na.action=na.roughfix, ntree=500, maxnodes = 10, cutoff = c(0.1, 0.9), type = 'regression')


#predictions_train_rf <- predict(modelrf,newdata = train)
predictions_test_rf <- predict(modelrf,newdata = test_)
MSE=mean((test_$SalePrice - predictions_test_rf)^2)
MSE
MAPE_rf=MAPE(y_pred = predictions_test_rf, y_true = test$SalePrice)
print(MAPE_rf)
RMSE= rmse(model =modelrf, test)
RMSE
R2= rsquare(model =modelrf , test)
R2

# log data
modelrf_log <- randomForest(SalePrice_Log ~., data=train_log, importance=TRUE,na.action=na.roughfix, ntree=100, maxnodes = 10, cutoff = c(0.1, 0.9), type = 'regression')
predictions_test_rf_log <- predict(modelrf_log,newdata = test_log)
MSE_log=mean((test_log$SalePrice_Log - predictions_test_rf_log)^2)
MSE_log
MAPE_rf_log=MAPE(y_pred = predictions_test_rf_log, y_true = test_log$SalePrice_Log)
print(MAPE_rf_log)
RMSE_log= rmse(model =modelrf_log , test_log)
RMSE_log
R2= rsquare(model =modelrf_log , test_log)
R2

plot_min_depth_distribution(modelrf)
importance_frame <- measure_importance(modelrf)
measure_importance(modelrf)

modelrf_log <- randomForest(SalePrice_Log ~., data=train_rf, importance=TRUE,na.action=na.roughfix, ntree=500, maxnodes = 10, cutoff = c(0.1, 0.9), type = 'regression')

##########################RANDOM FOREST WITH MTRY#############################################
###########################################################################
# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(SalePrice~., data=train, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_default)

###################XGBOOST WORKING##########################
############################################################

## setting the seed to make the partition reproducible
set.seed(123)
smp_size <- floor(0.75 * nrow(train_log))
train_ind <- sample(seq_len(nrow(train_log)), size = smp_size)

train_new <- train[train_ind, ]
validate <- train[-train_ind, ]
train_new <- subset(train_new,select=-c(Id))
validate <- subset(validate,select=-c(Id))
nrow(train_new)
nrow(validate)
str(train_new)

install.packages("xgboost")
library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(sapply(train_new[,-47], as.numeric)),label = as.matrix(train_new$SalePrice))
dtest <- xgb.DMatrix(data = as.matrix(sapply(validate[,-47], as.numeric)),label=as.matrix(validate$SalePrice))

#Building model
set.seed(111)
xgb <-  xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 2500, verbose = FALSE,
                objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01,
                gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)

mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb)
xgb.plot.importance (importance_matrix = mat[2:20])

prediction <- predict(xgb,newdata = dtest)
accuracy(prediction,dtest$SalePrice)

R-squared
rsq <- function(x){
  # Calculate total sum of squares
  tss =  sum((validate$SalePrice - mean(validate$SalePrice))^2 )
  # Calculate residual sum of squares
  rss =  sum((x - validate$SalePrice)^2)
  # Calculate R-squared: xgboost predicts about 90.8% of the variance in the data
  1 - (rss/tss)
}
rsq(prediction)
