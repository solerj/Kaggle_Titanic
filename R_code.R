# 1. Init ---------------------------------------------------------------------

set.seed(200591)

packagesNeeded <- c("RCurl", "dplyr", "stringr", "ggplot2", "reshape"
                    , "glmnet", "caret", "car", "pROC")

for (i in packagesNeeded){
  if(!(i %in% installed.packages())){
    install.packages(i)
  }
  library(i, character.only = T)
}

rawTrain <- getURL("https://raw.githubusercontent.com/solerj/Kaggle_Titanic/master/train.csv")
trainO <- read.csv(text = rawTrain)
rawTest <- getURL("https://raw.githubusercontent.com/solerj/Kaggle_Titanic/master/test.csv")
testO <- read.csv(text = rawTest)

testO$Survived <- NA
dataAll <- rbind(trainO, testO)

# DATA CLEANING ---------------------------------------------------------------

summary(dataAll)
str(dataAll)


getTitle <- function(name) {
  firstTitle <- str_sub(name,
                        start = (str_locate(name, ',')[1]+2),
                        end = (str_locate(name, '[.]')[1]-1))
  firstTitle
}
dataAll$Name      <- as.character(dataAll$Name)
dataAll$NameTitle <- sapply(dataAll$Name, getTitle)


fareMissing <- dataAll[is.na(dataAll$Fare),]
fareQuickFill <- median(dataAll[dataAll$NameTitle == "Mr" &
                                  dataAll$Pclass == 3 &
                                  dataAll$Embarked == "S"
                                , c("Fare")], na.rm = T)
dataAll$Fare[is.na(dataAll$Fare)] <- fareQuickFill


embarkedMissing <- dataAll[dataAll$Embarked == "",]
tbEmbark <- table(dataAll$Embarked[dataAll$Pclass == 1 &
                                     dataAll$SibSp == 0 &
                                     dataAll$Parch == 0])
modeEmbarked <- names(which.max(tbEmbark))
dataAll$Embarked[dataAll$Embarked == ""] <- modeEmbarked


myAsFactor <- function(tableName, variable){
  if(is.factor(tableName[,c(variable)])){
    tableName[,c(variable)] <- as.character(tableName[,c(variable)])
  }
  tb <- table(tableName[,c(variable)])
  factor(tableName[,c(variable)]
         , levels = names(tb[order(tb, decreasing = TRUE)]))
}

dataAll$SurvivedF <- myAsFactor(dataAll, "Survived")
variablesToRefactor <- c("Pclass", "Sex", "Embarked", "NameTitle")
for (variable in variablesToRefactor){
  dataAll[,c(variable)] <- myAsFactor(dataAll, variable)
}

dataAll$Name <- as.character(dataAll$Name)
dataAll$Cabin <- as.character(dataAll$Cabin)
dataAll$Fare[is.na(dataAll$Fare)] <- median(dataAll$Fare, na.rm = T)
dataAll$SibSpParCh <- as.factor(paste0(as.integer(dataAll$SibSp>0)
                                       , as.integer(dataAll$Parch>0)))

checkDependence <- function(variable1, variable2){
  contingency <- table(dataAll[,c(variable1, variable2)])
  chisq <- chisq.test(x = contingency, simulate.p.value = T)
  chisq$p.value
}


# 1. do different classes have different probab of survival? ------------------
# YES
table(dataAll$Pclass)
barClassSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                       , aes(x=Pclass
                             , y=Survived))
barClassSurv <- barClassSurv + stat_summary(fun.y="mean", geom="bar")
barClassSurv <- barClassSurv + ylim(0, 1)
barClassSurv
checkDependence("SurvivedF", "Pclass")


# 2. do different cabin letters have different probab of survival? ------------
# YES
dataAll$cabinLetter <- str_sub(dataAll$Cabin, 1, 1)
table(dataAll$cabinLetter)
dataAll$cabinLetter[dataAll$cabinLetter == ""] <- "N"
barCabinSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                       , aes(x=reorder(cabinLetter,-Survived,mean)
                             , y=Survived))
barCabinSurv <- barCabinSurv + stat_summary(fun.y="mean", geom="bar")
barCabinSurv <- barCabinSurv + ylim(0, 1)
barCabinSurv
dataAll$cabinLetter <- myAsFactor(dataAll, "cabinLetter")
checkDependence("SurvivedF", "cabinLetter")


# 3. does noCabins have different probab of survival? -------------------
# YES
dataAll$noCabins <- str_count(as.character(dataAll$Cabin), " ")
table(dataAll$noCabins)
barNoCabinSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                       , aes(x = noCabins
                             , y=Survived))
barNoCabinSurv <- barNoCabinSurv + stat_summary(fun.y="mean", geom="bar")
barNoCabinSurv <- barNoCabinSurv + ylim(0, 1)
barNoCabinSurv
checkDependence("SurvivedF", "noCabins")

dataAll$noCabinsGroup <- dataAll$noCabins
dataAll$noCabinsGroup[dataAll$noCabins != 0] <- "1+"
dataAll$noCabinsGroup <- myAsFactor(dataAll, "noCabinsGroup")
checkDependence("SurvivedF", "noCabinsGroup")


# 4. do different NameTitles have different probab of survival? ---------------

table(dataAll$NameTitle)

# clearly some relationship between NameTitle and probab of Survival
# but there are a bit too many categories and risk of overfitting
barTitleSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                       , aes(x=reorder(NameTitle,-Survived,mean)
                             , y=Survived))
barTitleSurv <- barTitleSurv + stat_summary(fun.y="mean", geom="bar")
barTitleSurv <- barTitleSurv + ylim(0, 1)
barTitleSurv

# created title groups for more important people 
dataAll$titleGroup <- as.character(dataAll$NameTitle)
dataAll$titleGroup[dataAll$NameTitle %in% c("Col", "Don", "Dr", "Jonkheer", "Major", "Rev", "Sir")
                   & dataAll$Sex == "male"] <- "Dr"
dataAll$titleGroup[dataAll$NameTitle %in% c("Dona", "Dr", "Lady", "Mlle", "Mme", "the Countess")
                   & as.character(dataAll$Sex) == "female"] <- "Mme"
dataAll$titleGroup[dataAll$NameTitle %in% c("Ms")
                   & as.character(dataAll$Sex) == "female"] <- "Mrs"
dataAll$titleGroup <- myAsFactor(dataAll, "titleGroup")
table(dataAll$titleGroup)


barTitleGrpSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                          , aes(x = reorder(titleGroup, -Survived, mean)
                                , y = Survived))
barTitleGrpSurv <- barTitleGrpSurv + stat_summary(fun.y="mean", geom="bar")
barTitleGrpSurv <- barTitleGrpSurv + ylim(0, 1)
barTitleGrpSurv
checkDependence("SurvivedF", "titleGroup")


# 5. is Age related to Survival? ----------------------------------------------
# first note that there are many NAs for age
# if age turns out to be significant, we will want to do something about that
summary(dataAll$Age)


lineAgeSurv <- ggplot(dataAll[!is.na(dataAll$Age) & !is.na(dataAll$Survived),]
                      , aes(x = Age
                            , y = Survived
                            , colour = Sex))
lineAgeSurv <- lineAgeSurv + stat_summary_bin(bins = 50, fun.y="mean", geom="line", size = 0.2)
lineAgeSurv <- lineAgeSurv + stat_summary_bin(bins = 30, fun.y="mean", geom="line", size = 0.5)
lineAgeSurv <- lineAgeSurv + stat_summary_bin(bins = 20, fun.y="mean", geom="line", size = 0.75)
lineAgeSurv <- lineAgeSurv + stat_summary_bin(bins = 10, fun.y="mean", geom="line", size = 1)
lineAgeSurv

# so yes, there is so dependency, especially on younger males
# but these are probably labelled "Master"
# so split age-survival relationship per titleGroup
# then relationship is much less evident

lineAgeTitleSurv <- ggplot(dataAll[!is.na(dataAll$Age)
                              & !is.na(dataAll$Survived)
                              & dataAll$titleGroup != "Capt",]
                      , aes(x = Age
                            , y = Survived
                            , colour = Sex))
lineAgeTitleSurv <- lineAgeTitleSurv + facet_grid(rows = vars(titleGroup))
lineAgeTitleSurv <- lineAgeTitleSurv + stat_summary_bin(bins = 50, fun.y="mean", geom="line", size = 0.2)
lineAgeTitleSurv <- lineAgeTitleSurv + stat_summary_bin(bins = 30, fun.y="mean", geom="line", size = 0.5)
lineAgeTitleSurv <- lineAgeTitleSurv + stat_summary_bin(bins = 20, fun.y="mean", geom="line", size = 0.75)
lineAgeTitleSurv <- lineAgeTitleSurv + stat_summary_bin(bins = 10, fun.y="mean", geom="line", size = 1)
#lineAgeTitleSurv <- lineAgeTitleSurv + geom_smooth(method = "lm")
lineAgeTitleSurv


dataAll$ageBucket <- NA
dataAll$ageBucket[dataAll$Age < 3] <- "0-3"
dataAll$ageBucket[dataAll$Age >= 3 & dataAll$Age < 6] <- "3-6"
dataAll$ageBucket[dataAll$Age >= 6 & dataAll$Age < 9] <- "6-9"
dataAll$ageBucket[dataAll$Age >= 9 & dataAll$Age < 12] <- "9-12"
dataAll$ageBucket[dataAll$Age >= 12 & dataAll$Age < 15] <- "12-15"
dataAll$ageBucket[dataAll$Age >= 15] <- "15+"
dataAll$ageBucket <- myAsFactor(dataAll, "ageBucket")

summary(dataAll$ageBucket)
barAgeSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                     , aes(x = ageBucket
                           , y = Survived))
#barEmbSurv <- barEmbSurv + facet_grid(cols = vars(cabinClass))
barAgeSurv <- barAgeSurv + stat_summary(fun.y="mean", geom="bar")
barAgeSurv <- barAgeSurv + ylim(0, 1)
barAgeSurv
checkDependence("SurvivedF", "ageBucket")


# 6. does different Embarking have different probab of survival? --------------
# YES, and might be best to join with cabinClass due to dependence

table(dataAll$Embarked)
barEmbSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                     , aes(x = Embarked
                           , y = Survived))
#barEmbSurv <- barEmbSurv + facet_grid(cols = vars(cabinClass))
barEmbSurv <- barEmbSurv + stat_summary(fun.y="mean", geom="bar")
barEmbSurv <- barEmbSurv + ylim(0, 1)
barEmbSurv
checkDependence("SurvivedF", "Embarked")

# 7. is Fare related to Survival? ---------------------------------------------
summary(dataAll$Fare)
hist(dataAll$Fare)
hist(log(dataAll$Fare+1))
dataAll$logFare <- log(dataAll$Fare + 1)


scatFareSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                       , aes(x = logFare
                             , y = Survived))
scatFareSurv <- scatFareSurv + stat_summary_bin(bins = 50, fun.y="mean", geom="line", size = 0.2)
scatFareSurv <- scatFareSurv + stat_summary_bin(bins = 30, fun.y="mean", geom="line", size = 0.5)
scatFareSurv <- scatFareSurv + stat_summary_bin(bins = 20, fun.y="mean", geom="line", size = 0.75)
scatFareSurv <- scatFareSurv + stat_summary_bin(bins = 10, fun.y="mean", geom="line", size = 1)
scatFareSurv <- scatFareSurv + geom_smooth(method = "lm")
scatFareSurv


medlogFare <- median(dataAll$logFare)
dataAll$logFareS <- dataAll$logFare / (2*medlogFare)
summary(dataAll$logFareS)


# 8. Parch --------------------------------------------------------------------
table(dataAll$Parch)
table(dataAll$Parch[!is.na(dataAll$Survived)])
barParchSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                       , aes(x = Parch
                             , y = Survived))
barParchSurv <- barParchSurv + stat_summary(fun.y="mean", geom="bar")
barParchSurv <- barParchSurv + ylim(0, 1)
barParchSurv

dataAll$parchGroup <- dataAll$Parch
dataAll$parchGroup[dataAll$Parch %in% c(1,2)] <- "1_2"
dataAll$parchGroup[dataAll$Parch >= 3] <- "3+"
dataAll$parchGroup <- myAsFactor(dataAll, "parchGroup")
table(dataAll$parchGroup)
checkDependence("SurvivedF", "parchGroup")


# 9. SibSp --------------------------------------------------------------------
table(dataAll$SibSp)
table(dataAll$SibSp[!is.na(dataAll$Survived)])
barSibSpSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                       , aes(x = SibSp
                             , y = Survived))
barSibSpSurv <- barSibSpSurv + stat_summary(fun.y="mean", geom="bar")
barSibSpSurv <- barSibSpSurv + ylim(0, 1)
barSibSpSurv

dataAll$sibspGroup <- dataAll$SibSp
dataAll$sibspGroup[dataAll$SibSp %in% c(1,2)] <- "1_2"
dataAll$sibspGroup[dataAll$SibSp %in% c(3,4)] <- "3_4"
dataAll$sibspGroup[dataAll$SibSp >= 5] <- "5+"
dataAll$sibspGroup <- myAsFactor(dataAll, "sibspGroup")
table(dataAll$sibspGroup)
checkDependence("SurvivedF", "sibspGroup")


# =============================================================================

dataTrain    <- dataAll[!is.na(dataAll$Survived),]
dataAge      <- dataAll[!is.na(dataAll$Age),]
dataTrainAge <- dataAll[!is.na(dataAll$Survived) & !is.na(dataAll$Age),]

myFitVIF <- function(myDf, modelFeatures){
  myFit <- glm(SurvivedF ~ .
                  , data = myDf[, c("SurvivedF", modelFeatures)]
                  , family = binomial)
  myFitSummary <- summary(myFit)
  varInfFact <- as.data.frame(vif(myFit))
  varInfFact <- varInfFact[order(-varInfFact$GVIF),]
  output <- list(myFitSummary, varInfFact)
}


variables0 <- c("Pclass", "cabinLetter", "noCabinsGroup", "titleGroup"
                , "ageBucket", "Embarked", "logFareS", "parchGroup"
                , "sibspGroup")
allFit <- myFitVIF(dataTrainAge, variables0)
allFit

corrMatrix <- matrix(rep(NA, length(variables0)^2), nrow = length(variables0))
rownames(corrMatrix) <- variables0
colnames(corrMatrix) <- variables0
for (var1 in 1:length(variables0)){
  for (var2 in 1:length(variables0)){
    if (var1 != var2){
      var1Type <- class(dataAll[,variables0[var1]])
      var2Type <- class(dataAll[,variables0[var2]])
      corrMatrix[var1,var2] <- if (var1Type == "factor" & var2Type == "factor"){
        checkDependence(variables0[var1], variables0[var2])
      } else if (var1Type == "numeric" & var2Type == "numeric"){
        cor(dataAll[,c(variables0[var1], variables0[var2])])
      # might want to investigate possible issues with applying ANOVA to imbalanced data
        } else if (var1Type == "factor" & var2Type == "numeric"){
        myAov <- aov(dataAll[,c(variables0[var2])] ~ dataAll[,c(variables0[var1])]
                     , data = dataAll)
        summary(myAov)[[1]][["Pr(>F)"]][[1]]
      } else if (var1Type == "numeric" & var2Type == "factor"){
        myAov <- aov(dataAll[,c(variables0[var1])] ~ dataAll[,c(variables0[var2])]
                     , data = dataAll)
        summary(myAov)[[1]][["Pr(>F)"]][[1]]
      }
    }
  }
}
corrMatrix
heatmap(corrMatrix, Rowv = NA, Colv = NA, symm = T)


# Pclass vs cabinLetter -------------------------
table(dataAll[,c("Pclass", "cabinLetter")])
checkDependence("Pclass", "cabinLetter")

barCabinNullSurv <- ggplot(dataAll[!is.na(dataAll$Survived)
                                   & dataAll$cabinLetter == "N",]
                       , aes(x = Pclass
                             , y = Survived))
barCabinNullSurv <- barCabinNullSurv + stat_summary(fun.y="mean", geom="bar")
barCabinNullSurv <- barCabinNullSurv + ylim(0, 1)
barCabinNullSurv

dataAll$cabinClass <- as.character(dataAll$cabinLetter)
dataAll$cabinClass[dataAll$cabinLetter == "N"] <- as.character(dataAll$Pclass[dataAll$cabinLetter == "N"])
dataAll$cabinClass <- myAsFactor(dataAll, "cabinClass")
table(dataAll$cabinClass)

barCabinClassSurv <- ggplot(dataAll[!is.na(dataAll$Survived),]
                       , aes(x=reorder(cabinClass, -Survived, mean)
                             , y = Survived))
barCabinClassSurv <- barCabinClassSurv + stat_summary(fun.y="mean", geom="bar")
barCabinClassSurv <- barCabinClassSurv + ylim(0, 1)
barCabinClassSurv


variables1 <- c("cabinClass", "noCabinsGroup", "titleGroup"
                , "ageBucket", "Embarked", "logFareS", "parchGroup"
                , "sibspGroup")
dataTrain    <- dataAll[!is.na(dataAll$Survived),]
dataAge      <- dataAll[!is.na(dataAll$Age),]
dataTrainAge <- dataAll[!is.na(dataAll$Survived) & !is.na(dataAll$Age),]
allFit1 <- myFitVIF(dataTrainAge, variables1)
allFit1
# AIC actually improved
# some VIF still high


# ageBucket vs titleGroup -----------------------
table(dataAge[, c("titleGroup", "ageBucket")])
table(dataTrainAge[, c("titleGroup", "ageBucket")])

variables2 <- c("cabinClass", "noCabinsGroup", "titleGroup"
                , "Embarked", "logFareS", "parchGroup"
                , "sibspGroup")
allFit2 <- myFitVIF(dataTrainAge, variables2)
allFit2
# remove Age from Model. now switch to full data set
# because missing age is no longer issue


# full data (no age missing) --------------------
allFit2_ <- myFitVIF(dataTrain, variables2)
allFit2_
# remove Age from Model. now switch to full data set
# because missing age is no longer issue


# removing cabinClass since high VIF ------------
variables3 <- variables2[!(variables2 %in% "cabinClass")]
allFit3 <- myFitVIF(dataTrain, variables3)
allFit3
# suddenly logFareS is very significant in model.
# Try removing that and re-introduce cabinClass


# removing logFareS -----------------------------
variables4 <- variables2[!(variables2 %in% "logFareS")]
allFit4 <- myFitVIF(dataTrain, variables4)
allFit4
# remove Age from Model. now switch to full data set
# because missing age is no longer issue


# adding interaction term logFareS:cabinClass ---
myFitInteraction1 <- glm(SurvivedF ~ . + cabinClass:logFareS - cabinClass - logFareS
                         , data = dataTrain[, c("SurvivedF", variables2)]
                         , family = binomial)
myFitInteraction1Summary <- summary(myFitInteraction1)
varInfFact1 <- as.data.frame(vif(myFitInteraction1))
varInfFact1 <- varInfFact1[order(-varInfFact1$GVIF),]
output1 <- list(myFitInteraction1Summary, varInfFact1)
output1


# removing Embarked -----------------------------
variables5 <- variables2[!(variables2 %in% "Embarked")]
myFitInteraction2 <- glm(SurvivedF ~ . + cabinClass:logFareS - cabinClass - logFareS
             , data = dataTrain[, c("SurvivedF", variables5)]
             , family = binomial)
myFitInteraction2Summary <- summary(myFitInteraction2)
varInfFact2 <- as.data.frame(vif(myFitInteraction2))
varInfFact2 <- varInfFact2[order(-varInfFact2$GVIF),]
output2 <- list(myFitInteraction2Summary, varInfFact2)
output2


# prediction accuracy (on training set) ---------------------------------------

dataTrain <- dataAll[!is.na(dataAll$Survived),]
dataTrain$SurvivedPred <- NA
dataTrain$SurvivedPred <- predict(myFitInteraction2, dataTrain, type = "response")

dataTrain$SurvivedPredF <- 0
dataTrain$SurvivedPredF[dataTrain$SurvivedPred > 0.5] <- 1
dataTrain$SurvivedPredF <- myAsFactor(dataTrain, "SurvivedPredF")

confusionMatrix(dataTrain$SurvivedPredF, dataTrain$SurvivedF)

hist(dataTrain$SurvivedPred)
myROC <- roc(dataTrain$SurvivedF, dataTrain$SurvivedPred)
plot.roc(myROC)
myROC$auc

# Might want to optimise threshold for "Balanced Accuracy"
# This is done below, but project Evaluation is on Accuracy
# So keeping the threshold at 0.5

optimalBalAcc <- min(abs(coords(myROC, x = "best", ret = "threshold")))
dataTrain$SurvivedPredFR <- 0
dataTrain$SurvivedPredFR[dataTrain$SurvivedPred > optimalBalAcc] <- 1
dataTrain$SurvivedPredFR <- myAsFactor(dataTrain, "SurvivedPredFR")

confusionMatrix(dataTrain$SurvivedPredFR, dataTrain$SurvivedF)


# Regularisation --------------------------------------------------------------
# Take a look again at model coefficients
# Some are quite large, e.g. for titleGroupMme & sibspGroup5+ or cabinClassT:logFareS
# This may suggest overfitting
# Try regluarisation to take these a notch down

myFitInteraction2Summary


glmnetD <- model.matrix(SurvivedF ~ . + cabinClass:logFareS - cabinClass - logFareS
                        , dataTrain[, c("SurvivedF", variables5)])

alphas  <- seq(from = 0, to = 1, by = 0.05)
lambdas <- exp(-seq(from = 1.5, to = 10, by = 0.05))
lambdAlpha <- c()
for (i in 1:10){
  print(i)
  for (a in alphas){
    myRegModel <- cv.glmnet(x = glmnetD
                            , y = dataTrain$SurvivedF
                            , family = "binomial"
                            , lambda = lambdas
                            , alpha = a)
    lambdAlpha <- rbind(lambdAlpha
                        , cbind(rep(a, length(lambdas))
                                , myRegModel$lambda
                                , myRegModel$cvm))
  }
}

lambdAlpha <- as.data.frame(lambdAlpha)
colnames(lambdAlpha) <- c("alpha", "lambda", "cvm")
lambdAlpha$alpha  <- as.factor(lambdAlpha$alpha)
lambdAlpha$lambda <- as.factor(lambdAlpha$lambda)
AgglambdAlpha <- lambdAlpha %>%
  group_by(alpha, lambda) %>%
  summarise(AvgCVM = mean(cvm))

optimLambdAlpha <- AgglambdAlpha[which.min(AgglambdAlpha$AvgCVM),]

myFitInteraction2r <- glmnet(x = glmnetD
                             , y = dataTrain$SurvivedF
                             , family = "binomial"
                             , lambda = c(as.numeric(as.character(optimLambdAlpha$lambda)))
                             , alpha = as.numeric(as.character(optimLambdAlpha$alpha))
                             , standardize = F)


# comparison of coefficients from:
#   regularised model using optimal lambda (cv.glmnet uses 10-fold validation)
#   regularised model using lambda = 0 (this should be same as non-regularised)
#   original non-regularised model using glm
# I still could not get exact matching results from glm and glmnet (lambda = 0)
# I suspect "lambda = 0" in glmnet is wrong though

myFitInteraction2r0 <- glmnet(x = glmnetD
                              , y = dataTrain$SurvivedF
                              , family = "binomial"
                              , lambda = 0
                              , standardize = F)


cbind(as.matrix(coef(myFitInteraction2r))
      , as.matrix(coef(myFitInteraction2r0))
      , c(myFitInteraction2$coefficients[1]
          , 0
          , myFitInteraction2$coefficients[-1]))


# =============================================================================
# =============================================================================

dataAll$SurvivedPredr <- NA
glmnetDall <- model.matrix(~ . + cabinClass:logFareS - cabinClass - logFareS
                           , dataAll[, variables5])
dataAll$SurvivedPredr <- as.numeric(predict(myFitInteraction2r, newx = glmnetDall, type = "response"))
dataAll$SurvivedPredrF <- 0
dataAll$SurvivedPredrF[dataAll$SurvivedPredr > 0.5] <- 1
dataAll$SurvivedPredrF <- myAsFactor(dataAll, "SurvivedPredrF")


dataTrain <- dataAll[!is.na(dataAll$Survived),]
confusionMatrix(dataTrain$SurvivedPredF, dataTrain$SurvivedF)
confusionMatrix(dataTrain$SurvivedPredFR, dataTrain$SurvivedF)
confusionMatrix(dataTrain$SurvivedPredrF, dataTrain$SurvivedF)

hist(dataTrain$SurvivedPredr)
myROCr <- roc(dataTrain$SurvivedF, dataTrain$SurvivedPredr)
plot.roc(myROCr)
myROCr$auc



dataTest <- dataAll[is.na(dataAll$Survived),]
summary(dataTest[,c("SurvivedPredFR", "SurvivedPredF", "SurvivedPredrF")])

selectModel <- dataTest[,c("PassengerId", "SurvivedPredrF")]
myAnswer <- selectModel
colnames(myAnswer) <- c("PassengerId", "Survived")
write.csv(x = myAnswer
          , file = paste0(colnames(selectModel)[2], "_clean.csv")
          , row.names = FALSE)




# =============================================================================
# APPENDIX --------------------------------------------------------------------
# =============================================================================


# example of pure independent
x1 <- round(runif(1000, 0, 5), 0)
x2 <- round(runif(1000, 0, 5), 0)
X <- as.data.frame(cbind(x1, x2))
contX <- table(X)
chisqX <- chisq.test(x = contX)
chisqX$p.value

# example of less independence
y1 <- round(runif(1000, 0, 5), 0)
y2 <- (y1 + round(rnorm(1000, 0, 3), 0)) %% 6
Y <- as.data.frame(cbind(y1, y2))
contY <- table(Y)
contY
chisqY <- chisq.test(x = contY)#, simulate.p.value = TRUE)
chisqY$p.value

# NOTE when applying chisq.test
# Use Monte Carlo Simulation when some classes are very scarce
# The simulation builds a likely distribution of the class
# Then uses that to compute a more reliable p-value
