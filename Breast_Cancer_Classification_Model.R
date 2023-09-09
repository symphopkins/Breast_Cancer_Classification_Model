#Question 1
library(readr)
breast_cancer_data <- read_csv("Documents/Maryville_University/DSCI_502/breast_cancer_data.csv")
View(breast_cancer_data)

#Question 2
BoxplotPredictorOnTarget <- function (target, predictor){
  library(ggplot2)
  result <- ggplot(data=breast_cancer_data, aes(x=predictor, y=target)) + 
            geom_boxplot(aes(col= target), notch = TRUE)
  return(result)
}

#Question 2A
BoxplotPredictorOnTarget(breast_cancer_data$diagnosis,breast_cancer_data$area_mean)

#Question 2B
BoxplotPredictorOnTarget(breast_cancer_data$diagnosis,breast_cancer_data$area_se)

#Question 2C
BoxplotPredictorOnTarget(breast_cancer_data$diagnosis,breast_cancer_data$texture_mean)

#Question 3
breast_cancer_data$diagnosis <- as.factor(breast_cancer_data$diagnosis)
levels(breast_cancer_data$diagnosis)
library(pscl)

#Question 3A
glm_result_3a <- glm(diagnosis ~ area_mean, family = binomial, data = breast_cancer_data)
summary(glm_result_3a)
pR2(glm_result_3a)

#Question 3B
glm_result_3b <- glm(diagnosis ~ area_mean + area_se, family = binomial, data = breast_cancer_data)
summary(glm_result_3b)
pR2(glm_result_3b)

#Question 3C
glm_result_3c <- glm(diagnosis ~ area_mean + area_se + texture_mean, family = binomial, data = breast_cancer_data)
summary(glm_result_3c)
pR2(glm_result_3c)

#Question 3D
glm_result_3d <- glm(diagnosis ~ area_mean + area_se + texture_mean + concavity_worst, family = binomial, data = breast_cancer_data)
summary(glm_result_3d)
pR2(glm_result_3d)

#Question 3E
glm_result_3e <- glm(diagnosis ~ area_mean + area_se + texture_mean + concavity_worst + concavity_mean, family = binomial, data = breast_cancer_data)
summary(glm_result_3e)
pR2(glm_result_3e)





















