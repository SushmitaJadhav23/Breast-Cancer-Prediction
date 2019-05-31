#Classification methods
#I am using this data to train machine learning model to predict forthcoming patients to cancerous/ non cancerous by reading.
#using KNN algorithm classification
#classification : Malignant , Non Malignant.
#K- nearest neighbor

#create object bc and import file
bc <- read.csv("Breast_Cancer.csv")
head(bc)
#here id is nominal data, which is a label
bc=bc[,-1] #removing first column

#frequency distribution for diagnosis
table(bc$diagnosis)
#357 no cancer , 212 cancer patient


#every variable has different scale, thus normalizating
#normalizing numeric data
normalize = function(x) {
        return((x-min(x)) / (max(x) - min(x)))
}

#3 chnages index, added new column diagnose r, normalize data
bc_n = as.data.frame(lapply(bc[2:31], normalize))
summary(bc_n) # <---- normalized 

set.seed(1000)

#randomly mixing data
bcrandom <- sample(1:nrow(bc_n), size = nrow(bc_n)*.8, replace = FALSE )   

#creating training and test data set

bc_train = bc_n[bcrandom, ]
bc_test = bc_n[-bcrandom, ]

bc_train_labels = bc[bcrandom, 1]
bc_test_labels = bc[-bcrandom, 1]

#Training a model on data
sqrt(569)
k=sqrt(nrow(bc))
#k=23.857 thus take odd number. Therefore k= 23

library(class)
bc_test_pred = knn(train = bc_train, test = bc_test, cl = bc_train_labels, k=23) 

#evaluate the model for prediction
table(bc_test_labels,bc_test_pred)

#Accuracy
install.packages("caret")
library(caret)
install.packages('e1071', dependencies=TRUE)

confusionMatrix(bc_test_pred, bc_test_labels)

#accuracy 98.25%



#The test data consisted of 114 observations. 
#Out of which 73 cases have been accurately predicted (TN->True Negatives) as Benign (B) in nature which constitutes 64%.
#Also, 39 out of 114 observations were accurately predicted (TP-> True Positives) as Malignant (M) in nature which constitutes 34.2%. 
#Thus a total of 39 out of 114 predictions where TP i.e, True Positive in nature.

#There were 2 cases of False Negatives (FN) meaning 2 cases were actually malignant in nature but got predicted as bening.


