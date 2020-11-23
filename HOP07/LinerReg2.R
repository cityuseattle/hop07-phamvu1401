library(caTools)
library(ggplot2)
# Read CSV, note the delimiter (sep)
df <- read.csv('student-mat.csv',header=TRUE, sep=';')

#Set a random see so your "random results are the same as this HOP"
set.seed(101)

#Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(df$G3, SplitRatio = 0.70) #SplitRatio = percent of sample==TRUE

# Training Data, 70% will be traininng data
train = subset(df, sample == TRUE)

#Testing Data, 30% will be test data 
test = subset(df, sample == FALSE)

#Train & Build Model
model = lm(G3 ~.,train)

print(summary(model))

#Grab residuals
res <- residuals(model)

#Convert to DataFrame for ggplot
res <- as.data.frame(res)

#print first 6 rows of the res column
print(head(res))

#Histogram
#print(ggplot(res,aes(res)) + geom_histogram(fill='blue',alpha=0.5))

#plot(model)

#We are going to make a variable G3.prediction and we want to use the
#predict function from stats. In order to do this to use to predict function,
#we should pass our model as first arg and our test data as second arg
#if you are wondering how the predict func knows which value to predict?
#This info in your model <- lm(G3~.,train)
#Your model knows the formula and know you are trying to predict G3 based
#off all the feature of your training dataset
#So it means your predict function needs the model that's already been trained
#and your test set should look same as your trainingset (same format)
G3.predictions <- predict(model,test)

#Create a dataframe with the two columns the prediction data and the 
#actual data
results <- cbind(G3.predictions,test$G3)

#name the two columns pred(prediction), real (actual data)
colnames(results) <- c('pred', 'real')
results <- as.data.frame(results)

print(head(results))

#Function to check if the number is < 0, then make it zero
to_zero <- function(x){
    if (x < 0){
        return(0)
    }else{
        return(x)
    }
}

#Applying zero function
results$pred <- sapply(results$pred,to_zero)

#To make sure that the min value of your predicted col is zero
print(min(results))

mse <- mean((results$real-results$pred)^2)

print('MSE')
print(mse)

#the R-Squared Value for our model (just for the predictions)
SSE <- sum((results$pred - results$real)^2)
SST <- sum((mean(df$G3) - results$real)^2)

R2 <- 1 - SSE/SST
print(R2)