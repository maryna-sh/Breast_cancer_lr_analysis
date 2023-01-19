# # importing libraries
library(ggplot2)
library(dplyr)
library(caret)
library(ggcorrplot)
library(ggpubr)
library(naniar)
options(warn = -1)


# importing data

df <- read.csv("breast_cancer.csv")

#studying the dataset
head(df)

# checking unique values
unique(df$Class)

# replacing the values with 0 and 1 for the purpose of building logistic regression model

df$Class <- ifelse(df$Class == 2, 0, 1)


# checking if any columns have missing values

colSums(is.na(df))

# using vis_miss function to visually identify missing values
vis_miss(df)

# finding correlations between the variables

correlation <- cor(df[,1:10])

ggcorrplot(correlation)

# pairwise correlation
pairs(df,
      cex.labels = 0.3,
      col = c("#E7AB79"),
      pch = 21,
      main = "Pairwise correlation",
      col.labels = "black")


# preparing data for the analysis

# splitting into test and train set

# creating the index for the split
index <- createDataPartition(df$Class, p=0.8, times = 1, list=FALSE)

# splitting the data into train and test sets

train_set <- df%>% slice(index)
test_set <- df%>% slice(-index)


#training logistic regression

classifier <- glm(Class ~.,
                  family = binomial,  # specification of logistic regression
                  data = train_set)


# -- finding y_hat
predicted_probability <- predict(classifier, 
                                 type = "response",  # gives result listed in a single vector
                                 newdata = test_set[-10])  # removing dep. var. column

y_hat <- ifelse(predicted_probability > 0.5, 1, 0)


# -- confusion matrix 


cm <- table(test_set[,10], y_hat)
cm


# accuracy

accuracy <- ((cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[2,1] + cm[1,2]))*100
cat(paste("Accuracy: ", round(accuracy,2), "%"))

# precison : when a model predicts a positive value, how often is it right?

precision <- (cm[1,1] / (cm[1,1] + cm[1,2]))*100
cat(paste("Precision: ", round(precision,2), "%"))


# recall - the model's ability to predict positive values
# (how often does a model predict the correct positive values)

recall <- (cm[1,1] / (cm[1,1] + cm[2,1]))*100
cat(paste("Recall: ", round(recall,2), "%"))


# F-1
F_1 <- (2*precision * recall) / (precision + recall)
cat(paste("F_1 score: ", round(F_1,2), "%"))



# Cross-validation of the model


# monte carlo simulation

lg <- replicate(100, {
  
  index <- createDataPartition(df$Class, p=0.8, times = 1, list=FALSE)
  train_set <- df%>% slice(index)
  test_set <- df%>% slice(-index)
  classifier <- glm(Class ~.,
                    family = binomial,
                    data = train_set)
  predicted_probability <- predict(classifier, 
                                   type = "response",
                                   newdata = test_set[-10])
  
  y_hat <- ifelse(predicted_probability > 0.5, 1, 0)
  
  correct_predictions <- sum(y_hat == test_set$Class)
  
  total_predictions <- length(y_hat)
  
  accuracy <- correct_predictions / total_predictions
  
  return(accuracy)
  
})

paste("Cross-validated accuracy of the model: ",mean(lg)*100)


# ------ summary ----

summary(classifier)


# removing Uniformity of cell size to see if there are any differences
classifier_2 <- glm(Class ~.,
                    family = binomial,  # specification of logistic regression
                    data = train_set[-2])
summary(classifier_2)


# -- visualization 

library(showtext)
showtext_auto()

ggplot() + 
  geom_point(aes(df$Clump.Thickness,df$Class)) + 
  geom_smooth(aes(df$Clump.Thickness,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  ggtitle("Breast Cancer Diagnosis \nusing Clump Thickness") +
  ylab("Tumour type") +
  xlab("Clump thickness") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.title = element_text(size = 25, face="bold", 
                                  family = "Mishafi Gold",
                                  margin = margin(10,0,10,0)),
        plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 15),
        axis.title.x = element_text(family = "Mishafi Gold", 
                                    size = 20, margin = margin(11,0,10,0)),
        axis.text.y = element_text(family = "Mishafi Gold", size = 15),
        axis.title.y = element_text(family = "Mishafi Gold", 
                                    size = 20, margin=margin(0,10,0,11)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))




# --------------------------


ggplot() + 
  geom_point(aes(df$Marginal.Adhesion,df$Class)) + 
  geom_smooth(aes(df$Marginal.Adhesion,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  ggtitle("Breast Cancer Diagnosis \nusing Marginal adhesion") +
  ylab("Tumour type") +
  xlab("Marginal adhesion") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.title = element_text(size = 25, face="bold", 
                                  family = "Mishafi Gold",
                                  margin = margin(10,0,10,0)),
        plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 15),
        axis.title.x = element_text(family = "Mishafi Gold", 
                                    size = 20, margin = margin(11,0,10,0)),
        axis.text.y = element_text(family = "Mishafi Gold", size = 15),
        axis.title.y = element_text(family = "Mishafi Gold", 
                                    size = 20, margin=margin(0,10,0,11)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))


# -------------------

ggplot() + 
  geom_point(aes(df$Bare.Nuclei,df$Class)) + 
  geom_smooth(aes(df$Bare.Nuclei,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  ggtitle("Breast Cancer Diagnosis \nusing Barei nuclei") +
  ylab("Tumour type") +
  xlab("Barei nuclei") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.title = element_text(size = 25, face="bold", 
                                  family = "Mishafi Gold",
                                  margin = margin(10,0,10,0)),
        plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 15),
        axis.title.x = element_text(family = "Mishafi Gold", 
                                    size = 20, margin = margin(11,0,10,0)),
        axis.text.y = element_text(family = "Mishafi Gold", size = 15),
        axis.title.y = element_text(family = "Mishafi Gold", 
                                    size = 20, margin=margin(0,10,0,11)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))

# ---------------------------------

ggplot() + 
  geom_point(aes(df$Mitoses,df$Class)) + 
  geom_smooth(aes(df$Mitoses,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  ggtitle("Breast Cancer Diagnosis \nusing Mitoses") +
  ylab("Tumour type") +
  xlab("Mitoses") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.title = element_text(size = 25, face="bold", 
                                  family = "Mishafi Gold",
                                  margin = margin(10,0,10,0)),
        plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 15),
        axis.title.x = element_text(family = "Mishafi Gold", 
                                    size = 20, margin = margin(11,0,10,0)),
        axis.text.y = element_text(family = "Mishafi Gold", size = 15),
        axis.title.y = element_text(family = "Mishafi Gold", 
                                    size = 20, margin=margin(0,10,0,11)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))


# ---------------------------

ggplot() + 
  geom_point(aes(df$Bland.Chromatin,df$Class)) + 
  geom_smooth(aes(df$Bland.Chromatin,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  ggtitle("Breast Cancer Diagnosis \nusing Bland chromatin") +
  ylab("Tumour type") +
  xlab("Bland chromatin") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.title = element_text(size = 25, face="bold", 
                                  family = "Mishafi Gold",
                                  margin = margin(10,0,10,0)),
        plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 15),
        axis.title.x = element_text(family = "Mishafi Gold", 
                                    size = 20, margin = margin(11,0,10,0)),
        axis.text.y = element_text(family = "Mishafi Gold", size = 15),
        axis.title.y = element_text(family = "Mishafi Gold", 
                                    size = 20, margin=margin(0,10,0,11)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))

# ---------------

ggplot() + 
  geom_point(aes(df$Normal.Nucleoli,df$Class)) + 
  geom_smooth(aes(df$Normal.Nucleoli,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  ggtitle("Breast Cancer Diagnosis \nusing Normal nucleoli") +
  ylab("Tumour type") +
  xlab("Normal nucleoli") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.title = element_text(size = 25, face="bold", 
                                  family = "Mishafi Gold",
                                  margin = margin(10,0,10,0)),
        plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 15),
        axis.title.x = element_text(family = "Mishafi Gold", 
                                    size = 20, margin = margin(11,0,10,0)),
        axis.text.y = element_text(family = "Mishafi Gold", size = 15),
        axis.title.y = element_text(family = "Mishafi Gold", 
                                    size = 20, margin=margin(0,10,0,11)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5,0,0, "cm"))


# ---- arranging all the plots together so they can be easily compared




# gathering all the plots together so they are easier to compare


ct <- ggplot() + 
  geom_point(aes(df$Clump.Thickness,df$Class)) + 
  geom_smooth(aes(df$Clump.Thickness,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Mishafi Gold", size = 10),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5, 0, 0, "cm")) +
  annotate("text",x=7.0, y=0.2, label="Clump thickness", size = 6)


# ------

ma <- ggplot() + 
  geom_point(aes(df$Marginal.Adhesion,df$Class)) + 
  geom_smooth(aes(df$Marginal.Adhesion,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Mishafi Gold", size = 10),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5, 0, 0, "cm")) +
  annotate("text",x=6.5, y=0.2, label="Marginal adhesion", size = 6)

# -----

bn <- ggplot() + 
  geom_point(aes(df$Bare.Nuclei,df$Class)) + 
  geom_smooth(aes(df$Bare.Nuclei,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Mishafi Gold", size = 10),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5, 0, 0, "cm")) +
  annotate("text",x=6.5, y=0.2, label="Barei nuclei", size = 6)

# ------

um <- ggplot() + 
  geom_point(aes(df$Mitoses,df$Class)) + 
  geom_smooth(aes(df$Mitoses,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Mishafi Gold", size = 10),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5, 0, 0, "cm")) +
  annotate("text",x=6.5, y=0.2, label="Mitoses", size = 6)

# --------

bc <- ggplot() + 
  geom_point(aes(df$Bland.Chromatin,df$Class)) + 
  geom_smooth(aes(df$Bland.Chromatin,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Mishafi Gold", size = 10),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5, 0, 0, "cm")) +
  annotate("text",x=6.5, y=0.2, label="Bland chromatin", size = 6)

# ------

nn <- ggplot() + 
  geom_point(aes(df$Normal.Nucleoli,df$Class)) + 
  geom_smooth(aes(df$Normal.Nucleoli,df$Class),
              method = "glm", se = FALSE, method.args = list(family = "binomial"), 
              color = "#557153", size = 1.2) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Benign", "Malignant")) +
  theme(plot.background = element_rect(fill = "#F0EBCE"),
        panel.background = element_rect(fill = "#F0EBCE"),
        axis.text.x = element_text(family = "Mishafi Gold", size = 10),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Mishafi Gold", size = 10),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.5, 0, 0, "cm")) +
  annotate("text",x=6.5, y=0.2, label="Normal nucleoli", size = 6)



ggarrange(ct, ma, bn, nn, um, bc,
          ncol = 2, nrow = 3)

































