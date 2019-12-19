library(readxl)
att.dat <- read_excel("C:/imarticus project/attrition/data set/Attrition.xlsx")
View(att.dat)

dim(att.dat)
head(att.dat) 

## Check data structure
str(att.dat)

## Check for any missing values in the data
colSums(is.na(att.dat))


## Checking for empty values
colSums(att.dat=='')

## Check number of uniques values for each of the column 
## to find out columns which we can convert to factors
sapply(att.dat, function(x) length(unique(x)))

library(dplyr)
att.dat <- att.dat %>% select(-c(EmployeeNumber, EmployeeCount))
str(att.dat)
#att.dat <-select(att.dat,-EmployeeNumber)

for (i in c("BusinessTravel","Department","Education","EducationField","EnvironmentSatisfaction","Gender",
            "JobInvolvement","JobLevel","JobRole","JobSatisfaction","MaritalStatus","Over18","OverTime",
            "PerformanceRating","RelationshipSatisfaction","StandardHours","StockOptionLevel","WorkLifeBalance")){
  att.dat[,i]=as.factor(att.dat[,i])
}


library(dummies)

att.dat <- dummy.data.frame(att.dat, names=c("BusinessTravel","Department","Education","EducationField","EnvironmentSatisfaction","Gender",
                                             "JobInvolvement","JobLevel","JobRole","JobSatisfaction","MaritalStatus","Over18","OverTime",
                                             "PerformanceRating","RelationshipSatisfaction","StandardHours","StockOptionLevel","WorkLifeBalance" ), sep="_")

str(att.dat)
library(caTools)
# Split data
set.seed(90)
split = sample.split(att.dat$Attrition,SplitRatio = 0.75)
attrition_train <- subset(att.dat,split == TRUE)
attrition_test <- subset(att.dat,split == FALSE)

print(c("Row in Train",nrow(attrition_train)))
print(c("Row in Test", nrow(attrition_test)))


table(attrition_train$Attrition)

table(attrition_test$Attrition)

#Model building-Logistic regression

set.seed(101)
attr_log <- glm(Attrition ~ ., data = attrition_train,family = 'binomial')
summary(attr_log)

### Predicting for test data
predict_test = predict(attr_log,newdata = attrition_test,type = 'response')



## Confusion matrix and statistics
library(caret)

confusion_mat <- table(attrition_test$Attrition, predict_test > 0.5)
confusion_mat

confusion_mat1 <- table(attrition_test$Attrition, predict_test > 0.3)
confusion_mat1


## ROC Curve and calculating the area under the curve(AUC)
library(ROCR)
predictions <- predict(attr_log,newdata=attrition_test,type="response")
ROCRpred <- prediction(predictions, attrition_test$Attrition)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))

#As a rule of thumb, a model with good predictive ability 
#should have an AUC closer to 1 (1 is ideal) 
auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc



#model building - Decision tree

library("rpart")
library("rpart.plot")
tree<-rpart(Attrition~., data = attrition_train, method = "class")
rpart.plot(tree)
plot(tree)
text(tree, digits = 3)
print(tree)
prp(tree,box.palette = "Reds")
rpart.rules(tree, roundint=FALSE, clip.facs=TRUE)
row.names(tree$frame)[tree$where]
library(rattle)
rattle::asRules(tree, TRUE)
#You can get the number of rules (leaves) in this way:
nrules <- as.integer(rownames(tree$frame[tree$frame$var == "<leaf>",]))
nrules
#You can also iterate for the rules like this:
#rules <- lapply(nrules, path.rpart, tree=fit, pretty=0, print.it=FALSE)
#Another alternative is using the package rpart.plot
#rules <- rpart.plot::rpart.rules(model, cover=T, nn=T)

predict_unseen <-predict(tree, attrition_train, type = 'class')

predict_unseen
table_mat <- table(attrition_train$Attrition, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
paste("Accuracy for Test set=",accuracy_Test)

####Variable importance
library(RColorBrewer)
var_imp <- data.frame(tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp
var_imp$importance <- round(var_imp$tree.variable.importance, 2)
var_imp$tree.variable.importance <- NULL

colorCount <- length(unique(var_imp$features))
colorCount
feature_importance <- var_imp %>%
  ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + geom_bar(stat='identity') + coord_flip() + 
  theme_minimal() + theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(), 
                          plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
                          axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                          axis.title=element_text(colour="white"), 
                          legend.background = element_rect(fill="#FFF9F5",
                                                           size=0.5, linetype="solid", 
                                                           colour ="black")) + scale_fill_manual(values = colorRampPalette(brewer.pal(24, "Set2"))(colorCount)) + 
  geom_label(aes(label=paste0(importance, "%")), colour = "white", fontface = "italic", hjust=0.6) + 
  labs(title="Feature Importance for our Decision Tree Model", x="Features", y="Importance")

feature_importance 



## Making correlation matrix for numeric variables
att_cor <- cor(att.dat[,setdiff(cont_vars,"EmployeeCount")])

library(corrplot)
# Plotting correlation plot

corrplot(att_cor)

##Attrition stats

att_target<-table(att.dat$Attrition)

pieperc<-round(att_target*100/sum(att_target),1)
pieperc
legend1<-c("Attrition-Yes","Attrition-No")


pie(att_target, labels = pieperc, main="Attrition Stats", col = rainbow(length(att_target)))
legend("topright", c("Attrition-Yes","Attrition-No"), cex=0.8, fill=rainbow(length(att_target)))

png(file = "Attrition.png")
dev.off()


####Gender - Attrition
att.dat1<-read.csv("C:/Users/Admin/Desktop/python2019/attrition/attrition.csv",header=T)

# Split data
set.seed(90)
split = sample.split(att.dat1$Attrition,SplitRatio = 0.75)
attrition_train <- subset(att.dat1,split == TRUE)
attrition_test <- subset(att.dat1,split == FALSE)

counts<-table(att.dat1$Attrition,att.dat1$Gender)
counts


barplot(counts, main="Gender base distribution vs Attrition",
        xlab="Gender", ylab="Attrition", col=c("darkblue","red"),
        names.arg=c("Female","Male" ),
        legend = rownames(counts), cex.names=0.6) 


barplot(counts, main="Gender base distribution vs Attrition",
        xlab="Gender", ylab="Attrition", col = c("lightblue", "mistyrose"),
        names.arg=c("Female","Male" ),
        legend = rownames(counts), beside=TRUE, cex.names=0.6) 



#################

library(ggplot2)
library(cowplot)


attritions_number <- att.dat1 %>% group_by(Attrition) %>% summarise(Count=n()) %>%
  ggplot(aes(x=Attrition, y=Count)) + geom_bar(stat="identity", fill="orange", color="grey40") + theme_bw() + coord_flip() + 
  geom_text(aes(x=Attrition, y=0.01, label= Count),
            hjust=-0.8, vjust=-1, size=3, 
            colour="black", fontface="bold",
            angle=360) + labs(title="Employee Attrition (Amount)", x="Employee Attrition",y="Amount") + theme(plot.title=element_text(hjust=0.5))

attrition_percentage <-  att.dat1 %>% group_by(Attrition) %>% summarise(Count=n()) %>% 
  mutate(pct=round(prop.table(Count),2) * 100) %>% 
  ggplot(aes(x=Attrition, y=pct)) + geom_bar(stat="identity", fill = "dodgerblue", color="grey40") + 
  geom_text(aes(x=Attrition, y=0.01, label= sprintf("%.2f%%", pct)),
            hjust=0.5, vjust=-3, size=4, 
            colour="black", fontface="bold") + theme_bw() + labs(x="Employee Attrition", y="Percentage") + 
  labs(title="Employee Attrition (%)") + theme(plot.title=element_text(hjust=0.5))




plot_grid(attritions_number, attrition_percentage, align="h", ncol=2)



# Boxplot with attrition in the X-axis and Job Satisfaction in the y-Axis
options(repr.plot.width=8, repr.plot.height=6) 


box.attrition <- att.dat1 %>% select(Attrition, JobSatisfaction, Gender) %>% 
  ggplot(aes(x=Attrition, y=JobSatisfaction, fill=Attrition)) + geom_boxplot(color="black") + theme_minimal() + facet_wrap(~Gender) + 
  scale_fill_manual(values=c("#FA5858", "#9FF781"))


# Distribution of Job Satisfaction
dist.satisfaction <- att.dat1 %>% select(JobSatisfaction) %>%
  ggplot(aes(x=JobSatisfaction)) + geom_density(color="#013ADF", fill="#81BEF7", trim=TRUE)  + xlim(range(c(1,4)))


plot_grid(box.attrition, dist.satisfaction, nrow=2)

plot_grid(box.attrition,  nrow=2)

########Average salary by gender

options(repr.plot.width=10, repr.plot.height=8) 

gender.income <- att.dat1 %>% select(Gender, MonthlyIncome) %>% group_by(Gender) %>% summarise(avg_income=round(mean(MonthlyIncome), 2)) %>%
  ggplot(aes(x=Gender, y=avg_income)) + geom_bar(stat="identity", fill="#2E9AFE", width=0.5) + 
  geom_text(aes(x=Gender, y=0.01, label= paste0("$ ", avg_income)),
            hjust=-2, vjust=0, size=3, 
            colour="black", fontface="bold",
            angle=360) + labs(title="Average Salary by Gender", x="Gender",y="Salary") + coord_flip() + 
  theme_minimal() + theme(plot.title=element_text(size=14, hjust=0.5))

# # How many people work in each department by gender
gender.department <- att.dat1 %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x=reorder(Department, -amount), y=amount, fill=Gender)) + geom_bar(stat="identity", position="dodge") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + scale_fill_manual(values=c("pink", "lightblue")) + 
  labs(title="Number of Employees \n
by Department",x="Department", y="Number of employees")


plot_grid(gender.income, gender.department, ncol=1, nrow=2)

####Attrition by education level

options(repr.plot.width=8, repr.plot.height=4) 
library(forcats)

# Give names for the different education levels.
att.dat1$Educational_Levels <-  ifelse(att.dat1$Education == 1, "Without College D.",
                                 ifelse(att.dat1$Education == 2 , "College D.",
                                        ifelse(att.dat1$Education == 3, "Bachelors D.",
                                               ifelse(att.dat1$Education == 4, "Masters D.", "Phd D."))))

# I want to know in terms of proportions if we are loosing key talent here.
edu.level <- att.dat1 %>% select(Educational_Levels, Attrition) %>% group_by(Educational_Levels, Attrition) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=fct_reorder(Educational_Levels,n), y=n, fill=Attrition, color=Attrition)) + geom_bar(stat="identity") + facet_wrap(~Attrition) + 
  coord_flip() + scale_fill_manual(values=c("#2EF688", "#F63A2E")) + scale_color_manual(values=c("#09C873","#DD1509")) + 
  geom_label(aes(label=n, fill = Attrition), colour = "white", fontface = "italic") + 
  labs(x="", y="Number of Employees", title="Attrition by Educational Level") + 
  theme(legend.position="none", plot.title=element_text(hjust=0.5, size=14))

edu.level


###Level of Attrition by Overtime Status

att.dat1 %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100)

options(repr.plot.width=10, repr.plot.height=5) 


overtime_percent <- att.dat1 %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>% 
  ggplot(aes(x="", y=pct, fill=OverTime)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  scale_fill_manual(values=c("#2EFE64", "#FE2E2E")) + 
  geom_label(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5), colour = "white",  fontface = "italic")+
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"), 
        legend.background = element_rect(fill="#FFF9F5",
                                         size=0.5, linetype="solid", colour ="black")) + 
  labs(title="Level of Attrition by Overtime Status", subtitle="In Percent", x="", y="") 


overtime_number <- att.dat1 %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>% 
  ggplot(aes(x=OverTime, y=n, fill=OverTime)) + geom_bar(stat="identity") + scale_fill_manual(values=c("#BCF5A9", "#F5BCA9")) + 
  geom_label(aes(label=paste0(n)), fill="#FFF9F5", colour = "black", fontface = "italic") + 
  labs(title="Level of Attrition by Overtime Status", subtitle="In Numbers", x="Overtime Status", y="Number of Employees") + theme_minimal() + 
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"), 
        legend.background = element_rect(fill="#FFF9F5",
                                         size=0.5, linetype="solid", 
                                         colour ="black")) 





plot_grid(overtime_percent, overtime_number)


##Attrition by Job Role
library(plotrix)

options(repr.plot.width=10, repr.plot.height=6) 
attr.job <- att.dat1 %>% select(JobRole, Attrition) %>% group_by(JobRole, Attrition) %>% summarize(amount=n()) %>%
  mutate(pct=round(prop.table(amount),2) * 100) %>% arrange(pct)

nofunc <- colorRampPalette(c("#A9F5A9", "#58FA58", "#01DF01"))
yesfunc <- colorRampPalette(c("#F5A9A9", "#FE2E2E", "#B40404"))

yes.attr <- attr.job %>% filter(Attrition == "Yes") %>% arrange(JobRole) 
no.attr <- attr.job %>% filter(Attrition == "No") %>% arrange(JobRole)

par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, labels = unique(attr.job$JobRole),
                       top.labels=c("No","","Yes"), main = "Attrition by Job Role", 
                       gap=30, show.values = T, rxcol = yesfunc(9), lxcol = nofunc(9)))




####income and hike impact

options(repr.plot.width=8, repr.plot.height=7) 



per.sal <- att.dat1 %>% select(Attrition, PercentSalaryHike, MonthlyIncome) %>% 
  ggplot(aes(x=PercentSalaryHike, y=MonthlyIncome)) + geom_jitter(aes(col=Attrition), alpha=0.5) + 
  theme(legend.position="none") + scale_color_manual(values=c("#58FA58", "#FA5858")) + 
  labs(title="Income and its Impact on Attrition") + theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                                                           axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                                                           axis.title=element_text(colour="white"))

perf.inc <- att.dat1 %>% select(PerformanceRating, MonthlyIncome, Attrition) %>% group_by(factor(PerformanceRating), Attrition) %>% 
  ggplot(aes(x=factor(PerformanceRating), y=MonthlyIncome, fill=Attrition)) + geom_violin() + coord_flip() + facet_wrap(~Attrition) + 
  scale_fill_manual(values=c("#58FA58", "#FA5858")) +   theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"), 
        legend.text=element_text(color="white")) + 
  labs(x="Performance Rating",y="Monthly Income") 


plot_grid(per.sal, perf.inc, nrow=2)

