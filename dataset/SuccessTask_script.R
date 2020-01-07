library(lme4)
library(plyr)  
library(ggplot2) 
library(stats)
library(dplyr)
library(MuMIn)
library(lsmeans)
library(lmerTest)
library(lemon)

# Function to calculate standard errors
st.err <- function(x) {
  round(sd(x)/sqrt(length(x)), 3)
}

# Set folder
setwd("/Users/francoisfoerster/Desktop/Paper")

## Load datasets for the Tool use and Tool move tasks
dataset_Use <- read.csv(file="SuccessTask_Use_dataset.csv", header=TRUE, sep=",")
dataset_Use <- dataset_Use[,2:ncol(dataset_Use)]

dataset_Move <- read.csv(file="SuccessTask_Move_dataset.csv", header=TRUE, sep=",")
dataset_Move <- dataset_Move[,2:ncol(dataset_Move)]


# Two participants are outliers in the tool use task: no learning
ggplot(data = dataset_Use, aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name)) +
  geom_point(shape = 21, size = 1.5, stroke = 1.5, alpha = 0.9) +
  ggtitle("Accuracy per block \n to Use the tools") + theme_bw() + 
  labs(x = "Trial Blocks", y = "Percentage of Success") + 
  ylim(0, 100) +
  theme(text = element_text(size=22))


# Removal of the two participants outliers
dataset_Use <- dplyr::filter(dataset_Use, dataset_Use$Subject != 34) # Very poor performance
dataset_Use <- dplyr::filter(dataset_Use, dataset_Use$Subject != 6) # Poor performance especially in block 1
dataset_Move <- dplyr::filter(dataset_Move, dataset_Move$Subject != 34)
dataset_Move <- dplyr::filter(dataset_Move, dataset_Move$Subject != 6)

# Data without two participant outliers
ggplot(data = dataset_Use, aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name)) +
  geom_point(shape = 21, size = 1.5, stroke = 1.5, alpha = 0.9) +
  ggtitle("Accuracy per block \n to Use the tools") + theme_bw() + 
  labs(x = "Trial Blocks", y = "Percentage of Success") + 
  ylim(0, 100) +
  theme(text = element_text(size=22))


# Create datasets with means and standard errors to plot for each task to use and to move the tools
data_Use_Means <- cbind( aggregate(PercentOfSuccess ~ TrialBlock + Name, dataset_Use, FUN = mean),
                  aggregate(PercentOfSuccess ~ TrialBlock + Name, dataset_Use, FUN = st.err)[,3])
colnames(data_Use_Means) <- c("TrialBlock", "Name", "PercentOfSuccess", "SE")

data_Move_Means <- cbind( aggregate(PercentOfSuccess ~ TrialBlock + Name, dataset_Move, FUN = mean),
                         aggregate(PercentOfSuccess ~ TrialBlock + Name, dataset_Move, FUN = st.err)[,3])
colnames(data_Move_Means) <- c("TrialBlock", "Name", "PercentOfSuccess", "SE")


# Accuracy in the Use task, based on means
ggplot(data = data_Use_Means,
       aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name)) +
  geom_path(position=position_dodge(-0.4)) +
  geom_point(aes(fill = Name), shape = 21, size = 2, stroke = 1.5, alpha = 1, position=position_dodge(-0.4)) +
  geom_errorbar(data = data_Use_Means, aes(ymin= PercentOfSuccess - SE, ymax= PercentOfSuccess + SE, x = TrialBlock, group = Name),
                position=position_dodge(-0.4), size = 0.8, width = 0.1) +
  ggtitle("Accuracy per block to Use the tools") + theme_bw() + 
  labs(x = "Trial Blocks", y = "Percentage of Success") + 
  ylim(55, 101) +
  theme(text = element_text(size=20))

# Accuracy in the Move task, based on means
ggplot(data = data_Move_Means,
       aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name)) +
  geom_path(position=position_dodge(-0.4)) +
  geom_point(aes(fill = Name), shape = 21, size = 2, stroke = 1.5, alpha = 1, position=position_dodge(-0.4)) +
  geom_errorbar(data = data_Move_Means, aes(ymin= PercentOfSuccess - SE, ymax= PercentOfSuccess + SE, x = TrialBlock, group = Name),
                position=position_dodge(-0.4), size = 0.8, width = 0.1) +
  ggtitle("Accuracy per block to Move the tools") + theme_bw() + 
  labs(x = "Trial Blocks", y = "Percentage of Success") + 
  ylim(55, 101) +
  theme(text = element_text(size=20))




# Accuracy in the Use task, with individual datapoints
ggplot(data = dataset_Use, aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name)) +
  geom_jitter(size = 0.3, stroke = 1, alpha = 1, position = position_jitterdodge(jitter.width = 0.4, dodge.width = -1) ) +
  geom_path(data = data_Use_Means, aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name), position=position_dodge(-0.2)) +
  geom_point(data = data_Use_Means, aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name, fill = Name), shape = 21, size = 2, stroke = 1.5, alpha = 1, position=position_dodge(-0.2)) +
  geom_errorbar(data = data_Use_Means, aes(ymin= PercentOfSuccess - SE, ymax= PercentOfSuccess + SE, x = TrialBlock, group = Name),
                position=position_dodge(-0.2), size = 0.8, width = 0.1) +
  ggtitle("Accuracy per block to Use the tools") + theme_bw() + 
  labs(x = "Trial Blocks", y = "Percentage of Success") + 
  ylim(55, 101) +
  theme(text = element_text(size=20))


# Accuracy in the Move task, with individual datapoints
ggplot(data = dataset_Move, aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name)) +
  geom_jitter(size = 0.3, stroke = 1, alpha = 1, position = position_jitterdodge(jitter.width = 0.4, dodge.width = -1) ) +
  geom_path(data = data_Move_Means, aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name), position=position_dodge(-0.2)) +
  geom_point(data = data_Move_Means, aes(x = as.factor(TrialBlock), y = PercentOfSuccess, group = Name, color = Name, fill = Name), shape = 21, size = 2, stroke = 1.5, alpha = 1, position=position_dodge(-0.2)) +
  geom_errorbar(data = data_Move_Means, aes(ymin= PercentOfSuccess - SE, ymax= PercentOfSuccess + SE, x = TrialBlock, group = Name),
                position=position_dodge(-0.2), size = 0.8, width = 0.1) +
  ggtitle("Accuracy per block to Move the tools") + theme_bw() + 
  labs(x = "Trial Blocks", y = "Percentage of Success") + 
  ylim(55, 101) +
  theme(text = element_text(size=20))





## -------- ## -------- ##
## Statistical Analysis ##
## -------- ## -------- ##

## -----  Analysis of the accuracy in the Use task

# M and SD of the accuracy in the Use task
mean(dataset_Use$PercentOfSuccess)
sd(dataset_Use$PercentOfSuccess)

## -- Main effects of the Label
full.Model <- lmer(PercentOfSuccess ~ Name + TrialBlock  + (1|Subject) + (0+TrialBlock|Subject),
                   dataset_Use, REML = F, control = lmerControl("bobyqa")) 
null.Model <- lmer(PercentOfSuccess ~ 1 + TrialBlock + (1|Subject)  + (0+TrialBlock|Subject),
                   dataset_Use, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name, dataset_Use, "mean"),
               aggregate(PercentOfSuccess ~ Name, dataset_Use, "sd")[,2])
names(table) <- c("Name", "Mean", "StandardDeviation")
print(table)


##   Interaction analyses
interaction.Model <- lmer(PercentOfSuccess ~ Name + TrialBlock + Name*TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                          dataset_Use, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ Name + TrialBlock + (1|Subject)  + (0+TrialBlock|Subject),
                   dataset_Use, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, interaction.Model, test = 'Chisq')
r.squaredLR(interaction.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name*TrialBlock, dataset_Use, "mean"),
               aggregate(PercentOfSuccess ~ Name*TrialBlock, dataset_Use, "sd")[,3])
names(table) <- c("Name", "Task", "Mean", "StandardDeviation")
print(table)



## -------- Analyses of Success to Use the tools per Block and Name:

## -- Effects of the Label in block 1
taskDatasetSubset <- subset(dataset_Use, dataset_Use$TrialBlock == 1)
full.Model <- lmer(PercentOfSuccess ~ Name + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ 1 + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "mean"),
               aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


## -- Effects of the Label in block 2
taskDatasetSubset <- subset(dataset_Use, dataset_Use$TrialBlock == 2)
full.Model <- lmer(PercentOfSuccess ~ Name  + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ 1 + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "mean"),
               aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


## -- Effects of the Label in block 3
taskDatasetSubset <- subset(dataset_Use, dataset_Use$TrialBlock == 3)
full.Model <- lmer(PercentOfSuccess ~ Name  + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ 1 + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "mean"),
               aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)





## -----  Analysis of the accuracy in the Move task

# M and SD of the accuracy in the Use task
mean(dataset_Move$PercentOfSuccess)
sd(dataset_Move$PercentOfSuccess)

## -- Main effects of the Label
full.Model <- lmer(PercentOfSuccess ~ Name + TrialBlock  + (1|Subject) + (0+TrialBlock|Subject),
                   dataset_Move, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ 1 + TrialBlock + (1|Subject)  + (0+TrialBlock|Subject),
                   dataset_Move, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name, dataset_Move, "mean"),
               aggregate(PercentOfSuccess ~ Name, dataset_Move, "sd")[,2])
names(table) <- c("Name", "Mean", "StandardDeviation")
print(table)



##   Interaction analyses
interaction.Model <- lmer(PercentOfSuccess ~ Name + TrialBlock + Name*TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                          dataset_Move, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ Name + TrialBlock + (1|Subject)  + (0+TrialBlock|Subject),
                   dataset_Move, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, interaction.Model, test = 'Chisq')
r.squaredLR(interaction.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name*TrialBlock, dataset_Move, "mean"),
               aggregate(PercentOfSuccess ~ Name*TrialBlock, dataset_Move, "sd")[,3])
names(table) <- c("Name", "Task", "Mean", "StandardDeviation")
print(table)



## -- Effects of the Label in block 1
taskDatasetSubset <- subset(dataset_Move, dataset_Move$TrialBlock == 1)
full.Model <- lmer(PercentOfSuccess ~ Name + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ 1 + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "mean"),
               aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


## -- Effects of the Label in block 2
taskDatasetSubset <- subset(dataset_Move, dataset_Move$TrialBlock == 2)
full.Model <- lmer(PercentOfSuccess ~ Name  + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ 1 + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "mean"),
               aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


## -- Effects of the Label in block 3
taskDatasetSubset <- subset(dataset_Move, dataset_Move$TrialBlock == 3)
full.Model <- lmer(PercentOfSuccess ~ Name  + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(PercentOfSuccess ~ 1 + (1|Subject),
                   taskDatasetSubset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "mean"),
               aggregate(PercentOfSuccess ~ Name, taskDatasetSubset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


