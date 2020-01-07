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

setwd("/Users/francoisfoerster/Desktop/Paper")

## Load the dataset

dataset <- read.csv(file="ReactionTimes_dataset.csv", header=TRUE, sep=",")
dataset <- dataset[,2:ncol(dataset)]


# Removal of the two participants outliers
dataset <- dplyr::filter(dataset, dataset$Subject != 34) # Very poor performance
dataset <- dplyr::filter(dataset, dataset$Subject != 6) # Very poor performance in block 1 only


#### Create Fig.2 - Plot of the reaction times data

dataTemp <- ddply(dataset, c("Task", "Name_learnt"), summarise,
                  RTMovement_m = mean(RTMovement, na.rm=TRUE), RTMovement_sd = sd(RTMovement, na.rm=TRUE)/sqrt(length(RTMovement)),
                  RTGrasping_m = mean(RTGrasping, na.rm=TRUE), RTGrasping_sd = sd(RTGrasping, na.rm=TRUE)/sqrt(length(RTGrasping)),
                  RTAction_m = mean(RTAction, na.rm=TRUE), RTAction_sd = sd(RTAction, na.rm=TRUE)/sqrt(length(RTAction)),
                  RTMovementGrasping_m = mean(RTMovementGrasping, na.rm=TRUE), RTMovementGrasping_sd = sd(RTMovementGrasping, na.rm=TRUE)/sqrt(length(RTMovementGrasping)),
                  RTGraspingAction_m = mean(RTGraspingAction, na.rm=TRUE), RTGraspingAction_sd = sd(RTGraspingAction, na.rm=TRUE)/sqrt(length(RTGraspingAction))
)

RTMovementBoxPlot <- ggplot(dataTemp, aes(Task, RTMovement_m, group = Name_learnt, color = Name_learnt, fill = Name_learnt)) +
  theme_bw() + theme(text = element_text(size=20)) + 
  geom_bar(stat="identity", colour = "black", position="dodge") +
  geom_errorbar(data = dataTemp, aes(ymin= RTMovement_m - RTMovement_sd, ymax= RTMovement_m + RTMovement_sd, x = Task, group = Name_learnt), position=position_dodge(.9), color = "black", size = 0.8, width = 0.5) +
  coord_cartesian(ylim=c(0.475, 0.575)) + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(x = "", y = "Reaction Times (s)")

RTMovementGraspingBoxPlot <- ggplot(dataTemp, aes(Task, RTMovementGrasping_m, group = Name_learnt, color = Name_learnt, fill = Name_learnt)) +
  theme_bw() + theme(text = element_text(size=20)) + 
  geom_bar(stat="identity", colour = "black", position="dodge") +
  geom_errorbar(data = dataTemp, aes(ymin= RTMovementGrasping_m - RTMovementGrasping_sd, ymax= RTMovementGrasping_m + RTMovementGrasping_sd, x = Task, group = Name_learnt), position=position_dodge(.9), color = "black", size = 0.8, width = 0.5) +
  coord_cartesian(ylim=c(0.9, 1.2)) + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(x = "", y = "Reach to Grasp Times (s)")

RTGraspingActionBoxPlot <- ggplot(dataTemp, aes(Task, RTGraspingAction_m, group = Name_learnt, color = Name_learnt, fill = Name_learnt)) +
  theme_bw() + theme(text = element_text(size=20)) + 
  geom_bar(stat="identity", colour = "black", position="dodge") +
  geom_errorbar(data = dataTemp, aes(ymin= RTGraspingAction_m - RTGraspingAction_sd, ymax= RTGraspingAction_m + RTGraspingAction_sd, x = Task, group = Name_learnt), position=position_dodge(.9), color = "black", size = 0.8, width = 0.5) +
  coord_cartesian(ylim=c(0.1, 1.2)) + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(x = "", y = "Grasp to Use/Move Times (s)")

blankPlot <- ggplot(dataTemp, aes(Task, RTAction_m, group = Name_learnt, color = Name_learnt, fill = Name_learnt)) +
  theme_bw() + theme(text = element_text(size=20)) + 
  geom_bar(stat="identity", colour = "black", position="dodge") +
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
        coord_cartesian(ylim=c(1000, 1000)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(x = "", y = "Action Times (ms)")


grid.arrange(RTMovementBoxPlot, RTMovementGraspingBoxPlot, RTGraspingActionBoxPlot,
             blankPlot, blankPlot, blankPlot,
             ncol=3, nrow=2, widths=c(1,1,1), heights=c(1, 1))



## -------- ## -------- ##
## Statistical Analysis ##
## -------- ## -------- ##

## -- Main effects of the Label

## Analyses of Initiation Times:
full.Model <- lmer(RTMovement.z ~ Name_learnt + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTMovement.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTMovement ~ Name_learnt, dataset, "mean"),
               aggregate(RTMovement ~ Name_learnt, dataset, "sd")[,2])
names(table) <- c("Label", "Mean", "StandardDeviation")
print(table)


## Analyses of Grasping Times:
full.Model <- lmer(RTMovementGrasping.z ~ Name_learnt + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTMovementGrasping.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTMovementGrasping ~ Name_learnt, dataset, "mean"),
               aggregate(RTMovementGrasping ~ Name_learnt, dataset, "sd")[,2])
names(table) <- c("Label", "Mean", "StandardDeviation")
print(table)


## Analyses of Execution Times:
full.Model <- lmer(RTGraspingAction.z ~ Name_learnt + TrialBlock + (1|Subject) + (0+Name_learnt|Subject) + (0+TrialBlock|Subject) + (1|Function),
                   dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTGraspingAction.z ~ 1 + TrialBlock + (1|Subject) + (0+Name_learnt|Subject) + (0+TrialBlock|Subject) + (1|Function),
                   dataset, REML = F, control = lmerControl("bobyqa"))

anova(null.Model, full.Model, test = 'Chisq')
# No convergence of the model: no report of the unlikely effect 
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTGraspingAction ~ Name_learnt, dataset, "mean"),
               aggregate(RTGraspingAction ~ Name_learnt, dataset, "sd")[,2])
names(table) <- c("Label", "Mean", "StandardDeviation")
print(table)

## -- Main effects of the Task

## Analyses of Movement Times:
full.Model <- lmer(RTMovement.z ~ Task + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTMovement.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTMovement ~ Task, dataset, "mean"),
               aggregate(RTMovement ~ Task, dataset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


## Analyses of Grasping Times:
full.Model <- lmer(RTMovementGrasping.z ~ Task + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTMovementGrasping.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTMovementGrasping ~ Task, dataset, "mean"),
               aggregate(RTMovementGrasping ~ Task, dataset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


## Analyses of Execution Times:
full.Model <- lmer(RTGraspingAction.z ~ Task + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTGraspingAction.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))

anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTGraspingAction ~ Task, dataset, "mean"),
               aggregate(RTGraspingAction ~ Task, dataset, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)



##  -- Interaction analyses

## Analyses of Movement Times:
interaction.Model <- lmer(RTMovement.z ~ Name_learnt + Task + Name_learnt*Task + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                          dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTMovement.z ~ Name_learnt + Task + TrialBlock + (1|Subject)  + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, interaction.Model, test = 'Chisq')
r.squaredLR(interaction.Model, null = null.Model)

table <- cbind(aggregate(RTMovement ~ Name_learnt*Task, dataset, "mean"),
               aggregate(RTMovement ~ Name_learnt*Task, dataset, "sd")[,3])
names(table) <- c("Name", "Task", "Mean", "StandardDeviation")
print(table)


## Analyses of Grasping Times:
interaction.Model <- lmer(RTMovementGrasping.z ~ Name_learnt + Task + Name_learnt*Task + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                          dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTMovementGrasping.z ~ Name_learnt + Task + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, interaction.Model, test = 'Chisq')
r.squaredLR(interaction.Model, null = null.Model)


## Analyses of Execution Times:
interaction.Model <- lmer(RTGraspingAction.z ~ Name_learnt + Task + Name_learnt*Task + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                          dataset, REML = F, control = lmerControl("bobyqa"))
null.Model <- lmer(RTGraspingAction.z ~ Name_learnt + Task + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   dataset, REML = F, control = lmerControl("bobyqa"))
anova(null.Model, interaction.Model, test = 'Chisq')
r.squaredLR(interaction.Model, null = null.Model)



##  -- Analyses of the contrasts
datasetMove <- subset(dataset, Task == "MOVE")
datasetUse <- subset(dataset, Task == "USE")

## Analyses of Movement Times:

# -- MOVE
full.Model <- lmer(RTMovement.z ~ Name_learnt + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetMove, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
null.Model <- lmer(RTMovement.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetMove, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTMovement ~ Name_learnt, datasetMove, "mean"),
               aggregate(RTMovement ~ Name_learnt, datasetMove, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


# -- USE
full.Model <- lmer(RTMovement.z ~ Name_learnt + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetUse, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
null.Model <- lmer(RTMovement.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetUse, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTMovement ~ Name_learnt, datasetUse, "mean"),
               aggregate(RTMovement ~ Name_learnt, datasetUse, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


## Analyses of Grasping Times:

# -- MOVE
full.Model <- lmer(RTMovementGrasping.z ~ Name_learnt + TrialBlock + (1|Subject)  + (0+TrialBlock|Subject),
                   datasetMove, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
null.Model <- lmer(RTMovementGrasping.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetMove, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTMovementGrasping ~ Name_learnt, datasetMove, "mean"),
               aggregate(RTMovementGrasping ~ Name_learnt, datasetMove, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)



# -- USE 
full.Model <- lmer(RTMovementGrasping.z ~ Name_learnt + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetUse, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
null.Model <- lmer(RTMovementGrasping.z ~ 1 + TrialBlock + (1|Subject)+ (0+TrialBlock|Subject),
                   datasetUse, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTMovementGrasping ~ Name_learnt, datasetUse, "mean"),
               aggregate(RTMovementGrasping ~ Name_learnt, datasetUse, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)





## Analyses of Execution Times:

# -- MOVE 
full.Model <- lmer(RTGraspingAction.z ~ Name_learnt + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetMove, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
null.Model <- lmer(RTGraspingAction.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetMove, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTGraspingAction ~ Name_learnt, datasetMove, "mean"),
               aggregate(RTGraspingAction ~ Name_learnt, datasetMove, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)


# -- USE
full.Model <- lmer(RTGraspingAction.z ~ Name_learnt + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetUse, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
null.Model <- lmer(RTGraspingAction.z ~ 1 + TrialBlock + (1|Subject) + (0+TrialBlock|Subject),
                   datasetUse, REML = F, control = lmerControl("bobyqa")) # summary(full.Model)
anova(null.Model, full.Model, test = 'Chisq')
r.squaredLR(full.Model, null = null.Model)

table <- cbind(aggregate(RTGraspingAction ~ Name_learnt, datasetUse, "mean"),
               aggregate(RTGraspingAction ~ Name_learnt, datasetUse, "sd")[,2])
names(table) <- c("Comparisons", "Mean", "StandardDeviation")
print(table)





