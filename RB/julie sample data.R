##julie's project

library(openxlsx)
library(readxl)  #plays nicer with tidyverse
library(tidyverse)

julie = read_excel("Julie/Summary_Exp2.xlsx")

fam = read_excel("Julie/Summary_Exp2.xlsx", sheet = 2)

names(fam) = make.names(names(fam), unique = TRUE) #make.names takes out all the spaces and puts dots. apparently read_excel doesn't do it for you.

age.data <- data.frame(
    Subject.Number = c(401, 408, 415, 416, 417, 418, 419, 422, 424, 426, 405, 406),
    Age = c("1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2")
)

fam<- left_join(fam, age.data, by = "Subject.Number")

fam$Talker.ID.Response <- as.factor(fam$Talker.ID.Response)
fam$Subject.Number <- as.factor(fam$Subject.Number)


fam <- filter(fam, Talker.ID.Response == 3)
fam <- fam[,-7]
fam$log_RT <- log10(fam$Response.Time)


#Monaural Conditions
fam_mono <- filter(fam, Presentation == 1, Condition <=20, Target.to.Masker.Ratio != 3)

fam_mono$TMR <- fam_mono$Target.to.Masker.Ratio
fam_mono$TMR <-as.factor(fam_mono$TMR)
fam_mono$Condition <- as.factor(fam_mono$Condition)

fam_mono$Condition[fam_mono$Condition==8] <- 1
fam_mono$Condition[fam_mono$Condition==9] <- 2
fam_mono$Condition[fam_mono$Condition==10] <- 3

fam_mono$TMR <- recode(fam_mono$TMR, "1" = "0", "2" = "-5")
fam_mono$Age <- recode(fam_mono$Age, "1" = "Young", "2" = "Older")

std.err = function(x) {
    output = sd(x)/sqrt(length(x))
}


fam_mono_mean <- fam_mono %>% group_by(Condition, TMR, Age) %>% summarise(mean = mean(Score_1),
                                                                          se = std.err(Score_1))
ggplot(fam_mono_mean, aes(TMR, mean, fill = Condition)) +
    geom_bar(stat = "identity", colour = "black", position = position_dodge2(padding = .2), width = 3) +
    geom_errorbar(aes(ymin = mean - (1.96*se), ymax = mean + (1.96*se)), position = position_dodge2(padding = .5)) +
    scale_fill_manual(name = "Identity of\nFamiliar Voice", 
                      labels=c("Familiar Target\n(Condition 1)", "Familiar Masker\n(Condition 2)", "Unfamiliar\n(Condition 3)"),
                      values=c("#8856a7","#8c96c6","#edf8fb")) +
    scale_y_continuous(name = "Proportion Corrent", expand = c(0,0), limits = 0:1.2) +
    theme_classic()+
    facet_wrap(~ Age)


ggplot(fam_mono_mean, aes(TMR, mean, fill = Condition)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin = mean - 1.96*se, 
                      ymax = mean + 1.96*se, 
                      group = Condition),
                  position = position_dodge(width = 0.9),
                  width = 0.3) +                    #if you change the width of the                                                             error bar, it dodges by that
    facet_wrap(~Age)




