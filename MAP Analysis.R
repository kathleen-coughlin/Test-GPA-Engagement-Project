setwd("~/R Programs")
WinterAnalysis <- read.csv("ELL Winter MAP, GPA, Engagement Analysis - ELL Anon.csv")

library(tidyverse)

summary(WinterAnalysis)

WinterAnalysis$Anonym_ID <- as.character(WinterAnalysis$Anonym_ID)
WinterAnalysis$Math_Pct_Disengaged_Responses <- as.numeric(WinterAnalysis$Math_Pct_Disengaged_Responses)
WinterAnalysis$ELA_Pct_Disengaged_Responses <- as.numeric(WinterAnalysis$ELA_Pct_Disengaged_Responses)
WinterAnalysis$ELA_MAP_RIT_Score <- as.numeric(WinterAnalysis$ELA_MAP_RIT_Score)
WinterAnalysis$Math_MAP_RIT_Score <- as.numeric(WinterAnalysis$Math_MAP_RIT_Score)


WinterAnalysis$SPED <- as.factor(WinterAnalysis$SPED)
WinterAnalysis$Q2_ELA_Letter <- ordered(WinterAnalysis$Q2_ELA_Letter, 
                                        levels=c("A", "B+", "B", "C+", "C", "F"))
WinterAnalysis$Q2_Math_Letter <- ordered(WinterAnalysis$Q2_Math_Letter, 
                                        levels=c("A", "B+", "B", "C+", "C", "F"))
WinterAnalysis$Q2_Hist_Letter <- ordered(WinterAnalysis$Q2_Hist_Letter, 
                                        levels=c("A", "B+", "B", "C+", "C", "F"))
WinterAnalysis$Q2_Sci_Letter <- ordered(WinterAnalysis$Q2_Sci_Letter, 
                                        levels=c("A", "B+", "B", "C+", "C", "F"))
WinterAnalysis$Grade <- ordered(WinterAnalysis$Grade,
                                levels = c("6", "7", "8", "9"),
                                labels = c("6th grade", "7th grade", "8th grade", "9th grade"))



summary(WinterAnalysis)
hist(WinterAnalysis$Q2_ELA_GPA)
hist(WinterAnalysis$Q2_Math_GPA)
hist(WinterAnalysis$Q2_Sci_GPA)
hist(WinterAnalysis$Q2_Hist_GPA)
hist(WinterAnalysis$Humanities_GPA)
hist(WinterAnalysis$STEM_GPA)
hist(WinterAnalysis$ELA_MAP_RIT_Score)
hist(WinterAnalysis$Math_MAP_RIT_Score)



## GPA
WinterAnalysis %>%
    group_by(Grade) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_ELA = mean(Q2_ELA_GPA),
              Av_Math = mean(Q2_Math_GPA),
              Av_Hist = mean(Q2_Hist_GPA),
              Av_Sci = mean(Q2_Sci_GPA))

WinterAnalysis %>%
    group_by(Grade) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_Humanities = mean(Humanities_GPA),
              Av_STEM = mean(STEM_GPA),
              Hum_Stem_Dif = mean(Humanities_GPA) - mean(STEM_GPA))

WinterAnalysis %>%
    group_by(SPED) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_ELA = mean(Q2_ELA_GPA),
              Av_Math = mean(Q2_Math_GPA),
              Av_Hist = mean(Q2_Hist_GPA),
              Av_Sci = mean(Q2_Sci_GPA))

WinterAnalysis %>%
    group_by(SPED) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_Humanities = mean(Humanities_GPA),
              Av_STEM = mean(STEM_GPA),
              Hum_Stem_Dif = mean(Humanities_GPA) - mean(STEM_GPA))

WinterAnalysis %>%
    group_by(ELA_Disengaged_YN) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_ELA = mean(Q2_ELA_GPA),
              Av_Hist = mean(Q2_Hist_GPA),
              Av_Humanities = mean(Humanities_GPA))

WinterAnalysis %>%
    group_by(Math_Disengaged_YN) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_Math = mean(Q2_Math_GPA),
              Av_Sci = mean(Q2_Sci_GPA),
              Av_STEM = mean(STEM_GPA))


## MAP
WinterAnalysis %>%
    group_by(Grade) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_MAP_ELA = mean(ELA_MAP_RIT_Score, na.rm=TRUE),
              Av_MAP_Math = mean(Math_MAP_RIT_Score, na.rm=TRUE))

WinterAnalysis %>%
    group_by(SPED) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_MAP_ELA = mean(ELA_MAP_RIT_Score, na.rm=TRUE),
              Av_MAP_Math = mean(Math_MAP_RIT_Score, na.rm=TRUE))


WinterAnalysis %>%
    group_by(ELA_Disengaged_YN) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_MAP_ELA = mean(ELA_MAP_RIT_Score, na.rm=TRUE))

WinterAnalysis %>%
    group_by(Math_Disengaged_YN) %>%
    summarise(n_students = n_distinct(Anonym_ID),
              Av_MAP_Math = mean(Math_MAP_RIT_Score, na.rm=TRUE))



# Correlation

plot(WinterAnalysis$ELA_Pct_Disengaged_Responses, WinterAnalysis$ELA_MAP_RIT_Score, pch=19)
abline(lm(WinterAnalysis$ELA_MAP_RIT_Score ~ WinterAnalysis$ELA_Pct_Disengaged_Responses))

cor.test(WinterAnalysis$ELA_Pct_Disengaged_Responses, WinterAnalysis$ELA_MAP_RIT_Score, use="complete.obs", method="spearman", exact=FALSE)  

plot(WinterAnalysis$Math_Pct_Disengaged_Responses, WinterAnalysis$Math_MAP_RIT_Score, pch=19)
abline(lm(WinterAnalysis$Math_MAP_RIT_Score ~ WinterAnalysis$Math_Pct_Disengaged_Responses))

cor.test(WinterAnalysis$Math_Pct_Disengaged_Responses, WinterAnalysis$Math_MAP_RIT_Score, use="complete.obs", method="spearman", exact=FALSE) 

plot(WinterAnalysis$ELA_Pct_Disengaged_Responses, WinterAnalysis$Humanities_GPA, pch=19)
abline(lm(WinterAnalysis$Humanities_GPA ~ WinterAnalysis$ELA_Pct_Disengaged_Responses))

cor.test(WinterAnalysis$ELA_Pct_Disengaged_Responses, WinterAnalysis$Humanities_GPA, use="complete.obs", method="spearman", exact=FALSE)

plot(WinterAnalysis$Math_Pct_Disengaged_Responses, WinterAnalysis$STEM_GPA, pch=19)
abline(lm(WinterAnalysis$STEM_GPA ~ WinterAnalysis$Math_Pct_Disengaged_Responses))

cor.test(WinterAnalysis$Math_Pct_Disengaged_Responses, WinterAnalysis$STEM_GPA, use="complete.obs", method="spearman", exact=FALSE) 

plot(WinterAnalysis$Humanities_GPA, WinterAnalysis$ELA_MAP_RIT_Score,
     col = ifelse(WinterAnalysis$ELA_Disengaged_YN == "FALSE", "red", "blue"),
     pch=19)

cor.test(WinterAnalysis$Humanities_GPA, WinterAnalysis$ELA_MAP_RIT_Score, use="complete.obs", method="spearman", exact=FALSE) 

plot(WinterAnalysis$STEM_GPA, WinterAnalysis$Math_MAP_RIT_Score,
     col = ifelse(WinterAnalysis$Math_Disengaged_YN == "FALSE", "red", "blue"),
     pch = 19)

cor.test(WinterAnalysis$STEM_GPA, WinterAnalysis$Math_MAP_RIT_Score, use="complete.obs", method="spearman", exact=FALSE) 


ELA_eng <- subset(WinterAnalysis, WinterAnalysis$ELA_Disengaged_YN == "FALSE")
Math_eng <- subset(WinterAnalysis, WinterAnalysis$Math_Disengaged_YN == "FALSE")
ELA_dis <- subset(WinterAnalysis, WinterAnalysis$ELA_Disengaged_YN == "TRUE")
Math_dis <- subset(WinterAnalysis, WinterAnalysis$Math_Disengaged_YN == "TRUE")

cor.test(ELA_eng$Humanities_GPA, ELA_eng$ELA_MAP_RIT_Score, use="complete.obs", method="spearman", exact=FALSE) 
cor.test(Math_eng$STEM_GPA, Math_eng$Math_MAP_RIT_Score, use="complete.obs", method="spearman", exact=FALSE) 

cor.test(ELA_dis$Humanities_GPA, ELA_dis$ELA_MAP_RIT_Score, use="complete.obs", method="spearman", exact=FALSE) 
cor.test(Math_dis$STEM_GPA, Math_dis$Math_MAP_RIT_Score, use="complete.obs", method="spearman", exact=FALSE) 


# Graphs

ggplot(WinterAnalysis, aes(y=ELA_Pct_Disengaged_Responses, x=Grade))+
    geom_count() +
    labs(title="Disengagement on ELA MAP by Grade", 
         subtitle="Percent of questions answered through rapid guessing. Adaptability feature disabled.", 
         y= "Percent of Responses", 
         size= "Students")+
    theme_bw() +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(color="red", face="bold"))


WinterAnalysis %>%
    group_by(Grade) %>%
    summarise(mean_dis_ELA = round(mean(ELA_Disengaged_YN, na.rm=TRUE), digits=2)*100)%>%
    ggplot(aes(x=mean_dis_ELA, y=Grade))+
    geom_col(aes(x=100), fill="gray80")+
    geom_bar(stat="summary", fill="firebrick1") +
    geom_text(aes(label=paste0(mean_dis_ELA, "%")), hjust=1.3, fontface="bold") +
    labs(title="Percent of Students Disengaged on ELA MAP", x="Percent of Students") +
    theme_bw()+
    theme(axis.title.y = element_blank(),
          plot.title = element_text(face="bold"))
    

WinterAnalysis %>%
    group_by(Grade) %>%
    summarise(mean_dis_Math = round(mean(Math_Disengaged_YN, na.rm=TRUE), digits=2)*100)%>%
    ggplot(aes(x=mean_dis_Math, y=Grade))+
    geom_col(aes(x=100), fill="gray80")+
    geom_bar(stat="summary", fill="firebrick1") +
    geom_text(aes(label=paste0(mean_dis_Math, "%")), hjust=1.3, fontface="bold") +
    labs(title="Percent of Students Disengaged on Math MAP", x="Percent of Students") +
    theme_bw()+
    theme(axis.title.y = element_blank(),
          plot.title = element_text(face="bold"))




WinterAnalysis %>%
    dplyr::filter(Grade == "6th grade") %>%
    ggplot(aes(x=Humanities_GPA, y=ELA_MAP_RIT_Score, color=ELA_Disengaged_YN)) +
    geom_jitter(size=2) +
    labs(title= "Winter ELA MAP vs. Humanities GPA", subtitle="6th Grade", x= "Humanities GPA", y="ELA MAP Score", color= "Disengaged?")+
    scale_color_manual(values=c("black", "red"), labels=c("Engaged", "Disengaged")) +
    theme_bw()

WinterAnalysis %>%
    dplyr::filter(Grade == "6th grade") %>%
    ggplot(aes(x=STEM_GPA, y=Math_MAP_RIT_Score, color=Math_Disengaged_YN)) +
    geom_jitter(size=2) +
    labs(title= "Winter Math MAP vs. STEM GPA", subtitle="6th Grade", x= "STEM GPA", y="Math MAP Score", color= "Disengaged?")+
    scale_color_manual(values=c("black", "red"), labels=c("Engaged", "Disengaged")) +
    theme_bw()



WinterAnalysis %>%
    dplyr::filter(Grade == "7th grade") %>%
    ggplot(aes(x=Humanities_GPA, y=ELA_MAP_RIT_Score, color=ELA_Disengaged_YN)) +
    geom_jitter(size=2) +
    labs(title= "Winter ELA MAP vs. Humanities GPA", subtitle="7th Grade", x= "Humanities GPA", y="ELA MAP Score", color= "Disengaged?")+
    scale_color_manual(values=c("black", "red"), labels=c("Engaged", "Disengaged"))+
    theme_bw()

WinterAnalysis %>%
    dplyr::filter(Grade == "7th grade") %>%
    ggplot(aes(x=STEM_GPA, y=Math_MAP_RIT_Score, color=Math_Disengaged_YN)) +
    geom_jitter(size=2) +
    labs(title= "Winter Math MAP vs. STEM GPA", subtitle="7th Grade", x= "STEM GPA", y="Math MAP Score", color= "Disengaged?")+
    scale_color_manual(values=c("black", "red"), labels=c("Engaged", "Disengaged")) +
    theme_bw()



WinterAnalysis %>%
    dplyr::filter(Grade == "8th grade") %>%
    ggplot(aes(x=Humanities_GPA, y=ELA_MAP_RIT_Score, color=ELA_Disengaged_YN)) +
    geom_jitter(size=2) +
    labs(title= "Winter ELA MAP vs. Humanities GPA", subtitle="8th Grade", x= "Humanities GPA", y="ELA MAP Score", color= "Disengaged?")+
    scale_color_manual(values=c("black", "red"), labels=c("Engaged", "Disengaged"))+
    theme_bw()

WinterAnalysis %>%    dplyr::filter(Grade == "8th grade") %>%
    ggplot(aes(x=STEM_GPA, y=Math_MAP_RIT_Score, color=Math_Disengaged_YN)) +
    geom_jitter(size=2) +
    labs(title= "Winter Math MAP vs. STEM GPA", subtitle="8th Grade", x= "STEM GPA", y="Math MAP Score", color= "Disengaged?")+
    scale_color_manual(values=c("black", "red"), labels=c("Engaged", "Disengaged")) +
    theme_bw()



WinterAnalysis %>%
    dplyr::filter(Grade == "9th grade") %>%
    ggplot(aes(x=Humanities_GPA, y=ELA_MAP_RIT_Score, color=ELA_Disengaged_YN)) +
    geom_jitter(size=2) +
    labs(title= "Winter ELA MAP vs. Humanities GPA", subtitle="9th Grade", x= "Humanities GPA", y="ELA MAP Score", color= "Disengaged?")+
    scale_color_manual(values=c("black", "red"), labels=c("Engaged", "Disengaged"))+
    theme_bw()

WinterAnalysis %>%
    dplyr::filter(Grade == "9th grade") %>%
    ggplot(aes(x=STEM_GPA, y=Math_MAP_RIT_Score, color=Math_Disengaged_YN)) +
    geom_jitter(size=2) +
    labs(title= "Winter Math MAP vs. STEM GPA", subtitle="9th Grade", x= "STEM GPA", y="Math MAP Score", color= "Disengaged?")+
    scale_color_manual(values=c("black", "red"), labels=c("Engaged", "Disengaged"))+
    theme_bw()

