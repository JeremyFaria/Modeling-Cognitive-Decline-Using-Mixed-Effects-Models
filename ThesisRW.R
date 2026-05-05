library(haven)
HRS <- read_sas("randhrs1992_2022v1.sas7bdat")

Depression_2022 <- HRS$R16DEPYR
light_phys_activity_2022 <- HRS$R16LTACTX
Depression_Light_phys_activity_2022 <- data.frame(Depression_2022, light_phys_activity_2022)
cleaned_Depression_Light_phys_activity_2022 <- na.omit(Depression_Light_phys_activity_2022)
library(dplyr)
Light_phys_activity_by_depression <- cleaned_Depression_Light_phys_activity_2022 %>%
  group_by(Depression_2022) %>%
  summarise(Exercise = mean(light_phys_activity_2022))
library(ggplot2)
ggplot(data = cleaned_Depression_Light_phys_activity_2022, aes(x = light_phys_activity_2022)) + 
  geom_bar() + 
  facet_wrap(~ Depression_2022)
freq_cleaned_Depression_Light_phys_activity_2022 <- cleaned_Depression_Light_phys_activity_2022 %>%
  group_by(Depression_2022, light_phys_activity_2022) %>% 
  summarise(n=n()) %>%
  mutate(freq=n/sum(n))
ggplot(freq_cleaned_Depression_Light_phys_activity_2022, aes(x=light_phys_activity_2022, fill = Depression_2022, group = Depression_2022)) +
  geom_bar(aes(y=freq), stat = 'identity', position='dodge')


ggplot(data = HRS, aes(x = R1AGEY_E)) +
  geom_bar() +
  labs(title = "Age Distribution of Respondants in 1992",
       x = "Age")

summary(HRS$R1AGEY_E)

ggplot(data = HRS, aes(x = R16AGEY_E)) +
  geom_bar() +
  labs(title = "Age Distribution of Respondants in 2022",
       x = "Age")
  
summary(HRS$RAWTSAMP)

cognition_overtime <- subset(HRS, select = c(R3COG27, R4COG27, R5COG27, R6COG27, R7COG27, R8COG27, R9COG27, R10COG27, R11COG27, R12COG27, R13COG27, R14COG27, R15COG27))
cognition_overtime_cleaned <- cognition_overtime[!is.na(cognition_overtime$R3COG27), ]
cognition_overtime_cleaned_means <- colMeans(cognition_overtime_cleaned, na.rm = TRUE)
plot(
  x  = 3:(length(cognition_overtime_cleaned_means)+2),
  y = cognition_overtime_cleaned_means,
  type = 'l',
  xlab = "Wave",
  ylab = "Mean Cognition Score",
  main = "Cognition over waves from wave 3 respondants")

Hallucinations_overtime <- subset(HRS, select = c(R2HALUC, R3HALUC, R4HALUC, R5HALUC, R6HALUC, R7HALUC, R8HALUC, R9HALUC, R10HALUC, R11HALUC, R12HALUC, R13HALUC, R14HALUC, R15HALUC, R16HALUC))
Hallucinations_overtime_cleaned <- Hallucinations_overtime[!is.na(Hallucinations_overtime$R2HALUC), ]
Hallucinations_overtime_cleaned_means <- colMeans(Hallucinations_overtime_cleaned, na.rm=TRUE)
plot(
  x = 2:(length(Hallucinations_overtime_cleaned_means)+1),
  y = Hallucinations_overtime_cleaned_means,
  type = 'l',
  xlab = 'Wave',
  ylab = "Percentage",
  main = "Percentage of People experiencing Hallucinations \n in the Wave 2 Cohort Over waves")

plot(x = HRS$R3CESD, y = HRS$R3DRINKD)
CESD_Drinking_1996_freq <- as.data.frame(table(HRS$R3CESD, HRS$R3DRINKD))
colnames(CESD_Drinking_1996_freq) <- c("CESD_score", "Days_of_week_drinking", "Frequency")
ggplot(CESD_Drinking_1996_freq, aes(x = CESD_score, y = Days_of_week_drinking, size = Frequency)) +
  geom_point(alpha = 0.7)

ggplot(HRS, aes(x = R3CESD, y = R3DRINKD))+
  geom_jitter(width = 0,2, height = 0.2, alpha = 0.6)


## Mondat Sept 29th

cog <- read_sas("cogfinalimp_9520wide.sas7bdat")

cognition_overtime_cleaned_2 <- na.omit(cognition_overtime_cleaned)
correlation_matrix_cog_w3 <- cor(cognition_overtime_cleaned_2)
lower_correlation_matrix_cog_w3 <- correlation_matrix_cog_w3
lower_correlation_matrix_cog_w3[upper.tri(lower_correlation_matrix_cog_w3)] <- NA
print(lower_correlation_matrix_cog_w3)
library(corrplot)
corrplot(correlation_matrix_cog_w3, type = "upper", method = "color")
#acf(cognition_overtime_cleaned_2)

sum(cognition_overtime_cleaned$R3COG27<7)/length(cognition_overtime_cleaned$R3COG27)

#October 6th

Wave10Dates <- as.Date(HRS$R10IWBEG, origin = "1960-01-01")
hist(Wave10Dates, breaks = "months",
     main = "Histogram of interview dates for wave 10 (2010-2011)",
     xlab = "Date",
     ylab = "frequency")

install.packages("geoR")

library(geoR)

library(dplyr)

cognition_overtime_cleaned_filled <- cognition_overtime_cleaned %>% replace(is.na(.), 28)

time <- 1:13
id <- rep(1:nrow(cognition_overtime_cleaned_filled), each = 13)
time_rep <- rep(time, times = nrow(cognition_overtime_cleaned_filled))
y <- as.vector(t(as.matrix(cognition_overtime_cleaned_filled)))
df.variograph <- data.frame(id = id, time = time_rep, y = y)
df.variograph <- df.variograph[df.variograph$y != 28,]
v <- variog(coords = cbind(df.variograph$time), data = df.variograph$y, option = "bin")

library(nlme)

vario.model <- gls(y ~ time, data = df.variograph, correlation = corAR1(form = ~ time | id))

v <- Variogram(vario.model, form = ~ time, maxDist = 12)
plot(v, main = "Variogram for wave 3 respondents \n from wave 3 to wave 15 (1996-2020)")

#vario.model.exp <- lme(y ~ time, random = ~1 | id, correlation = corExp(form = ~ time|id), data = df.variograph)
head(v)

df <- as.data.frame(v)
df$lag <- df$dist
df$gamma <- df$variog
df$npairs <- df$n.pairs
df <- df[, intersect(c("lag", "gamma", "npairs"), names(df)), drop=FALSE]
head(df)

#October 29th

matplot(t(cognition_overtime_cleaned_2), 
        type = 'l', 
        col = 1:nrow(cognition_overtime_cleaned_2),
        xlab = "Time Point",
        ylab = "cognition score",
        main = "Individual Cognition scores overtime")

library(tidyr)

cognition_overtime_cleaned_2$ID <- 1:nrow(cognition_overtime_cleaned_2)
cognition_overtime_cleaned_2_pivot <- pivot_longer(cognition_overtime_cleaned_2,
                                                   cols = starts_with("R"),
                                                   names_to = "TimePoint",
                                                   values_to = "Value")
cognition_overtime_cleaned_2_pivot$TimePoint <- factor(cognition_overtime_cleaned_2_pivot$TimePoint,
                                                       levels = c("R3COG27",
                                                                  "R4COG27",
                                                                  "R5COG27",
                                                                  "R6COG27",
                                                                  "R7COG27",
                                                                  "R8COG27",
                                                                  "R9COG27",
                                                                  "R10COG27",
                                                                  "R11COG27",
                                                                  "R12COG27",
                                                                  "R13COG27",
                                                                  "R14COG27",
                                                                  "R15COG27"))
cognition_overtime_cleaned_means_df <- data.frame(x = cognition_overtime_cleaned_2_pivot$TimePoint, y = cognition_overtime_cleaned_means)
cognition_overtime_cleaned_means_df <- cognition_overtime_cleaned_means_df[1:13,]
ggplot(cognition_overtime_cleaned_2_pivot, aes(x = TimePoint, y = Value, group = ID))+
  geom_line(alpha=0.03)+
  labs(title = "Cognition overtime for each person in wave 3",
       x = "Time Point",
       y = "Value")+
  theme(axis.text.x = element_text(angle = 60))+
  geom_line(data = cognition_overtime_cleaned_means_df, aes(x = x, y = y, group = 1), color = "red", size = 2, inherit.aes =  FALSE)

#October 31st
library(tidyr)
all_cog <- subset(HRS, select = c(HHIDPN, R3COG27,
                                  R3AGEY_E,
                                  R4COG27,
                                  R4AGEY_E,
                                  R5COG27,
                                  R5AGEY_E,
                                  R6COG27,
                                  R6AGEY_E,
                                  R7COG27,
                                  R7AGEY_E,
                                  R8COG27,
                                  R8AGEY_E,
                                  R9COG27,
                                  R9AGEY_E,
                                  R10COG27,
                                  R10AGEY_E,
                                  R11COG27,
                                  R11AGEY_E,
                                  R12COG27,
                                  R12AGEY_E,
                                  R13COG27,
                                  R13AGEY_E,
                                  R14COG27,
                                  R14AGEY_E,
                                  R15COG27,
                                  R15AGEY_E))
all_cog_pivot <- data.frame(ID = all_cog$HHIDPN, Score = all_cog$R3COG27, Age = all_cog$R3AGEY_E)
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R4COG27, Age = all_cog$R4AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R5COG27, Age = all_cog$R5AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R6COG27, Age = all_cog$R6AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R7COG27, Age = all_cog$R7AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R8COG27, Age = all_cog$R8AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R9COG27, Age = all_cog$R9AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R10COG27, Age = all_cog$R10AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R11COG27, Age = all_cog$R11AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R12COG27, Age = all_cog$R12AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R13COG27, Age = all_cog$R13AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R14COG27, Age = all_cog$R14AGEY_E))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R15COG27, Age = all_cog$R15AGEY_E))
all_cog_pivot$Age_group <- NA
library(dplyr)
all_cog_pivot <- all_cog_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))
all_cog_pivot <- all_cog_pivot[all_cog_pivot$Age >= 50, ]
ggplot(all_cog_pivot, aes(x = Age_group, y = Score))+
  geom_boxplot(fill = "lightblue")+
  labs(title = "Distribution of Cognition Scores Across Age Groups",
       x = "Age Group",
       y = "Cognition Score")
ggplot(all_cog_pivot, aes(x = Age, y = Score, group = ID))+
  geom_line(alpha = 0.04)+labs(
    title = "Cognition Scores Across Ages for Each Individual",
    x = "Age",
    y = "Cognition Score"
  )
library(nlme)
all_cog_pivot_cleaned <- na.omit(all_cog_pivot)
m <- lme(Score ~ Age, random = ~ 1 | ID, data = all_cog_pivot_cleaned)
v <- Variogram(m, form =~ Age | ID)
plot(v, main = "Empirical variogram of Model With\n Random Intercept With Age as a Covariate")

library(joineR)

library(dplyr)
all_cog_pivot_cleaned_subset <- all_cog_pivot_cleaned %>% group_by(ID) %>% filter(n() >= 10) %>% ungroup()
all_cog_pivot_cleaned_subset_grouped <- all_cog_pivot_cleaned_subset %>% group_by(ID)
set.seed(456)
random_groups_frac <- all_cog_pivot_cleaned_subset_grouped %>%
  sample_n(size = 1, replace = FALSE) %>%
  ungroup() %>%
  distinct(ID)%>%
  sample_frac(size = 0.05, replace = FALSE)

all_cog_pivot_cleaned_subset2 <- all_cog_pivot_cleaned_subset %>%
  filter(ID %in% random_groups_frac$ID)

vgm <- variogram(indv = all_cog_pivot_cleaned_subset2$ID, time = all_cog_pivot_cleaned_subset2$Age, Y = all_cog_pivot_cleaned_subset2$Score)
plot(vgm)


all_cog_smoke <- subset(HRS, select = c(HHIDPN, R3COG27,
                                  R3AGEY_E,
                                  R3SMOKEN,
                                  R4COG27,
                                  R4AGEY_E,
                                  R4SMOKEN,
                                  R5COG27,
                                  R5AGEY_E,
                                  R5SMOKEN,
                                  R6COG27,
                                  R6AGEY_E,
                                  R6SMOKEN,
                                  R7COG27,
                                  R7AGEY_E,
                                  R7SMOKEN,
                                  R8COG27,
                                  R8AGEY_E,
                                  R8SMOKEN,
                                  R9COG27,
                                  R9AGEY_E,
                                  R9SMOKEN,
                                  R10COG27,
                                  R10AGEY_E,
                                  R10SMOKEN,
                                  R11COG27,
                                  R11AGEY_E,
                                  R11SMOKEN,
                                  R12COG27,
                                  R12AGEY_E,
                                  R12SMOKEN,
                                  R13COG27,
                                  R13AGEY_E,
                                  R13SMOKEN,
                                  R14COG27,
                                  R14AGEY_E,
                                  R14SMOKEN,
                                  R15COG27,
                                  R15AGEY_E,
                                  R15SMOKEN))
all_cog_smoke_pivot <- data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R3COG27, Age = all_cog_smoke$R3AGEY_E, Smoke = all_cog_smoke$R3SMOKEN)
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R4COG27, Age = all_cog_smoke$R4AGEY_E, Smoke = all_cog_smoke$R4SMOKEN))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R5COG27, Age = all_cog_smoke$R5AGEY_E, Smoke = all_cog_smoke$R5SMOKEN))                           
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R6COG27, Age = all_cog_smoke$R6AGEY_E, Smoke = all_cog_smoke$R6SMOKEN))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R7COG27, Age = all_cog_smoke$R7AGEY_E, Smoke = all_cog_smoke$R7SMOKEN))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R8COG27, Age = all_cog_smoke$R8AGEY_E, Smoke = all_cog_smoke$R8SMOKEN))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R9COG27, Age = all_cog_smoke$R9AGEY_E, Smoke = all_cog_smoke$R9SMOKEN))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R10COG27, Age = all_cog_smoke$R10AGEY_E, Smoke = all_cog_smoke$R10SMOKEN))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R11COG27, Age = all_cog_smoke$R11AGEY_E, Smoke = all_cog_smoke$R11SMOKEN))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R12COG27, Age = all_cog_smoke$R12AGEY_E, Smoke = all_cog_smoke$R12SMOKEN))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R13COG27, Age = all_cog_smoke$R13AGEY_E, Smoke = all_cog_smoke$R13SMOKEN)) 
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R14COG27, Age = all_cog_smoke$R14AGEY_E, Smoke = all_cog_smoke$R14SMOKEN))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R15COG27, Age = all_cog_smoke$R15AGEY_E, Smoke = all_cog_smoke$R15SMOKEN))  
all_cog_smoke_pivot$Age_group <- NA
library(dplyr)
all_cog_smoke_pivot <- all_cog_smoke_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))
all_cog_smoke_pivot <- all_cog_smoke_pivot[all_cog_smoke_pivot$Age >= 50, ]
all_cog_smoke_pivot <- na.omit(all_cog_smoke_pivot)
library(ggplot2)
ggplot(all_cog_smoke_pivot, aes(x = Age_group, y = Score, fill = factor(Smoke)))+
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8))+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Smoking Status",
                    labels = c("Non-Smoker", "Smoker"))+
  labs(title = "Distribution of Cognition Scores Across Age Groups\nBetween Smokers and Non Smokers",
       x = "Age Group",
       y = "Cognition Score")

all_cog_depressed <- subset(HRS, select = c(HHIDPN, R3COG27,
                                  R3AGEY_E,
                                  R3CESD,
                                  R4COG27,
                                  R4AGEY_E,
                                  R4CESD,
                                  R5COG27,
                                  R5AGEY_E,
                                  R5CESD,
                                  R6COG27,
                                  R6AGEY_E,
                                  R6CESD,
                                  R7COG27,
                                  R7AGEY_E,
                                  R7CESD,
                                  R8COG27,
                                  R8AGEY_E,
                                  R8CESD,
                                  R9COG27,
                                  R9AGEY_E,
                                  R9CESD,
                                  R10COG27,
                                  R10AGEY_E,
                                  R10CESD,
                                  R11COG27,
                                  R11AGEY_E,
                                  R11CESD,
                                  R12COG27,
                                  R12AGEY_E,
                                  R12CESD,
                                  R13COG27,
                                  R13AGEY_E,
                                  R13CESD,
                                  R14COG27,
                                  R14AGEY_E,
                                  R14CESD,
                                  R15COG27,
                                  R15AGEY_E,
                                  R15CESD))
all_cog_depressed_pivot <- data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R3COG27, Age = all_cog_depressed$R3AGEY_E, Depressed = all_cog_depressed$R3CESD)
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R4COG27, Age = all_cog_depressed$R4AGEY_E, Depressed = all_cog_depressed$R4CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R5COG27, Age = all_cog_depressed$R5AGEY_E, Depressed = all_cog_depressed$R5CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R6COG27, Age = all_cog_depressed$R6AGEY_E, Depressed = all_cog_depressed$R6CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R7COG27, Age = all_cog_depressed$R7AGEY_E, Depressed = all_cog_depressed$R7CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R8COG27, Age = all_cog_depressed$R8AGEY_E, Depressed = all_cog_depressed$R8CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R9COG27, Age = all_cog_depressed$R9AGEY_E, Depressed = all_cog_depressed$R9CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R10COG27, Age = all_cog_depressed$R10AGEY_E, Depressed = all_cog_depressed$R10CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R11COG27, Age = all_cog_depressed$R11AGEY_E, Depressed = all_cog_depressed$R11CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R12COG27, Age = all_cog_depressed$R12AGEY_E, Depressed = all_cog_depressed$R12CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R13COG27, Age = all_cog_depressed$R13AGEY_E, Depressed = all_cog_depressed$R13CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R14COG27, Age = all_cog_depressed$R14AGEY_E, Depressed = all_cog_depressed$R14CESD))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R15COG27, Age = all_cog_depressed$R15AGEY_E, Depressed = all_cog_depressed$R15CESD))
library(dplyr)
all_cog_depressed_pivot$Age_group <- NA
all_cog_depressed_pivot <- all_cog_depressed_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))
all_cog_depressed_pivot$Is_depressed <- NA
all_cog_depressed_pivot <- all_cog_depressed_pivot %>%
  mutate(Is_depressed = case_when(
    Depressed < 3 ~ "0",
    TRUE ~ "1"
  ))
all_cog_depressed_pivot <- all_cog_depressed_pivot[all_cog_depressed_pivot$Age >= 50, ]
all_cog_depressed_pivot <- na.omit(all_cog_depressed_pivot)

ggplot(all_cog_depressed_pivot, aes(x = Age_group, y = Score, fill = factor(Is_depressed)))+
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8))+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Depression Status",
                    labels = c("Not Depressed", "Depressed"))+
  labs(title = "Distribution of Cognition Scores Across Age Groups\nBetween Depressed and Non-Depressed Individuals",
       x = "Age Group",
       y = "Cognition Score")

all_cog_BMI <- subset(HRS, select = c(HHIDPN, R3COG27,
                                  R3AGEY_E,
                                  R3BMI,
                                  R4COG27,
                                  R4AGEY_E,
                                  R4BMI,
                                  R5COG27,
                                  R5AGEY_E,
                                  R5BMI,
                                  R6COG27,
                                  R6AGEY_E,
                                  R6BMI,
                                  R7COG27,
                                  R7AGEY_E,
                                  R7BMI,
                                  R8COG27,
                                  R8AGEY_E,
                                  R8BMI,
                                  R9COG27,
                                  R9AGEY_E,
                                  R9BMI,
                                  R10COG27,
                                  R10AGEY_E,
                                  R10BMI,
                                  R11COG27,
                                  R11AGEY_E,
                                  R11BMI,
                                  R12COG27,
                                  R12AGEY_E,
                                  R12BMI,
                                  R13COG27,
                                  R13AGEY_E,
                                  R13BMI,
                                  R14COG27,
                                  R14AGEY_E,
                                  R14BMI,
                                  R15COG27,
                                  R15AGEY_E,
                                  R15BMI))
all_cog_BMI_pivot <- data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R3COG27, Age = all_cog_BMI$R3AGEY_E, BMI = all_cog_BMI$R3BMI)
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R4COG27, Age = all_cog_BMI$R4AGEY_E, BMI = all_cog_BMI$R4BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R5COG27, Age = all_cog_BMI$R5AGEY_E, BMI = all_cog_BMI$R5BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R6COG27, Age = all_cog_BMI$R6AGEY_E, BMI = all_cog_BMI$R6BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R7COG27, Age = all_cog_BMI$R7AGEY_E, BMI = all_cog_BMI$R7BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R8COG27, Age = all_cog_BMI$R8AGEY_E, BMI = all_cog_BMI$R8BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R9COG27, Age = all_cog_BMI$R9AGEY_E, BMI = all_cog_BMI$R9BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R10COG27, Age = all_cog_BMI$R10AGEY_E, BMI = all_cog_BMI$R10BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R11COG27, Age = all_cog_BMI$R11AGEY_E, BMI = all_cog_BMI$R11BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R12COG27, Age = all_cog_BMI$R12AGEY_E, BMI = all_cog_BMI$R12BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R13COG27, Age = all_cog_BMI$R13AGEY_E, BMI = all_cog_BMI$R13BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R14COG27, Age = all_cog_BMI$R14AGEY_E, BMI = all_cog_BMI$R14BMI))
all_cog_BMI_pivot <- rbind(all_cog_BMI_pivot, data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R15COG27, Age = all_cog_BMI$R15AGEY_E, BMI = all_cog_BMI$R15BMI))
library(dplyr)
all_cog_BMI_pivot$Age_group <- NA
all_cog_BMI_pivot <- all_cog_BMI_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))
all_cog_BMI_pivot <- all_cog_BMI_pivot[all_cog_BMI_pivot$Age >= 50, ]
all_cog_BMI_pivot <- na.omit(all_cog_BMI_pivot)
all_cog_BMI_pivot$BMI_rounded <- round(all_cog_BMI_pivot$BMI/2) * 2
all_cog_BMI_summary <- all_cog_BMI_pivot %>%
  group_by(Age_group, BMI_rounded) %>%
  summarise(mean_score = mean(Score, na.rm=TRUE), .groups = "drop")
library(ggplot2)
ggplot(all_cog_BMI_summary, aes(x = BMI_rounded, y = mean_score, color = factor(Age_group))) +
  geom_line(size = 1.2) +
  labs(
    title = "Cognition Scores by BMI Across Age Groups",
    x = "BMI",
    y = "Mean Cognition Score",
    color = "Age Group"
  )+
  scale_fill_brewer(palette = "Pastel1")
ggplot(all_cog_BMI_pivot, aes(x = BMI, y = Score, color = factor(Age_group)))+
  geom_point(alpha = 0.3) + 
  geom_smooth(method = lm, se = FALSE, size = 1.2)+
  labs(
    title = "Cognition Scores by BMI Across Age Groups",
    x = "BMI",
    y = "Cognition Score",
    color = "Age Group"
  )
ggplot(all_cog_BMI_summary, aes(x = BMI_rounded, y = mean_score, color = factor(Age_group))) +
  geom_point(size = 2, alpha = 0.7) +  # scatter points
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +  # regression lines
  labs(
    title = "Cognition Scores by BMI Across Age Groups",
    x = "BMI",
    y = "Mean Cognition Score",
    color = "Age Group"
  )
all_cog_pivot <- na.omit(all_cog_pivot)
ols_model <- lm(data = all_cog_pivot, Score ~ Age)
plot(fitted(ols_model), resid(ols_model),
     pch = 16,
     col = rbg(0,0,0,0.2),
     main = "Residual Plot of The OLS Model of Cognition Score by Age",
     xlab = "Fitted values",
     ylab = "Residuals")
library(ggplot2)
ggplot(data = data.frame(
  fitted = fitted(ols_model),
  resid = resid(ols_model)
), aes(x = fitted, y = resid))+
  geom_point(alpha = 0.02)+
  labs(title ="Residual Plot with Densisty Visualization\nof The OLS Model of Cognition Score by Age",
       x = "Fitted values", 
       y = "Residuals")

Inwave_cohort <- subset(HRS, select = c(INW1, INW2, INW3, INW4, INW5, INW6, INW7, INW8,
                                        INW9, INW10, INW11, INW12, INW13, INW14, INW15, INW16, HACOHORT))
Inwave_cohort$Cohort <- NA
Inwave_cohort <- Inwave_cohort %>%
  mutate(Cohort = case_when(
    HACOHORT == 1 ~ "AHEAD",
    HACOHORT == 2 ~ "Child of Depression",
    HACOHORT == 3 ~ "HRS",
    HACOHORT == 4 ~ "War Baby",
    HACOHORT == 5 ~ "Early Baby Boomer",
    HACOHORT == 6 ~ "Mid Baby Boomer",
    HACOHORT == 7 ~ "Late Baby Boomer",
    HACOHORT == 8 ~ "Early Generation X"
  ))

Wave_count <- Inwave_cohort %>%
  group_by(Cohort) %>%
  summarize(wave1 = sum(INW1),
            wave2 = sum(INW2),
            wave3 = sum(INW3),
            wave4 = sum(INW4),
            wave5 = sum(INW5),
            wave6 = sum(INW6),
            wave7 = sum(INW7),
            wave8 = sum(INW8),
            wave9 = sum(INW9),
            wave10 = sum(INW10),
            wave11 = sum(INW11),
            wave12 = sum(INW12),
            wave13 = sum(INW13),
            wave14 = sum(INW14),
            wave15 = sum(INW15),
            wave16 = sum(INW16))
Wave_count <- Wave_count[1:8,]

Wave_count_pivot <- Wave_count %>%
  pivot_longer(
    cols = starts_with("wave"),
    names_to = "count_number",
    values_to = "value"
  )

Wave_count_pivot$count_number <- factor(Wave_count_pivot$count_number,
                                  levels = c("wave1", "wave2",
                                             "wave3", "wave4",
                                             "wave5", "wave6",
                                             "wave7", "wave8",
                                             "wave9", "wave10",
                                             "wave11", "wave12",
                                             "wave13", "wave14",
                                             "wave15", "wave16"))

Wave_count_pivot$Cohort <- factor(Wave_count_pivot$Cohort,
                                  levels = c("HRS", "AHEAD",
                                             "Child of Depression", "War Baby",
                                             "Early Baby Boomer", "Mid Baby Boomer",
                                             "Late Baby Boomer", "Early Generation X"))

ggplot(Wave_count_pivot, aes(x = count_number, y = value, fill = Cohort))+
  geom_col(position = "stack")+
  labs(
    x = "Wave",
    y = "Number of responses",
    fill = "Cohort",
    title = "Survey Response for Each Wave by Cohort"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))


all_cog <- subset(HRS, select = c(HHIDPN, R3COG27,
                                  R3AGEY_E,
                                  R4COG27,
                                  R4AGEY_E,
                                  R5COG27,
                                  R5AGEY_E,
                                  R6COG27,
                                  R6AGEY_E,
                                  R7COG27,
                                  R7AGEY_E,
                                  R8COG27,
                                  R8AGEY_E,
                                  R9COG27,
                                  R9AGEY_E,
                                  R10COG27,
                                  R10AGEY_E,
                                  R11COG27,
                                  R11AGEY_E,
                                  R12COG27,
                                  R12AGEY_E,
                                  R13COG27,
                                  R13AGEY_E,
                                  R14COG27,
                                  R14AGEY_E,
                                  R15COG27,
                                  R15AGEY_E,
                                  R3WTRESP,
                                  R4WTRESP,
                                  R5WTRESP,
                                  R6WTRESP,
                                  R7WTRESP,
                                  R8WTRESP,
                                  R9WTRESP,
                                  R10WTRESP,
                                  R11WTRESP,
                                  R12WTRESP,
                                  R13WTRESP,
                                  R14WTRESP,
                                  R15WTRESP))
all_cog$weight <- NA

all_cog <- all_cog %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    TRUE ~ R3WTRESP
  ))

all_cog_pivot <- data.frame(ID = all_cog$HHIDPN, Score = all_cog$R3COG27, Age = all_cog$R3AGEY_E, weight = all_cog$weight)
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R4COG27, Age = all_cog$R4AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R5COG27, Age = all_cog$R5AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R6COG27, Age = all_cog$R6AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R7COG27, Age = all_cog$R7AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R8COG27, Age = all_cog$R8AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R9COG27, Age = all_cog$R9AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R10COG27, Age = all_cog$R10AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R11COG27, Age = all_cog$R11AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R12COG27, Age = all_cog$R12AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R13COG27, Age = all_cog$R13AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R14COG27, Age = all_cog$R14AGEY_E, weight = all_cog$weight))
all_cog_pivot <- rbind(all_cog_pivot, data.frame(ID = all_cog$HHIDPN, Score = all_cog$R15COG27, Age = all_cog$R15AGEY_E, weight = all_cog$weight))

library(dplyr)

all_cog_pivot <- all_cog_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))

all_cog_pivot$weight_norm <- all_cog_pivot$weight / mean(all_cog_pivot$weight, na.rm=TRUE)

all_cog_pivot <- na.omit(all_cog_pivot)

all_cog_pivot <- all_cog_pivot[all_cog_pivot$Age >= 50,]

library(ggplot2)

ggplot(all_cog_pivot, aes(x = Age_group, y = Score, weight = weight_norm))+
  geom_violin(fill = "lightblue")+
  labs(title = "Weighted Distribution of Cognition Scores Across Age Groups",
       x = "Age Group",
       y = "Cognition Score")+
  geom_boxplot(width=0.18)

in3_not4 <- all_cog[!is.na(all_cog$R3WTRESP) &  is.na(all_cog$R4WTRESP),]
in3_in4  <- all_cog[!is.na(all_cog$R3WTRESP) & !is.na(all_cog$R4WTRESP),]

in4_not5 <- all_cog[!is.na(all_cog$R4WTRESP) &  is.na(all_cog$R5WTRESP),]
in4_in5  <- all_cog[!is.na(all_cog$R4WTRESP) & !is.na(all_cog$R5WTRESP),]

in5_not6 <- all_cog[!is.na(all_cog$R5WTRESP) &  is.na(all_cog$R6WTRESP),]
in5_in6  <- all_cog[!is.na(all_cog$R5WTRESP) & !is.na(all_cog$R6WTRESP),]

in6_not7 <- all_cog[!is.na(all_cog$R6WTRESP) &  is.na(all_cog$R7WTRESP),]
in6_in7  <- all_cog[!is.na(all_cog$R6WTRESP) & !is.na(all_cog$R7WTRESP),]

in7_not8 <- all_cog[!is.na(all_cog$R7WTRESP) &  is.na(all_cog$R8WTRESP),]
in7_in8  <- all_cog[!is.na(all_cog$R7WTRESP) & !is.na(all_cog$R8WTRESP),]

in8_not9 <- all_cog[!is.na(all_cog$R8WTRESP) &  is.na(all_cog$R9WTRESP),]
in8_in9  <- all_cog[!is.na(all_cog$R8WTRESP) & !is.na(all_cog$R9WTRESP),]

in9_not10 <- all_cog[!is.na(all_cog$R9WTRESP) &  is.na(all_cog$R10WTRESP),]
in9_in10  <- all_cog[!is.na(all_cog$R9WTRESP) & !is.na(all_cog$R10WTRESP),]

in10_not11 <- all_cog[!is.na(all_cog$R10WTRESP) &  is.na(all_cog$R11WTRESP),]
in10_in11  <- all_cog[!is.na(all_cog$R10WTRESP) & !is.na(all_cog$R11WTRESP),]

in11_not12 <- all_cog[!is.na(all_cog$R11WTRESP) &  is.na(all_cog$R12WTRESP),]
in11_in12  <- all_cog[!is.na(all_cog$R11WTRESP) & !is.na(all_cog$R12WTRESP),]

in12_not13 <- all_cog[!is.na(all_cog$R12WTRESP) &  is.na(all_cog$R13WTRESP),]
in12_in13  <- all_cog[!is.na(all_cog$R12WTRESP) & !is.na(all_cog$R13WTRESP),]

in13_not14 <- all_cog[!is.na(all_cog$R13WTRESP) &  is.na(all_cog$R14WTRESP),]
in13_in14  <- all_cog[!is.na(all_cog$R13WTRESP) & !is.na(all_cog$R14WTRESP),]

in14_not15 <- all_cog[!is.na(all_cog$R14WTRESP) &  is.na(all_cog$R15WTRESP),]
in14_in15  <- all_cog[!is.na(all_cog$R14WTRESP) & !is.na(all_cog$R15WTRESP),]

all_cog_BMI_pivot <- data.frame(ID = all_cog_BMI$HHIDPN, Score = all_cog_BMI$R3COG27, Age = all_cog_BMI$R3AGEY_E, BMI = all_cog_BMI$R3BMI)

weight_attrition <- data.frame(weight = in3_in4$R3WTRESP/mean(in3_in4$R3WTRESP), attrition = 0, wave = "3-4")

weight_attrition <- rbind(
  weight_attrition,
  
  # 3 → 4
  data.frame(weight = in3_not4$R3WTRESP / mean(in3_not4$R3WTRESP), attrition = 1, wave = "3-4"),
  
  # 4 → 5
  data.frame(weight = in4_in5$R4WTRESP / mean(in4_in5$R4WTRESP),   attrition = 0, wave = "4-5"),
  data.frame(weight = in4_not5$R4WTRESP / mean(in4_not5$R4WTRESP), attrition = 1, wave = "4-5"),
  
  # 5 → 6
  data.frame(weight = in5_in6$R5WTRESP / mean(in5_in6$R5WTRESP),   attrition = 0, wave = "5-6"),
  data.frame(weight = in5_not6$R5WTRESP / mean(in5_not6$R5WTRESP), attrition = 1, wave = "5-6"),
  
  # 6 → 7
  data.frame(weight = in6_in7$R6WTRESP / mean(in6_in7$R6WTRESP),   attrition = 0, wave = "6-7"),
  data.frame(weight = in6_not7$R6WTRESP / mean(in6_not7$R6WTRESP), attrition = 1, wave = "6-7"),
  
  # 7 → 8
  data.frame(weight = in7_in8$R7WTRESP / mean(in7_in8$R7WTRESP),   attrition = 0, wave = "7-8"),
  data.frame(weight = in7_not8$R7WTRESP / mean(in7_not8$R7WTRESP), attrition = 1, wave = "7-8"),
  
  # 8 → 9
  data.frame(weight = in8_in9$R8WTRESP / mean(in8_in9$R8WTRESP),   attrition = 0, wave = "8-9"),
  data.frame(weight = in8_not9$R8WTRESP / mean(in8_not9$R8WTRESP), attrition = 1, wave = "8-9"),
  
  # 9 → 10
  data.frame(weight = in9_in10$R9WTRESP / mean(in9_in10$R9WTRESP),   attrition = 0, wave = "9-10"),
  data.frame(weight = in9_not10$R9WTRESP / mean(in9_not10$R9WTRESP), attrition = 1, wave = "9-10"),
  
  # 10 → 11
  data.frame(weight = in10_in11$R10WTRESP / mean(in10_in11$R10WTRESP),   attrition = 0, wave = "10-11"),
  data.frame(weight = in10_not11$R10WTRESP / mean(in10_not11$R10WTRESP), attrition = 1, wave = "10-11"),
  
  # 11 → 12
  data.frame(weight = in11_in12$R11WTRESP / mean(in11_in12$R11WTRESP),   attrition = 0, wave = "11-12"),
  data.frame(weight = in11_not12$R11WTRESP / mean(in11_not12$R11WTRESP), attrition = 1, wave = "11-12"),
  
  # 12 → 13
  data.frame(weight = in12_in13$R12WTRESP / mean(in12_in13$R12WTRESP),   attrition = 0, wave = "12-13"),
  data.frame(weight = in12_not13$R12WTRESP / mean(in12_not13$R12WTRESP), attrition = 1, wave = "12-13"),
  
  # 13 → 14
  data.frame(weight = in13_in14$R13WTRESP / mean(in13_in14$R13WTRESP),   attrition = 0, wave = "13-14"),
  data.frame(weight = in13_not14$R13WTRESP / mean(in13_not14$R13WTRESP), attrition = 1, wave = "13-14"),
  
  # 14 → 15
  data.frame(weight = in14_in15$R14WTRESP / mean(in14_in15$R14WTRESP),   attrition = 0, wave = "14-15"),
  data.frame(weight = in14_not15$R14WTRESP / mean(in14_not15$R14WTRESP), attrition = 1, wave = "14-15")
)
weight_attrition <- weight_attrition[weight_attrition$weight > 0, ]

weight_attrition$attrition <- factor(weight_attrition$attrition)

ggplot(data = weight_attrition, aes(x = attrition, y = weight))+
  geom_boxplot()+
  facet_wrap(~ wave, ncol = 3)+
  labs(title = "Comparing Distirbution of Normalized Sampling Weights for Attirition Across Waves")

binned_all_cog <- all_cog_pivot_cleaned %>%
  group_by(ID) %>%
  mutate(Time_Bin = cut(Age, breaks = seq(min(Age), max(Age), by = 2), include.lowest = TRUE)) %>%
  group_by(ID, Time_Bin) %>%
  summarise(Mean_Value = mean(Score, na.rm=TRUE)) %>%
  ungroup()

library(dplyr)
library(tidyr)
library(purrr)
all_cog_pivot <- all_cog_pivot[all_cog_pivot$Age >= 50, ]
all_cog_pivot <- na.omit(all_cog_pivot)

all_cog_pairs <- all_cog_pivot %>%
  arrange(ID, Age)%>%
  group_by(ID) %>%
  mutate(row = row_number()) %>%
  inner_join(.,., by="ID", suffix = c("_1", "_2"), relationship = "many-to-many") %>%
  filter(row_2 > row_1) %>%
  mutate(age_diff = Age_2 - Age_1)

all_cog_pairs$lag_group <- round(all_cog_pairs$age_diff)

lag_corr <- all_cog_pairs %>%
  group_by(lag_group) %>%
  summarize(corr = cor(Score_1, Score_2, use = "pairwise"), n = n())

lag_corr <- lag_corr[lag_corr$lag_group >= 2,]

library(ggplot2)

ggplot(lag_corr, aes(x = lag_group, y = corr, weight = n))+
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, span = 0.5) +
  labs(
    x = "Age Lag (years)",
    y = "Correlation Between Scores",
    title = "Lag Correlation of Cognitive Score by Age Difference \nWith Weighted LOESS Smoothing"
  )

all_cog_smoke <- subset(HRS, select = c(HHIDPN, R3COG27,
                                        R3AGEY_E,
                                        R3SMOKEN,
                                        R4COG27,
                                        R4AGEY_E,
                                        R4SMOKEN,
                                        R5COG27,
                                        R5AGEY_E,
                                        R5SMOKEN,
                                        R6COG27,
                                        R6AGEY_E,
                                        R6SMOKEN,
                                        R7COG27,
                                        R7AGEY_E,
                                        R7SMOKEN,
                                        R8COG27,
                                        R8AGEY_E,
                                        R8SMOKEN,
                                        R9COG27,
                                        R9AGEY_E,
                                        R9SMOKEN,
                                        R10COG27,
                                        R10AGEY_E,
                                        R10SMOKEN,
                                        R11COG27,
                                        R11AGEY_E,
                                        R11SMOKEN,
                                        R12COG27,
                                        R12AGEY_E,
                                        R12SMOKEN,
                                        R13COG27,
                                        R13AGEY_E,
                                        R13SMOKEN,
                                        R14COG27,
                                        R14AGEY_E,
                                        R14SMOKEN,
                                        R15COG27,
                                        R15AGEY_E,
                                        R15SMOKEN,
                                        R3WTRESP,
                                        R4WTRESP,
                                        R5WTRESP,
                                        R6WTRESP,
                                        R7WTRESP,
                                        R8WTRESP,
                                        R9WTRESP,
                                        R10WTRESP,
                                        R11WTRESP,
                                        R12WTRESP,
                                        R13WTRESP,
                                        R14WTRESP,
                                        R15WTRESP))

all_cog_smoke$weight <- NA

library(dplyr)

all_cog_smoke <- all_cog_smoke %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))

all_cog_smoke_pivot <- data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R3COG27, Age = all_cog_smoke$R3AGEY_E, Smoke = all_cog_smoke$R3SMOKEN, Weight = all_cog_smoke$weight)
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R4COG27, Age = all_cog_smoke$R4AGEY_E, Smoke = all_cog_smoke$R4SMOKEN, Weight = all_cog_smoke$weight))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R5COG27, Age = all_cog_smoke$R5AGEY_E, Smoke = all_cog_smoke$R5SMOKEN, Weight = all_cog_smoke$weight))                           
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R6COG27, Age = all_cog_smoke$R6AGEY_E, Smoke = all_cog_smoke$R6SMOKEN, Weight = all_cog_smoke$weight))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R7COG27, Age = all_cog_smoke$R7AGEY_E, Smoke = all_cog_smoke$R7SMOKEN, Weight = all_cog_smoke$weight))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R8COG27, Age = all_cog_smoke$R8AGEY_E, Smoke = all_cog_smoke$R8SMOKEN, Weight = all_cog_smoke$weight))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R9COG27, Age = all_cog_smoke$R9AGEY_E, Smoke = all_cog_smoke$R9SMOKEN, Weight = all_cog_smoke$weight))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R10COG27, Age = all_cog_smoke$R10AGEY_E, Smoke = all_cog_smoke$R10SMOKEN, Weight = all_cog_smoke$weight))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R11COG27, Age = all_cog_smoke$R11AGEY_E, Smoke = all_cog_smoke$R11SMOKEN, Weight = all_cog_smoke$weight))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R12COG27, Age = all_cog_smoke$R12AGEY_E, Smoke = all_cog_smoke$R12SMOKEN, Weight = all_cog_smoke$weight))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R13COG27, Age = all_cog_smoke$R13AGEY_E, Smoke = all_cog_smoke$R13SMOKEN, Weight = all_cog_smoke$weight)) 
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R14COG27, Age = all_cog_smoke$R14AGEY_E, Smoke = all_cog_smoke$R14SMOKEN, Weight = all_cog_smoke$weight))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R15COG27, Age = all_cog_smoke$R15AGEY_E, Smoke = all_cog_smoke$R15SMOKEN, Weight = all_cog_smoke$weight))
all_cog_smoke_pivot$Age_group <- NA
library(dplyr)
all_cog_smoke_pivot <- all_cog_smoke_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))

all_cog_smoke_pivot <- all_cog_smoke_pivot[all_cog_smoke_pivot$Age >= 50, ]
all_cog_smoke_pivot <- na.omit(all_cog_smoke_pivot)
library(ggplot2)
ggplot(all_cog_smoke_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Smoke)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Smoking Status",
                    labels = c("Non-Smoker", "Smoker"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nBetween Smokers and Non-Smokers",
       x = "Age Group",
       y = "Cognition Score")


ggplot(all_cog_smoke_pivot, aes(x = Age_group, weight = Weight, fill = factor(Smoke)))+
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Smoking Status",
                    labels = c("Non-Smoker", "Smoker"))+
  labs(y = "Proportion", x = "Age_group", title = "Weighted Distribution of Smokers for each Age Group")


all_cog_depressed <- subset(HRS, select = c(HHIDPN, R3COG27,
                                        R3AGEY_E,
                                        R3CESD,
                                        R4COG27,
                                        R4AGEY_E,
                                        R4CESD,
                                        R5COG27,
                                        R5AGEY_E,
                                        R5CESD,
                                        R6COG27,
                                        R6AGEY_E,
                                        R6CESD,
                                        R7COG27,
                                        R7AGEY_E,
                                        R7CESD,
                                        R8COG27,
                                        R8AGEY_E,
                                        R8CESD,
                                        R9COG27,
                                        R9AGEY_E,
                                        R9CESD,
                                        R10COG27,
                                        R10AGEY_E,
                                        R10CESD,
                                        R11COG27,
                                        R11AGEY_E,
                                        R11CESD,
                                        R12COG27,
                                        R12AGEY_E,
                                        R12CESD,
                                        R13COG27,
                                        R13AGEY_E,
                                        R13CESD,
                                        R14COG27,
                                        R14AGEY_E,
                                        R14CESD,
                                        R15COG27,
                                        R15AGEY_E,
                                        R15CESD,
                                        R3WTRESP,
                                        R4WTRESP,
                                        R5WTRESP,
                                        R6WTRESP,
                                        R7WTRESP,
                                        R8WTRESP,
                                        R9WTRESP,
                                        R10WTRESP,
                                        R11WTRESP,
                                        R12WTRESP,
                                        R13WTRESP,
                                        R14WTRESP,
                                        R15WTRESP))

all_cog_smoke$weight <- NA

library(dplyr)

all_cog_depressed <- all_cog_depressed %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))


all_cog_depressed_pivot <- data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R3COG27, Age = all_cog_depressed$R3AGEY_E, CESD = all_cog_depressed$R3CESD, Weight = all_cog_depressed$weight)
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R4COG27, Age = all_cog_depressed$R4AGEY_E, CESD = all_cog_depressed$R4CESD, Weight = all_cog_depressed$weight))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R5COG27, Age = all_cog_depressed$R5AGEY_E, CESD = all_cog_depressed$R5CESD, Weight = all_cog_depressed$weight))                           
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R6COG27, Age = all_cog_depressed$R6AGEY_E, CESD = all_cog_depressed$R6CESD, Weight = all_cog_depressed$weight))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R7COG27, Age = all_cog_depressed$R7AGEY_E, CESD = all_cog_depressed$R7CESD, Weight = all_cog_depressed$weight))  
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R8COG27, Age = all_cog_depressed$R8AGEY_E, CESD = all_cog_depressed$R8CESD, Weight = all_cog_depressed$weight))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R9COG27, Age = all_cog_depressed$R9AGEY_E, CESD = all_cog_depressed$R9CESD, Weight = all_cog_depressed$weight))  
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R10COG27, Age = all_cog_depressed$R10AGEY_E, CESD = all_cog_depressed$R10CESD, Weight = all_cog_depressed$weight))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R11COG27, Age = all_cog_depressed$R11AGEY_E, CESD = all_cog_depressed$R11CESD, Weight = all_cog_depressed$weight))  
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R12COG27, Age = all_cog_depressed$R12AGEY_E, CESD = all_cog_depressed$R12CESD, Weight = all_cog_depressed$weight))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R13COG27, Age = all_cog_depressed$R13AGEY_E, CESD = all_cog_depressed$R13CESD, Weight = all_cog_depressed$weight)) 
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R14COG27, Age = all_cog_depressed$R14AGEY_E, CESD = all_cog_depressed$R14CESD, Weight = all_cog_depressed$weight))
all_cog_depressed_pivot <- rbind(all_cog_depressed_pivot, data.frame(ID = all_cog_depressed$HHIDPN, Score = all_cog_depressed$R15COG27, Age = all_cog_depressed$R15AGEY_E, CESD = all_cog_depressed$R15CESD, Weight = all_cog_depressed$weight))
all_cog_depressed_pivot$Age_group <- NA
library(dplyr)
all_cog_depressed_pivot <- all_cog_depressed_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))


all_cog_depressed_pivot <- all_cog_depressed_pivot %>%
  mutate(Depression = case_when(
    CESD > 2 ~ 1,
    TRUE ~ 0
  ))

all_cog_depressed_pivot <- all_cog_depressed_pivot[all_cog_depressed_pivot$Age >= 50, ]
all_cog_depressed_pivot <- na.omit(all_cog_depressed_pivot)
library(ggplot2)
ggplot(all_cog_depressed_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Depression)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Depression Status",
                    labels = c("Not Depressed", "Depressed"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Depression Status",
       x = "Age Group",
       y = "Cognition Score")

ggplot(all_cog_depressed_pivot, aes(x = Age_group, weight = Weight, fill = factor(Depression)))+
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Depression Status",
                    labels = c("Not Depressed", "Depressed"))+
  labs(y = "Proportion", x = "Age_group", title = "Weighted Distribution of Depression for each Age Group")



all_cog_hispanic <- NA
all_cog_hispanic_pivot <- NA

all_cog_hispanic <- subset(HRS, select = c(HHIDPN, R3COG27,
                                            R3AGEY_E,
                                            R3CESD,
                                            R4COG27,
                                            R4AGEY_E,
                                            R4CESD,
                                            R5COG27,
                                            R5AGEY_E,
                                            R5CESD,
                                            R6COG27,
                                            R6AGEY_E,
                                            R6CESD,
                                            R7COG27,
                                            R7AGEY_E,
                                            R7CESD,
                                            R8COG27,
                                            R8AGEY_E,
                                            R8CESD,
                                            R9COG27,
                                            R9AGEY_E,
                                            R9CESD,
                                            R10COG27,
                                            R10AGEY_E,
                                            R10CESD,
                                            R11COG27,
                                            R11AGEY_E,
                                            R11CESD,
                                            R12COG27,
                                            R12AGEY_E,
                                            R12CESD,
                                            R13COG27,
                                            R13AGEY_E,
                                            R13CESD,
                                            R14COG27,
                                            R14AGEY_E,
                                            R14CESD,
                                            R15COG27,
                                            R15AGEY_E,
                                            R15CESD,
                                            R3WTRESP,
                                            R4WTRESP,
                                            R5WTRESP,
                                            R6WTRESP,
                                            R7WTRESP,
                                            R8WTRESP,
                                            R9WTRESP,
                                            R10WTRESP,
                                            R11WTRESP,
                                            R12WTRESP,
                                            R13WTRESP,
                                            R14WTRESP,
                                            R15WTRESP,
                                           RAHISPAN))

all_cog_hispanic$weight <- NA

library(dplyr)

all_cog_hispanic <- all_cog_hispanic %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))

all_cog_hispanic_pivot <- data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R3COG27, Age = all_cog_hispanic$R3AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight)
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R4COG27, Age = all_cog_hispanic$R4AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R5COG27, Age = all_cog_hispanic$R5AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R6COG27, Age = all_cog_hispanic$R6AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R7COG27, Age = all_cog_hispanic$R7AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R8COG27, Age = all_cog_hispanic$R8AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R9COG27, Age = all_cog_hispanic$R9AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R10COG27, Age = all_cog_hispanic$R10AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R11COG27, Age = all_cog_hispanic$R11AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R12COG27, Age = all_cog_hispanic$R12AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R13COG27, Age = all_cog_hispanic$R13AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R14COG27, Age = all_cog_hispanic$R14AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))
all_cog_hispanic_pivot <- rbind(all_cog_hispanic_pivot, data.frame(ID = all_cog_hispanic$HHIDPN, Score = all_cog_hispanic$R15COG27, Age = all_cog_hispanic$R15AGEY_E, Hispanic = all_cog_hispanic$RAHISPAN, Weight = all_cog_hispanic$weight))

all_cog_hispanic_pivot$Age_group <- NA
library(dplyr)
all_cog_hispanic_pivot <- all_cog_hispanic_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))

all_cog_hispanic_pivot <- all_cog_hispanic_pivot[all_cog_hispanic_pivot$Age >= 50, ]
all_cog_hispanic_pivot <- na.omit(all_cog_hispanic_pivot)
library(ggplot2)
ggplot(all_cog_hispanic_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Hispanic)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Hispanic",
                    labels = c("Not Hispanic", "Hispanic"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Whether they are Hispanic",
       x = "Age Group",
       y = "Cognition Score")


all_cog_blood_pressure <- subset(HRS, select = c(HHIDPN, R3COG27,
                                           R3AGEY_E,
                                           R3CESD,
                                           R4COG27,
                                           R4AGEY_E,
                                           R4CESD,
                                           R5COG27,
                                           R5AGEY_E,
                                           R5CESD,
                                           R6COG27,
                                           R6AGEY_E,
                                           R6CESD,
                                           R7COG27,
                                           R7AGEY_E,
                                           R7CESD,
                                           R8COG27,
                                           R8AGEY_E,
                                           R8CESD,
                                           R9COG27,
                                           R9AGEY_E,
                                           R9CESD,
                                           R10COG27,
                                           R10AGEY_E,
                                           R10CESD,
                                           R11COG27,
                                           R11AGEY_E,
                                           R11CESD,
                                           R12COG27,
                                           R12AGEY_E,
                                           R12CESD,
                                           R13COG27,
                                           R13AGEY_E,
                                           R13CESD,
                                           R14COG27,
                                           R14AGEY_E,
                                           R14CESD,
                                           R15COG27,
                                           R15AGEY_E,
                                           R15CESD,
                                           R3WTRESP,
                                           R4WTRESP,
                                           R5WTRESP,
                                           R6WTRESP,
                                           R7WTRESP,
                                           R8WTRESP,
                                           R9WTRESP,
                                           R10WTRESP,
                                           R11WTRESP,
                                           R12WTRESP,
                                           R13WTRESP,
                                           R14WTRESP,
                                           R15WTRESP,
                                           RAHISPAN,
                                           R8BPSYS,
                                           R9BPSYS,
                                           R10BPSYS,
                                           R11BPSYS,
                                           R12BPSYS,
                                           R13BPSYS,
                                           R14BPSYS))
library(dplyr)
all_cog_blood_pressure <- all_cog_blood_pressure %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))

all_cog_blood_pressure_pivot <- data.frame(ID = all_cog_blood_pressure$HHIDPN, Score = all_cog_blood_pressure$R8COG27, Age = all_cog_blood_pressure$R8AGEY_E, BloodPressure = all_cog_blood_pressure$R8BPSYS, Weight = all_cog_blood_pressure$weight)
all_cog_blood_pressure_pivot <- rbind(all_cog_blood_pressure_pivot, data.frame(ID = all_cog_blood_pressure$HHIDPN, Score = all_cog_blood_pressure$R9COG27, Age = all_cog_blood_pressure$R9AGEY_E, BloodPressure = all_cog_blood_pressure$R9BPSYS, Weight = all_cog_blood_pressure$weight))
all_cog_blood_pressure_pivot <- rbind(all_cog_blood_pressure_pivot, data.frame(ID = all_cog_blood_pressure$HHIDPN, Score = all_cog_blood_pressure$R10COG27, Age = all_cog_blood_pressure$R10AGEY_E, BloodPressure = all_cog_blood_pressure$R10BPSYS, Weight = all_cog_blood_pressure$weight))
all_cog_blood_pressure_pivot <- rbind(all_cog_blood_pressure_pivot, data.frame(ID = all_cog_blood_pressure$HHIDPN, Score = all_cog_blood_pressure$R11COG27, Age = all_cog_blood_pressure$R11AGEY_E, BloodPressure = all_cog_blood_pressure$R11BPSYS, Weight = all_cog_blood_pressure$weight))
all_cog_blood_pressure_pivot <- rbind(all_cog_blood_pressure_pivot, data.frame(ID = all_cog_blood_pressure$HHIDPN, Score = all_cog_blood_pressure$R12COG27, Age = all_cog_blood_pressure$R12AGEY_E, BloodPressure = all_cog_blood_pressure$R12BPSYS, Weight = all_cog_blood_pressure$weight))
all_cog_blood_pressure_pivot <- rbind(all_cog_blood_pressure_pivot, data.frame(ID = all_cog_blood_pressure$HHIDPN, Score = all_cog_blood_pressure$R13COG27, Age = all_cog_blood_pressure$R13AGEY_E, BloodPressure = all_cog_blood_pressure$R13BPSYS, Weight = all_cog_blood_pressure$weight))
all_cog_blood_pressure_pivot <- rbind(all_cog_blood_pressure_pivot, data.frame(ID = all_cog_blood_pressure$HHIDPN, Score = all_cog_blood_pressure$R14COG27, Age = all_cog_blood_pressure$R14AGEY_E, BloodPressure = all_cog_blood_pressure$R14BPSYS, Weight = all_cog_blood_pressure$weight))

all_cog_blood_pressure_pivot <- all_cog_blood_pressure_pivot %>%
  mutate(Age_group = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))

all_cog_blood_pressure_pivot <- all_cog_blood_pressure_pivot[all_cog_blood_pressure_pivot$Age >= 50, ]
all_cog_blood_pressure_pivot <- na.omit(all_cog_blood_pressure_pivot)
all_cog_blood_pressure_pivot$blood_pressure_rounded <- round(all_cog_blood_pressure_pivot$BloodPressure/2) * 2
all_cog_blood_pressure_summary <- all_cog_blood_pressure_pivot %>%
  group_by(Age_group, blood_pressure_rounded) %>%
  summarise(mean_score = mean(Score, na.rm=TRUE), .groups = "drop")

ggplot(all_cog_blood_pressure_summary, aes(x = blood_pressure_rounded, y = mean_score, color = factor(Age_group))) +
  geom_point(size = 2, alpha = 0.7) +  
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +  
  labs(
    title = "Cognition Scores by Blood Pressure Across Age Groups",
    x = "Blood Pressure",
    y = "Mean Cognition Score",
    color = "Age Group"
  )

library(ggplot2)

ggplot(all_cog_blood_pressure_pivot, aes(x = BloodPressure, y = Score, color = factor(Age_group), size = Weight))+
  geom_smooth(
    method = "lm",
    mapping = aes(weight = Weight),
    size = 1.5
  )+
  labs(
    title = "Weighted Regression Lines on Cognitions Scores By Blood Pressure\n Across Age Groups",
    x = "Blood Pressure",
    y = "Cognition Score",
    color = "Age Group"
  )

all_cog_blood_pressure_pivot <- all_cog_blood_pressure_pivot %>%
  mutate(Age_group2 = case_when(
    Age < 80 ~ "50-65",
    Age <  ~ "65-80",
    TRUE ~ "80+"
  ))

all_cog_exercise <- subset(HRS, select = c(HHIDPN, R3COG27,
                                           R3AGEY_E,
                                           R3CESD,
                                           R4COG27,
                                           R4AGEY_E,
                                           R4CESD,
                                           R5COG27,
                                           R5AGEY_E,
                                           R5CESD,
                                           R6COG27,
                                           R6AGEY_E,
                                           R6CESD,
                                           R7COG27,
                                           R7AGEY_E,
                                           R7CESD,
                                           R8COG27,
                                           R8AGEY_E,
                                           R8CESD,
                                           R9COG27,
                                           R9AGEY_E,
                                           R9CESD,
                                           R10COG27,
                                           R10AGEY_E,
                                           R10CESD,
                                           R11COG27,
                                           R11AGEY_E,
                                           R11CESD,
                                           R12COG27,
                                           R12AGEY_E,
                                           R12CESD,
                                           R13COG27,
                                           R13AGEY_E,
                                           R13CESD,
                                           R14COG27,
                                           R14AGEY_E,
                                           R14CESD,
                                           R15COG27,
                                           R15AGEY_E,
                                           R15CESD,
                                           R3WTRESP,
                                           R4WTRESP,
                                           R5WTRESP,
                                           R6WTRESP,
                                           R7WTRESP,
                                           R8WTRESP,
                                           R9WTRESP,
                                           R10WTRESP,
                                           R11WTRESP,
                                           R12WTRESP,
                                           R13WTRESP,
                                           R14WTRESP,
                                           R15WTRESP,
                                           RAHISPAN,
                                           R7VGACTX,
                                           R8VGACTX,
                                           R9VGACTX,
                                           R10VGACTX,
                                           R11VGACTX,
                                           R12VGACTX,
                                           R13VGACTX,
                                           R14VGACTX,
                                           R15VGACTX))
library(dplyr)
all_cog_exercise <- all_cog_exercise %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))


all_cog_exercise_pivot <- data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R3COG27, Age = all_cog_exercise$R7AGEY_E, Exercise = all_cog_exercise$R7VGACTX, Weight = all_cog_exercise$weight)
all_cog_exercise_pivot <- rbind(all_cog_exercise_pivot, data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R4COG27, Age = all_cog_exercise$R8AGEY_E, Exercise = all_cog_exercise$R8VGACTX, Weight = all_cog_exercise$weight))
all_cog_exercise_pivot <- rbind(all_cog_exercise_pivot, data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R4COG27, Age = all_cog_exercise$R9AGEY_E, Exercise = all_cog_exercise$R9VGACTX, Weight = all_cog_exercise$weight))
all_cog_exercise_pivot <- rbind(all_cog_exercise_pivot, data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R4COG27, Age = all_cog_exercise$R10AGEY_E, Exercise = all_cog_exercise$R10VGACTX, Weight = all_cog_exercise$weight))
all_cog_exercise_pivot <- rbind(all_cog_exercise_pivot, data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R4COG27, Age = all_cog_exercise$R11AGEY_E, Exercise = all_cog_exercise$R11VGACTX, Weight = all_cog_exercise$weight))
all_cog_exercise_pivot <- rbind(all_cog_exercise_pivot, data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R4COG27, Age = all_cog_exercise$R12AGEY_E, Exercise = all_cog_exercise$R12VGACTX, Weight = all_cog_exercise$weight))
all_cog_exercise_pivot <- rbind(all_cog_exercise_pivot, data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R4COG27, Age = all_cog_exercise$R13AGEY_E, Exercise = all_cog_exercise$R13VGACTX, Weight = all_cog_exercise$weight))
all_cog_exercise_pivot <- rbind(all_cog_exercise_pivot, data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R4COG27, Age = all_cog_exercise$R14AGEY_E, Exercise = all_cog_exercise$R14VGACTX, Weight = all_cog_exercise$weight))
all_cog_exercise_pivot <- rbind(all_cog_exercise_pivot, data.frame(ID = all_cog_exercise$HHIDPN, Score = all_cog_exercise$R4COG27, Age = all_cog_exercise$R15AGEY_E, Exercise = all_cog_exercise$R15VGACTX, Weight = all_cog_exercise$weight))

all_cog_exercise_pivot <- all_cog_exercise_pivot %>%
  mutate(Age_group = case_when(
    Age < 65 ~ "50-65",
    Age < 80 ~ "65-80",
    TRUE ~ "80+"
  ))

all_cog_exercise_pivot <- all_cog_exercise_pivot[all_cog_exercise_pivot$Age >= 50, ]
all_cog_exercise_pivot <- na.omit(all_cog_exercise_pivot)

all_cog_exercise_pivot <- all_cog_exercise_pivot %>%
  mutate(Weekly_Exercise = case_when(
    Exercise > 2 ~ 1,
    TRUE ~ 0
  ))

library(ggplot2)
ggplot(all_cog_exercise_pivot, aes(x = factor(Exercise), y = Score, weight = Weight))+
  geom_violin(fill = "lightblue")+
  labs(title = "Weighted Distribution of Cognition Score Based on Vigorous Exercise",
       x = "Age Group",
       y = "Cognition Score")+
  facet_wrap(~Age_group, ncol = 3)


HRS_10thwave <- HRS[(is.na(HRS$R3WTRESP) | HRS$R3WTRESP == 0) &
                    (is.na(HRS$R4WTRESP) | HRS$R4WTRESP == 0) & 
                    (is.na(HRS$R5WTRESP) | HRS$R5WTRESP == 0) &
                    (is.na(HRS$R6WTRESP) | HRS$R6WTRESP == 0) &
                    (is.na(HRS$R7WTRESP) | HRS$R7WTRESP == 0) &
                    (is.na(HRS$R8WTRESP) | HRS$R8WTRESP == 0) &
                    (is.na(HRS$R9WTRESP) | HRS$R9WTRESP == 0) &
                      !is.na(HRS$R10WTRESP)&
                      !is.na(HRS$R11WTRESP)&
                      !is.na(HRS$R12WTRESP)&
                      !is.na(HRS$R13WTRESP)&
                      !is.na(HRS$R14WTRESP),]
HRS_3rdwave <- HRS[!is.na(HRS$R3WTRESP)&
                     !is.na(HRS$R4WTRESP)&
                     !is.na(HRS$R5WTRESP)&
                     !is.na(HRS$R6WTRESP)&
                     !is.na(HRS$R7WTRESP)&
                     !is.na(HRS$R8WTRESP)&
                     !is.na(HRS$R9WTRESP)&
                     !is.na(HRS$R10WTRESP)&
                     !is.na(HRS$R11WTRESP)&
                     !is.na(HRS$R12WTRESP)&
                     !is.na(HRS$R13WTRESP)&
                     !is.na(HRS$R14WTRESP),]

all_cog_3rd_wave <- subset(HRS_3rdwave, select = c(HHIDPN, R3COG27,
                                  R3AGEY_E,
                                  R4COG27,
                                  R4AGEY_E,
                                  R5COG27,
                                  R5AGEY_E,
                                  R6COG27,
                                  R6AGEY_E,
                                  R7COG27,
                                  R7AGEY_E,
                                  R8COG27,
                                  R8AGEY_E,
                                  R9COG27,
                                  R9AGEY_E,
                                  R10COG27,
                                  R10AGEY_E,
                                  R11COG27,
                                  R11AGEY_E,
                                  R12COG27,
                                  R12AGEY_E,
                                  R13COG27,
                                  R13AGEY_E,
                                  R14COG27,
                                  R14AGEY_E,
                                  R15COG27,
                                  R15AGEY_E))
sample3 <- all_cog_3rd_wave[113,]

all_cog_10th_wave <- subset(HRS_10thwave, select = c(HHIDPN, R3COG27,
                                                   R3AGEY_E,
                                                   R4COG27,
                                                   R4AGEY_E,
                                                   R5COG27,
                                                   R5AGEY_E,
                                                   R6COG27,
                                                   R6AGEY_E,
                                                   R7COG27,
                                                   R7AGEY_E,
                                                   R8COG27,
                                                   R8AGEY_E,
                                                   R9COG27,
                                                   R9AGEY_E,
                                                   R10COG27,
                                                   R10AGEY_E,
                                                   R11COG27,
                                                   R11AGEY_E,
                                                   R12COG27,
                                                   R12AGEY_E,
                                                   R13COG27,
                                                   R13AGEY_E,
                                                   R14COG27,
                                                   R14AGEY_E,
                                                   R15COG27,
                                                   R15AGEY_E))

sample10 <- all_cog_10th_wave[3717,]

df <- data.frame(cog_score = c(sample3$R3COG27[1],
                               sample3$R4COG27[1],
                               sample3$R5COG27[1],
                               sample3$R6COG27[1],
                               sample3$R7COG27[1],
                               sample3$R8COG27[1],
                               sample3$R9COG27[1],
                               sample3$R10COG27[1],
                               sample3$R11COG27[1],
                               sample3$R12COG27[1],
                               sample3$R13COG27[1],
                               sample3$R14COG27[1],
                               sample10$R10COG27[1],
                               sample10$R11COG27[1],
                               sample10$R12COG27[1],
                               sample10$R13COG27[1],
                               sample10$R14COG27[1]),
                 age = c(sample3$R3AGEY_E[1],
                         sample3$R4AGEY_E[1],
                         sample3$R5AGEY_E[1],
                         sample3$R6AGEY_E[1],
                         sample3$R7AGEY_E[1],
                         sample3$R8AGEY_E[1],
                         sample3$R9AGEY_E[1],
                         sample3$R10AGEY_E[1],
                         sample3$R11AGEY_E[1],
                         sample3$R12AGEY_E[1],
                         sample3$R13AGEY_E[1],
                         sample3$R14AGEY_E[1],
                         sample10$R10AGEY_E[1],
                         sample10$R11AGEY_E[1],
                         sample10$R12AGEY_E[1],
                         sample10$R13AGEY_E[1],
                         sample10$R14AGEY_E[1]),
                 year = c(1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,
                          2010,2012,2014,2016,2018),
                person = c("Person A","Person A","Person A","Person A","Person A","Person A",
                           "Person A","Person A","Person A","Person A","Person A","Person A",
                           "Person B","Person B","Person B","Person B","Person B"))

library(ggplot2)                
plot1 <- ggplot(df, aes(x = year, y = cog_score, color = person))+
  geom_line(size=2)+
  labs(x = "Year", y = "Cognition Score",
       title = "Cognition Score by Year for Two Different People")

plot2 <- ggplot(df, aes(x = age, y = cog_score, color = person))+
  geom_line(size=2)+
  labs(x = "Age", y = "Cognition Score",
       title = "Cognition Score by Age for Two Different People")

library(patchwork)

stacked_plots <- plot1 / plot2
print(stacked_plots)

#January 18th



HRS$RAESTRAT #Stratums
sum(unique(HRS$RAESTRAT)) #Stratums 
sum(unique(HRS$RAEHSAMP)) #clusters 

all_cog_smoke <- subset(HRS, select = c(HHIDPN, R3COG27,
                                        R3AGEY_E,
                                        R3SMOKEN,
                                        R4COG27,
                                        R4AGEY_E,
                                        R4SMOKEN,
                                        R5COG27,
                                        R5AGEY_E,
                                        R5SMOKEN,
                                        R6COG27,
                                        R6AGEY_E,
                                        R6SMOKEN,
                                        R7COG27,
                                        R7AGEY_E,
                                        R7SMOKEN,
                                        R8COG27,
                                        R8AGEY_E,
                                        R8SMOKEN,
                                        R9COG27,
                                        R9AGEY_E,
                                        R9SMOKEN,
                                        R10COG27,
                                        R10AGEY_E,
                                        R10SMOKEN,
                                        R11COG27,
                                        R11AGEY_E,
                                        R11SMOKEN,
                                        R12COG27,
                                        R12AGEY_E,
                                        R12SMOKEN,
                                        R13COG27,
                                        R13AGEY_E,
                                        R13SMOKEN,
                                        R14COG27,
                                        R14AGEY_E,
                                        R14SMOKEN,
                                        R15COG27,
                                        R15AGEY_E,
                                        R15SMOKEN,
                                        R3WTRESP,
                                        R4WTRESP,
                                        R5WTRESP,
                                        R6WTRESP,
                                        R7WTRESP,
                                        R8WTRESP,
                                        R9WTRESP,
                                        R10WTRESP,
                                        R11WTRESP,
                                        R12WTRESP,
                                        R13WTRESP,
                                        R14WTRESP,
                                        R15WTRESP,
                                        RAESTRAT,
                                        RAEHSAMP))

all_cog_smoke$weight <- NA

library(dplyr)

all_cog_smoke <- all_cog_smoke %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))

all_cog_smoke_pivot <- data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R3COG27, Age = all_cog_smoke$R3AGEY_E, Smoke = all_cog_smoke$R3SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP)
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R4COG27, Age = all_cog_smoke$R4AGEY_E, Smoke = all_cog_smoke$R4SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R5COG27, Age = all_cog_smoke$R5AGEY_E, Smoke = all_cog_smoke$R5SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))                           
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R6COG27, Age = all_cog_smoke$R6AGEY_E, Smoke = all_cog_smoke$R6SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R7COG27, Age = all_cog_smoke$R7AGEY_E, Smoke = all_cog_smoke$R7SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R8COG27, Age = all_cog_smoke$R8AGEY_E, Smoke = all_cog_smoke$R8SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R9COG27, Age = all_cog_smoke$R9AGEY_E, Smoke = all_cog_smoke$R9SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R10COG27, Age = all_cog_smoke$R10AGEY_E, Smoke = all_cog_smoke$R10SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R11COG27, Age = all_cog_smoke$R11AGEY_E, Smoke = all_cog_smoke$R11SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))  
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R12COG27, Age = all_cog_smoke$R12AGEY_E, Smoke = all_cog_smoke$R12SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R13COG27, Age = all_cog_smoke$R13AGEY_E, Smoke = all_cog_smoke$R13SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP)) 
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R14COG27, Age = all_cog_smoke$R14AGEY_E, Smoke = all_cog_smoke$R14SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))
all_cog_smoke_pivot <- rbind(all_cog_smoke_pivot, data.frame(ID = all_cog_smoke$HHIDPN, Score = all_cog_smoke$R15COG27, Age = all_cog_smoke$R15AGEY_E, Smoke = all_cog_smoke$R15SMOKEN, Weight = all_cog_smoke$weight, stratum = all_cog_smoke$RAESTRAT, cluster = all_cog_smoke$RAEHSAMP))

#good code just had to change stuff
#all_cog_strat_clust <- cbind(all_cog, HRS$RAESTRAT, HRS$RAEHSAMP)
#all_cog_strat_clust <- all_cog_strat_clust[all_cog_strat_clust[,ncol(all_cog_strat_clust)] != 99, ]
#names(all_cog_strat_clust)[(ncol(all_cog_strat_clust)-1):ncol(all_cog_strat_clust)] <- c("Stratum", "Cluster")

library(survey)

all_cog_smoke_pivot <- all_cog_smoke_pivot[all_cog_smoke_pivot[ncol(all_cog_smoke_pivot)] != 99, ]
all_cog_smoke_pivot <- all_cog_smoke_pivot[all_cog_smoke_pivot$Weight > 0, ]

all_cog_smoke_pivot <- na.omit(all_cog_smoke_pivot)

dclus <- svydesign(ids=~all_cog_smoke_pivot$ID,
          strata = all_cog_smoke_pivot$Stratum, data = all_cog_smoke_pivot,
          weights = all_cog_smoke_pivot$Weight, nest = TRUE)
#cant load due to memory
#library(svylme)
#library(Matrix)

#svy2lme(Score ~ Age+ Smoke + (1 | ID), design = dclus)

library(lme4)
fm1 <- lmer(data = all_cog_smoke_pivot, Score ~ Age + Smoke + (1 | ID), weights = Weight)
summary(fm1)
plot(fm1)
coef(fm1)

all_cog_var <- subset(HRS, select = c(HHIDPN, R3COG27,
                                        R3AGEY_E,
                                        R3SMOKEN,
                                        R4COG27,
                                        R4AGEY_E,
                                        R4SMOKEN,
                                        R5COG27,
                                        R5AGEY_E,
                                        R5SMOKEN,
                                        R6COG27,
                                        R6AGEY_E,
                                        R6SMOKEN,
                                        R7COG27,
                                        R7AGEY_E,
                                        R7SMOKEN,
                                        R8COG27,
                                        R8AGEY_E,
                                        R8SMOKEN,
                                        R9COG27,
                                        R9AGEY_E,
                                        R9SMOKEN,
                                        R10COG27,
                                        R10AGEY_E,
                                        R10SMOKEN,
                                        R11COG27,
                                        R11AGEY_E,
                                        R11SMOKEN,
                                        R12COG27,
                                        R12AGEY_E,
                                        R12SMOKEN,
                                        R13COG27,
                                        R13AGEY_E,
                                        R13SMOKEN,
                                        R14COG27,
                                        R14AGEY_E,
                                        R14SMOKEN,
                                        R15COG27,
                                        R15AGEY_E,
                                        R15SMOKEN,
                                        R3WTRESP,
                                        R4WTRESP,
                                        R5WTRESP,
                                        R6WTRESP,
                                        R7WTRESP,
                                        R8WTRESP,
                                        R9WTRESP,
                                        R10WTRESP,
                                        R11WTRESP,
                                        R12WTRESP,
                                        R13WTRESP,
                                        R14WTRESP,
                                        R15WTRESP,
                                        R3CESD,
                                        R4CESD,
                                        R5CESD,
                                        R6CESD,
                                        R7CESD,
                                        R8CESD,
                                        R9CESD,
                                        R10CESD,
                                        R11CESD,
                                        R12CESD,
                                        R13CESD,
                                        R14CESD,
                                        R15CESD,
                                        RAHISPAN,
                                        RAESTRAT,
                                        RAEHSAMP))

library(dplyr)

all_cog_var <- all_cog_var %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))

all_cog_var_pivot <- data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R3COG27, Age = all_cog_var$R3AGEY_E, Smoke = all_cog_var$R3SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD)
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R4COG27, Age = all_cog_var$R4AGEY_E, Smoke = all_cog_var$R4SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R4CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R5COG27, Age = all_cog_var$R5AGEY_E, Smoke = all_cog_var$R5SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R5CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R6COG27, Age = all_cog_var$R6AGEY_E, Smoke = all_cog_var$R6SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R6CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R7COG27, Age = all_cog_var$R7AGEY_E, Smoke = all_cog_var$R7SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R7CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R8COG27, Age = all_cog_var$R8AGEY_E, Smoke = all_cog_var$R8SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R8CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R9COG27, Age = all_cog_var$R9AGEY_E, Smoke = all_cog_var$R9SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R9CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R10COG27, Age = all_cog_var$R10AGEY_E, Smoke = all_cog_var$R10SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R10CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R11COG27, Age = all_cog_var$R11AGEY_E, Smoke = all_cog_var$R11SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R11CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R12COG27, Age = all_cog_var$R12AGEY_E, Smoke = all_cog_var$R12SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R12CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R13COG27, Age = all_cog_var$R13AGEY_E, Smoke = all_cog_var$R13SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R13CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R14COG27, Age = all_cog_var$R14AGEY_E, Smoke = all_cog_var$R14SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R14CESD))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R15COG27, Age = all_cog_var$R15AGEY_E, Smoke = all_cog_var$R15SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R15CESD))
colSums(is.na(all_cog_var_pivot))
all_cog_var_pivot <- na.omit(all_cog_var_pivot)
all_cog_var_pivot <- all_cog_var_pivot[all_cog_var_pivot$Weight > 0, ]

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Depression = case_when(
    Depressed > 2 ~ 1,
    TRUE ~ 0
  ))

fm1 <- lmer(data = all_cog_var_pivot, Score ~ Age + Smoke + Hispanic + Depression + (1 | ID), weights = Weight)
summary(fm1)
plot(fm1)
plot(fitted(fm1), resid(fm1))
library(ggplot2)
ggplot(aes(fitted(fm1), resid(fm1)))+
  geom_point(alpha = 0.03)

# Jan 26th

all_cog_var <- subset(HRS, select = c(HHIDPN, R3COG27,
                                      R3AGEY_E,
                                      R3SMOKEN,
                                      R4COG27,
                                      R4AGEY_E,
                                      R4SMOKEN,
                                      R5COG27,
                                      R5AGEY_E,
                                      R5SMOKEN,
                                      R6COG27,
                                      R6AGEY_E,
                                      R6SMOKEN,
                                      R7COG27,
                                      R7AGEY_E,
                                      R7SMOKEN,
                                      R8COG27,
                                      R8AGEY_E,
                                      R8SMOKEN,
                                      R9COG27,
                                      R9AGEY_E,
                                      R9SMOKEN,
                                      R10COG27,
                                      R10AGEY_E,
                                      R10SMOKEN,
                                      R11COG27,
                                      R11AGEY_E,
                                      R11SMOKEN,
                                      R12COG27,
                                      R12AGEY_E,
                                      R12SMOKEN,
                                      R13COG27,
                                      R13AGEY_E,
                                      R13SMOKEN,
                                      R14COG27,
                                      R14AGEY_E,
                                      R14SMOKEN,
                                      R15COG27,
                                      R15AGEY_E,
                                      R15SMOKEN,
                                      R3WTRESP,
                                      R4WTRESP,
                                      R5WTRESP,
                                      R6WTRESP,
                                      R7WTRESP,
                                      R8WTRESP,
                                      R9WTRESP,
                                      R10WTRESP,
                                      R11WTRESP,
                                      R12WTRESP,
                                      R13WTRESP,
                                      R14WTRESP,
                                      R15WTRESP,
                                      R3CESD,
                                      R4CESD,
                                      R5CESD,
                                      R6CESD,
                                      R7CESD,
                                      R8CESD,
                                      R9CESD,
                                      R10CESD,
                                      R11CESD,
                                      R12CESD,
                                      R13CESD,
                                      R14CESD,
                                      R15CESD,
                                      RAHISPAN,
                                      RAEDEGRM,
                                      R3DIABE,
                                      R4DIABE,
                                      R5DIABE,
                                      R6DIABE,
                                      R7DIABE,
                                      R8DIABE,
                                      R9DIABE,
                                      R10DIABE,
                                      R11DIABE,
                                      R12DIABE,
                                      R13DIABE,
                                      R14DIABE,
                                      R15DIABE,
                                      R3BMI,
                                      R4BMI,
                                      R5BMI,
                                      R6BMI,
                                      R7BMI,
                                      R8BMI,
                                      R9BMI,
                                      R10BMI,
                                      R11BMI,
                                      R12BMI,
                                      R13BMI,
                                      R14BMI,
                                      R15BMI,
                                      RAESTRAT,
                                      RAEHSAMP))

library(dplyr)

all_cog_var <- all_cog_var %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))

all_cog_var_pivot <- data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R3COG27, Age = all_cog_var$R3AGEY_E, Smoke = all_cog_var$R3SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R3BMI)
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R4COG27, Age = all_cog_var$R4AGEY_E, Smoke = all_cog_var$R4SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R4CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R4DIABE, BMI = all_cog_var$R4BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R5COG27, Age = all_cog_var$R5AGEY_E, Smoke = all_cog_var$R5SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R5CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R5DIABE, BMI = all_cog_var$R5BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R6COG27, Age = all_cog_var$R6AGEY_E, Smoke = all_cog_var$R6SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R6CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R6DIABE, BMI = all_cog_var$R6BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R7COG27, Age = all_cog_var$R7AGEY_E, Smoke = all_cog_var$R7SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R7CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R7DIABE, BMI = all_cog_var$R7BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R8COG27, Age = all_cog_var$R8AGEY_E, Smoke = all_cog_var$R8SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R8CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R8DIABE, BMI = all_cog_var$R8BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R9COG27, Age = all_cog_var$R9AGEY_E, Smoke = all_cog_var$R9SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R9CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R9DIABE, BMI = all_cog_var$R9BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R10COG27, Age = all_cog_var$R10AGEY_E, Smoke = all_cog_var$R10SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R10CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R10DIABE, BMI = all_cog_var$R10BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R11COG27, Age = all_cog_var$R11AGEY_E, Smoke = all_cog_var$R11SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R11CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R11DIABE, BMI = all_cog_var$R11BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R12COG27, Age = all_cog_var$R12AGEY_E, Smoke = all_cog_var$R12SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R12CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R12DIABE, BMI = all_cog_var$R12BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R13COG27, Age = all_cog_var$R13AGEY_E, Smoke = all_cog_var$R13SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R13CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R13DIABE, BMI = all_cog_var$R13BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R14COG27, Age = all_cog_var$R14AGEY_E, Smoke = all_cog_var$R14SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R14CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R14DIABE, BMI = all_cog_var$R14BMI))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R15COG27, Age = all_cog_var$R15AGEY_E, Smoke = all_cog_var$R15SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R15CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R15DIABE, BMI = all_cog_var$R15BMI))
colSums(is.na(all_cog_var_pivot))
all_cog_var_pivot <- na.omit(all_cog_var_pivot)
all_cog_var_pivot <- all_cog_var_pivot[all_cog_var_pivot$Weight > 0, ]
all_cog_var_pivot <- all_cog_var_pivot[!is.na(all_cog_var_pivot$Score), ]

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Depression = case_when(
    Depressed > 2 ~ 1,
    TRUE ~ 0
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Education_Level = case_when(
    Education == 0 ~ 0,
    Education < 4 ~ 1,
    Education < 6 ~ 2,
    Education < 8 ~ 3,
    TRUE ~ 5
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Age_group = case_when(
    Age < 65 ~ "50-65",
    Age < 80 ~ "65-80",
    TRUE ~ "80+"
  ))

all_cog_var_pivot <- all_cog_var_pivot[all_cog_var_pivot$Age >= 50, ]
all_cog_var_pivot <- all_cog_var_pivot[all_cog_var_pivot$Education_Level!=5, ]
all_cog_var_pivot <- na.omit(all_cog_var_pivot)
library(ggplot2)
ggplot(all_cog_var_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Education_Level)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#DABFFF", "907AD6", "4F518C", "2C2A4A"),
                    name = "Education Level",
                    labels = c("No Degree", "High School or GED", "Associates or Bachelors", "Masters or Doctorate"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Education Level",
       x = "Age Group",
       y = "Cognition Score")

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Age_group2 = case_when(
      Age < 55 ~ "50-54",
      Age < 60 ~ "55-59",
      Age < 65 ~ "60-64",
      Age < 70 ~ "65-69",
      Age < 75 ~ "70-74",
      Age < 80 ~ "75-79",
      Age < 85 ~ "80-84",
      Age < 90 ~ "85-89",
      Age < 95 ~ "90-94",
      TRUE ~ "95+"
  ))

library(ggplot2)
ggplot(all_cog_var_pivot, aes(x = Age_group2, y = Score, weight = Weight, fill = factor(diabetes)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Has Diabetes",
                    labels = c("No", "Yes"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Diabetic vs Non Diabetic",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

ggplot(all_cog_var_pivot, aes(x = BMI, y = Score, color = factor(Age_group2), size = Weight))+
  geom_smooth(
    method = "lm",
    mapping = aes(weight = Weight),
    size = 1.5
  )+
  labs(
    title = "Weighted Regression Lines on Cognitions Scores By BMI\n Across Age Groups",
    x = "BMI",
    y = "Cognition Score",
    color = "Age Group"
  )

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(BMI_category = case_when(
    BMI < 18.5 ~ 0,
    BMI < 24.9 ~ 1,
    BMI < 29.9 ~ 2,
    BMI < 34.9 ~ 3,
    TRUE ~ 4
  ))

ggplot(all_cog_var_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(BMI_category)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#B2AC88", "#DCAE96", "#FDF5E6", "#5F6F79", "#A69076"),
                    name = "BMI Category",
                    labels = c("Underweight", "Healthy Weight", "Overweight", "Moderate Obesity", "Severe Obesity"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn BMI Category",
       x = "Age Group",
       y = "Cognition Score")

library(lme4)
fm1 <- lmer(data = all_cog_var_pivot, Score ~ Age + Smoke + Hispanic + Depression + factor(Education_Level) + (1 | ID), weights = Weight)
summary(fm1)
plot(fm1)

boxplot(HRS$R12DRINKN)

unique(HRS$R8LBLONELY3)

unique(round(na.omit(HRS$R8LBLONELY3)))

length(unique(HRS$RAESTRAT))


fm2 <- lmer(data = all_cog_var_pivot, Score ~ Age + Smoke + Hispanic + Depression + factor(Education_Level) + (1 | ID) + (0 + Age | ID), weights = Weight)
summary(fm2)


all_cog_var_pivot$Age.Minus.55 <- all_cog_var_pivot$Age - 55
fm1 <- lmer(data = all_cog_var_pivot, Score ~ Age.Minus.55 + Smoke + Hispanic + Depression + factor(Education_Level) + (1 | ID), weights = Weight)
summary(fm1)

fm2 <- lmer(data = all_cog_var_pivot, Score ~ Age.Minus.55 + Smoke + Hispanic + Depression + factor(Education_Level) + (1 | ID) + (0 + Age | ID), weights = Weight)
summary(fm2)

fm3 <- lmer(data = all_cog_var_pivot, Score ~ Age.Minus.55 + Smoke + Hispanic + Depression + factor(Education_Level) + diabetes + (1 + Age | ID), weights = Weight)
summary(fm3)
library(lmerTest)
fm3test <- as_lmerModLmerTest(fm3)
anova(fm3test)
ranova(fm3test)
step(fm3test)
plot(fm3)


#Feb 9th
all_cog_var_pivot_2 <- data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R8COG27, Age = all_cog_var$R8AGEY_E, Smoke = all_cog_var$R8SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R8CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R8DIABE, BMI = all_cog_var$R8BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP)
all_cog_var_pivot_2 <- rbind(all_cog_var_pivot_2,data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R9COG27, Age = all_cog_var$R9AGEY_E, Smoke = all_cog_var$R9SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R9CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R9DIABE, BMI = all_cog_var$R9BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP))
all_cog_var_pivot_2 <- rbind(all_cog_var_pivot_2,data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R10COG27, Age = all_cog_var$R10AGEY_E, Smoke = all_cog_var$R10SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R10CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R10DIABE, BMI = all_cog_var$R10BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP))
all_cog_var_pivot_2 <- rbind(all_cog_var_pivot_2,data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R11COG27, Age = all_cog_var$R11AGEY_E, Smoke = all_cog_var$R11SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R11CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R11DIABE, BMI = all_cog_var$R11BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP))
all_cog_var_pivot_2 <- rbind(all_cog_var_pivot_2,data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R12COG27, Age = all_cog_var$R12AGEY_E, Smoke = all_cog_var$R12SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R12CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R12DIABE, BMI = all_cog_var$R12BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP))
all_cog_var_pivot_2 <- rbind(all_cog_var_pivot_2,data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R13COG27, Age = all_cog_var$R13AGEY_E, Smoke = all_cog_var$R13SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R13CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R13DIABE, BMI = all_cog_var$R13BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP))
all_cog_var_pivot_2 <- rbind(all_cog_var_pivot_2,data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R14COG27, Age = all_cog_var$R14AGEY_E, Smoke = all_cog_var$R14SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R14CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R14DIABE, BMI = all_cog_var$R14BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP))
all_cog_var_pivot_2 <- rbind(all_cog_var_pivot_2,data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R15COG27, Age = all_cog_var$R15AGEY_E, Smoke = all_cog_var$R15SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R15CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R15DIABE, BMI = all_cog_var$R15BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP))
all_cog_var_pivot_2 <- all_cog_var_pivot_2[all_cog_var_pivot_2$Weight > 0, ]
all_cog_var_pivot_2 <- all_cog_var_pivot_2[!is.na(all_cog_var_pivot_2$Score), ]
library(svylme)
library(Matrix)
library(survey)
dclus <- svydesign(id=~1,
                   strata = ~Stratum, data = all_cog_var_pivot_2,
                   weights = ~Weight)
#wont work
#still too big
#svy2lme(Score ~ Age+ Smoke + (1 | Stratum), design = dclus)
#summary(weights(dclus))


library(WeMix)
all_cog_var_pivot_2$Weight2 <- 1

set.seed(123)

all_cog_var_pivot_2 <- all_cog_var_pivot_2 %>%
  mutate(Depression = case_when(
    Depressed > 2 ~ 1,
    TRUE ~ 0
  ))

all_cog_var_pivot_2 <- all_cog_var_pivot_2 %>%
  mutate(Education_Level = case_when(
    Education == 0 ~ 0,
    Education < 4 ~ 1,
    Education < 6 ~ 2,
    Education < 8 ~ 3,
    TRUE ~ 5
  ))


fm1 <- mix(Score ~ Age + Smoke + Hispanic + Depressed + factor(Education_Level) + (1|ID), data = all_cog_var_pivot_2, weights = c("Weight", "Weight2"))

library(dplyr)
all_cog_var_exercise <- subset(HRS, select = c(HHIDPN,
                                               R7VGACTX,
                                               R8VGACTX,
                                               R9VGACTX,
                                               R10VGACTX,
                                               R11VGACTX,
                                               R12VGACTX,
                                               R13VGACTX,
                                               R14VGACTX,
                                               R15VGACTX,
                                               R7MDACTX,
                                               R8MDACTX,
                                               R9MDACTX,
                                               R10MDACTX,
                                               R11MDACTX,
                                               R12MDACTX,
                                               R13MDACTX,
                                               R14MDACTX,
                                               R15MDACTX,
                                               R7LTACTX,
                                               R8LTACTX,
                                               R9LTACTX,
                                               R10LTACTX,
                                               R11LTACTX,
                                               R12LTACTX,
                                               R13LTACTX,
                                               R14LTACTX,
                                               R15LTACTX,
                                               R7WTRESP,
                                               R8WTRESP,
                                               R9WTRESP,
                                               R10WTRESP,
                                               R11WTRESP,
                                               R12WTRESP,
                                               R13WTRESP,
                                               R14WTRESP,
                                               R15WTRESP,
                                               R7COG27,
                                               R8COG27,
                                               R9COG27,
                                               R10COG27,
                                               R11COG27,
                                               R12COG27,
                                               R13COG27,
                                               R14COG27,
                                               R15COG27,
                                               R7AGEY_E,
                                               R8AGEY_E,
                                               R9AGEY_E,
                                               R10AGEY_E,
                                               R11AGEY_E,
                                               R12AGEY_E,
                                               R13AGEY_E,
                                               R14AGEY_E,
                                               R15AGEY_E))
all_cog_var_exercise <- all_cog_var_exercise %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    TRUE ~ 0
  ))

all_cog_var_exercise_pivot <- data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R7COG27 , Age = all_cog_var_exercise$R7AGEY_E, Vigorous = all_cog_var_exercise$R7VGACTX, Moderate = all_cog_var_exercise$R7MDACTX, Light = all_cog_var_exercise$R7LTACTX)
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R8COG27 , Age = all_cog_var_exercise$R8AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R8VGACTX, Moderate = all_cog_var_exercise$R8MDACTX, Light = all_cog_var_exercise$R8LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R9COG27 , Age = all_cog_var_exercise$R9AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R9VGACTX, Moderate = all_cog_var_exercise$R9MDACTX, Light = all_cog_var_exercise$R9LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R10COG27 , Age = all_cog_var_exercise$R10AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R10VGACTX, Moderate = all_cog_var_exercise$R10MDACTX, Light = all_cog_var_exercise$R10LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R11COG27 , Age = all_cog_var_exercise$R11AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R11VGACTX, Moderate = all_cog_var_exercise$R11MDACTX, Light = all_cog_var_exercise$R11LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R12COG27 , Age = all_cog_var_exercise$R12AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R12VGACTX, Moderate = all_cog_var_exercise$R12MDACTX, Light = all_cog_var_exercise$R12LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R13COG27 , Age = all_cog_var_exercise$R13AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R13VGACTX, Moderate = all_cog_var_exercise$R13MDACTX, Light = all_cog_var_exercise$R13LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R14COG27 , Age = all_cog_var_exercise$R14AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R14VGACTX, Moderate = all_cog_var_exercise$R14MDACTX, Light = all_cog_var_exercise$R14LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R15COG27 , Age = all_cog_var_exercise$R15AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R15VGACTX, Moderate = all_cog_var_exercise$R15MDACTX, Light = all_cog_var_exercise$R15LTACTX))
library(ggplot2)
all_cog_var_exercise_pivot<- na.omit(all_cog_var_exercise_pivot)
all_cog_var_exercise_pivot <- all_cog_var_exercise_pivot[!is.na(all_cog_var_exercise_pivot$Weight), ]
ggplot(all_cog_var_exercise_pivot, aes(x = factor(Vigorous), y = Score, weight = Weight))+
  geom_boxplot()
ggplot(all_cog_var_exercise_pivot, aes(x = factor(Moderate), y = Score, weight = Weight))+
  geom_boxplot()
ggplot(all_cog_var_exercise_pivot, aes(x = factor(Light), y = Score, weight = Weight))+
  geom_boxplot()

all_cog_var_exercise_pivot <- all_cog_var_exercise_pivot %>%
  mutate(Age_group = case_when(
    Age < 65 ~ "50-65",
    Age < 80 ~ "65-80",
    TRUE ~ "80+"
  ))

ggplot(all_cog_var_exercise_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Light)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"),
                    name = "Frequency of Light Physical Activity\n(finer scale)",
                    labels = c("Every Day", ">1 Per Week", "1 per week", "1-3 per month", "Never"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Frequency of Light Physical Activity",
       x = "Age Group",
       y = "Cognition Score")


all_cog_var_industry <- subset(HRS, select = c(HHIDPN,
                                               R3WTRESP,
                                               R4WTRESP,
                                               R5WTRESP,
                                               R6WTRESP,
                                               R7WTRESP,
                                               R8WTRESP,
                                               R9WTRESP,
                                               R10WTRESP,
                                               R11WTRESP,
                                               R12WTRESP,
                                               R13WTRESP,
                                               R14WTRESP,
                                               R15WTRESP,
                                               R3COG27,
                                               R4COG27,
                                               R5COG27,
                                               R6COG27,
                                               R7COG27,
                                               R8COG27,
                                               R9COG27,
                                               R10COG27,
                                               R11COG27,
                                               R12COG27,
                                               R13COG27,
                                               R14COG27,
                                               R15COG27,
                                               R3AGEY_E,
                                               R4AGEY_E,
                                               R5AGEY_E,
                                               R6AGEY_E,
                                               R7AGEY_E,
                                               R8AGEY_E,
                                               R9AGEY_E,
                                               R10AGEY_E,
                                               R11AGEY_E,
                                               R12AGEY_E,
                                               R13AGEY_E,
                                               R14AGEY_E,
                                               R15AGEY_E,
                                               R3JLIND,
                                               R4JLIND,
                                               R5JLIND,
                                               R6JLIND,
                                               R7JLIND,
                                               R8JLIND,
                                               R9JLIND,
                                               R10JLIND,
                                               R11JLIND,
                                               R12JLIND,
                                               R13JLIND,
                                               R14JLIND,
                                               R15JLIND))
all_cog_var_industry <- all_cog_var_industry %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))

all_cog_var_industry_pivot <- data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                         Score = all_cog_var_industry$R3COG27, Industry = all_cog_var_industry$R3JLIND)
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R4COG27, Industry = all_cog_var_industry$R4JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R5COG27, Industry = all_cog_var_industry$R5JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R6COG27, Industry = all_cog_var_industry$R6JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R7COG27, Industry = all_cog_var_industry$R7JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R8COG27, Industry = all_cog_var_industry$R8JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R9COG27, Industry = all_cog_var_industry$R9JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R10COG27, Industry = all_cog_var_industry$R10JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R11COG27, Industry = all_cog_var_industry$R11JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R12COG27, Industry = all_cog_var_industry$R12JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R13COG27, Industry = all_cog_var_industry$R13JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R14COG27, Industry = all_cog_var_industry$R14JLIND))
all_cog_var_industry_pivot <- rbind(all_cog_var_industry_pivot, data.frame(ID = all_cog_var_industry$HHIDPN, Weight = all_cog_var_industry$weight, 
                                                                           Score = all_cog_var_industry$R15COG27, Industry = all_cog_var_industry$R15JLIND))

all_cog_var_industry_pivot<- na.omit(all_cog_var_industry_pivot)
all_cog_var_industry_pivot <- all_cog_var_industry_pivot[!is.na(all_cog_var_industry_pivot$Weight), ]
ggplot(all_cog_var_industry_pivot, aes(x = factor(Industry), y = Score, weight = Weight))+
  geom_boxplot()+
  labs(title = "Box plot of Cognition Scores Based off of Industry")


all_cog_var_lonely <- subset(HRS, select = c(HHIDPN,
                                             R9WTRESP,
                                             R10WTRESP,
                                             R11WTRESP,
                                             R12WTRESP,
                                             R13WTRESP,
                                             R14WTRESP,
                                             R15WTRESP,
                                             R9COG27,
                                             R10COG27,
                                             R11COG27,
                                             R12COG27,
                                             R13COG27,
                                             R14COG27,
                                             R15COG27,
                                             R9AGEY_E,
                                             R10AGEY_E,
                                             R11AGEY_E,
                                             R12AGEY_E,
                                             R13AGEY_E,
                                             R14AGEY_E,
                                             R15AGEY_E,
                                             R9LBLONELY11,
                                             R10LBLONELY11,
                                             R11LBLONELY11,
                                             R12LBLONELY11,
                                             R13LBLONELY11,
                                             R14LBLONELY11,
                                             R15LBLONELY11))

all_cog_var_lonely <- all_cog_var_lonely %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    TRUE ~ 0
  ))

all_cog_var_lonely_pivot <- data.frame(ID = all_cog_var_lonely$HHIDPN, Weight = all_cog_var_lonely$weight, Score = all_cog_var_lonely$R9COG27, Age = all_cog_var_lonely$R9AGEY_E,Lonely = all_cog_var_lonely$R9LBLONELY11)
all_cog_var_lonely_pivot <- rbind(all_cog_var_lonely_pivot, data.frame(ID = all_cog_var_lonely$HHIDPN, Weight = all_cog_var_lonely$weight, 
                                                                       Score = all_cog_var_lonely$R10COG27, Age = all_cog_var_lonely$R10AGEY_E,Lonely = all_cog_var_lonely$R10LBLONELY11))
all_cog_var_lonely_pivot <- rbind(all_cog_var_lonely_pivot, data.frame(ID = all_cog_var_lonely$HHIDPN, Weight = all_cog_var_lonely$weight, 
                                                                       Score = all_cog_var_lonely$R11COG27, Age = all_cog_var_lonely$R11AGEY_E,Lonely = all_cog_var_lonely$R11LBLONELY11))
all_cog_var_lonely_pivot <- rbind(all_cog_var_lonely_pivot, data.frame(ID = all_cog_var_lonely$HHIDPN, Weight = all_cog_var_lonely$weight, 
                                                                       Score = all_cog_var_lonely$R12COG27, Age = all_cog_var_lonely$R12AGEY_E,Lonely = all_cog_var_lonely$R12LBLONELY11))
all_cog_var_lonely_pivot <- rbind(all_cog_var_lonely_pivot, data.frame(ID = all_cog_var_lonely$HHIDPN, Weight = all_cog_var_lonely$weight, 
                                                                       Score = all_cog_var_lonely$R13COG27, Age = all_cog_var_lonely$R13AGEY_E,Lonely = all_cog_var_lonely$R13LBLONELY11))
all_cog_var_lonely_pivot <- rbind(all_cog_var_lonely_pivot, data.frame(ID = all_cog_var_lonely$HHIDPN, Weight = all_cog_var_lonely$weight, 
                                                                       Score = all_cog_var_lonely$R14COG27, Age = all_cog_var_lonely$R14AGEY_E,Lonely = all_cog_var_lonely$R14LBLONELY11))
all_cog_var_lonely_pivot <- rbind(all_cog_var_lonely_pivot, data.frame(ID = all_cog_var_lonely$HHIDPN, Weight = all_cog_var_lonely$weight, 
                                                                       Score = all_cog_var_lonely$R15COG27, Age = all_cog_var_lonely$R15AGEY_E,Lonely = all_cog_var_lonely$R15LBLONELY11))

all_cog_var_lonely_pivot<- na.omit(all_cog_var_lonely_pivot)
all_cog_var_lonely_pivot <- all_cog_var_lonely_pivot[!is.na(all_cog_var_lonely_pivot$Weight), ]

all_cog_var_lonely_pivot <- all_cog_var_lonely_pivot %>%
  mutate(Lonely_Category = case_when(
    Lonely < 1.5 ~ 1,
    Lonely < 2 ~ 2,
    Lonely < 2.5 ~ 3,
    TRUE ~ 4
  ))

all_cog_var_lonely_pivot <- all_cog_var_lonely_pivot %>%
  mutate(Age_group = case_when(
    Age < 65 ~ "50-65",
    Age < 80 ~ "65-80",
    TRUE ~ "80+"
  ))

ggplot(all_cog_var_lonely_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Lonely_Category)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1", "#054a91"),
                    name = "Score on 11 Question  Loneliness Scale",
                    labels = c("1-1.5 Points", "1.5-2 Points", "2-2.5 Points", "2.5-3 Points"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Frequency of Loneliness",
       x = "Age Group",
       y = "Cognition Score")


all_cog_var_stress <- subset(HRS, select = c(HHIDPN,
                                             R8WTRESP,
                                             R10WTRESP,
                                             R11WTRESP,
                                             R12WTRESP,
                                             R13WTRESP,
                                             R14WTRESP,
                                             R15WTRESP,
                                             R8COG27,
                                             R10COG27,
                                             R11COG27,
                                             R12COG27,
                                             R13COG27,
                                             R14COG27,
                                             R15COG27,
                                             R8AGEY_E,
                                             R10AGEY_E,
                                             R11AGEY_E,
                                             R12AGEY_E,
                                             R13AGEY_E,
                                             R14AGEY_E,
                                             R15AGEY_E,
                                             R8LBONCHRSTR,
                                             R10LBONCHRSTR,
                                             R11LBONCHRSTR,
                                             R12LBONCHRSTR,
                                             R13LBONCHRSTR,
                                             R14LBONCHRSTR,
                                             R15LBONCHRSTR))
all_cog_var_stress <- all_cog_var_stress %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    TRUE ~ 0
  ))

all_cog_var_stress_pivot <- data.frame(ID = all_cog_var_stress$HHIDPN, Weight = all_cog_var_stress$weight, Score = all_cog_var_stress$R8COG27, Age = all_cog_var_stress$R8AGEY_E, Stress = all_cog_var_stress$R8LBONCHRSTR)
all_cog_var_stress_pivot <- rbind(all_cog_var_stress_pivot, data.frame(ID = all_cog_var_stress$HHIDPN, Weight = all_cog_var_stress$weight,
                                                                       Score = all_cog_var_stress$R10COG27, Age = all_cog_var_stress$R10AGEY_E, Stress = all_cog_var_stress$R10LBONCHRSTR))
all_cog_var_stress_pivot <- rbind(all_cog_var_stress_pivot, data.frame(ID = all_cog_var_stress$HHIDPN, Weight = all_cog_var_stress$weight,
                                                                       Score = all_cog_var_stress$R11COG27, Age = all_cog_var_stress$R11AGEY_E, Stress = all_cog_var_stress$R11LBONCHRSTR))
all_cog_var_stress_pivot <- rbind(all_cog_var_stress_pivot, data.frame(ID = all_cog_var_stress$HHIDPN, Weight = all_cog_var_stress$weight,
                                                                       Score = all_cog_var_stress$R12COG27, Age = all_cog_var_stress$R12AGEY_E, Stress = all_cog_var_stress$R12LBONCHRSTR))
all_cog_var_stress_pivot <- rbind(all_cog_var_stress_pivot, data.frame(ID = all_cog_var_stress$HHIDPN, Weight = all_cog_var_stress$weight,
                                                                       Score = all_cog_var_stress$R13COG27, Age = all_cog_var_stress$R13AGEY_E, Stress = all_cog_var_stress$R13LBONCHRSTR))
all_cog_var_stress_pivot <- rbind(all_cog_var_stress_pivot, data.frame(ID = all_cog_var_stress$HHIDPN, Weight = all_cog_var_stress$weight,
                                                                       Score = all_cog_var_stress$R14COG27, Age = all_cog_var_stress$R14AGEY_E, Stress = all_cog_var_stress$R14LBONCHRSTR))
all_cog_var_stress_pivot <- rbind(all_cog_var_stress_pivot, data.frame(ID = all_cog_var_stress$HHIDPN, Weight = all_cog_var_stress$weight,
                                                                       Score = all_cog_var_stress$R15COG27, Age = all_cog_var_stress$R15AGEY_E, Stress = all_cog_var_stress$R15LBONCHRSTR))

all_cog_var_stress_pivot <- all_cog_var_stress_pivot %>%
  mutate(Age_group = case_when(
    Age < 65 ~ "50-65",
    Age < 80 ~ "65-80",
    TRUE ~ "80+"
  ))

all_cog_var_stress_pivot <- all_cog_var_stress_pivot %>%
  mutate(Num_Stress = case_when(
    Stress < 1 ~ 0,
    Stress < 2 ~ 1,
    TRUE ~ 2
  ))

all_cog_var_stress_pivot<- na.omit(all_cog_var_stress_pivot)
all_cog_var_stress_pivot <- all_cog_var_stress_pivot[!is.na(all_cog_var_stress_pivot$Weight), ]

ggplot(all_cog_var_stress_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Num_Stress)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1"),
                    name = "Number of Chronic Stressers in Life",
                    labels = c("0 Stressers", "1 Stressers", "2 or More Stressers"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Number of Chronic Stressers",
       x = "Age Group",
       y = "Cognition Score")


all_cog_sleep <- subset(HRS, select = c(HHIDPN,
                                        R6WTRESP,
                                        R7WTRESP,
                                        R8WTRESP,
                                        R9WTRESP,
                                        R10WTRESP,
                                        R11WTRESP,
                                        R12WTRESP,
                                        R13WTRESP,
                                        R14WTRESP,
                                        R15WTRESP,
                                        R6COG27,
                                        R7COG27,
                                        R8COG27,
                                        R9COG27,
                                        R10COG27,
                                        R11COG27,
                                        R12COG27,
                                        R13COG27,
                                        R14COG27,
                                        R15COG27,
                                        R6AGEY_E,
                                        R7AGEY_E,
                                        R8AGEY_E,
                                        R9AGEY_E,
                                        R10AGEY_E,
                                        R11AGEY_E,
                                        R12AGEY_E,
                                        R13AGEY_E,
                                        R14AGEY_E,
                                        R15AGEY_E,
                                        R6SLEEPFAL,
                                        R7SLEEPFAL,
                                        R8SLEEPFAL,
                                        R9SLEEPFAL,
                                        R10SLEEPFAL,
                                        R11SLEEPFAL,
                                        R12SLEEPFAL,
                                        R13SLEEPFAL,
                                        R14SLEEPFAL,
                                        R15SLEEPFAL,
                                        R6SLEEPWKN,
                                        R7SLEEPWKN,
                                        R8SLEEPWKN,
                                        R9SLEEPWKN,
                                        R10SLEEPWKN,
                                        R11SLEEPWKN,
                                        R12SLEEPWKN,
                                        R13SLEEPWKN,
                                        R14SLEEPWKN,
                                        R15SLEEPWKN,
                                        R6SLEEPWKE,
                                        R7SLEEPWKE,
                                        R8SLEEPWKE,
                                        R9SLEEPWKE,
                                        R10SLEEPWKE,
                                        R11SLEEPWKE,
                                        R12SLEEPWKE,
                                        R13SLEEPWKE,
                                        R14SLEEPWKE,
                                        R15SLEEPWKE))
all_cog_sleep <- all_cog_sleep %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    TRUE ~ 0
  ))

all_cog_sleep_pivot <- data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, Score = all_cog_sleep$R6COG27, Age = all_cog_sleep$R6AGEY_E, fall = all_cog_sleep$R6SLEEPFAL, wakenight = all_cog_sleep$R6SLEEPWKN, early = all_cog_sleep$R6SLEEPWKE)
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R7COG27, Age = all_cog_sleep$R7AGEY_E, fall = all_cog_sleep$R7SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R7SLEEPWKN, early = all_cog_sleep$R7SLEEPWKE))
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R8COG27, Age = all_cog_sleep$R8AGEY_E, fall = all_cog_sleep$R8SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R8SLEEPWKN, early = all_cog_sleep$R8SLEEPWKE))
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R9COG27, Age = all_cog_sleep$R9AGEY_E, fall = all_cog_sleep$R9SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R9SLEEPWKN, early = all_cog_sleep$R9SLEEPWKE))
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R10COG27, Age = all_cog_sleep$R10AGEY_E, fall = all_cog_sleep$R10SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R10SLEEPWKN, early = all_cog_sleep$R10SLEEPWKE))
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R11COG27, Age = all_cog_sleep$R11AGEY_E, fall = all_cog_sleep$R11SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R11SLEEPWKN, early = all_cog_sleep$R11SLEEPWKE))
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R12COG27, Age = all_cog_sleep$R12AGEY_E, fall = all_cog_sleep$R12SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R12SLEEPWKN, early = all_cog_sleep$R12SLEEPWKE))
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R13COG27, Age = all_cog_sleep$R13AGEY_E, fall = all_cog_sleep$R13SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R13SLEEPWKN, early = all_cog_sleep$R13SLEEPWKE))
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R14COG27, Age = all_cog_sleep$R14AGEY_E, fall = all_cog_sleep$R14SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R14SLEEPWKN, early = all_cog_sleep$R14SLEEPWKE))
all_cog_sleep_pivot <- rbind(all_cog_sleep_pivot, data.frame(ID = all_cog_sleep$HHIDPN, Weight = all_cog_sleep$weight, 
                                                             Score = all_cog_sleep$R15COG27, Age = all_cog_sleep$R15AGEY_E, fall = all_cog_sleep$R15SLEEPFAL, 
                                                             wakenight = all_cog_sleep$R15SLEEPWKN, early = all_cog_sleep$R15SLEEPWKE))
all_cog_sleep_pivot<- na.omit(all_cog_sleep_pivot)
all_cog_sleep_pivot <- all_cog_sleep_pivot[!is.na(all_cog_sleep_pivot$Weight), ]

all_cog_sleep_pivot <- all_cog_sleep_pivot %>%
  mutate(Age_group = case_when(
    Age < 65 ~ "50-65",
    Age < 80 ~ "65-80",
    TRUE ~ "80+"
  ))

ggplot(all_cog_sleep_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(fall)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1"),
                    name = "Trouble Falling Asleep",
                    labels = c("Most of the Time", "Sometimes", "Rarely or Never"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Whether They Have Trouble Falling Asleep",
       x = "Age Group",
       y = "Cognition Score")

ggplot(all_cog_sleep_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(wakenight)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1"),
                    name = "Waking Up During Night",
                    labels = c("Most of the Time", "Sometimes", "Rarely or Never"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Whether They Wake Up During the Night",
       x = "Age Group",
       y = "Cognition Score")

ggplot(all_cog_sleep_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(early)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1"),
                    name = "Waking Up During Night",
                    labels = c("Most of the Time", "Sometimes", "Rarely or Never"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Whether They Wake Up Too Early",
       x = "Age Group",
       y = "Cognition Score")





#------------Feb 16th------------

library(lme4)
library(WeMix)

library(dplyr)
all_cog_var_exercise <- subset(HRS, select = c(HHIDPN,
                                               R7VGACTX,
                                               R8VGACTX,
                                               R9VGACTX,
                                               R10VGACTX,
                                               R11VGACTX,
                                               R12VGACTX,
                                               R13VGACTX,
                                               R14VGACTX,
                                               R15VGACTX,
                                               R7MDACTX,
                                               R8MDACTX,
                                               R9MDACTX,
                                               R10MDACTX,
                                               R11MDACTX,
                                               R12MDACTX,
                                               R13MDACTX,
                                               R14MDACTX,
                                               R15MDACTX,
                                               R7LTACTX,
                                               R8LTACTX,
                                               R9LTACTX,
                                               R10LTACTX,
                                               R11LTACTX,
                                               R12LTACTX,
                                               R13LTACTX,
                                               R14LTACTX,
                                               R15LTACTX,
                                               R7WTRESP,
                                               R8WTRESP,
                                               R9WTRESP,
                                               R10WTRESP,
                                               R11WTRESP,
                                               R12WTRESP,
                                               R13WTRESP,
                                               R14WTRESP,
                                               R15WTRESP,
                                               R7COG27,
                                               R8COG27,
                                               R9COG27,
                                               R10COG27,
                                               R11COG27,
                                               R12COG27,
                                               R13COG27,
                                               R14COG27,
                                               R15COG27,
                                               R7AGEY_E,
                                               R8AGEY_E,
                                               R9AGEY_E,
                                               R10AGEY_E,
                                               R11AGEY_E,
                                               R12AGEY_E,
                                               R13AGEY_E,
                                               R14AGEY_E,
                                               R15AGEY_E))
all_cog_var_exercise <- all_cog_var_exercise %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    TRUE ~ 0
  ))

all_cog_var_exercise_pivot <- data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R7COG27 , Age = all_cog_var_exercise$R7AGEY_E, Vigorous = all_cog_var_exercise$R7VGACTX, Moderate = all_cog_var_exercise$R7MDACTX, Light = all_cog_var_exercise$R7LTACTX)
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R8COG27 , Age = all_cog_var_exercise$R8AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R8VGACTX, Moderate = all_cog_var_exercise$R8MDACTX, Light = all_cog_var_exercise$R8LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R9COG27 , Age = all_cog_var_exercise$R9AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R9VGACTX, Moderate = all_cog_var_exercise$R9MDACTX, Light = all_cog_var_exercise$R9LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R10COG27 , Age = all_cog_var_exercise$R10AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R10VGACTX, Moderate = all_cog_var_exercise$R10MDACTX, Light = all_cog_var_exercise$R10LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R11COG27 , Age = all_cog_var_exercise$R11AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R11VGACTX, Moderate = all_cog_var_exercise$R11MDACTX, Light = all_cog_var_exercise$R11LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R12COG27 , Age = all_cog_var_exercise$R12AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R12VGACTX, Moderate = all_cog_var_exercise$R12MDACTX, Light = all_cog_var_exercise$R12LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R13COG27 , Age = all_cog_var_exercise$R13AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R13VGACTX, Moderate = all_cog_var_exercise$R13MDACTX, Light = all_cog_var_exercise$R13LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R14COG27 , Age = all_cog_var_exercise$R14AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R14VGACTX, Moderate = all_cog_var_exercise$R14MDACTX, Light = all_cog_var_exercise$R14LTACTX))
all_cog_var_exercise_pivot <- rbind(all_cog_var_exercise_pivot, data.frame(ID = all_cog_var_exercise$HHIDPN, Weight = all_cog_var_exercise$weight,Score = all_cog_var_exercise$R15COG27 , Age = all_cog_var_exercise$R15AGEY_E,
                                                                           Vigorous = all_cog_var_exercise$R15VGACTX, Moderate = all_cog_var_exercise$R15MDACTX, Light = all_cog_var_exercise$R15LTACTX))
all_cog_var_exercise_pivot <- all_cog_var_exercise_pivot[,c(1,2,3,4,7)]

all_cog_var_exercise_pivot<- na.omit(all_cog_var_exercise_pivot)
all_cog_var_exercise_pivot <- all_cog_var_exercise_pivot[!is.na(all_cog_var_exercise_pivot$Weight), ]

rm(HRS)
rm(all_cog_var_exercise)

#standardize weights

weight_sum <- sum(all_cog_var_exercise_pivot$Weight)
all_cog_var_exercise_pivot$Weight <- all_cog_var_exercise_pivot$Weight/weight_sum

rm(weight_sum)

all_cog_var_exercise_pivot$age.minus.55 <- all_cog_var_exercise_pivot$Age -55
all_cog_var_exercise_pivot <- all_cog_var_exercise_pivot[,c(1,2,3,5,6)]

all_cog_var_exercise_pivot <- all_cog_var_exercise_pivot %>%
  mutate(exercise_freq = case_when(
    Light >= 4 ~ 2,
    Light >= 2 ~ 1,
    TRUE ~ 0
  ))

all_cog_var_exercise_pivot <- all_cog_var_exercise_pivot[,c(1,2,3,5,6)]

all_cog_var_exercise_pivot$Weight2 <- 1
#mix(Score ~ age.minus.55 + exercise_freq + (1|ID), data = all_cog_var_exercise_pivot, weights = c("Weight", "Weight2"))
#Doesnt work

write.csv(all_cog_var_exercise_pivot,"~all_cog_var_exercise_pivot.csv", row.names = TRUE)

# -----------


library(dplyr)
all_cog_var <- subset(HRS, select = c(HHIDPN, R3COG27,
                                      R3AGEY_E,
                                      R3SMOKEN,
                                      R4COG27,
                                      R4AGEY_E,
                                      R4SMOKEN,
                                      R5COG27,
                                      R5AGEY_E,
                                      R5SMOKEN,
                                      R6COG27,
                                      R6AGEY_E,
                                      R6SMOKEN,
                                      R7COG27,
                                      R7AGEY_E,
                                      R7SMOKEN,
                                      R8COG27,
                                      R8AGEY_E,
                                      R8SMOKEN,
                                      R9COG27,
                                      R9AGEY_E,
                                      R9SMOKEN,
                                      R10COG27,
                                      R10AGEY_E,
                                      R10SMOKEN,
                                      R11COG27,
                                      R11AGEY_E,
                                      R11SMOKEN,
                                      R12COG27,
                                      R12AGEY_E,
                                      R12SMOKEN,
                                      R13COG27,
                                      R13AGEY_E,
                                      R13SMOKEN,
                                      R14COG27,
                                      R14AGEY_E,
                                      R14SMOKEN,
                                      R15COG27,
                                      R15AGEY_E,
                                      R15SMOKEN,
                                      R3WTRESP,
                                      R4WTRESP,
                                      R5WTRESP,
                                      R6WTRESP,
                                      R7WTRESP,
                                      R8WTRESP,
                                      R9WTRESP,
                                      R10WTRESP,
                                      R11WTRESP,
                                      R12WTRESP,
                                      R13WTRESP,
                                      R14WTRESP,
                                      R15WTRESP,
                                      R3CESD,
                                      R4CESD,
                                      R5CESD,
                                      R6CESD,
                                      R7CESD,
                                      R8CESD,
                                      R9CESD,
                                      R10CESD,
                                      R11CESD,
                                      R12CESD,
                                      R13CESD,
                                      R14CESD,
                                      R15CESD,
                                      RAHISPAN,
                                      RAEDEGRM,
                                      R3DIABE,
                                      R4DIABE,
                                      R5DIABE,
                                      R6DIABE,
                                      R7DIABE,
                                      R8DIABE,
                                      R9DIABE,
                                      R10DIABE,
                                      R11DIABE,
                                      R12DIABE,
                                      R13DIABE,
                                      R14DIABE,
                                      R15DIABE,
                                      R3BMI,
                                      R4BMI,
                                      R5BMI,
                                      R6BMI,
                                      R7BMI,
                                      R8BMI,
                                      R9BMI,
                                      R10BMI,
                                      R11BMI,
                                      R12BMI,
                                      R13BMI,
                                      R14BMI,
                                      R15BMI,
                                      RAESTRAT,
                                      RAEHSAMP,
                                      R7LTACTX,
                                      R8LTACTX,
                                      R9LTACTX,
                                      R10LTACTX,
                                      R11LTACTX,
                                      R12LTACTX,
                                      R13LTACTX,
                                      R14LTACTX,
                                      R15LTACTX,
                                      R7SLEEPWKE,
                                      R8SLEEPWKE,
                                      R9SLEEPWKE,
                                      R10SLEEPWKE,
                                      R11SLEEPWKE,
                                      R12SLEEPWKE,
                                      R13SLEEPWKE,
                                      R14SLEEPWKE,
                                      R15SLEEPWKE,
                                      R7SLEEPFAL,
                                      R8SLEEPFAL,
                                      R9SLEEPFAL,
                                      R10SLEEPFAL,
                                      R11SLEEPFAL,
                                      R12SLEEPFAL,
                                      R13SLEEPFAL,
                                      R14SLEEPFAL,
                                      R15SLEEPFAL,
                                      R3HIBPE,
                                      R4HIBPE,
                                      R5HIBPE,
                                      R6HIBPE,
                                      R7HIBPE,
                                      R8HIBPE,
                                      R9HIBPE,
                                      R10HIBPE,
                                      R11HIBPE,
                                      R12HIBPE,
                                      R13HIBPE,
                                      R14HIBPE,
                                      R15HIBPE,
                                      R3PSYCHE,
                                      R4PSYCHE,
                                      R5PSYCHE,
                                      R6PSYCHE,
                                      R7PSYCHE,
                                      R8PSYCHE,
                                      R9PSYCHE,
                                      R10PSYCHE,
                                      R11PSYCHE,
                                      R12PSYCHE,
                                      R13PSYCHE,
                                      R14PSYCHE,
                                      R15PSYCHE,
                                      R3DRINKN,
                                      R4DRINKN,
                                      R5DRINKN,
                                      R6DRINKN,
                                      R7DRINKN,
                                      R8DRINKN,
                                      R9DRINKN,
                                      R10DRINKN,
                                      R11DRINKN,
                                      R12DRINKN,
                                      R13DRINKN,
                                      R14DRINKN,
                                      R15DRINKN))

library(dplyr)
all_cog_var <- all_cog_var %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    TRUE ~ 0
  ))
#---Wave 7 and on---
all_cog_var_pivot <- data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R7COG27, Age = all_cog_var$R7AGEY_E, Smoke = all_cog_var$R7SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R7CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R7DIABE, BMI = all_cog_var$R7BMI, Exercise = all_cog_var$R7LTACTX, Early = all_cog_var$R7SLEEPWKE, fall = all_cog_var$R7SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R7HIBPE, Psych_Problems = all_cog_var$R7PSYCHE, Drinks_day = all_cog_var$R7DRINKN)
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R8COG27, Age = all_cog_var$R8AGEY_E, Smoke = all_cog_var$R8SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R8CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R8DIABE, BMI = all_cog_var$R8BMI, Exercise = all_cog_var$R8LTACTX, Early = all_cog_var$R8SLEEPWKE, fall = all_cog_var$R8SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R8HIBPE, Psych_Problems = all_cog_var$R8PSYCHE, Drinks_day = all_cog_var$R8DRINKN))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R9COG27, Age = all_cog_var$R9AGEY_E, Smoke = all_cog_var$R9SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R9CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R9DIABE, BMI = all_cog_var$R9BMI, Exercise = all_cog_var$R9LTACTX, Early = all_cog_var$R9SLEEPWKE, fall = all_cog_var$R9SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R9HIBPE, Psych_Problems = all_cog_var$R9PSYCHE, Drinks_day = all_cog_var$R9DRINKN))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R10COG27, Age = all_cog_var$R10AGEY_E, Smoke = all_cog_var$R10SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R10CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R10DIABE, BMI = all_cog_var$R10BMI, Exercise = all_cog_var$R10LTACTX, Early = all_cog_var$R10SLEEPWKE, fall = all_cog_var$R10SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R10HIBPE, Psych_Problems = all_cog_var$R10PSYCHE, Drinks_day = all_cog_var$R10DRINKN))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R11COG27, Age = all_cog_var$R11AGEY_E, Smoke = all_cog_var$R11SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R11CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R11DIABE, BMI = all_cog_var$R11BMI, Exercise = all_cog_var$R11LTACTX, Early = all_cog_var$R11SLEEPWKE, fall = all_cog_var$R11SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R11HIBPE, Psych_Problems = all_cog_var$R11PSYCHE, Drinks_day = all_cog_var$R11DRINKN))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R12COG27, Age = all_cog_var$R12AGEY_E, Smoke = all_cog_var$R12SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R12CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R12DIABE, BMI = all_cog_var$R12BMI, Exercise = all_cog_var$R12LTACTX, Early = all_cog_var$R12SLEEPWKE, fall = all_cog_var$R12SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R12HIBPE, Psych_Problems = all_cog_var$R12PSYCHE, Drinks_day = all_cog_var$R12DRINKN))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R13COG27, Age = all_cog_var$R13AGEY_E, Smoke = all_cog_var$R13SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R13CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R13DIABE, BMI = all_cog_var$R13BMI, Exercise = all_cog_var$R13LTACTX, Early = all_cog_var$R13SLEEPWKE, fall = all_cog_var$R13SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R13HIBPE, Psych_Problems = all_cog_var$R13PSYCHE, Drinks_day = all_cog_var$R13DRINKN))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R14COG27, Age = all_cog_var$R14AGEY_E, Smoke = all_cog_var$R14SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R14CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R14DIABE, BMI = all_cog_var$R14BMI, Exercise = all_cog_var$R14LTACTX, Early = all_cog_var$R14SLEEPWKE, fall = all_cog_var$R14SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R14HIBPE, Psych_Problems = all_cog_var$R14PSYCHE, Drinks_day = all_cog_var$R14DRINKN))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R15COG27, Age = all_cog_var$R15AGEY_E, Smoke = all_cog_var$R15SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R15CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R15DIABE, BMI = all_cog_var$R15BMI, Exercise = all_cog_var$R15LTACTX, Early = all_cog_var$R15SLEEPWKE, fall = all_cog_var$R15SLEEPFAL, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R15HIBPE, Psych_Problems = all_cog_var$R15PSYCHE, Drinks_day = all_cog_var$R15DRINKN))



all_cog_var_pivot$Age.minus.50 = all_cog_var_pivot$Age - 50
all_cog_var_pivot$Age.minus.55 <- NULL

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Is.depressed = case_when(
    Depressed < 3 ~ 0,
    TRUE ~ 1
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Education_Level = case_when(
    Education == 0 ~ 0,
    Education < 4 ~ 1,
    Education < 6 ~ 2,
    Education < 8 ~ 3,
    TRUE ~ 5
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(exercise_freq = case_when(
    Exercise >= 4 ~ 2,
    Exercise >= 2 ~ 1,
    TRUE ~ 0
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Trouble.early = case_when(
    Early > 1 ~ "0",
    TRUE ~ "1"
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Trouble.fall = case_when(
    fall > 1 ~ "0",
    TRUE ~ "1"
  ))
#weight_sum <- sum(all_cog_var_pivot$Weight)
#all_cog_var_pivot$Weight.standardized <- all_cog_var_pivot$Weight/weight_sum
#all_cog_var_pivot <- all_cog_var_pivot[,c(1,2,4,6,9,10,14,15,16,17,18,19,20,21,22)]
all_cog_var_pivot <- all_cog_var_pivot[all_cog_var_pivot$Education_Level != 5,]
all_cog_var_pivot<-na.omit(all_cog_var_pivot)
#write.csv(all_cog_var_pivot,"~all_cog_var_pivot.csv", row.names = TRUE)

library(ggplot2)

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Age_group2 = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))

ggplot(all_cog_var_pivot, aes(x = Age_group2, y = Score, weight = Weight, fill = factor(Blood_pressure)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Has Ever Had High Blood Pressure",
                    labels = c("No", "Yes"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Ever Having High Blood Pressure",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

ggplot(all_cog_var_pivot, aes(x = Age_group2, y = Score, weight = Weight, fill = factor(Psych_Problems)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Has Ever Had Psych Problems",
                    labels = c("No", "Yes"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Ever Having Psych Problems",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(heavy_drink= case_when(
    Drinks_day < 1 ~ 0,
    Drinks_day < 3 ~ 1,
    Drinks_day < 5 ~ 2,
    TRUE ~ 3
  ))

ggplot(all_cog_var_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(heavy_drink)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1", "#054a91"),
                    name = "How Many Drinks Do You Have When You Drink?",
                    labels = c("Doesn't Drink", "1-2 Drinks", "3-4 Drinks", "5+ Drinks"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Heavy Drinking",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

library(WeMix)
library(svylme)
library(survey)

#all_cog_var_pivot$OBS <- seq_len(nrow(all_cog_var_pivot))

#all_cog_var_pivot$SSU <- interaction(
  all_cog_var_pivot$Stratum,
  all_cog_var_pivot$Cluster,
  drop = TRUE
)


#dclus <- svydesign(
  #ids = ~Stratum+ID+OBS,
  #weights = ~Weight.standardized,
 # data = all_cog_var_pivot
#)
#rm(HRS)
#rm(all_cog_var)
#rm(weight_sum)
#fit <- svy2lme(
 # Score ~ Age.minus.55 +
  #  factor(Smoke) +
   # factor(exercise_freq) +
    #factor(Education_Level) +
  #  (1 | ID),
#  design = dclus,
#  method = "nested"
#)

all_cog_var_pivot$weight2 <- 1

all_cog_var_pivot_small <- all_cog_var_pivot[,c("Score", "Age.minus.55", "Smoke", "exercise_freq", "Education_Level", "ID", "Weight.standardized", "weight2")]

library(data.table)
data_small <- as.data.table(all_cog_var_pivot_small)
rm(dclus)
rm(all_cog_var)
rm(all_cog_var_pivot_small)
data_small$ID <- as.factor(data_small$ID )

fit1 <- mix(Score~Age.minus.55 + factor(Smoke) + factor(exercise_freq)+ factor(Education_Level)+(1|ID), 
            data = data_small,
            weights = c("Weight.standardized", "weight2"))


##--##
library(dplyr)
all_cog_var <- subset(HRS, select = c(HHIDPN, R3COG27,
                                      R3AGEY_E,
                                      R3SMOKEN,
                                      R4COG27,
                                      R4AGEY_E,
                                      R4SMOKEN,
                                      R5COG27,
                                      R5AGEY_E,
                                      R5SMOKEN,
                                      R6COG27,
                                      R6AGEY_E,
                                      R6SMOKEN,
                                      R7COG27,
                                      R7AGEY_E,
                                      R7SMOKEN,
                                      R8COG27,
                                      R8AGEY_E,
                                      R8SMOKEN,
                                      R9COG27,
                                      R9AGEY_E,
                                      R9SMOKEN,
                                      R10COG27,
                                      R10AGEY_E,
                                      R10SMOKEN,
                                      R11COG27,
                                      R11AGEY_E,
                                      R11SMOKEN,
                                      R12COG27,
                                      R12AGEY_E,
                                      R12SMOKEN,
                                      R13COG27,
                                      R13AGEY_E,
                                      R13SMOKEN,
                                      R14COG27,
                                      R14AGEY_E,
                                      R14SMOKEN,
                                      R15COG27,
                                      R15AGEY_E,
                                      R15SMOKEN,
                                      R3WTRESP,
                                      R4WTRESP,
                                      R5WTRESP,
                                      R6WTRESP,
                                      R7WTRESP,
                                      R8WTRESP,
                                      R9WTRESP,
                                      R10WTRESP,
                                      R11WTRESP,
                                      R12WTRESP,
                                      R13WTRESP,
                                      R14WTRESP,
                                      R15WTRESP,
                                      R3CESD,
                                      R4CESD,
                                      R5CESD,
                                      R6CESD,
                                      R7CESD,
                                      R8CESD,
                                      R9CESD,
                                      R10CESD,
                                      R11CESD,
                                      R12CESD,
                                      R13CESD,
                                      R14CESD,
                                      R15CESD,
                                      RAHISPAN,
                                      RAEDEGRM,
                                      R3DIABE,
                                      R4DIABE,
                                      R5DIABE,
                                      R6DIABE,
                                      R7DIABE,
                                      R8DIABE,
                                      R9DIABE,
                                      R10DIABE,
                                      R11DIABE,
                                      R12DIABE,
                                      R13DIABE,
                                      R14DIABE,
                                      R15DIABE,
                                      R3BMI,
                                      R4BMI,
                                      R5BMI,
                                      R6BMI,
                                      R7BMI,
                                      R8BMI,
                                      R9BMI,
                                      R10BMI,
                                      R11BMI,
                                      R12BMI,
                                      R13BMI,
                                      R14BMI,
                                      R15BMI,
                                      RAESTRAT,
                                      RAEHSAMP,
                                      R7LTACTX,
                                      R8LTACTX,
                                      R9LTACTX,
                                      R10LTACTX,
                                      R11LTACTX,
                                      R12LTACTX,
                                      R13LTACTX,
                                      R14LTACTX,
                                      R15LTACTX,
                                      R7SLEEPWKE,
                                      R8SLEEPWKE,
                                      R9SLEEPWKE,
                                      R10SLEEPWKE,
                                      R11SLEEPWKE,
                                      R12SLEEPWKE,
                                      R13SLEEPWKE,
                                      R14SLEEPWKE,
                                      R15SLEEPWKE,
                                      R7SLEEPFAL,
                                      R8SLEEPFAL,
                                      R9SLEEPFAL,
                                      R10SLEEPFAL,
                                      R11SLEEPFAL,
                                      R12SLEEPFAL,
                                      R13SLEEPFAL,
                                      R14SLEEPFAL,
                                      R15SLEEPFAL,
                                      R3HIBPE,
                                      R4HIBPE,
                                      R5HIBPE,
                                      R6HIBPE,
                                      R7HIBPE,
                                      R8HIBPE,
                                      R9HIBPE,
                                      R10HIBPE,
                                      R11HIBPE,
                                      R12HIBPE,
                                      R13HIBPE,
                                      R14HIBPE,
                                      R15HIBPE,
                                      R3PSYCHE,
                                      R4PSYCHE,
                                      R5PSYCHE,
                                      R6PSYCHE,
                                      R7PSYCHE,
                                      R8PSYCHE,
                                      R9PSYCHE,
                                      R10PSYCHE,
                                      R11PSYCHE,
                                      R12PSYCHE,
                                      R13PSYCHE,
                                      R14PSYCHE,
                                      R15PSYCHE,
                                      R3DRINKN,
                                      R4DRINKN,
                                      R5DRINKN,
                                      R6DRINKN,
                                      R7DRINKN,
                                      R8DRINKN,
                                      R9DRINKN,
                                      R10DRINKN,
                                      R11DRINKN,
                                      R12DRINKN,
                                      R13DRINKN,
                                      R14DRINKN,
                                      R15DRINKN,
                                      R3DRINKD,
                                      R4DRINKD,
                                      R5DRINKD,
                                      R6DRINKD,
                                      R7DRINKD,
                                      R8DRINKD,
                                      R9DRINKD,
                                      R10DRINKD,
                                      R11DRINKD,
                                      R12DRINKD,
                                      R13DRINKD,
                                      R14DRINKD,
                                      R15DRINKD,
                                      R3MSTAT,
                                      R4MSTAT,
                                      R5MSTAT,
                                      R6MSTAT,
                                      R7MSTAT,
                                      R8MSTAT,
                                      R9MSTAT,
                                      R10MSTAT,
                                      R11MSTAT,
                                      R12MSTAT,
                                      R13MSTAT,
                                      R14MSTAT,
                                      R15MSTAT,
                                      R3URBRUR,
                                      R4URBRUR,
                                      R5URBRUR,
                                      R6URBRUR,
                                      R7URBRUR,
                                      R8URBRUR,
                                      R9URBRUR,
                                      R10URBRUR,
                                      R11URBRUR,
                                      R12URBRUR,
                                      R13URBRUR,
                                      R14URBRUR,
                                      R15URBRUR))
library(dplyr)
all_cog_var <- all_cog_var %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    !is.na(R6WTRESP) & R6WTRESP != 0 ~ R6WTRESP,
    !is.na(R5WTRESP) & R5WTRESP != 0 ~ R5WTRESP,
    !is.na(R4WTRESP) & R4WTRESP != 0 ~ R4WTRESP,
    !is.na(R3WTRESP) & R3WTRESP != 0 ~ R3WTRESP,
    TRUE ~ 0
  ))
all_cog_var_pivot <- data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R3COG27, Age = all_cog_var$R3AGEY_E, Smoke = all_cog_var$R3SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R3BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R3HIBPE, Psych_Problems = all_cog_var$R3PSYCHE, Drinks_day = all_cog_var$R3DRINKN, Drinks_a_week = all_cog_var$R3DRINKD, marital_status = all_cog_var$R3MSTAT, urban = all_cog_var$R3URBRUR)
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R4COG27, Age = all_cog_var$R4AGEY_E, Smoke = all_cog_var$R4SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R4BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R4HIBPE, Psych_Problems = all_cog_var$R4PSYCHE, Drinks_day = all_cog_var$R4DRINKN, Drinks_a_week = all_cog_var$R4DRINKD, marital_status = all_cog_var$R4MSTAT, urban = all_cog_var$R4URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R5COG27, Age = all_cog_var$R5AGEY_E, Smoke = all_cog_var$R5SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R5BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R5HIBPE, Psych_Problems = all_cog_var$R5PSYCHE, Drinks_day = all_cog_var$R5DRINKN,Drinks_a_week = all_cog_var$R5DRINKD, marital_status = all_cog_var$R5MSTAT, urban = all_cog_var$R5URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R6COG27, Age = all_cog_var$R6AGEY_E, Smoke = all_cog_var$R6SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R6BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R6HIBPE, Psych_Problems = all_cog_var$R6PSYCHE, Drinks_day = all_cog_var$R6DRINKN,Drinks_a_week = all_cog_var$R6DRINKD, marital_status = all_cog_var$R6MSTAT, urban = all_cog_var$R6URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R7COG27, Age = all_cog_var$R7AGEY_E, Smoke = all_cog_var$R7SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R7BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R7HIBPE, Psych_Problems = all_cog_var$R7PSYCHE, Drinks_day = all_cog_var$R7DRINKN,Drinks_a_week = all_cog_var$R7DRINKD, marital_status = all_cog_var$R7MSTAT, urban = all_cog_var$R7URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R8COG27, Age = all_cog_var$R8AGEY_E, Smoke = all_cog_var$R8SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R8BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R8HIBPE, Psych_Problems = all_cog_var$R8PSYCHE, Drinks_day = all_cog_var$R8DRINKN,Drinks_a_week = all_cog_var$R8DRINKD, marital_status = all_cog_var$R8MSTAT, urban = all_cog_var$R8URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R9COG27, Age = all_cog_var$R9AGEY_E, Smoke = all_cog_var$R9SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R9BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R9HIBPE, Psych_Problems = all_cog_var$R9PSYCHE, Drinks_day = all_cog_var$R9DRINKN,Drinks_a_week = all_cog_var$R9DRINKD, marital_status = all_cog_var$R9MSTAT, urban = all_cog_var$R9URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R10COG27, Age = all_cog_var$R10AGEY_E, Smoke = all_cog_var$R10SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R10BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R10HIBPE, Psych_Problems = all_cog_var$R10PSYCHE, Drinks_day = all_cog_var$R10DRINKN,Drinks_a_week = all_cog_var$R10DRINKD, marital_status = all_cog_var$R10MSTAT, urban = all_cog_var$R10URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R11COG27, Age = all_cog_var$R11AGEY_E, Smoke = all_cog_var$R11SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R11BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R11HIBPE, Psych_Problems = all_cog_var$R11PSYCHE, Drinks_day = all_cog_var$R11DRINKN,Drinks_a_week = all_cog_var$R11DRINKD, marital_status = all_cog_var$R11MSTAT, urban = all_cog_var$R11URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R12COG27, Age = all_cog_var$R12AGEY_E, Smoke = all_cog_var$R12SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R12BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R12HIBPE, Psych_Problems = all_cog_var$R12PSYCHE, Drinks_day = all_cog_var$R12DRINKN,Drinks_a_week = all_cog_var$R12DRINKD, marital_status = all_cog_var$R12MSTAT, urban = all_cog_var$R12URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R13COG27, Age = all_cog_var$R13AGEY_E, Smoke = all_cog_var$R13SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R13BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R13HIBPE, Psych_Problems = all_cog_var$R13PSYCHE, Drinks_day = all_cog_var$R13DRINKN,Drinks_a_week = all_cog_var$R13DRINKD, marital_status = all_cog_var$R13MSTAT, urban = all_cog_var$R13URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R14COG27, Age = all_cog_var$R14AGEY_E, Smoke = all_cog_var$R14SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R14BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R14HIBPE, Psych_Problems = all_cog_var$R14PSYCHE, Drinks_day = all_cog_var$R14DRINKN,Drinks_a_week = all_cog_var$R14DRINKD, marital_status = all_cog_var$R14MSTAT, urban = all_cog_var$R14URBRUR))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R15COG27, Age = all_cog_var$R15AGEY_E, Smoke = all_cog_var$R15SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R3CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R3DIABE, BMI = all_cog_var$R15BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R15HIBPE, Psych_Problems = all_cog_var$R15PSYCHE, Drinks_day = all_cog_var$R15DRINKN,Drinks_a_week = all_cog_var$R15DRINKD, marital_status = all_cog_var$R15MSTAT, urban = all_cog_var$R15URBRUR))

library(ggplot2)

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Age_group = case_when(
    Age < 65 ~ "50-64",
    Age < 80 ~ "65-79",
    TRUE ~ "80+"
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Age_group2 = case_when(
    Age < 55 ~ "50-54",
    Age < 60 ~ "55-59",
    Age < 65 ~ "60-64",
    Age < 70 ~ "65-69",
    Age < 75 ~ "70-74",
    Age < 80 ~ "75-79",
    Age < 85 ~ "80-84",
    Age < 90 ~ "85-89",
    Age < 95 ~ "90-94",
    TRUE ~ "95+"
  ))

ggplot(all_cog_var_pivot, aes(x = Age_group2, y = Score, weight = Weight, fill = factor(Blood_pressure)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Has Ever Had High Blood Pressure",
                    labels = c("No", "Yes"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Ever Having High Blood Pressure",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

ggplot(all_cog_var_pivot, aes(x = Age_group2, y = Score, weight = Weight, fill = factor(Psych_Problems)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Has Ever Had Psych Problems",
                    labels = c("No", "Yes"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Ever Having Psych Problems",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(heavy_drink= case_when(
    Drinks_day < 1 ~ 0,
    Drinks_day < 4 ~ 1,
    TRUE ~ 3
  ))

ggplot(all_cog_var_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(heavy_drink)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1"),
                    name = "How Many Drinks Do You Have When You Drink?",
                    labels = c("Doesn't Drink", "1-3 Drinks (moderate)", "4+ Drinks (heavy)"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Heavy Drinking",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Drink_Day_week= case_when(
    Drinks_a_week < 1 ~ 0,
    Drinks_a_week < 3 ~ 1,
    Drinks_a_week < 5 ~ 2,
    TRUE ~ 3
  ))

ggplot(all_cog_var_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Drink_Day_week)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1", "skyblue"),
                    name = "How Many Days a Week Do You Drink?",
                    labels = c("0 Days", "1-2 Days", "3-4 Days", "5+ Days"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Drinking Frequency",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Marital_status= case_when(
    (marital_status == 1)|(marital_status == 3)~ 0,
    (marital_status == 4)|(marital_status == 5)|(marital_status == 6)~1,
    marital_status == 7 ~ 2,
    marital_status == 8 ~ 3))

all_cog_var_pivot <- all_cog_var_pivot[!is.na(all_cog_var_pivot$Marital_status),]
ggplot(all_cog_var_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(Marital_status)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1", "skyblue"),
                    name = "Marital Status",
                    labels = c("Married or Partnered", "Divorced or Seperated", "Widowed", "Never Married"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Marital Status",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

all_cog_var_pivot <- all_cog_var_pivot[!is.na(all_cog_var_pivot$urban),]

ggplot(all_cog_var_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(urban, exclude = NA)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1"),
                    name = "Area Predominately Lived In",
                    labels = c("Urban", "Suburban", "Rural"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nArea Predominantely Lived In",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")
##--##
library(dplyr)
all_cog_var <- subset(HRS, select = c(HHIDPN, R3COG27,
                                      R3AGEY_E,
                                      R3SMOKEN,
                                      R4COG27,
                                      R4AGEY_E,
                                      R4SMOKEN,
                                      R5COG27,
                                      R5AGEY_E,
                                      R5SMOKEN,
                                      R6COG27,
                                      R6AGEY_E,
                                      R6SMOKEN,
                                      R7COG27,
                                      R7AGEY_E,
                                      R7SMOKEN,
                                      R8COG27,
                                      R8AGEY_E,
                                      R8SMOKEN,
                                      R9COG27,
                                      R9AGEY_E,
                                      R9SMOKEN,
                                      R10COG27,
                                      R10AGEY_E,
                                      R10SMOKEN,
                                      R11COG27,
                                      R11AGEY_E,
                                      R11SMOKEN,
                                      R12COG27,
                                      R12AGEY_E,
                                      R12SMOKEN,
                                      R13COG27,
                                      R13AGEY_E,
                                      R13SMOKEN,
                                      R14COG27,
                                      R14AGEY_E,
                                      R14SMOKEN,
                                      R15COG27,
                                      R15AGEY_E,
                                      R15SMOKEN,
                                      R3WTRESP,
                                      R4WTRESP,
                                      R5WTRESP,
                                      R6WTRESP,
                                      R7WTRESP,
                                      R8WTRESP,
                                      R9WTRESP,
                                      R10WTRESP,
                                      R11WTRESP,
                                      R12WTRESP,
                                      R13WTRESP,
                                      R14WTRESP,
                                      R15WTRESP,
                                      R3CESD,
                                      R4CESD,
                                      R5CESD,
                                      R6CESD,
                                      R7CESD,
                                      R8CESD,
                                      R9CESD,
                                      R10CESD,
                                      R11CESD,
                                      R12CESD,
                                      R13CESD,
                                      R14CESD,
                                      R15CESD,
                                      RAHISPAN,
                                      RAEDEGRM,
                                      R3DIABE,
                                      R4DIABE,
                                      R5DIABE,
                                      R6DIABE,
                                      R7DIABE,
                                      R8DIABE,
                                      R9DIABE,
                                      R10DIABE,
                                      R11DIABE,
                                      R12DIABE,
                                      R13DIABE,
                                      R14DIABE,
                                      R15DIABE,
                                      R3BMI,
                                      R4BMI,
                                      R5BMI,
                                      R6BMI,
                                      R7BMI,
                                      R8BMI,
                                      R9BMI,
                                      R10BMI,
                                      R11BMI,
                                      R12BMI,
                                      R13BMI,
                                      R14BMI,
                                      R15BMI,
                                      RAESTRAT,
                                      RAEHSAMP,
                                      R7LTACTX,
                                      R8LTACTX,
                                      R9LTACTX,
                                      R10LTACTX,
                                      R11LTACTX,
                                      R12LTACTX,
                                      R13LTACTX,
                                      R14LTACTX,
                                      R15LTACTX,
                                      R7SLEEPWKE,
                                      R8SLEEPWKE,
                                      R9SLEEPWKE,
                                      R10SLEEPWKE,
                                      R11SLEEPWKE,
                                      R12SLEEPWKE,
                                      R13SLEEPWKE,
                                      R14SLEEPWKE,
                                      R15SLEEPWKE,
                                      R7SLEEPFAL,
                                      R8SLEEPFAL,
                                      R9SLEEPFAL,
                                      R10SLEEPFAL,
                                      R11SLEEPFAL,
                                      R12SLEEPFAL,
                                      R13SLEEPFAL,
                                      R14SLEEPFAL,
                                      R15SLEEPFAL,
                                      R3HIBPE,
                                      R4HIBPE,
                                      R5HIBPE,
                                      R6HIBPE,
                                      R7HIBPE,
                                      R8HIBPE,
                                      R9HIBPE,
                                      R10HIBPE,
                                      R11HIBPE,
                                      R12HIBPE,
                                      R13HIBPE,
                                      R14HIBPE,
                                      R15HIBPE,
                                      R3PSYCHE,
                                      R4PSYCHE,
                                      R5PSYCHE,
                                      R6PSYCHE,
                                      R7PSYCHE,
                                      R8PSYCHE,
                                      R9PSYCHE,
                                      R10PSYCHE,
                                      R11PSYCHE,
                                      R12PSYCHE,
                                      R13PSYCHE,
                                      R14PSYCHE,
                                      R15PSYCHE,
                                      R3DRINKN,
                                      R4DRINKN,
                                      R5DRINKN,
                                      R6DRINKN,
                                      R7DRINKN,
                                      R8DRINKN,
                                      R9DRINKN,
                                      R10DRINKN,
                                      R11DRINKN,
                                      R12DRINKN,
                                      R13DRINKN,
                                      R14DRINKN,
                                      R15DRINKN,
                                      R3DRINKD,
                                      R4DRINKD,
                                      R5DRINKD,
                                      R6DRINKD,
                                      R7DRINKD,
                                      R8DRINKD,
                                      R9DRINKD,
                                      R10DRINKD,
                                      R11DRINKD,
                                      R12DRINKD,
                                      R13DRINKD,
                                      R14DRINKD,
                                      R15DRINKD,
                                      R3MSTAT,
                                      R4MSTAT,
                                      R5MSTAT,
                                      R6MSTAT,
                                      R7MSTAT,
                                      R8MSTAT,
                                      R9MSTAT,
                                      R10MSTAT,
                                      R11MSTAT,
                                      R12MSTAT,
                                      R13MSTAT,
                                      R14MSTAT,
                                      R15MSTAT,
                                      R3URBRUR,
                                      R4URBRUR,
                                      R5URBRUR,
                                      R6URBRUR,
                                      R7URBRUR,
                                      R8URBRUR,
                                      R9URBRUR,
                                      R10URBRUR,
                                      R11URBRUR,
                                      R12URBRUR,
                                      R13URBRUR,
                                      R14URBRUR,
                                      R15URBRUR
                                     ))
library(dplyr)
all_cog_var <- all_cog_var %>%
  mutate(weight = case_when(
    !is.na(R15WTRESP) & R15WTRESP != 0 ~ R15WTRESP,
    !is.na(R14WTRESP) & R14WTRESP != 0 ~ R14WTRESP,
    !is.na(R13WTRESP) & R13WTRESP != 0~ R13WTRESP,
    !is.na(R12WTRESP) & R12WTRESP != 0 ~ R12WTRESP,
    !is.na(R11WTRESP) & R11WTRESP != 0 ~ R11WTRESP,
    !is.na(R10WTRESP) & R10WTRESP != 0 ~ R10WTRESP,
    !is.na(R9WTRESP) & R9WTRESP != 0 ~ R9WTRESP,
    !is.na(R8WTRESP) & R8WTRESP != 0 ~ R8WTRESP,
    !is.na(R7WTRESP) & R7WTRESP != 0 ~ R7WTRESP,
    TRUE ~ 0
  ))

all_cog_var_pivot <- data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R7COG27, Age = all_cog_var$R7AGEY_E, Smoke = all_cog_var$R7SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R7CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R7DIABE, BMI = all_cog_var$R7BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R7HIBPE, Psych_Problems = all_cog_var$R7PSYCHE, Drinks_day = all_cog_var$R7DRINKN, Drinks_a_week = all_cog_var$R7DRINKD, marital_status = all_cog_var$R7MSTAT, urban = all_cog_var$R7URBRUR, Exercise = all_cog_var$R7LTACTX, Early = all_cog_var$R7SLEEPWKE, fall = all_cog_var$R7SLEEPFAL)
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R8COG27, Age = all_cog_var$R8AGEY_E, Smoke = all_cog_var$R8SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R8CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R8DIABE, BMI = all_cog_var$R8BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R8HIBPE, Psych_Problems = all_cog_var$R8PSYCHE, Drinks_day = all_cog_var$R8DRINKN, Drinks_a_week = all_cog_var$R8DRINKD, marital_status = all_cog_var$R8MSTAT, urban = all_cog_var$R8URBRUR,  Exercise = all_cog_var$R8LTACTX, Early = all_cog_var$R8SLEEPWKE, fall = all_cog_var$R8SLEEPFAL))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R9COG27, Age = all_cog_var$R9AGEY_E, Smoke = all_cog_var$R9SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R9CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R9DIABE, BMI = all_cog_var$R9BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R9HIBPE, Psych_Problems = all_cog_var$R9PSYCHE, Drinks_day = all_cog_var$R9DRINKN, Drinks_a_week = all_cog_var$R9DRINKD, marital_status = all_cog_var$R9MSTAT, urban = all_cog_var$R9URBRUR,  Exercise = all_cog_var$R9LTACTX, Early = all_cog_var$R9SLEEPWKE, fall = all_cog_var$R9SLEEPFAL))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R10COG27, Age = all_cog_var$R10AGEY_E, Smoke = all_cog_var$R10SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R10CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R10DIABE, BMI = all_cog_var$R10BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R10HIBPE, Psych_Problems = all_cog_var$R10PSYCHE, Drinks_day = all_cog_var$R10DRINKN, Drinks_a_week = all_cog_var$R10DRINKD, marital_status = all_cog_var$R10MSTAT, urban = all_cog_var$R10URBRUR,  Exercise = all_cog_var$R10LTACTX, Early = all_cog_var$R10SLEEPWKE, fall = all_cog_var$R10SLEEPFAL))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R11COG27, Age = all_cog_var$R11AGEY_E, Smoke = all_cog_var$R11SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R11CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R11DIABE, BMI = all_cog_var$R11BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R11HIBPE, Psych_Problems = all_cog_var$R11PSYCHE, Drinks_day = all_cog_var$R11DRINKN, Drinks_a_week = all_cog_var$R11DRINKD, marital_status = all_cog_var$R11MSTAT, urban = all_cog_var$R11URBRUR,  Exercise = all_cog_var$R11LTACTX, Early = all_cog_var$R11SLEEPWKE, fall = all_cog_var$R11SLEEPFAL))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R12COG27, Age = all_cog_var$R12AGEY_E, Smoke = all_cog_var$R12SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R12CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R12DIABE, BMI = all_cog_var$R12BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R12HIBPE, Psych_Problems = all_cog_var$R12PSYCHE, Drinks_day = all_cog_var$R12DRINKN, Drinks_a_week = all_cog_var$R12DRINKD, marital_status = all_cog_var$R12MSTAT, urban = all_cog_var$R12URBRUR, Exercise = all_cog_var$R12LTACTX, Early = all_cog_var$R12SLEEPWKE, fall = all_cog_var$R12SLEEPFAL))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R13COG27, Age = all_cog_var$R13AGEY_E, Smoke = all_cog_var$R13SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R13CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R13DIABE, BMI = all_cog_var$R13BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R13HIBPE, Psych_Problems = all_cog_var$R13PSYCHE, Drinks_day = all_cog_var$R13DRINKN, Drinks_a_week = all_cog_var$R13DRINKD, marital_status = all_cog_var$R13MSTAT, urban = all_cog_var$R13URBRUR, Exercise = all_cog_var$R13LTACTX, Early = all_cog_var$R13SLEEPWKE, fall = all_cog_var$R13SLEEPFAL))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R14COG27, Age = all_cog_var$R14AGEY_E, Smoke = all_cog_var$R14SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R14CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R14DIABE, BMI = all_cog_var$R14BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R14HIBPE, Psych_Problems = all_cog_var$R14PSYCHE, Drinks_day = all_cog_var$R14DRINKN, Drinks_a_week = all_cog_var$R14DRINKD, marital_status = all_cog_var$R14MSTAT, urban = all_cog_var$R14URBRUR, Exercise = all_cog_var$R14LTACTX, Early = all_cog_var$R14SLEEPWKE, fall = all_cog_var$R14SLEEPFAL))
all_cog_var_pivot <- rbind(all_cog_var_pivot, data.frame(ID = all_cog_var$HHIDPN, Score = all_cog_var$R14COG27, Age = all_cog_var$R15AGEY_E, Smoke = all_cog_var$R15SMOKEN, Weight = all_cog_var$weight, Hispanic = all_cog_var$RAHISPAN, Depressed = all_cog_var$R15CESD, Education = all_cog_var$RAEDEGRM, diabetes = all_cog_var$R15DIABE, BMI = all_cog_var$R15BMI, Stratum = all_cog_var$RAESTRAT, Cluster = all_cog_var$RAEHSAMP, Blood_pressure = all_cog_var$R15HIBPE, Psych_Problems = all_cog_var$R15PSYCHE, Drinks_day = all_cog_var$R15DRINKN, Drinks_a_week = all_cog_var$R15DRINKD, marital_status = all_cog_var$R15MSTAT, urban = all_cog_var$R15URBRUR, Exercise = all_cog_var$R15LTACTX, Early = all_cog_var$R15SLEEPWKE, fall = all_cog_var$R15SLEEPFAL))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Is_depressed = case_when(
    Depressed < 3 ~ 0,
    TRUE ~ 1
  ))
all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Education_Level = case_when(
    Education == 0 ~ 0,
    Education < 4 ~ 1,
    Education < 6 ~ 2,
    Education < 8 ~ 3,
    TRUE ~ 5
  ))
all_cog_var_pivot <- all_cog_var_pivot[all_cog_var_pivot$Education_Level != 5,]

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(BMI_category = case_when(
    BMI < 18.5 ~ 0,
    BMI < 24.9 ~ 1,
    BMI < 29.9 ~ 2,
    BMI < 34.9 ~ 3,
    TRUE ~ 4
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(heavy_drink= case_when(
    Drinks_day < 1 ~ 0,
    Drinks_day < 4 ~ 1,
    Drinks_day < 5 ~ 2,
    TRUE ~ 3
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Drink_Day_week= case_when(
    Drinks_a_week < 1 ~ 0,
    Drinks_a_week < 3 ~ 1,
    Drinks_a_week < 5 ~ 2,
    TRUE ~ 3
  ))
all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Marital_status= case_when(
    (marital_status == 1)|(marital_status == 3)~ 0,
    (marital_status == 4)|(marital_status == 5)|(marital_status == 6)~1,
    marital_status == 7 ~ 2,
    marital_status == 8 ~ 3))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Lonely_Category = case_when(
    lonely < 1.5 ~ 1,
    lonely < 2 ~ 2,
    lonely < 2.5 ~ 3,
    TRUE ~ 4
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(exercise_freq = case_when(
    Exercise >= 4 ~ 2,
    Exercise >= 2 ~ 1,
    TRUE ~ 0
  ))

all_cog_var_pivot$ageminus50 <- all_cog_var_pivot$Age - 50


all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(doesnt_drink = case_when(
    heavy_drink ==0 ~ 1,
    TRUE ~ 0
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(heavy_drinker =case_when(
    heavy_drink == 3 |heavy_drink == 2 ~ 1,
    TRUE ~ 0
  ))

all_cog_var_pivot <- all_cog_var_pivot %>%
  mutate(Marital_end = case_when(
    Marital_status == 1 | Marital_status == 2 ~ 1,
    TRUE ~ 0 
  ))

all_cog_var_pivot <- all_cog_var_pivot %>% 
  mutate(trouble_falling = case_when(
    fall == 1 ~ 1,
    TRUE ~0
  ))

library(ggplot2)

all_cog_var_pivot <- all_cog_var_pivot[all_cog_var_pivot$ageminus50 >= 0,]
ggplot(all_cog_var_pivot, aes(ageminus50, Score, weight = Weight))+
 geom_jitter(alpha=0.01)+
 geom_smooth(se = FALSE)+
  labs(title = "Weighted Jittered Scatter Plot of Age vs Cognition Score (alpha = 0.01)\nWith Generalized Additive Model Smoothing")

all_cog_var_pivot$bmidiff <- abs(all_cog_var_pivot$BMI - 25)

all_cog_var_pivot <- all_cog_var_pivot %>% 
 mutate(wake_up_early = case_when(
   Early == 1 ~ 1,
   TRUE ~0
 ))

all_cog_var_pivot <- all_cog_var_pivot %>% 
  mutate(underweight = case_when(
    BMI_category == 0 ~ 1,
    TRUE ~0
  ))

all_cog_var_pivot <- all_cog_var_pivot[!is.na(all_cog_var_pivot$Score), ]
colSums(is.na(all_cog_var_pivot))
hist(rowSums(is.na(all_cog_var_pivot)))
all_cog_var_pivot <- all_cog_var_pivot[rowSums(is.na(all_cog_var_pivot)) <= 5,]

#Replacing  NA

all_cog_var_pivot$Smoke[is.na(all_cog_var_pivot$Smoke)] <- 1

get_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Check if the vector is empty after removing NAs
  if (length(x) == 0) return(NA)
  
  ux <- unique(x)
  # Use which.max() to find the index of the most frequent value
  ux[which.max(tabulate(match(x, ux)))]
}

all_cog_var_pivot <- all_cog_var_pivot %>%
  group_by(ID)%>%
  mutate(Early = ifelse(is.na(Early), get_mode(Early), Early)) %>%
  ungroup()

all_cog_var_pivot <- all_cog_var_pivot %>%
  group_by(ID)%>%
  mutate(fall = ifelse(is.na(fall), get_mode(fall), fall)) %>%
  ungroup()

all_cog_var_pivot <- all_cog_var_pivot %>% 
  mutate(trouble_falling = case_when(
    fall == 1 ~ 1,
    TRUE ~0
  ))

all_cog_var_pivot <- all_cog_var_pivot %>% 
  mutate(wake_up_early = case_when(
    Early == 1 ~ 1,
    TRUE ~0
  ))

all_cog_var_pivot_df <- all_cog_var_pivot[, c(1,2,4,5,6,9,13,14,22,23, 28, 29, 30,31,32,33,35,36)]

all_cog_var_pivot_df <- na.omit(all_cog_var_pivot_df)
all_cog_var_pivot_df <- all_cog_var_pivot_df[all_cog_var_pivot_df$ageminus50 >= 0,]
all_cog_var_pivot_df$stdweights <-  all_cog_var_pivot_df$Weight / mean(all_cog_var_pivot_df$Weight)
write.csv(all_cog_var_pivot_df,"~all_cog_var_pivot_df.csv", row.names = TRUE)

cor_matrix <- cor(all_cog_var_pivot_df[,c(2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18)], method = "pearson")
round(cor_matrix,2)
library(corrplot)
corrplot(cor_matrix,
         method = "circle",   # Visualization method: "circle", "square", "number", "color", etc.
         type = "upper",      # Display the upper triangle only
         order = "hclust",    # Reorder variables by hierarchical clustering
         tl.col = "black",    # Color of text labels
         tl.srt = 45          # Text label rotation (degrees)
)

library(ggplot2)

ggplot(all_cog_var_pivot, aes(x = Age_group, y = Score, weight = Weight, fill = factor(exercise_freq, exclude = NA)))+
  geom_boxplot()+
  scale_fill_manual(values = c("#dbe4ee", "#81a4cd", "#3e7cb1"),
                    name = "Amount of Frequent Light Physical Activity",
                    labels = c("Frequent", "Somewhat Frequent", "Never"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Frequency of Light Physical Activity",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")


ggplot(all_cog_var_pivot, aes(x = Age_group2, y = Score, weight = Weight, fill = factor(diabetes)))+
  geom_boxplot()+
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Has Diabetes?",
                    labels = c("No", "Yes"))+
  labs(title = "Weighted Distribution of Cognition Score Across Age Groups\nOn Having Diabetes",
       x = "Age Group",
       y = "Cognition Score")+xlab("Age Group")

ggplot(all_cog_var_pivot, aes(x = Age_group2, weight = Weight, fill = factor(diabetes)))+
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Has Diabetes?",
                    labels = c("No", "Yes"))+
  labs(y = "Proportion", x = "Age_group", title = "Weighted Distribution of Diabetics for each Age Group")

ggplot(all_cog_var_pivot, aes(x = Age_group2, weight = Weight, fill = factor(Blood_pressure)))+
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("skyblue", "lightcoral"),
                    name = "Has Ever Had High Blood Pressure?",
                    labels = c("No", "Yes"))+
  labs(y = "Proportion", x = "Age_group", title = "Weighted Distribution of Ever Having Blood Pressure for each Age Group")
