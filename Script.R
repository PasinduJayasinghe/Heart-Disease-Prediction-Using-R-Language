install.packages("tidyverse")

library(tidyverse)

heart_data<-read.csv("heart.csv")

view(heart_data)

glimpse(heart_data)

library(Hmisc)

describe(heart_data)

#glimpse and describe commands do the kind of same task by-
#-providing the summary of the data set."glimpse" command is from the "dplyr" library.
#"describe" command is from the "Hmisc" library.
#However "describe" command gives more intensive overview about the dataset.


colnames(heart_data)

#Changing the data set to visualize data more meaningfully

mutated_data <- heart_data %>%
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
         fbs = if_else(fbs == 1, ">120", "<=120"),
         exang = if_else(exang == 1, "YES" ,"NO"),
         cp = if_else(cp == 1, "ATYPICAL ANGINA",
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = if_else(target == 1, "YES", "NO")
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())

#Visualization of the count of the presence and absence of a heart disease

ggplot(mutated_data, aes(x=mutated_data$target, fill=mutated_data$target))+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("count")+
  ggtitle("Presence & Absence of Heart Disease")+
  scale_fill_discrete(name= 'Heart Disease', labels =c("Absence", "Presence"))

#Proportion of the occurrence of a heart disease

prop.table(table(mutated_data$target))

#Frequency of age

mutated_data %>%
  group_by(ï..age) %>%
  count() %>%
  filter(n>10) %>%
  ggplot()+
  geom_col(aes(ï..age, n), fill = 'green')+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Agecount")

#Comparing blood pressure and chest pain

mutated_data %>%
  ggplot(aes(x=sex, y=trestbps))+
  geom_boxplot(fill ='purple')+
  xlab('sex')+
  ylab('BP')+
  facet_grid(~cp)

#Comparing cholesterol and chest pain

mutated_data %>%
  ggplot(aes(x=sex, y=chol))+
  geom_boxplot(fill ='orange')+
  xlab('sex')+
  ylab('Chol')+
  facet_grid(~cp)
  

install.packages("corrplot")
library(corrplot)
install.packages("ggplot2")
library(ggplot2)

cor_heart <- cor(mutated_data[, 10:14])

cor_heart

corrplot(cor_heart, method ='square', type='upper')


