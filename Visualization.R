library(reshape2)
library(tidyverse)

setwd("/Users/maido/Documents/OBU/Survey Fundamentals DALT7003/Asgm/")

survey <- read_csv("survey.csv")


### Extra questions
# Select question 28-29
colla <- survey[c(29:30)]

# Column => Row
colla <- data.frame(Questions=names(colla), t(colla), row.names=NULL)

# Make table long form
colla <- melt(colla,id.var="Questions")

# Frequency
fre <- colla %>% group_by(Questions, value) %>%
  summarize(fre = n())

fre

colla2 <- survey[c(31)]



### Main questions
# Select question 1-27
survey <- survey[c(2:28)]

# NA => 0
survey[is.na(survey)] <- 0

# Column => Row
survey <- data.frame(Questions=names(survey), t(survey), row.names=NULL)



# Mean
mean <- rowMeans(survey[,2:35])
withmean <- cbind(survey, mean)
result <- filter(withmean, mean < 3)

# Print the questions have mean of responses lower than 3
low.responses <- result[c(1, 36)]
low.responses



# Rename
survey[survey == "0"] <- "unknow"
survey[survey == "1"] <- "strongly disagree"
survey[survey == "2"] <- "somewhat disagree"
survey[survey == "3"] <- "neither agree/disagree"
survey[survey == "4"] <- "somewhat agree"
survey[survey == "5"] <- "strongly agree"


## For appendix B
# Make table long form
surveytotal <- melt(survey,id.var="Questions")

# Frequency
frequency <- surveytotal %>% group_by(Questions, value) %>% 
  summarize(frequency = n())

# Sum 
sumtotal <- surveytotal %>% group_by(Questions) %>% summarize(sum = n())

# Dataframe
surveytotal <- merge(frequency, sumtotal, by="Questions")
surveytotal <- surveytotal %>% mutate(ratio = frequency*100/sum)





# Communication (Q1, Q4, Q18, Q21, Q22, Q25)
comm <- survey %>% slice(1, 4, 18, 21, 22, 25)

# Make table long form
comm <- melt(comm,id.var="Questions")

# Rename
comm <- comm %>% rename(Responses = value)

# Frequency
fre1 <- comm %>% group_by(Questions, Responses) %>% 
  summarize(frequent = n())

# Sum 
sum1 <- comm %>% group_by(Questions) %>% summarize(sum = n())

# Dataframe
comm <- merge(fre1, sum1, by="Questions")
comm <- comm %>% mutate(ratio = frequent/sum)

# Count frequency
check1 <- comm %>% group_by(Responses) %>% summarize(frequency = sum(frequent))


# Plot
ggplot() + 
  geom_bar(aes(x=Questions, y=ratio, fill = Responses),
           data=comm, stat="identity", position="dodge") + 
  coord_flip() + ggtitle("Does Communication aid the teaching experience?") +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  guides(fill=guide_legend(title=NULL,ncol=3)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::percent)




# Course Quality (Q3, Q5, Q6, Q8, Q10, Q16)
course <- survey %>% slice(3, 5, 6, 8, 10, 16)

# Make table long form
course <- melt(course,id.var="Questions")

# Rename
course <-  course %>% rename(Responses = value)

# Frequency
fre2 <-  course %>% group_by(Questions, Responses) %>% 
  summarize(frequent = n())

# Sum 
sum2 <-  course %>% group_by(Questions) %>% summarize(sum = n())

# Dataframe
course <- merge(fre2, sum2, by="Questions")
course <-  course %>% mutate(ratio = frequent/sum)

# Count frequency
check2 <- course %>% group_by(Responses) %>% summarize(frequency = sum(frequent))



ggplot() + 
  geom_bar(aes(x=Questions, y=ratio, fill = Responses),
           data=course, stat="identity", position="dodge") + 
  coord_flip() + ggtitle("Does the Course Quality aid the teaching experience?") +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  guides(fill=guide_legend(title=NULL,ncol=3)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::percent)





# Environment (Q7, Q15, Q17, Q19, Q20, Q23, Q24)
envi <- survey %>% slice(7, 15, 17, 19, 20, 23, 24)

# Make table long form
envi <- melt(envi,id.var="Questions")

# Rename
envi <-  envi %>% rename(Responses = value)

# Frequency
fre3 <-  envi %>% group_by(Questions, Responses) %>% 
  summarize(frequent = n())

# Sum 
sum3 <-  envi %>% group_by(Questions) %>% summarize(sum = n())

# Dataframe
envi <- merge(fre3, sum3, by="Questions")
envi <-  envi %>% mutate(ratio = frequent/sum)

# Count frequency
check3 <- envi %>% group_by(Responses) %>% summarize(frequency = sum(frequent))


ggplot() + 
  geom_bar(aes(x=Questions, y=ratio, fill = Responses),
           data=envi, stat="identity", position="dodge") + 
  coord_flip() + ggtitle("Does the University Environment aid the teaching experience?") +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  guides(fill=guide_legend(title=NULL,ncol=3)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::percent)





# Teaching Experience (Q2, Q9, Q11, Q12, Q13, Q14, Q26)
teaching <- survey %>% slice(2, 9, 11, 12, 13, 14, 26)

# Make table long form
teaching <- melt(teaching,id.var="Questions")

# Rename
teaching <-  teaching %>% rename(Responses = value)

# Frequency
fre4 <-  teaching %>% group_by(Questions, Responses) %>% 
  summarize(frequent = n())

# Sum 
sum4 <-  teaching %>% group_by(Questions) %>% summarize(sum = n())

# Dataframe
teaching <- merge(fre4, sum4, by="Questions")
teaching <-  teaching %>% mutate(ratio = frequent/sum)

# Count frequency
check4 <- teaching %>% group_by(Responses) %>% summarize(frequency = sum(frequent))


ggplot() + 
  geom_bar(aes(x=Questions, y=ratio, fill = Responses),
           data=teaching, stat="identity", position="dodge") + 
  coord_flip() + ggtitle("Does Teaching Experience match expectations?") +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  guides(fill=guide_legend(title=NULL,ncol=3)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::percent)





# Overall (4,10,14,17,27)
overall <- survey %>% slice(4,10,14,17,27)

# Make table long form
overall <- melt(overall,id.var="Questions")

# Rename
overall <-  overall %>% rename(Responses = value)

# Frequency
fre5 <-  overall %>% group_by(Questions, Responses) %>% 
  summarize(frequent = n())

# Sum 
sum5 <-  overall %>% group_by(Questions) %>% summarize(sum = n())

# Dataframe
overall <- merge(fre5, sum5, by="Questions")
overall <-  overall %>% mutate(ratio = frequent/sum)

# Count frequency
check5 <- overall %>% group_by(Responses) %>% summarize(frequency = sum(frequent))


ggplot() + 
  geom_bar(aes(x=Questions, y=ratio, fill = Responses),
           data=overall, stat="identity", position="dodge") + 
  coord_flip() + ggtitle("Overall Questions") +
  theme(legend.position = "bottom", axis.title.x = element_blank()) +
  guides(fill=guide_legend(title=NULL,ncol=3)) +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::percent)




