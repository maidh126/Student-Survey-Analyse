library(tidyverse)
library(dplyr)
library(car)
library(lmtest)
library(zoo)

survey <- read_csv("survey.csv")

# Select question 1-27
survey <- survey[c(2:28)]

# Rename column name
survey <- survey %>% rename(Q1 = '1. Course changes have been communicated effectively.',
                            Q2 = '2. Lecturers have tried to engage students with the content.',
                            Q3 = '3. The course enables discussion of ideas from many sources.',
                            Q4 = '4. Communication channels facilitate the teaching experience.',
                            Q5 = '5. The course is intellectually stimulating.',
                            Q6 = '6. I have been able to work to my ability.',
                            Q7 = '7. Lecturers have facilitated a good learning experience.',
                            Q8 = '8. The course allows deeper discussion of topics than is normally possible.',
                            Q9 = '9. There has been opportunity to provide feedback on the teaching aspects of the course.',
                            Q10 = '10. Course quality aids the teaching experience.',
                            Q11 = '11. Marking criteria is made clear.',
                            Q12 = '12. Lecturers have provided feedback in a timely fashion.',
                            Q13 = '13. It is clear where student feedback has improved teaching quality.',
                            Q14 = '14. The teaching experience matches expectations.',
                            Q15 = '15. Student views on teaching are represented by the student union.',
                            Q16 = '16. The course is run smoothly.',
                            Q17 = '17. The University environment aids the teaching experience.',
                            Q18 = '18. Module timetabling works well with my personal schedule.',
                            Q19 = '19. I have had valuable support from the IT department.',
                            Q20 = '20. I have had valuable support from the librarians.',
                            Q21 = '21. Moodle facilitates a good learning experience',
                            Q22 = '22. Zoom facilitates a good learning experience.',
                            Q23 = '23. The university community makes a good learning environment.',
                            Q24 = '24. When working with other students, the teaching experience becomes more interesting.',
                            Q25 = '25. There has been guidance on how best to study difficult topics.',
                            Q26 = '26. Lecturers value student opinion.',
                            Q27 = '27. Overall, I am satisfied with the quality of the course.')

# NA => mean
survey[] <- lapply(survey, na.aggregate)



# Communication (Q1, Q4, Q18, Q21, Q22, Q25)
comm <- survey[c(1, 4, 18, 21, 22, 25)]

# ANOVA model
comm.model <- lm(Q4 ~ Q1 + Q18 + Q21 + Q22 + Q25, data = comm)


# influence.measures(comm.model)

# Summary
summary(comm.model)

par(mfrow=(c(2,2)))

# Diagnostics
plot(comm.model)

# Check outlier
outlierTest(comm.model)





# Course Quality (Q3, Q5, Q6, Q8, Q10, Q16)
course <- survey[c(3,5,6,8,10,16)]

# ANOVA model
course.model <- lm(Q10 ~ Q3 + Q5 + Q6 + Q8 + Q16, data = course)


# Summary
summary(course.model)


# Selecting the best regression variables
reduced.model <- step(course.model, direction = "backward")

# Summary
summary(reduced.model)

# Diagnostics
plot(reduced.model)

# Check outlier
outlierTest(reduced.model)





# Environment (Q7, Q15, Q17, Q19, Q20, Q23, Q24)
envi <- survey[c(7,15,17,19,20,23,24)]

# ANOVA model
envi.model <- lm(Q17 ~ Q7 + Q15 + Q19 + Q20 + Q23 + Q24, data = envi)

# Summary
summary(envi.model)

# Diagnostics
plot(envi.model)

# Check outlier
outlierTest(envi.model)





# Teaching Experience (Q2, Q9, Q11, Q12, Q13, Q14, Q26)
teaching <- survey[c(2,9,11,12,13,14,26)]

# ANOVA model
teaching.model <- lm(Q14 ~ Q2 + Q9 + Q11 + Q12 + Q13 + Q26, data = teaching)

# Summary
summary(teaching.model)

# Diagnostics
plot(teaching.model)

# Check outlier
outlierTest(teaching.model)





# Overall1 (Q4,Q10,Q14,Q17,Q27)
overall <- survey[c(4,10,14,17,27)]

# ANOVA model
overall.model <- lm(Q27 ~ Q4 + Q10 + Q14 + Q17, data = overall)


# Summary
summary(overall.model)

# Diagnostics
plot(overall.model)

# Check outlier
outlierTest(overall.model)

