# Practical 1
# Raeven van den Acker
# 11-09-2021

# Load packages ----
library(ISLR)
library(tidyverse)
library(haven)
library(readxl)

# Create objects ----
object_1 <- 1:5
object_2 <- 1L:5L
object_3 <- "-123.456"
object_4 <- as.numeric(object_2)
object_5 <- letters[object_1]
object_6 <- as.factor(rep(object_5, 2))
object_7 <- c(1, 2, 3, "4", "5", "6")

# 1. Inspect classes ----
a <- class(object_1) # integer
b <- class(object_2) # integer
c <- class(object_3) # character
d <- class(object_4) # numeric/double
e <- class(object_5) # character
f <- class(object_6) # farctor
g <- class(object_7) # character

# 2. Converting object_7 back to numbers
numbers_7 <- as.numeric(object_7)

# 3. Make a list of all 7 objects
objects <- list(object_1,object_2,object_3,object_4,object_5,object_6,object_7)

# 4. Make a data frame of objects 1, 2 and 5
df <- data.frame(object_1,object_2,object_5)
ncol(df) # ncols = 3
nrow(df) # nrows = 5

# 6. load google play data
apps <- read_csv('Desktop/UU/SL_V/SLV_practical1/data/googleplaystore.csv')
# 7. column          actual type     expected type
#     rating          character       numeric
#     last updated    character       date
head(apps)

# 9. load students data
students <- read_excel('Desktop/UU/SL_V/SLV_practical1/data/students.xlsx')
ncol(students) # ncols = 3
nrow(students) # nrowa = 37
# all columns have type expected
tail(students)
View(students)

# 10. create a summary of the students data set
summary(students)
# grade range: 4.844 - 9.2291

# 11. filter for students with grade < 5.5
fail <- filter(students, grade < 5.5)

# 12. fiter for students with grade > 8 & from program A
overachievers <- filter(students, programme == 'A' & grade > 8)

# 13. arrange students: programme A -> B, grade high -> low
arranged_students <- arrange(students, programme, grade)

# 14. show only student number and programme of each student
stdnum_prog <- select(students, student_number, programme)

students <- mutate(students, pass = grade > 5.5)

# 15. recode the programme column
students_recoded <- students %>% mutate(programme=recode(programme, 'A'='Science', 'B'='Social Science'))

students_dataset <-
  read_xlsx("data/students.xlsx") %>% 
  mutate(prog = recode(programme, "A" = "Science", "B" = "Social Science")) %>% 
  filter(grade > 5.5) %>% 
  arrange(programme, desc(grade)) %>% 
  select(student_number, prog, grade)

# 16. create data processing pipeline popular_apps
popular_apps <-
  read_csv("data/googleplaystore.csv") %>% 
  mutate(Downloads = parse_number(Installs)) %>% 
  filter(Downloads > 5e8) %>% 
  arrange(Rating) %>% 
  select(App, Rating, Category, Price) %>% 
  distinct(App, .keep_all = TRUE)

# 17. Median, min and max of popular_apps

popular_apps %>% 
  summarize(
    min = min(Rating),
    max = max(Rating),
    median = median(as.numeric(Rating))
  )

# 18. Add MAD to summarize function
mad <- function(x) {
  median(abs(x - median(x)))
}

popular_apps %>% 
  summarize(
    min = min(Rating),
    max = max(Rating),
    median = median(as.numeric(Rating)),
    mad = mad(as.numeric(Rating))
  )

# 19. Grouped summary per category
popular_apps %>% 
  group_by(Category) %>% 
  summarize(
    min = min(Rating),
    max = max(Rating),
    median = median(as.numeric(Rating)),
    mad = mad(as.numeric(Rating))
  )
