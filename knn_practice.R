# Clear the environment, removing all objects
rm(list=ls(all=TRUE))

#assigning variables 
X<-27
Y<-21

X
Student_name<-"KARA"
Student_name
x=23
x

#creating a list
Ages<-c(23,21,25,26,22)
print(Ages)
Ages

student_info<-list(name="Alberto",age=24, is_student=TRUE)
student_info
student_info$name

#create a new dataframe
df<-data.frame(
  name=c("Nika", "Venu","Van"),
  age=c(23,26,25),
  is_student=c(TRUE,FALSE,FALSE),
  coding_exp=c(1,4,7),
  work_exp=c(5,4,9)
)

df

df$is_student
small_df<-df$name
small_df


#arthemetic and booelan operations
a=X+Y
b=X-Y
a
b
a==b
#working example------------------------------------------------------
if (df$work_exp>7){
  print(print(df$name[df$coding_exp > 7]))
}

for (i in 1:length(df$work_exp)){
  
  if(df$work_exp[i]>7){
    
    print(df$name[i])
    
  }
  
  
# Initialize an empty vector to store names
selected_names <- c()

# Loop through the data frame
for (i in 1:nrow(df)) {
  if (df$work_exp[i] > 7) {
    selected_names <- c(selected_names, df$name[i])
  }
}
  
selected_names


#----------------------------------------------------------------


#if loop 
if (b>a){
  print("b is greater than a")
} else{
  print("a is greater than b")
}


#nested if loop 
if (b > a) {
  
  print("b is greater than a")
  
} else if (a == b) {
  
  print("a and b are equal")
  
} else if(a>b){
  
  print("a is greater than b")
  
}



#nested if loop 
if (b > a) {
  
  print("b is greater than a")
  
} else {
  
  print("a is greater than b")
  
} else if(a==b) {
  
  print("a is equal to b")
  
}


#--------------------------------------------------

#while loop
counter<-1
while (counter<=5){
  print(counter)
  counter<-counter+1
}


#for loop
for (i in 1:100){
  print(i)
}


#nested for loop
for(i in 1:3){
  for(j in 1:2){
    print(paste("i",i,"j",j))
  }
}


#-----------------------------------------------------
#defining functions
add_numbers<-function(a,b,c){
  result<-(a+b)*c
  result
}

#calling function
add_numbers(10,20,10)


#----------------------------------------------------------------

data("mtcars")
mtcars

#glimpse of data
head(mtcars)
tail(mtcars)

#accessing specific rows and columns
mtcars[1,]
mtcars[,1]
mtcars[1,1]
mtcars[1:3,]
mtcars[,1:3]
mtcars[3:10,c("disp","gear","wt")]


#-----------------------------------------------------------
#dplyr
install.packages("dplyr")
library(dplyr)
library(neuralnet)

mtcars
summary(mtcars)


#----------------
#functions from dplyr

small_df<-select(mtcars,"disp","hp","wt")
small_df

filtered_data<-filter(mtcars,mpg>20)
filtered_data

new_df<-arrange(filtered_data,mpg)
new_df


mutate_df<-mutate(mtcars,hp_to_wt=hp/wt)
mutate_df

mutate_new<-arrange(mutate_df,desc(hp_to_wt))
mutate_new

#installing ggplot library
install.packages("ggplot2")
library(ggplot2)

#scatter plot: mpg vs hp
ggplot(mtcars, aes(x = hp, y = mpg))+geom_point()+labs(title="mpg vs hp",x="horsepower",y="miles per gallon")

# Histogram: Distribution of mpg
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribution of MPG", x = "Miles per Gallon")
