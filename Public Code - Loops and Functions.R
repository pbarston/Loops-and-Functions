#fyi: http://erdavenport.github.io/R-ecology-lesson/03-loops-and-functions.html

surveys=read.csv("./combined.csv",na.strings = "")

str(surveys)
library(lubridate)
#create a character vector and then use that in YMD
#then add to surveys
surveys$date=ymd(paste(surveys$year,surveys$month,surveys$day,sep = "-"))
str(surveys)
class(surveys$date)

#quick viz of data
library(tidyverse)
p=ggplot(surveys)
p+geom_histogram(aes(x=weight))

#was the record taken in 1984? 

if(surveys[1,4]==1984) {
  print("we got a problem!")
} else { print("phew")
}
#check the first record: here, we say literally 1st, 4th column
#but i bet we don't always know column number so easily:
#can also do this: call out the column and just say the 1st entry
if(surveys$year[1]==1984) {
  print("we got a problem!")
} else { print("phew")
}


#first challenge!
#write an if/else statement thhat evaluates whether animal #40 is larger than one ounce
#1 ounce = 28.3g btw
if(surveys$weight[40]/28.3>1) {
  print("score")
} else {print("tiny guy")
  }

#loops to repeat the above operation (check year) for each record
#How can we make R look at each row and tell us if an entry is from 1984? 
#Loops are a powerful tool that will let us repeat operations. For example, we can do something to every row of our dataframe. 
#There are many type of loops, but today we will focus on the for loop. 

#let's use a loop to examine all the years our data was collected
#basic syntax: for (i in 1:dim(surveys)[1]) {}
#to start, dim:surveys gets us the rows x columns
#34786 rows x 14 columns
#since we want to go row by row,we thus need to index dim(surveys) on the first column [1]
dim(surveys)[1] #or really, any column
#gives us number of rows
#we also know that the 1: will create a numeric list starting at the number before the colon and incrementing by one to the number after the colon.

#so for the for the loop:
#each row in the dataframe will be iterated as the variable and thus
#the loop will execute that code for each row

for (i in 1:dim(surveys)[1]) {
  if(surveys$year[i]==1984) {print("boom!")
  } else {print("idiot, no")
    }
}

#so to break it down
#we are first finding the number of rows and making that the end of our sequential vector
#then we are saying, iterate so that the variable equals one value in the vector each time
#and then at each iteration, take i and do this thing to it: 
#and this thing is asking whether the row number (the i) has a year column value of 1984

#next
surveys_adjusted=surveys
#print out new weight value for those specimens measured in 1984
for (i in 1:dim(surveys_adjusted)[1]) {
  if(surveys_adjusted$year[i]==1984) {print(surveys_adjusted$weight[i]*1.1)
  } else {print("idiot, no")
  }
}

#find the number of rows with dim(surveys_adjusted)[1])
#make that the end point of the sequence 1:___ to make it first to last row
#then set the variable to iterate at each value in the sequence
#then, take each iteration and do the following: 
#if the year of the value is 1984, print the adjusted weight of that row
#if not, move on

#since we're not changing the the values if it's not 1984, then just remove that last part
for (i in 1:dim(surveys_adjusted)[1]) {
  if(surveys_adjusted$year[i]==1984) {print(surveys_adjusted$weight[i]*1.1)
  }
}

#now, assign these new values back to their cell
for (i in 1:dim(surveys_adjusted)[1]) {
  if(surveys_adjusted$year[i]==1984) {
    surveys_adjusted$weight[i]=surveys_adjusted$weight[i]*1.1
  }
}
#nothing printed but that's right as we didn't ask it to 
#check that values really did change
mean(surveys$weight[surveys$year==1984],na.rm=TRUE)
mean(surveys_adjusted$weight[surveys_adjusted$year==1984],na.rm=TRUE)


#ok another challenge
#how many animals weigh over an ounce in our dataset?
#to start, create no na dataframe
surveys_adj_no_na=surveys_adjusted[complete.cases(surveys_adjusted$weight),]
colSums(is.na(surveys_adj_no_na))

#next, we need to create the loop
#i created a new column, I'm not sure if this is the easiest way to do this though
for (i in 1:dim(surveys_adj_no_na)[1]) {
  if(surveys_adj_no_na$weight[i]/28.3>1) {
    surveys_adj_no_na$weight_bool[i]=TRUE
  } else {surveys_adj_no_na$weight_bool[i]=FALSE
  }
}

#to count:
sum(surveys_adj_no_na$weight_bool[surveys_adj_no_na$weight_bool==TRUE])
#or (because it's boolean)
sum(surveys_adj_no_na$weight_bool)
#or because it only has a few levels:
table(surveys_adj_no_na$weight_bool)


#of the ones not over an ounce, what is male and female count?
surveys_adj_no_na %>%
  filter(weight_bool=="FALSE") %>%
  group_by(sex) %>%
  count()
#or
table(surveys_adj_no_na$sex[surveys_adj_no_na$weight_bool==FALSE])


#next up: functions
#but now, both weight and hindfeet length are mistaken
#to get the correct values, we will need multiply the recorded values by 1.1245697375083747 and add 10 to both of those variables. 
#Your collaborator is very insistent that you use all of the significant digits provided when you convert values!

#could write out another loop and add that into the "do" area

#more easily, can write a function
#function arguments allow us into insert variables into the "body" portion
#the body is the steps taken by the function

#creating the "convert 1984 function"
convert_1984=function(x){
  myval_adjusted <- x*1.1245697375083747+10
  return(myval_adjusted)
}
#This function will take in a value (x), convert it by multiplying it by 1.1245697375083747 and adding 10, and return the adjusted value to the user. 
#Let’s try it out on some numbers:

#now let's use this in our loop
for (i in 1:dim(surveys)[1]) {
  if(surveys$year[i]==1984) {
    surveys$weight[i]=convert_1984(surveys$weight[i])
    surveys$hindfoot_length[i]=convert_1984(surveys$hindfoot_length[i])
  }
}

#create a function that calculates the weight of the animals in ounces
gram2oz=function(x){
  weightinoz=x/28.3
  return(weightinoz)
}







