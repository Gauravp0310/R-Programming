####################################  R- PROGRAMMING  ################################################

############################# Project on "Wholesale" Data set ########################################

# Group Members - 
## 1) Neha Pramod Dahiwal
## 2) Gaurav Nimbaji Patil
## 3) Akshay Dhanawale

#____________________________________________________________________________________________________
#____________________________________________________________________________________________________



# Install library to read file
library(readxl)

##  First We have to  LOAD & READ  the file.


# Technique - Applying by file.choose() function (This function will choose file from out storage without giving path)
wholesale=read.csv(file.choose())
wholesale
## To get Details & lebels of each column we can use   str()  
str(wholesale)


## To get count of Columns And Rows
dim(wholesale)

nrow(wholesale)                                   # This will show only Rows
ncol(wholesale)                                   # This will show only Columns

## To get Mathematical information such as Mean , Mode , Median..etc.
summary(wholesale)


## finding out Factor / Labels  from specific column
wholesale$Channel=as.factor(wholesale$Channel)
wholesale$Region=as.factor(wholesale$Region)


unique(wholesale$Channel)
unique(wholesale$Region)

## Find Mean of Fresh, Milk & Grocery
mean(wholesale$Fresh & wholesale$Milk)
mean(wholesale$Delicassen)
mean(wholesale$Detergents_Paper)
mean(wholesale$Grocery)
mean(wholesale$Frozen)


## Find Median of Frozen, Milk, Detergents_paper & Grocery 
median(wholesale$Grocery)
median(wholesale$Frozen)
median(wholesale$Milk)
median(wholesale$Delicassen)
median(wholesale$Fresh)
median(wholesale$Detergents_Paper)


## Find Standard Deviation of Frozen, Milk & Grocery 
sd(wholesale$Milk)
sd(wholesale$Frozen)
sd(wholesale$Grocery)
sd(wholesale$Delicassen)
sd(wholesale$Fresh)
sd(wholesale$Detergents_Paper)
## Find Variance of Frozen, Milk & Grocery
var(wholesale$Frozen)
var(wholesale$Milk)
var(wholesale$Grocery)
var(wholesale$Delicassen)
var(wholesale$Fresh)
var(wholesale$Detergents_Paper)


## finding rows whose Milk==871
subset(wholesale,wholesale$Milk=="871")

subset(wholesale,(Fresh==7780))
subset(wholesale,(Fresh==5963 & Channel=="Horeca"))
subset(wholesale,(Fresh==15177 | Delicassen==1716 | Frozen==425))


# Another Technique to get data from data set
wholesale[c(3,9,11,12,245),c(1,3,4,7)]         # first c() =Rows & second c()=Columns


#___________________________________________________________________________________________________________

####  dplyr library installed

library(dplyr)


# Arrange

wholesale=arrange(wholesale,Channel,Fresh)    # one factor and one number
wholesale

# show prize in  reverse order
wholesale=arrange(wholesale,Channel,desc(Milk))
wholesale



## select

# select command is used to select required columns in dataset

select=dplyr::select(wholesale,Fresh,Frozen,Milk,Delicassen)
select

select(wholesale,1:3)            # this will select 1 to 3 (1,2,3)
select(wholesale,c(1,3))         # This will select 1 & 3

select(wholesale,Frozen:Delicassen)

# Show those columns in which "r" is present (this is not case sensitive)
select(wholesale,contains("H")) 
select(wholesale,contains("r"))

## mutate() 

# to add new column

new_col=mutate(wholesale,Milk_Fresh=Milk+Fresh)   
new_col


## pipe()  " %>% "operation 

wholesale

# show only five columns , filter data and then  sort the data 

wholesale %>% 
  select(1:7) %>%
  filter(Milk>1000) %>%
  arrange(Fresh,desc(Region)) %>%
  summarize(mean(Grocery),mean(Fresh))

#_______________________________________________________________________________

#####   Data Visulization

str(wholesale)

####  Histogram
wholesale$Frozen
hist(wholesale$Frozen)
hist(wholesale$Grocery)
hist(wholesale$Delicassen)
hist(wholesale$Detergents_Paper)

sort(wholesale$Frozen) # sort by asending to descending order

# In histogram whoever value is high for that we called (bin)
# 57-59 column has highest value that why we called that column "Bin"

hist(wholesale$Grocery,col="Blue",main="TABLE HISTOGRAM",xlab="stages value",ylab="No, of records")



#### Scatter PLot 

str(wholesale)
summary(wholesale)


# Create Scatter Plot Of x , y 
plot(x=wholesale$Milk,y=wholesale$Fresh,main="Milk and Fresh ")

# give axis lebel and point character (pch=)
plot(x=wholesale$Delicassen,y=wholesale$Frozen,main="Milk and Fresh",xlab="Milk",ylab="Fresh",col="red",pch=12)

# cex=1/2/3/4/5 stands for charater expansion
plot(x=wholesale$Grocery,y=wholesale$Fresh,main="Milk and Fresh",xlab="Milk",ylab="Fresh",col="green",pch=16, cex=1)


____________________________________________________________________________________________________________
#### BOX PLOT 


#  table_name$column_name1 ~  table_name$column_name2 

boxplot(wholesale$Fresh ~ wholesale$Region, main="BOXPLOT OF Fresh For Region",col=c("wheat","blue","azure4"),horizontal=TRUE)

boxplot(wholesale$Frozen ~ wholesale$Region, main="BOXPLOT OF Frozen For Region",col=c("wheat","green","azure4"),vertical=TRUE)


#____________________________________________________________________________________________________________

#######   ggplot Graphics

library(ggplot2)

str(wholesale)
summary(wholesale)


qplot(Region,Fresh, data=wholesale)
qplot(Region,Fresh, data=wholesale,col=Frozen)


qplot(Channel,Grocery, data=wholesale,col=Fresh)


## Plot using ggplot()

gg=ggplot(wholesale , aes(Frozen, Fresh)) +  geom_point()
# aes stands for aesthetics (details about shape size col)
print(gg)

gr=ggplot(wholesale , aes(Region, Fresh)) +  geom_point() + geom_point(aes(color="green",size=1.5))+ labs(x="Frozen label",y="Fresh label") 
gr




####  BAR Graph

gr1=ggplot(wholesale,aes(Region))+geom_bar(fill="brown")        # we used [geom_bar]
gr1

gr2=ggplot(wholesale,aes(Channel))+geom_bar(fill="orange")        # we used [geom_bar]
gr2


