setwd("~/Desktop/FACULTY_DATA")

library(tidyverse)
library(gsubfn)
library(htmlwidgets)
library(tidyr)
library(stringr)
library(dplyr)
library(plyr)


#### READING IN THE RAW DATA AND INITIAL CLEANING #####

##Read in info
Y_1875=read.delim("1875.txt")
Y_1880=read.delim("1880.txt")
Y_1885=read.delim("1885.txt")
Y_1890=read.delim("1890.txt")
Y_1890_g=read.delim("1890_guests.txt")
Y_1895_g=read.delim("1895_guests.txt")
Y_1895=read.delim("1895.txt")
Y_1900=read.delim("1900.txt")
Y_1905=read.delim("1905.txt")
Y_1910=read.delim("1910.txt")
Y_1915=read.delim("1915.txt")


##Fill in the years for each of the data frames
Y_1875$Year<-"1875"
Y_1880$Year<-"1880"
Y_1885$Year<-"1885"
Y_1890$Year<-"1890"
Y_1895$Year<-"1895"
Y_1900$Year<-"1900"
Y_1905$Year<-"1905"
Y_1910$Year<-"1910"
Y_1915$Year<-"1915"

##Fill in student count (total) for each of the data frames
Y_1875$Students<-"543"
Y_1880$Students<-"399"
Y_1885$Students<-"649"
Y_1890$Students<-"1390"
Y_1895$Students<-"1702"
Y_1900$Students<-"1987"
Y_1905$Students<-"3068"
Y_1910$Students<-"4241"
Y_1915$Students<-"5296"

##Fill in student count (engineering) for each of the data frames
Y_1875$e_Students<-"138"
Y_1880$e_Students<-"71"
Y_1885$e_Students<-"206"
Y_1890$e_Students<-"581"
Y_1895$e_Students<-"626"
Y_1900$e_Students<-"844"
Y_1905$e_Students<-"1521"
Y_1910$e_Students<-"1631"
Y_1915$e_Students<-"1392"



##MOVE ALL STUFF AFTER THE COMMA TO A DEGREE COLUMN
Y_1875$Degree1<-gsub("^.*?, ","",Y_1875$First_Name)
Y_1880$Degree1<-gsub("^.*?, ","",Y_1880$First_Name)
Y_1885$Degree1<-gsub("^.*?, ","",Y_1885$First_Name)
Y_1890$Degree1<-gsub("^.*?, ","",Y_1890$First_Name)
Y_1895$Degree1<-gsub("^.*?, ","",Y_1895$First_Name)
Y_1900$Degree1<-gsub("^.*?, ","",Y_1900$First_Name)
Y_1905$Degree1<-gsub("^.*?, ","",Y_1905$First_Name)
Y_1910$Degree1<-gsub("^.*?, ","",Y_1910$First_Name)
Y_1915$Degree1<-gsub("^.*?, ","",Y_1915$First_Name)


#### CLEANING EACH INDIVIDUAL TABLE ####

#CLEANING 1875

#split into rows containing different kinds of information
Y_1875b<-separate_rows(Y_1875, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the graduation year
Y_1875b$gradyear<-str_extract(Y_1875b$Degree1, "(\\d){4}")
#extract the degree (first degree)
Y_1875b$degree<-str_extract(Y_1875b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
#delete the previously extracted info from the main info cell
Y_1875b$Degree1<-str_replace(Y_1875b$Degree1, "(\\d){4}", "")
Y_1875b$Degree1<-str_replace(Y_1875b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1875b$Degree1<-str_replace(Y_1875b$Degree1, "^(\\s+)$|^(\\s+)", "")
Y_1875b$Degree1<-str_replace(Y_1875b$Degree1, "Cornell,", "")
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1875b$degree<-str_replace(Y_1875b$degree, ",", "")
Y_1875b$Degree1<-str_replace(Y_1875b$Degree1, "^(\\s),", "")
Y_1875b$Degree1<-str_replace(Y_1875b$Degree1, "^,", "")
Y_1875b$Degree1<-str_replace(Y_1875b$Degree1, "^(\\s)", "")
#Fill empty cells in the Degree1 column with NA
Y_1875b$Degree1[which(Y_1875b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1875b)[colnames(Y_1875b) == 'Degree1'] <- 'Position'


#CLEANING 1880

#split into rows containing different kinds of information
Y_1880b<-separate_rows(Y_1880, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the graduation year
Y_1880b$gradyear<-str_extract(Y_1880b$Degree1, "(\\d){4}")
#extract the degree (first degree)
Y_1880b$degree<-str_extract(Y_1880b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
#delete the previously extracted info from the main info cell
Y_1880b$Degree1<-str_replace(Y_1880b$Degree1, "(\\d){4}", "")
Y_1880b$Degree1<-str_replace(Y_1880b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1880b$Degree1<-str_replace(Y_1880b$Degree1, "^(\\s+)$|^(\\s+)", "")
#Note if the person graduated from Cornell or not
Y_1880b$Cornell <-ifelse(grepl("Cornell", Y_1880b$Degree1), 'Yes', 'No');
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1880b$Degree1<-str_replace(Y_1880b$Degree1, "Cornell,", "")
Y_1880b$degree<-str_replace(Y_1880b$degree, ",", "")
Y_1880b$Degree1<-str_replace(Y_1880b$Degree1, "^(\\s),", "")
Y_1880b$Degree1<-str_replace(Y_1880b$Degree1, "^,", "")
Y_1880b$Degree1<-str_replace(Y_1880b$Degree1, "^(\\s)", "")
#Fill empty cells in the Degree1 column with NA
Y_1880b$Degree1[which(Y_1880b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1880b)[colnames(Y_1880b) == 'Degree1'] <- 'Position'


#CLEANING 1885 

#split into rows containing different kinds of information
Y_1885b<-separate_rows(Y_1885, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the degree (first degree)
Y_1885b$degree<-str_extract(Y_1885b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
#delete the previously extracted info from the main info cell
Y_1885b$Degree1<-str_replace(Y_1885b$Degree1, "(\\d){4}", "")
Y_1885b$Degree1<-str_replace(Y_1885b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1885b$Degree1<-str_replace(Y_1885b$Degree1, "^(\\s+)$|^(\\s+)", "")
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1885b$degree<-str_replace(Y_1885b$degree, ",", "")
Y_1885b$Degree1<-str_replace(Y_1885b$Degree1, "^(\\s),", "")
Y_1885b$Degree1<-str_replace(Y_1885b$Degree1, "^,", "")
Y_1885b$Degree1<-str_replace(Y_1885b$Degree1, "^(\\s)", "")
#Extract second degree
Y_1885b$secondDegree<-str_extract(Y_1885b$Degree1, "Doc[.] Eng[.]")
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1885b$Degree1<-str_replace(Y_1885b$Degree1, "Doc[.] Eng[.],", "")
Y_1885b$Degree1<-str_replace(Y_1885b$Degree1, "[.]", "")
#Fill empty cells in the Degree1 column with NA
Y_1885b$Degree1[which(Y_1885b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1885b)[colnames(Y_1885b) == 'Degree1'] <- 'Position'



#CLEANING 1890

#split into rows containing different kinds of information
Y_1890b<-separate_rows(Y_1890, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the graduation year
Y_1890b$gradyear<-str_extract(Y_1890b$Degree1, "(\\d){4}")
#extract the degree (first degree)
Y_1890b$degree<-str_extract(Y_1890b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
#delete the previously extracted info from the main info cell
Y_1890b$Degree1<-str_replace(Y_1890b$Degree1, "(\\d){4}", "")
Y_1890b$Degree1<-str_replace(Y_1890b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1890b$Degree1<-str_replace(Y_1890b$Degree1, "^(\\s+)$|^(\\s+)", "")
#Note if the person graduated from Cornell or not
Y_1890b$Cornell <- ifelse(grepl("Cornell", Y_1890b$Degree1), 'Yes', 'No');
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1890b$Degree1<-str_replace(Y_1890b$Degree1, "Cornell,", "")
Y_1890b$degree<-str_replace(Y_1890b$degree, ",", "")
Y_1890b$Degree1<-str_replace(Y_1890b$Degree1, "^(\\s),", "")
Y_1890b$Degree1<-str_replace(Y_1890b$Degree1, "^,", "")
Y_1890b$Degree1<-str_replace(Y_1890b$Degree1, "^(\\s)", "")
#Fill empty cells in the Degree1 column with NA
Y_1890b$Degree1[which(Y_1890b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1890b)[colnames(Y_1890b) == 'Degree1'] <- 'Position'


#CLEANING 1895

#split into rows containing different kinds of information
Y_1895b<-separate_rows(Y_1895, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the graduation year
Y_1895b$gradyear<-str_extract(Y_1895b$Degree1, "(\\d){4}")
#extract the degree (first degree)
Y_1895b$degree<-str_extract(Y_1895b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
#delete the previously extracted info from the main info cell
Y_1895b$Degree1<-str_replace(Y_1895b$Degree1, "(\\d){4}", "")
Y_1895b$Degree1<-str_replace(Y_1895b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1895b$Degree1<-str_replace(Y_1895b$Degree1, "^(\\s+)$|^(\\s+)", "")
#Note if the person graduated from Cornell or not
Y_1895b$Cornell <- ifelse(grepl("Cornell", Y_1895b$Degree1), 'Yes', 'No');
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1895b$Degree1<-str_replace(Y_1895b$Degree1, "Cornell,", "")
Y_1895b$degree<-str_replace(Y_1895b$degree, ",", "")
Y_1895b$Degree1<-str_replace(Y_1895b$Degree1, "^(\\s),", "")
Y_1895b$Degree1<-str_replace(Y_1895b$Degree1, "^,", "")
Y_1895b$Degree1<-str_replace(Y_1895b$Degree1, "^(\\s)", "")
#Fill empty cells in the Degree1 column with NA
Y_1895b$Degree1[which(Y_1895b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1895b)[colnames(Y_1895b) == 'Degree1'] <- 'Position'


#CLEANING 1900

#split into rows containing different kinds of information
Y_1900b<-separate_rows(Y_1900, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the graduation year
Y_1900b$gradyear<-str_extract(Y_1900b$Degree1, "(\\d){4}")
#extract the degree (first degree)
Y_1900b$degree<-str_extract(Y_1900b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
#delete the previously extracted info from the main info cell
Y_1900b$Degree1<-str_replace(Y_1900b$Degree1, "(\\d){4}", "")
Y_1900b$Degree1<-str_replace(Y_1900b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1900b$Degree1<-str_replace(Y_1900b$Degree1, "^(\\s+)$|^(\\s+)", "")
#Note if the person graduated from Cornell or not
Y_1900b$Cornell <- ifelse(grepl("Cornell", Y_1900b$Degree1), 'Yes', 'No');
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1900b$Degree1<-str_replace(Y_1900b$Degree1, "Cornell,", "")
Y_1900b$degree<-str_replace(Y_1900b$degree, ",", "")
Y_1900b$Degree1<-str_replace(Y_1900b$Degree1, "^(\\s),", "")
Y_1900b$Degree1<-str_replace(Y_1900b$Degree1, "^,", "")
Y_1900b$Degree1<-str_replace(Y_1900b$Degree1, "^(\\s)", "")
#Fill empty cells in the Degree1 column with NA
Y_1900b$Degree1[which(Y_1900b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1900b)[colnames(Y_1900b) == 'Degree1'] <- 'Position'

#CLEANING 1905

#split into rows containing different kinds of information
Y_1905b<-separate_rows(Y_1905, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the graduation year
Y_1905b$gradyear<-str_extract(Y_1905b$Degree1, "(\\d){4}")
#extract the degree (first degree)
#Y_1905b$degree<-str_extract(Y_1905b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
Y_1905b$degree<-str_extract(Y_1905b$Degree1, "^.*?[A-Z].$")
Y_1905b$college<-str_extract(Y_1905b$Degree1, "^.*?University(\\s)(\\w)(\\s)(\\w)")
#delete the previously extracted info from the main info cell
Y_1905b$Degree1<-str_replace(Y_1905b$Degree1, "(\\d){4}", "")
Y_1905b$Degree1<-str_replace(Y_1905b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1905b$Degree1<-str_replace(Y_1905b$Degree1, "^(\\s+)$|^(\\s+)", "")
#Note if the person graduated from Cornell or not
Y_1905b$Cornell <- ifelse(grepl("Cornell", Y_1905b$Degree1), 'Yes', 'No');
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1905b$Degree1<-str_replace(Y_1905b$Degree1, "Cornell,", "")
Y_1905b$degree<-str_replace(Y_1905b$degree, ",", "")
Y_1905b$Degree1<-str_replace(Y_1905b$Degree1, "^(\\s),", "")
Y_1905b$Degree1<-str_replace(Y_1905b$Degree1, "^,", "")
Y_1905b$Degree1<-str_replace(Y_1905b$Degree1, "^(\\s)", "")
#Fill empty cells in the Degree1 column with NA
Y_1905b$Degree1[which(Y_1905b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1905b)[colnames(Y_1905b) == 'Degree1'] <- 'Position'



#CLEANING 1910

#split into rows containing different kinds of information
Y_1910b<-separate_rows(Y_1910, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the graduation year
Y_1910b$gradyear<-str_extract(Y_1910b$Degree1, "(\\d){4}")
#extract the degree (first degree)
Y_1910b$degree<-str_extract(Y_1910b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
#delete the previously extracted info from the main info cell
Y_1910b$Degree1<-str_replace(Y_1910b$Degree1, "(\\d){4}", "")
Y_1910b$Degree1<-str_replace(Y_1910b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1910b$Degree1<-str_replace(Y_1910b$Degree1, "^(\\s+)$|^(\\s+)", "")
#Note if the person graduated from Cornell or not
Y_1910b$Cornell <- ifelse(grepl("Cornell", Y_1910b$Degree1), 'Yes', 'No');
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1910b$Degree1<-str_replace(Y_1910b$Degree1, "Cornell,", "")
Y_1910b$degree<-str_replace(Y_1910b$degree, ",", "")
Y_1910b$Degree1<-str_replace(Y_1910b$Degree1, "^(\\s),", "")
Y_1910b$Degree1<-str_replace(Y_1910b$Degree1, "^,", "")
Y_1910b$Degree1<-str_replace(Y_1910b$Degree1, "^(\\s)", "")
#Fill empty cells in the Degree1 column with NA
Y_1910b$Degree1[which(Y_1910b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1910b)[colnames(Y_1910b) == 'Degree1'] <- 'Position'


#CLEANING 1915

#split into rows containing different kinds of information
Y_1915b<-separate_rows(Y_1915, Degree1, sep="[,](\\s)(?=[A-Z][.])|[,](\\s)(?=Ph[.])|[,](\\s)(?=LL[.])|[,](\\s)(?=Dr[.])")
#extract the graduation year
Y_1915b$gradyear<-str_extract(Y_1915b$Degree1, "(\\d){4}")
#extract the degree (first degree)
Y_1915b$degree<-str_extract(Y_1915b$Degree1, "^.*?[,]|^.*?[A-Z][.]$")
#delete the previously extracted info from the main info cell
Y_1915b$Degree1<-str_replace(Y_1915b$Degree1, "(\\d){4}", "")
Y_1915b$Degree1<-str_replace(Y_1915b$Degree1, "^.*?[,]|^.*?[A-Z][.]$", "")
Y_1915b$Degree1<-str_replace(Y_1915b$Degree1, "^(\\s+)$|^(\\s+)", "")
#Note if the person graduated from Cornell or not
Y_1915b$Cornell <- ifelse(grepl("Cornell", Y_1915b$Degree1), 'Yes', 'No');
#General cleaning; remove extra spaces at the beginning of cells, extra commas, trailing periods, etc.
Y_1915b$Degree1<-str_replace(Y_1915b$Degree1, "Cornell,", "")
Y_1915b$degree<-str_replace(Y_1915b$degree, ",", "")
Y_1915b$Degree1<-str_replace(Y_1915b$Degree1, "^(\\s),", "")
Y_1915b$Degree1<-str_replace(Y_1915b$Degree1, "^,", "")
Y_1915b$Degree1<-str_replace(Y_1915b$Degree1, "^(\\s)", "")
#Fill empty cells in the Degree1 column with NA
Y_1915b$Degree1[which(Y_1915b$Degree1=="")]<-NA
#Rename the Degree1 column (which initially contained all non-name info) to Position
colnames(Y_1915b)[colnames(Y_1915b) == 'Degree1'] <- 'Position'

##### CLEANING AND CONSOLIDATING MASTER DATAFRAME ####

#DELETE EVERYTHING AFTER NAME
Y_1875b$First_Name <-sub(",.*","",Y_1875b$First_Name)
Y_1880b$First_Name <-sub(",.*","",Y_1880b$First_Name)
Y_1885b$First_Name <-sub(",.*","",Y_1885b$First_Name)
Y_1890b$First_Name <-sub(",.*","",Y_1890b$First_Name)
Y_1895b$First_Name <-sub(",.*","",Y_1895b$First_Name)
Y_1900b$First_Name <-sub(",.*","",Y_1900b$First_Name)
Y_1905b$First_Name <-sub(",.*","",Y_1905b$First_Name)
Y_1910b$First_Name <-sub(",.*","",Y_1910b$First_Name)
Y_1915b$First_Name <-sub(",.*","",Y_1915b$First_Name)



##SEPARATE LAST NAME
Y_1875b$Last_Name <- str_extract(Y_1875b$First_Name, '\\w+$')
Y_1880b$Last_Name <- str_extract(Y_1880b$First_Name, '\\w+$')
Y_1885b$Last_Name <- str_extract(Y_1885b$First_Name, '\\w+$')
Y_1890b$Last_Name <- str_extract(Y_1890b$First_Name, '\\w+$')
Y_1895b$Last_Name <- str_extract(Y_1895b$First_Name, '\\w+$')
Y_1900b$Last_Name <- str_extract(Y_1900b$First_Name, '\\w+$')
Y_1905b$Last_Name <- str_extract(Y_1905b$First_Name, '\\w+$')
Y_1910b$Last_Name <- str_extract(Y_1910b$First_Name, '\\w+$')
Y_1915b$Last_Name <- str_extract(Y_1915b$First_Name, '\\w+$')



#BIND ALL DATAFRAMES TOGETHER INTO ONE MASTER FRAME
consolidated<-rbind.fill(Y_1875b, Y_1880b, Y_1885b, Y_1890b, Y_1895b, Y_1900b, Y_1905b, Y_1910b, Y_1915b)

#find university and colleges and separate into the "College" column of the new consolidated dataframe
consolidated$college<-ifelse(is.na(consolidated$college), 
                             str_extract(consolidated$Position, "(\\w+)(\\s+)College|(\\w+)(\\s+)Institute|State Normal Art School|Colorado School of Mines|Mass. Vor. Art School|Graduate School Industrial Art|Dartmouth|Karlsruhe Polytechnic|Lafayette|Stevens|North Carolina Agricultural and Mechanical College|Indiana|Tufts|Tennessee|Maine|Norwich|New Hampshire|New Hampshire State|North Dakota|Purdue|Swarthmore|Texas A. and M.|R. I. State|California|U.S.N. Academy|Michigan|Trinity|Washington|Missouri State|Wisconsin|Rutgers|Haverford|Georgia School of Technology|Maryland Agricultural College|Michigan Agricultural College|Kentucky State|Grove City College|Stanford|Yale|Institute of the Ways of Communication|(\\w+)(\\s+)Institute of Technology|(\\w+)(\\s+)(\\w+)(\\s+)Institute|Cornell|Rensselaer Polytechnic|Lehigh|Brown|Leigh|Union|University(\\s)of(\\s)(\\w+)|Conciliar Seminary,|Lafayette,|Mass. State Art School."), 
                             consolidated$college)

#delete these colleges from the position column (lots of little cleaning, as well)
consolidated$Position<-str_replace(consolidated$Position,"(\\w+)(\\s+)College|(\\w+)(\\s+)Institute|North Carolina Agricultural and Mechanical College|Indiana|Tufts|Lafayette|Tennessee|College City of New York|Colorado School of Mines|of Technology,|Maine|Norwich|New Hampshire|New Hampshire State|North Dakota|Purdue|Swarthmore|Texas A. and M.|R. I. State|California|U.S.N. Academy|Michigan|Trinity|Washington|Missouri State|Wisconsin|Rutgers|Haverford|Georgia School of Technology|Maryland Agricultural College|Michigan Agricultural College|Kentucky State|Grove City College|Stanford|Yale|Institute of the Ways of Communication|(\\w+)(\\s+)Institute of Technology|(\\w+)(\\s+)(\\w+)(\\s+)Institute|Cornell|Rensselaer Polytechnic|Lehigh|Brown|Leigh|Union|University(\\s)of(\\s)(\\w+)|Conciliar Seminary,|Lafayette,|Mass. State Art School| of San Lldefonso(Salamanca Jurisdiction)|Fellow at , 1886-7.|FE.|Purdue University,|of Medicine, St. John| , West Hill.|, 0 4 0 E. Seneca St.|-72,", "" )
consolidated$degree<-str_replace(consolidated$degree,"120 Cascadilla|Heustis St|Esq.|Brown|University of Maine|JR.", "" )
consolidated$college<-str_replace(consolidated$college, "Sibley College", "")
consolidated$college<-str_replace(consolidated$college, "Conciliar Seminary,|Royal College", "Conciliar College")
consolidated$college<-str_replace(consolidated$college, "Massachusett Institute", "Massachusett Institute of Technology")
consolidated$Position<-str_replace(consolidated$Position, "University Avenue,|Dryden Road,|Leland Stanford Univ., |57 Cascadilla,|, Troy, , |, KY, 100 Cascadilla.|, , , KA. University Avenue. (Absent in Europe.)|, , Cortland| St. John, Porto-Rico|. KA.| of San Lldefonso(Salamanca Jurisdiction),", "")
consolidated$college<-str_replace(consolidated$college, "the College", "")
consolidated$Position<-str_replace(consolidated$Position, "Mechanician in", "Mechanician in the College")
consolidated$Position<-str_replace(consolidated$Position, "Director of", "Director of Sibley College")

#Make sure that Cornell is listed as the college for all of the people who went there
consolidated$college <-ifelse(grepl("Yes", consolidated$Cornell), 'Cornell', consolidated$college);

#Fill in the rest of the colleges, edit some formatting and grab ones that weren't gotten in the original list for whatever reason
consolidated$Cornell<-ifelse(is.na(consolidated$college), 'No', consolidated$Cornell)
consolidated$college<-str_replace(consolidated$college, "Stevens|Stevens Institute of Technology", "Stevens Institute of Technology")
consolidated$Position<-str_replace(consolidated$Position, "Karlsruhe Polytechnic (Germany)|Jr.|Western|State|M.C.E., , 1894| of Medicine, St. John|VI.E.,|; M.M.E., 1913|Leland|Dartmouth", "")

#fix some more degree and Position issues (mostly just aesthetic cleaning and correcting wonky values)
consolidated$Position<-ifelse(grepl("Instructor in Industrial Art", consolidated$degree), 'Instructor in Industrial Art', consolidated$Position);
consolidated$Position<-ifelse(grepl("Instructor in Industrial Drawing and Art", consolidated$degree), 'Instructor in Industrial Drawing and Art', consolidated$Position);
consolidated$Position<-ifelse(grepl("Instructor in Drawing", consolidated$degree), 'Instructor in Drawing', consolidated$Position);
consolidated$degree<-str_replace(consolidated$degree, " , 1899, First Siblev Prize, 1890, Member A.S.M.E.|Washington and Jefferson|Instructor in Drawing|Instructor in Industrial Drawing and Art|Instructor in Industrial Art", "")
consolidated$college<-ifelse(grepl("M.I.T", consolidated$degree), 'M.I.T', consolidated$college)
consolidated$degree<-ifelse(grepl("M.I.T", consolidated$degree), ' ', consolidated$degree)
consolidated$degree<-ifelse(grepl("U. S. N. Engineer", consolidated$degree), ' ', consolidated$degree)
consolidated$degree<-ifelse(grepl("U. S. Assist. Eng'r", consolidated$degree), ' ', consolidated$degree)
consolidated$Position<-ifelse(grepl("CHARLES DAVID MARX", consolidated$First_Name), 'Assistant Professor of Civil Engineering, In Charge of the Graphics of Engineering', consolidated$Position)


#fix spelling bug from original data :)
consolidated$Position<-ifelse(grepl("Dartmout", consolidated$college), 'Professor of Mechanical Engineering', consolidated$Position)

#Some more cleaning, of irrelevant information in the position and degree columns
consolidated$Position<-str_replace(consolidated$Position, "Author of The Manufacturing Interests of New York City, Water Consumption of New York City.'; Report to the American Government on the Practicability of a Ship Canal across the American Isthmus, etc., 170 E,  St.|Professor of Civil Engineering,  , Contributor to Van Ostrand's Engineering Magazine, and the Analyist, 105 Cascadilla Place.|of San Lldefonso(Salamanca Jurisdiction),|. Engineer-in-Chief of the U.S. Expedition to Tehauntepec and Nicarauga, Member of Royal Academy of Letters, Royal Economic Society, Society of Geography and Statistics, American Society of Civil Engineers, etc.", "")
consolidated$Position<-str_replace(consolidated$Position, "of San Lldefonso", "")
consolidated$Position<-str_replace(consolidated$Position, "(Salamanca Jurisdiction)| Member of Loyal Legion, Am. Soc. Mech. Engineers, etc., Am., Brit., French, German, Austrian, etc., Societies of Engineers, and of various Academies of Science.|Göttingen, Fellow of the A. A. A. S. Member of the Kansas Academy of Science, of the Am. Inst. of Elect, Engineers, of the National Electric Light Association.|, , Member American Society of Electrical Engineers, Honorary member Buffalo Electrical Society.|Ass't Eng., Boston, New York and Montreal R. R. Aid U. S. Coast Survey, 1878,  Member Am. Soc. Civil Engs.|", "")



#CREATE MEMBERSHIP COLUMN

consolidated$Professional_organization<-ifelse(grepl("Member", consolidated$Position), 'Yes',"")
consolidated$Professional_organization<-ifelse(grepl(" M.A.S.C.E.|A.S.H|A. S. C. E.|A. S. M. E.|A. I. M. E.|A. A. A. S.|B. A. A. S.|M. A. S. C. E.|M.A.I.M.E.|M.A.A.A.S.|A.M.A.S.C.E.|F.A.A.A.S.|A.M.A.I.E.E.|A.M.S.|A.R.M.M.A.|M.C.B.A.|A.S.M.E.|M.A.S.M.E.|A.A.S.C.E.|A.I.E.E.|A.S.I.R.A.", consolidated$degree), 'Yes', consolidated$Professional_organization)
consolidated$Professional_organization<-ifelse(grepl(" M.A.S.C.E.|A.S.H|F.E.|A. S. C. E.|A. S. M. E.|A. I. M. E.|A. A. A. S.|B. A. A. S.|M. A. S. C. E.|M.A.I.M.E.|M.A.A.A.S.|A.M.A.S.C.E.|F.A.A.A.S.|A.M.A.I.E.E.|A.M.S.|A.R.M.M.A.|M.C.B.A.|A.S.M.E.|M.A.S.M.E.|A.A.S.C.E.|A.I.E.E.|A.S.I.R.A.", consolidated$Position), 'Yes', consolidated$Professional_organization)
consolidated$Position<-ifelse(grepl(" M.A.S.C.E.|A.S.H|F.E.|A. S. C. E.|A. S. M. E.|A. I. M. E.|A. A. A. S.|B. A. A. S.|M. A. S. C. E.|M.A.I.M.E.|M.A.A.A.S.|A.M.A.S.C.E.|F.A.A.A.S.|A.M.A.I.E.E.|A.M.S.|A.R.M.M.A.|M.C.B.A.|A.S.M.E.|M.A.S.M.E.|A.A.S.C.E.|A.I.E.E.|A.S.I.R.A.", consolidated$Position), ' ', consolidated$Position)
consolidated$Professional_organization<-consolidated$Professional_organization<-ifelse(grepl("Society", consolidated$Position), 'Yes',"")
consolidated$degree<-ifelse(grepl(" M.A.S.C.E.|A.S.H|M. A. S. C. E.|A. S. C. E.|A. S. M. E.|A. I. M. E.|A. A. A. S.|B. A. A. S|M.A.I.M.E.|M.A.A.A.S.|A.M.A.S.C.E.|F.A.A.A.S.|A.M.A.I.E.E.|A.M.S.|A.R.M.M.A.|M.C.B.A.|A.S.M.E.|M.A.S.M.E.|A.A.S.C.E.|A.I.E.E.|A.S.I.R.A.", consolidated$degree), ' ', consolidated$degree)
consolidated$Position<-ifelse(grepl("Member", consolidated$Position), ' ', consolidated$Position)
consolidated$Position<-ifelse(grepl("Society", consolidated$Position), '', consolidated$Position)
consolidated$Position<-ifelse(grepl("A.S.M.E., 1894|  Univ., |Past Assistant Engineer|Sphinx Head, Vereines Deutscher Engenieure.|President|London|Edinburgh|Columbia, , Hibbert Traveling Fellow in , 1878-80,  Honorary Fellow of , London.|KA|Bloomsburg N.S., Pa.,| Iowa |of Technology|, St. Petersburg, Russia, |and V.E.|University|M.C.E., 1878|E E.|Clark,|and B.S. in Mining", consolidated$Position), ' ', consolidated$Position)

#Degree and position cleaning - making sure that only correct values are in these columns, and deleting excess information missed by regex
consolidated$degree<-str_replace(consolidated$degree, "Cornell, 1888, Member:|, 1903, Junior Member ", "")
consolidated$Position<-str_replace(consolidated$Position, ",  University Avenue.|Absent in Europe.|Missouri River Improvement: -83|Karlsruhe Polytechnic|Germany|U. S. Assist. Eng'r in charge Osceola Div. Mississippi River Improvement: -84.|of San Ildefonso|Stevens,|Franklin Inst., American Inst.,Ofﬁcier de I'Instruction Publique de France, Royal Soc. Sweden, Brit. Inst. N. A., Scottish Inst. Shipbuilders. and Engineers., Verein Deutscher Ing., Assoc. des Ing. Civils de France, Oest. Ver. Archt.", "")
consolidated$Position<-ifelse(grepl("THURSTON", consolidated$First_Name), 'Director of Sibley College, Professor of Mechanical Engineering', consolidated$Position)
consolidated$Position<-ifelse(grepl("Fellow at , 1886-7|Met.E.|Salamanca Jurisdiction|Agricultural College", consolidated$Position), ' ', consolidated$Position)
consolidated$degree<-ifelse(grepl("D.Sc.", consolidated$Position), 'D.Sc.', consolidated$degree)
consolidated$Position<-ifelse(grepl("D.Sc.|U. S. Navy|Iowa , |B.S.|retired", consolidated$Position), ' ', consolidated$Position)
consolidated$Position<-ifelse(grepl("SCHURMAN", consolidated$First_Name), 'President', consolidated$Position)
consolidated$college<-ifelse(grepl("GEORGE HUGH SHEPARD", consolidated$First_Name), 'U.S. Naval Academy', consolidated$college)
consolidated$Position<-ifelse(grepl("GEORGE HUGH SHEPARD", consolidated$First_Name), 'Instructor in Machine Design', consolidated$Position)
consolidated$degree<-ifelse(grepl("GEORGE HUGH SHEPARD", consolidated$First_Name), 'M.E.', consolidated$degree)
consolidated$Position<-ifelse(grepl("JOHN SIMPSON REID|JOHN S. REID", consolidated$First_Name), 'Instructor in Mechanical Drawing and Design', consolidated$Position)
consolidated$degree<-str_replace(consolidated$degree, "Instructor in Mechanical Drawing and Design", " ")
consolidated$Position<-ifelse(grepl("DAVID REID", consolidated$First_Name), 'Instructor in Drawing and Designing in Sibley College', consolidated$Position)
consolidated$Position<-ifelse(grepl("BISSELL", consolidated$First_Name), 'Instructor in Sibley College', consolidated$Position)
consolidated$degree<-ifelse(grepl("C.E.", consolidated$Position), 'C.E.', consolidated$degree)
consolidated$Position<-str_replace(consolidated$Position, "C.E.", "")
consolidated$degree<-ifelse(grepl("M.E., Cornell", consolidated$Position), 'M.E.', consolidated$degree)
consolidated$Position<-str_replace(consolidated$Position, "M.E., Cornell", "")
consolidated$degree<-ifelse(grepl("ERNEST WILLIAM SCHODER", consolidated$First_Name), 'B.S.', consolidated$degree)
consolidated$secondDegree<-ifelse(grepl("ERNEST WILLIAM SCHODER", consolidated$First_Name), 'Ph.D.', consolidated$secondDegree)
consolidated$Position<-ifelse(grepl("ERNEST WILLIAM SCHODER", consolidated$First_Name), 'Assistant Professor of Experimental Hydraulics', consolidated$Position)
consolidated$secondDegree<-ifelse(grepl("ADELBERT PHILO MILLS", consolidated$First_Name), 'B.S.', consolidated$secondDegree)
consolidated$Position<-ifelse(grepl("Professor of Structural Design", consolidated$degree), 'Professor of Structural Design', consolidated$Position)
consolidated$degree<-ifelse(grepl("Professor of Structural Design", consolidated$degree), ' ', consolidated$degree)
consolidated$Professional_organization<-ifelse(grepl("GEORGE ROBERT MCDERMOTT", consolidated$First_Name), 'Yes', consolidated$Professional_organization)
consolidated$college<-ifelse(grepl("Institute of Ways of Communication", consolidated$Position), 'Institute of Ways of Communication', consolidated$college)
consolidated$Position<-str_replace(consolidated$Position, "Institute of Ways of Communication, Saint Petersburg, Russia,", " ")
consolidated$gradyear<-ifelse(grepl("FRANK GIRARD TAPPAN", consolidated$First_Name), '1909', consolidated$gradyear)
consolidated <- consolidated[-c(968), ]
consolidated$Position<-str_replace(consolidated$Position, "the Col-", "")
consolidated$degree<-str_replace(consolidated$degree, "M.A.S.C.E.", "")

#Remove all extra commas
consolidated$Position <- str_replace(consolidated$Position,"[,]","")

#Re-order and name the columns, for a cleaner "master list"
colnames(consolidated)[colnames(consolidated) == 'First_Name'] <- 'Name'
colnames(consolidated)[colnames(consolidated) == 'gradyear'] <- 'Grad_year'
colnames(consolidated)[colnames(consolidated) == 'degree'] <- 'Degree'
colnames(consolidated)[colnames(consolidated) == 'college'] <- 'College'

consolidated <- consolidated %>% relocate(Name, Last_Name, .after = Year)
consolidated <- consolidated %>% relocate(secondDegree, .after = Degree)
consolidated <- consolidated %>% relocate(College, .after = secondDegree)




#INSERTED BY PROF. SOLARES: cleaning and counting multiple degrees  
#consolidated2<-separate_rows(consolidated, secondDegree, sep="(?<=^)")
#consolidated2$secondDegree[which(consolidated2$secondDegree=="")]<-NA
#consolidated2$degree<-ifelse(is.na(consolidated2$secondDegree), consolidated2$Degree, consolidated2$secondDegree)
#consolidated2$secondDegree<-NULL
#consolidated2<-subset(consolidated2, !is.na(consolidated2$Degree)&(consolidated2$Degree!="")&(consolidated2$Degree!=" "))
#consolidated2<-consolidated2[order(consolidated2$Name, consolidated2$Grad_year)]
#for(i in 2:nrow(consolidated2)){
#  if(is.na(consolidated2$Grad_year[i])&!is.na(consolidated2$Grad_year[i-1])&(consolidated2$Name[i]==consolidated2$Name[i-1])){
#    consolidated2$Grad_year[i]<-consolidated2$Grad_year[i-1]
#  }else{}
#}
#consolidated3<-distinct(consolidated2, First_Name, degree, gradyear, .keep_all=T)
#simplertable<-ddply(consolidated3, c("First_Name", "Year","gradyear"), summarize, degrees=length(degree))





#create simpleprof table - takes all of the distinct firstnames
simpleprof<-distinct(consolidated, Name, Year,.keep_all=T)

#format a table to allow for easy creation of visualization
Professors<-ddply(simpleprof, 
                 c("Year"), 
                 summarize, 
                 profesors=length(Year), 
                 students=mean(as.numeric(Students)), 
                 e_Students=mean(as.numeric(e_Students)))



#Rename a few columns
colnames(Professors)[colnames(Professors) == 'profesors'] <- 'Professors'
colnames(Professors)[colnames(Professors) == 'e_Students'] <- 'Engineering_Students'
colnames(Professors)[colnames(Professors) == 'students'] <- 'Students'

#Create a column with the ratio of students to professors
Professors$student_faculty_ratio<-Professors$Engineering_Students/Professors$Professors

#Create some temporary tables in order to do visualizations 1b and 1c 
tabled<-pivot_longer(subset(Professors, select=c("Year","Students", "Engineering_Students")), 
                            cols=c("Students", "Engineering_Students"), names_to = "Type", values_to="Number")

tablec<-pivot_longer(subset(Professors, select=c("Year","Professors", "Engineering_Students")), 
                     cols=c("Professors", "Engineering_Students"), names_to = "Type", values_to="Number")


##### VISUALIZATION 2: RETURNING VS. NEW FACULTY ####

#Split the list of unique professors into different lists for each year
year_prof_lists <- split(simpleprof, f = simpleprof$Year) 
split_1875<-data.frame(year_prof_lists[1])
split_1875<- subset(split_1875, select=c("X1875.Last_Name", "X1875.Year"))
split_1880<-data.frame(year_prof_lists[2])
split_1880<- subset(split_1880, select=c("X1880.Last_Name", "X1880.Year"))
split_1885<-data.frame(year_prof_lists[3])
split_1885<- subset(split_1885, select=c("X1885.Last_Name", "X1885.Year"))
split_1890<-data.frame(year_prof_lists[4])
split_1890<- subset(split_1890, select=c("X1890.Last_Name", "X1890.Year"))
split_1895<-data.frame(year_prof_lists[5])
split_1895<- subset(split_1895, select=c("X1895.Last_Name", "X1895.Year"))
split_1900<-data.frame(year_prof_lists[6])
split_1900<- subset(split_1900, select=c("X1900.Last_Name", "X1900.Year"))
split_1905<-data.frame(year_prof_lists[7])
split_1905<- subset(split_1905, select=c("X1905.Last_Name", "X1905.Year"))
split_1910<-data.frame(year_prof_lists[8])
split_1910<- subset(split_1910, select=c("X1910.Last_Name", "X1910.Year"))
split_1915<-data.frame(year_prof_lists[9])
split_1915<- subset(split_1915, select=c("X1915.Last_Name", "X1915.Year"))


#Determine which professors appeared on the list of the year before
split_1880$Status <- as.integer(split_1880$X1880.Last_Name %in% split_1875$X1875.Last_Name)
split_1885$Status <- as.integer(split_1885$X1885.Last_Name %in% split_1880$X1880.Last_Name)
split_1890$Status <- as.integer(split_1890$X1890.Last_Name %in% split_1885$X1885.Last_Name)
split_1895$Status <- as.integer(split_1895$X1895.Last_Name %in% split_1890$X1890.Last_Name)
split_1900$Status <- as.integer(split_1900$X1900.Last_Name %in% split_1895$X1895.Last_Name)
split_1905$Status <- as.integer(split_1905$X1905.Last_Name %in% split_1900$X1900.Last_Name)
split_1910$Status <- as.integer(split_1910$X1910.Last_Name %in% split_1905$X1905.Last_Name)
split_1915$Status <- as.integer(split_1915$X1915.Last_Name %in% split_1910$X1910.Last_Name)


#Rename columns so they match accross years
colnames(split_1880)[colnames(split_1880) == 'X1880.Last_Name'] <- 'LASTNAME'
colnames(split_1885)[colnames(split_1885) == 'X1885.Last_Name'] <- 'LASTNAME'
colnames(split_1890)[colnames(split_1890) == 'X1890.Last_Name'] <- 'LASTNAME'
colnames(split_1895)[colnames(split_1895) == 'X1895.Last_Name'] <- 'LASTNAME'
colnames(split_1900)[colnames(split_1900) == 'X1900.Last_Name'] <- 'LASTNAME'
colnames(split_1905)[colnames(split_1905) == 'X1905.Last_Name'] <- 'LASTNAME'
colnames(split_1910)[colnames(split_1910) == 'X1910.Last_Name'] <- 'LASTNAME'
colnames(split_1915)[colnames(split_1915) == 'X1915.Last_Name'] <- 'LASTNAME'
colnames(split_1880)[colnames(split_1880) == 'X1880.Year'] <- 'YEAR'
colnames(split_1885)[colnames(split_1885) == 'X1885.Year'] <- 'YEAR'
colnames(split_1890)[colnames(split_1890) == 'X1890.Year'] <- 'YEAR'
colnames(split_1895)[colnames(split_1895) == 'X1895.Year'] <- 'YEAR'
colnames(split_1900)[colnames(split_1900) == 'X1900.Year'] <- 'YEAR'
colnames(split_1905)[colnames(split_1905) == 'X1905.Year'] <- 'YEAR'
colnames(split_1910)[colnames(split_1910) == 'X1910.Year'] <- 'YEAR'
colnames(split_1915)[colnames(split_1915) == 'X1915.Year'] <- 'YEAR'

#Bind all of the year columns back together
Return <- rbind.fill(split_1880, split_1885, split_1890, split_1895, split_1900, split_1905, split_1910, split_1915)
#Label return status
Return$Status<-ifelse(grepl(1, Return$Status), 'Returning', 'New')
#Count number of each return status by year
RETURN_PROFS <- count(Return, c("YEAR", "Status"))


#### VISUALIZATION 3: DISTRIBUTION OF DEPARTMENTS ####
##Extract department information from the consolidated table
consolidated$Department<-str_extract(tolower(consolidated$Position), "mechanical|bridge|field|mechanics|hydraulics|power|surveying|structural|drawing|experimental engineering|electrical|civil|sanitary|naval|physics|marine|steam|railway|mining")%>%str_to_title()
titles<-subset(consolidated, select=c("Year","Name", "Department"))
titles<-subset(titles, !is.na(titles$Department))
titles<-distinct(titles, Year, Name, .keep_all = T)

#Create a table with the frequenies of different engineering departments by year
Department<-count(titles, c("Year", "Department"))
Department<-Department[which(!is.na(Department$Department)),]


#### VISUALIZATION 4: LEVEL OF FACULTY ####

##Make a new table with all of the individuals, sorted by level
consolidated$Level<-str_extract(tolower(consolidated$Position), "professor|special|director|president|foreman|instructor|mechanician|scholar|superindendent|secretary|clerk|librarian|stenographer|assistant in|lecturer")%>%str_to_title()
levels<-subset(consolidated, select=c("Year", "Name", "Level"))
levels <-arrange(levels, Level, is.na(Level))

#Remove duplicates
levels <-distinct(levels, Year, Name, .keep_all=T)

#Fix a few code bus
levels$Level <-ifelse(grepl("FUERTES", levels$Name), 'Professor', levels$Level)
levels$Level <-ifelse(grepl("IRVING", levels$Name), 'Professor', levels$Level)

#Based on position, assign appropriate Type value to each individual
levels$Type <-ifelse(grepl("Director|Professor|President", levels$Level), 'Professor/Administrative', "")
levels$Type <-ifelse(grepl("Instructor|Lecturer|Scholar", levels$Level), 'Instructor/Lecturer', levels$Type)
levels$Type <-ifelse(grepl("Assistant In|Special|Foreman|Superintendent|Secretary|Clerk|Librarian|Mechanician|Stenographer", levels$Level), 'Lab/Shop/Support Staff', levels$Type)
levels$Type <-ifelse(is.na(levels$Level), 'Lab/Shop/Support Staff', levels$Type)

#Count the frequency of each type, by year
Levels<-count(levels, c("Year", "Type"))

#### SPECIAL SECTION: ACCOUNTING FOR THE TWO GUEST LECTURER LISTS ####

##Consolidate lists of guest lecturers
Y_1890_g$Year<-1890
Y_1895_g$Year<-1895
consolidated_guests<-rbind.fill(Y_1890_g, Y_1895_g)
consolidated_guests$guest<-"Yes"
guests<-count(consolidated_guests, c("guest", "Year"))


#### VISUALIZATION 5: NETWORKS OF EXPERTISE

##calculate number in a professional organization
prof_org<-subset(consolidated, select=c("Year","Name", "Professional_organization"))
prof_org$Professional_organization[which(prof_org$Professional_organization=="")]<-"No"
prof_org$Name[which(prof_org$Name=="lege Dean of the Faculty and Professor of Sanitary Engineering")]<-"ESTEVAN ANTONIO FUERTES"
prof_org <-arrange(prof_org, Professional_organization, is.na(Professional_organization))
prof_org <- prof_org[-c(182), ]
PROFORG<-count(prof_org, c("Year", "Professional_organization"))


##Consolidated number who went to Cornell
cornell_profs<-subset(consolidated, select=c("Year", "Name", "Cornell"))
cornell_profs$Cornell<-ifelse(is.na(cornell_profs$Cornell), "No", cornell_profs$Cornell)
cornell_profs$sort<-ifelse(grepl("Yes", cornell_profs$Cornell), 1, 2)
cornell_profs<-cornell_profs[with(cornell_profs, order(sort, Year)), ] 
cornell_profs<-distinct(cornell_profs, Name, Year, .keep_all=T)
C_PROFS<-count(cornell_profs, c("Year", "Cornell"))



#### EXPORTING MAIN DATA FRAMES AS CSV ####

#EXPORT AS CSV
write.csv(consolidated,"ConsolidatedData.csv", row.names = FALSE)
write.csv(Department, "Departments.csv", row.names = FALSE)
write.csv(RETURN_PROFS,"Returnprofs.csv", row.names = FALSE)
write.csv(Professors,"Professors.csv", row.names = FALSE)
write.csv(PROFORG,"PROFORG.csv", row.names = FALSE)
write.csv(guests,"guests.csv", row.names = FALSE)
write.csv(Levels,"Levels.csv", row.names = FALSE)
write.csv(C_PROFS,"CPROFS.csv", row.names = FALSE)




