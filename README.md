# This is the repo for our project so far.

##NOTE:
So as to avoid dealing with git merging, which we don't fully understand just yet, we're going to each write to our own files, then combine them into one file toward the end. So please, make a file with your name in it to code in.

## Enjoy

Also, if you want to have anything ignored by git, just put it in a folder named `ignore`. So for instance, we'll put our data in there. You will have to create the folder first (since git will ignore it).

#This file will house a sort of todo list, where we will brainstorm on our plan of attack.

##Part 1: Read the data into a language-native format.

    The downloaded data comes in the convenient format of a bunch of csv's. These can be easy read in with read.csv or read_csv.

    - Try and read in all data at once?
        -> No unique yearly identifier
            - Can make one; 'year' as chr identifier THIS WORKS
        -> May be too big for R
            - Won't know till we try
            - Looks like it should be fine

##Part 2: Asses (this is on purpose) the quality of the data

    - Which variables do we have for all the years?
    - Any variables that change by year?
    - What is the null character?
        -> "NULL"? => can read into R using read_csv and specify as NA
    - What kind of quality scores would apply?

##Part 3: Recode any variables of interest

    Start to look at picking variables of interest and transforming them to usable formats.
    
    - adding Year identifier to allow aggregation on years
