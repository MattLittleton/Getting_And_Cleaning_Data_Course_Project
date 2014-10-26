---
title: "README.MD"
author: "Matt Littleton"
date: "Sunday, October 26, 2014"
output: html_document
---

## Course Project - Getting and Cleaning Data

###Files in repo

README.MD
CODEBOOK.MD
run_analysis.R

###Insturctions

Download files listed in CodeBook.MD, and set working directory to "UCI HAR Dataset\test".  
Run run_analysis.R which will:  
  1. Process test data
  2. Process train data
  3. Merge test and train data
  4. Caculate the mean for of each variable for each subject and each activity
  5. Output a new file "data_final.txt" with the calculated means 