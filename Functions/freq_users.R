#Function to identify the users with the top N classifications on zooniverse from a particular week.

#this function takes a data frame of zooniverse classifications and identifies the top N classifiers by week

freq_classifiers<-function(df){
	require(dplyr)
	require(stringr)
	require(lubridate)

#sort out  users by making user name a factor and determining the levels of the factor. Will do so by creating a new variable.

	df$userFactor <- as.factor(df$user_name)
	names<-levels(df$userFactor)

	#now group all the "not logged in" users into 		#one id category

	pattern<-"not-logged-in"
	replacement<-grep("not-logged-in",df$user_name)
	#identifies all rows with user name string that 	#includes not logged in

	#now reassign factor name
	df$user_name[replacement]<- "Not logged in"

	#now fix Erika's name since it is in the dataset 	#in 2 ways
	#UPDATE IN FUTURE TO CHECK IF NAMES ARE THERE 		#FIRST

	#Erika_Barthelmess needs to be grouped with 			#barthelmess

	change<- which(
		df$user_name == "barthelmess" |
		df$user_name=="Erika_Barthelmess")

	#And fix
	df$user_name[change]<-"Erika Barthelmess"
	#and now use to replace userFactor
	df$userFactor<-as.factor(df$user_name)
	names<- levels(df$userFactor)

	df$created_at<-ymd_hms(df$created_at)

	#make date into posix format with lubridate
	df$created_at<-ymd_hms(df$created_at)

	#Assign each day to a week of the year
	df$week<-isoweek(df$created_at)

	#and change to factor
	df$weekFactor<- as.factor(as.numeric(df$week))

	#add the date of the last day of the week, with 		#week ending on a Sunday.
	df$endDay<-strptime(
		paste(year(df$created_at),df$week,7),
		format = "%Y %U %u")

	#Breakdown by week but keep end date in df
	df$endDayFactor<-as.factor(df$endDay)

	#determine users by week
	UserByWeek<-df %>% group_by(
		endDayFactor,
		userFactor,
		week) %>% summarise(
		NumClass = length(classification_id),
	)

	#filter by week and to N max classifications per 	week to get final product.  Ask user to supply N

	top_classifiers<-as.integer(readline(
		prompt = "For each week, how many of the top classifers would you like to display? "))

	#And this is what we've been looking for: the 		#final product!

	MainContributers<-UserByWeek %>% group_by(week) %>% top_n(top_classifiers,NumClass)
}

#This works!


##function to limit the result of freq_users.R to folks who have posted at least 15 classifications.#Requires DF that results from freq_users function

at_least_15<-function(df){
#filter to include only those who have made at least 15 classificaitons.
	require(dplyr)
	meet_min<-filter(df, NumClass >= 15 )
	return(meet_min)
}

#Works!