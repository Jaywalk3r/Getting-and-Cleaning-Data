library( dplyr)

getwd()



# Skip this section if uncompressed raw data is in working directory exactly as
#	it "unzips"

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	# The URL where the file is located

filePath = "./RawData.zip"

download.file( fileUrl, filePath, method = "curl")
	# download .zip file to working directory as RawData.zip

unzip( filePath,  exdir = "./")
	# uncompress raw data file
	
	
	
	
	

# load relevant data files into R data frames

	# Test Data

subject.test = read.table( "./UCI HAR Dataset/test/subject_test.txt")
	# 2947 x 1 matrix identifying subject by integer identifier

X.test = read.table( "./UCI HAR Dataset/test/X_test.txt")
	# 2947 x 561 matrix variable values

y.test = read.table( "./UCI HAR Dataset/test/y_test.txt")
	# 2947 x 1 matrix identifying acivities by integer 1,...,6



	# Training Data
	
subject.train = read.table( "./UCI HAR Dataset/train/subject_train.txt")
	# 7352 x 1

X.train = read.table( "./UCI HAR Dataset/train/X_train.txt")
	# 7352 x 561
	
y.train = read.table( "./UCI HAR Dataset/train/y_train.txt")
	# 7351 x 1



	# Variable and Activity labels
	
features = read.table( "./UCI HAR Dataset/features.txt", row.names = 1, stringsAsFactors = FALSE)
	# 561 x 1 variable names

activity.labels = read.table( "./UCI HAR Dataset/activity_labels.txt", row.names = 1, stringsAsFactors = FALSE)
	# 6 x 1 activity names
	


# Merge Data

subject.master = rbind( subject.train, subject.test)

colnames( subject.master) = "Subject"

subject.master$Subject = paste( "subject.", formatC( subject.master$Subject, width = 2, flag = "0"), sep = "")
	# prepend "subject." to integer subject identifier after adding leading zero
	#	to single digit integers.

subject.master$Subject = as.factor( subject.master$Subject)
	# convert subject strings to factors

X.master = rbind( X.train, X.test)
	# will provide variable names later, after using regular expression
	#	manipulation on features

y.master = rbind( y.train, y.test)

colnames( y.master) = "Activity"




# Extract mean and standard deviation variables from data


index.mean = grep( "mean()", features$V2, fixed = TRUE)
	# identify and save index values of variables representing means

index.std = grep( "std()", features$V2, fixed = TRUE)
	# identify and save index values of variables representing standard
	#	deviations

index = c( index.mean, index.std)

index = sort( index)
	# index is a vector containing the column indices of the variables of
	#	interest.

features.subset = features[ index,]
	# this vector will be used for column names of variables of interest after
	#	being modified with regular expressions. Note that features.subset is a
	#	vector, not a data frame.

X.subset = X.master[ , index]
	# this is a matrix of the values of our variables of interest






# Use descriptive activity names to name the activities in the data set

activity.labels$V2 = tolower( activity.labels$V2)
	# convert activity names to lowercase

for (i in 1 : nrow( activity.labels)) {

	y.master$Activity[ y.master$Activity == i] = activity.labels$V2[ i]
		# replace integer identifiers with descriptive character strings

}

y.master$Activity = as.factor( y.master$Activity)
	# convert descriptive character strings to factors




# Appropriately label the data set with descriptive variable names

features.subset = gsub( "^t", "time.", features.subset)

features.subset = gsub( "^f", "freq.", features.subset)

features.subset = gsub( "[(][)]", "", features.subset)
	# parentheses placed inside brackets because they are otherwise used in
	#	regex syntax

features.subset = gsub( "-", ".", features.subset)

features.subset = sub( "BodyBody", "Body", features.subset)
	# frequency magnitude variables appear to have extra "Body" in name
	
features.subset = sub( "std", "sd", features.subset)

colnames( X.subset) = features.subset



# Combine subject identifier, activity identifier and variable values into
#	single tidy data frame.


dataset = cbind( subject.master, y.master, X.subset)

tidy.data = group_by( dataset, Subject, Activity)

write.csv( tidy.data, "Tidy_Data_1.csv", row.names = FALSE)
	# save tidy.data as a .csv file in the working directory
	
write.table( tidy.data, "Tidy_Data_1.txt", row.names = FALSE)
	# # save tidy.data as a .txt file in the working directory

save( tidy.data, file = "tidy.data.1.Rdata")
	# save tidy.data as an .Rdata file in working directory





# Create a second, independent tidy data set with the average of each variable
#	for each activity and each subject

tidy.data.2 = summarize_each( tidy.data, "mean")

write.csv( tidy.data.2, "Tidy_Data_2.csv", row.names = FALSE)
	# save tidy.data.2 as a .csv file in the working directory

write.table( tidy.data.2, "Tidy_Data_2.txt", row.names = FALSE)
	# # save tidy.data.2 as a .txt file in the working directory

save( tidy.data.2, file = "tidy.data.2.Rdata")
	# save tidy.data.2 as a .Rdata file in the working directory











