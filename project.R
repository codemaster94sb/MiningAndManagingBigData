# import data from csv
data = read.csv("crimes2017.csv")

# remove all the lines with NA
data.clean = na.omit(data)
# get summary of the field primary type
summary_primaryType = summary(data.clean$Primary.Type,maxsum = 10)
# calculate the percentage of each category
piepercent = paste(round(100*summary_primaryType/sum(summary_primaryType),2),"%")
# draw pie chart
pie(summary_primaryType,piepercent,col=rainbow((length(summary_primaryType))),main = "Distribution of Primary Type of Crimes")
# draw legend
legend("topright",names(summary_primaryType),cex=0.5,fill=rainbow(length(summary_primaryType)))


# get summary of community area
summary_communityArea = table(data.clean$Community.Area)
# sort areas according to number of crimes
summary_communityArea = sort(summary_communityArea,decreasing = TRUE)
# get the 20 areas with most number of cimes
summary_communityArea = summary_communityArea[1:20]
barplot(
  height = summary_communityArea, 
  names.arg = names(summary_communityArea),
  xlab = "Area Code",
  ylab = "Number of Crimes",
  main="Crimes for Each Area",
  col="yellow")


# get arrest rate for different type of crimes
arrest_type_true = rep(0,10)
arrest_type_true_percentage = rep(0,10)
for(i in 1:10){
  arrest_type_true[i] = dim(data.clean[data.clean$Arrest == "true" & data.clean$Primary.Type == names(summary_primaryType[i]),])[1]
  arrest_type_true_percentage[i] = paste(round(arrest_type_true[i]*100 / summary_primaryType[i],2),"%")
}
# draw pie chart
pie(arrest_type_true,arrest_type_true_percentage,col=rainbow((length(arrest_type_true))),main = "Arrest rate for different type of crimes")
# draw legend
legend("topright",names(summary_primaryType),cex=0.8,fill=rainbow(length(arrest_type_true)))


# draw crimes based on their severity
crime_type = rep(0,10)
crime-types_order = rep(0,10)

crime_types <- read.csv("crimes2017.csv")
plot(
  x=crime_types$Primary,
  main="Crime Visualisation",
  xlab="catogery of crime",
  ylab="Count of the crime")




summary_communityArea
dotchart(
  x=table(crime_types$Primary.Type),
  main="Crime Visualisation",
  ylab="category of crime",
  xlab="Count of the crime"
)
primtype <- table(crime_types$Primary.Type)

primtype<-primtype[1:10]
primtype <- factor(primetype)
primtype



primtype
primtype = primtype[1:5]
primtype = factor(primtype)

areas = areas[1:5]
areas

mtcars

areas = (1:5)
y_array = rep(0,15)
label_array = rep(0,15)
counter = 0
for (i in areas){
  tmp = data.clean[data.clean$Community.Area == i,]
  summary_tmp = summary(tmp$Primary.Type,maxsum=3)
  for(j in seq(1:3)){
    y_array[counter*3+j] = summary_tmp[j]
    label_array[counter*3+j] = names(summary_tmp[j])
  }
  counter = counter + 1
}

y_array
label_array

g_areas <- matrix(list(), nrows=5, ncols=3)
mat[[1,1]]
mat[[1,2]]
mat[[1,3]]
mat[[2,1]]
mat[[2,2]]
mat[[2,3]]
mat[[3,1]]
mat[[3,2]]
mat[[3,3]]
mat[[4,1]]
mat[[4,2]]
mat[[4,3]]
mat[[5,1]]
mat[[5,2]]
mat[[5,3]]
dotchart(
  x=y_array,
  labels=label_array,
  groups = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),
  xlab="Counts of Crimes",
  pch=16
)
