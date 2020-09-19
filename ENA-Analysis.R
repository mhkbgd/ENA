library(readxl)
library(rENA)

dataPath = file.choose()
enaData <- read_excel(toString(dataPath))
#selecting units for ENA
units = enaData[,c("course","groupid","username.y")]

#selecting conversation for ENA

conversation = enaData[,c("course","groupid", "username.y")]

#selecting code columns 
codes = enaData[,c('Q. Open', 'Q. Critical', 'Q. Verification', 'R. Statement', 'R. Argument',"R. Acceptance", "S. Info. & materials", 'C. Arg.', 'C. Evaluation', 'OT. Socializing', "PI. Task Management")]
head(codes)

accum = ena.accumulate.data(
  units = units,
  conversation = conversation,
  codes = codes,
  window.size.back = 10
)

set = ena.make.set(enadata = accum)

#group analysis# 

first.gruop.lineweights = as.matrix(set$line.weights$groupid$"865")
second.group.lineweights = as.matrix(set$line.weights$groupid$"866")
first.group.mean = as.vector(colMeans(first.gruop.lineweights))
second.group.mean = as.vector(colMeans(second.group.lineweights))


#points
first.group.points = as.matrix(set$points$groupid$`865`)
second.group.points = as.matrix(set$points$groupid$`866`)

#for all groups
lineweights = as.matrix(set$line.weights)
linesmean = as.vector(colMeans(lineweights))
allpoints = as.matrix(set$points)

#plots group netwroks
plotgroup <- function(title, mean, point){
  plot = ena.plot(set, scale.to = "network", title = title)
 # plot = ena.plot.group(plot, points = point, confidence.interval = "box", colors = c("blue"))
 # plot = ena.plot.points(plot, points = point, confidence.interval = "box", colors = c("blue"))
  plot = ena.plot.network(plot, network = mean*2)
  plot$plot
}

#compare two groups
comparegroup <- function(title, mean, point1, point2){
  plot = ena.plot(set, scale.to = "network", title = title)
  #plot = ena.plot.group(plot, points = point1, confidence.interval = "box", colors = c("red"))
#  plot = ena.plot.points(plot, points = point1, confidence.interval = "box", colors = c("red"))
  #plot = ena.plot.group(plot, points = point2, confidence.interval = "box", colors = c("blue"))
#  plot = ena.plot.points(plot, points = point2, confidence.interval = "box", colors = c("blue"))
  plot = ena.plot.network(plot, network = mean*2)
  plot$plot
}

comparegroup("Group 865 & 866",first.group.mean - second.group.mean, first.group.points, second.group.points)

plotgroup("Group 865",first.group.mean, first.group.points)
plotgroup("Group 866",second.group.mean, second.group.points)
#plotgroup("All groups",linesmean, allpoints)


# plot students 
first.student.lineweights = as.matrix(set$line.weights$username.y$`341101534`)

second.student.lineweights = as.matrix(set$line.weights$username.y$`341111714`)


plotstudent <- function(title, mean){
  plot = ena.plot(set, scale.to = "network", title = title)
  plot = ena.plot.network(plot, network = mean)
  plot$plot
}

plotstudent("Student 1097",first.student.lineweights)
plotstudent("Student 1087",second.student.lineweights)
plotstudent("Student 1097 & 1087",first.student.lineweights - second.student.lineweights)

