## declaring global variables



totalWorkTime = vector()
classTimeIndex = vector()
numClasses = vector()
expectedTime = vector()
workDiscrepancy = vector()
freshCount = sum(woRkload$grade==9)/8
freshIDs = woRkload[woRkload[,2]==9, 1][1:freshCount]
sophCount = sum(woRkload$grade==10)/8
sophIDs = woRkload[woRkload[,2]==10, 1][1:sophCount]
junCount = sum(woRkload$grade==11)/8
junIDs = woRkload[woRkload[,2]==11, 1][1:junCount]
senCount = sum(woRkload$grade==12)/8
senIDs = woRkload[woRkload[,2]==12, 1][1:senCount]

# defining worktime/classtime index/number of classes per student

for (i in 1:max(woRkload$studentNum)) {
  totalWorkTime[i] = sum(na.omit(woRkload[woRkload[,1]==i,5]))
  numClasses[i] = sum(!is.na(woRkload$workTime[which(woRkload$studentNum==i)]))
  classTimeIndex[i]= sum(classonetype[which(woRkload$studentNum==i)])
  expectedTime[i] = numClasses[i]*45 + 15*classTimeIndex[i]
  workDiscrepancy[i] = totalWorkTime[i] - expectedTime[i]
}

# defining course indices for compareWorkloadClass()

classonetype <- vector()
freshClasses = c("Principles of Scientific Inquiry", "English 1", "Roots of the Modern World", "Algebra 1", "Algebra/Geometry")
for (i in 1:nrow(woRkload)) {
  if (woRkload$className[i] == "Skills for Academic Success") {
    classonetype[i] = -3
  } else if (is.element(woRkload$className[i], freshClasses)) {
    classonetype[i] = -1
  } else if (is.na(woRkload$className[i])) {
    classonetype[i] = NA
  } else if (length(grep("Honor",woRkload$className[i],fixed = TRUE)) == 0 && length(grep("AP",woRkload$className[i],fixed = TRUE)) == 0 && length(grep("GAATPH",woRkload$className[i],fixed = TRUE)) == 0) {
    classonetype[i] = 0
  } else {
    classonetype[i] = 1
  }
}




## plots workload distributions for arbitrary selection of classes ##

classWorkDist = function(clNames, graphType, graphTitle) {
  
  dummyVector = list()
  
  for (i in 1:length(clNames)) {
    dblDummyVector = vector()
    for (j in 1:nrow(woRkload)) {
      if (woRkload$className[j] == clNames[i]) {
        dblDummyVector[j] = woRkload$workTime[j]
      }
    }
    dummyVector[[i]] = na.omit(dblDummyVector)
  }
  
  
  if (length(dummyVector) == 1) {
    workClassTitle = paste("Workload for", clNames[1], "(n =", paste(length(dummyVector[[1]]), ")", sep = ""), sep = " ")
    if (graphType == "hist") {
      hist(dummyVector[[1]], main = workClassTitle)
    } else if (graphType == "boxplot") {
      boxplot(dummyVector, xlab = "time spent (minutes)", horizontal = TRUE)
      title(workClassTitle)
      print(summary(dummyVector[[1]]))
    } else {
      return("only hist or boxplot functionality")
    }
  } else if (graphType == "boxplot") {
    if (graphTitle == "potato") {
      workClassTitle = paste("Workload Distribution for", length(dummyVector), "classes", sep = " ")
    } else {
      workClassTitle = paste("Workload Distribution for", graphTitle, sep = " ")
    }
    boxplot(dummyVector, names = clNames, las = 1, xlab = "time spent (minutes)", horizontal = TRUE)
    title(workClassTitle)
  }
  
  
  
}


## workload per student ##
workByStudent = function(graphType) {
  anotherVector = vector()
  for (i in 1:max(woRkload$studentNum)) {
    anotherVector[i] = sum(na.omit(woRkload[woRkload[,1]==i,5]))
  }
  
  workStudentTitle = paste("Distribution of Workload per student", "n =", length(anotherVector), sep = " ")
  if (graphType == "hist") {
    hist(anotherVector, main = workStudentTitle, breaks = 30)
  } else if (graphType == "boxplot") {
    boxplot(anotherVector, xlab = "time spent (minutes)", horizontal = TRUE)
    title(workStudentTitle)
  } else {
    return("Please input a graph type.")
  }
  summary(anotherVector)
  
}

## workload by grade ##

workByGrade = function(vectInput) {
  workGradeContainer = vector()
  
  for (i in vectInput) {
    workGradeContainer[i] = sum(na.omit(woRkload[woRkload[,1]==i, 5]))
  }
  
  boxplot(workGradeContainer, horizontal = TRUE)
  workGradeTitle= switch(deparse(substitute(vectInput)), freshIDs = "Workload for Freshmen", sophIDs = "Workload for Sophomores", 
         junIDs = "Workload for Juniors", senIDs = "Workload for Seniors")
  title(workGradeTitle)
    
}

## workload for each grade plotted concurrently ##

workAllGrades = function() {
  freshTime = totalWorkTime[freshIDs]
  sophTime = totalWorkTime[sophIDs]
  junTime = totalWorkTime[junIDs]
  senTime = totalWorkTime[senIDs]
  
  
  boxplot(senTime, junTime, sophTime, freshTime, names = c("12th", "11th", "10th", "9th"), xlab = "Total time spent (min)", horizontal = TRUE)
  title("Workload per Student, Separated by Grade")
}

## workload discrepancy for each student ##

compareWorkload = function(graphType) {
  
  workStudentTitle = "Workload Discrepancy by Student"
  if (graphType == "hist") {
    hist(workDiscrepancy, main = workStudentTitle)
  } else if (graphType == "boxplot") {
    boxplot(workDiscrepancy, xlab = "time discrepancy from expected (min)", horizontal = TRUE)
    title(workStudentTitle)
  } else {
    return("Please input a proper graph type.")
  }
  
}

## workload discrepancy, separated by grade ##

compareWorkloadGrade = function() {
  freshDisc = workDiscrepancy[freshIDs]
  sophDisc = workDiscrepancy[sophIDs]
  junDisc = workDiscrepancy[junIDs]
  senDisc = workDiscrepancy[senIDs]

  workStudentTitle = "Workload Discrepancy by Grade"
  boxplot(senDisc, junDisc, sophDisc, freshDisc, names = c("12th", "11th", "10th", "9th"), xlab = "time discrepancy from expected (min)", horizontal = TRUE)
  title(workStudentTitle)
  
}

## workload discrepancies for arbitrary number of classes ##

compareWorkloadClass = function(clNames, graphType, meanVsMedian = "potato", graphTitle = "potato") {
  dummyVector = list()
  specClassIndex = vector()
  expectedVector = vector()
  statDiscrepancy = vector()
  for (i in 1:length(clNames)) {
    dblDummyVector = vector()
    classTrigger = FALSE
    for (j in 1:nrow(woRkload)) {
      if (woRkload$className[j] == clNames[i]) {
        dblDummyVector[j] = woRkload$workTime[j]
        if (!classTrigger) {
          specClassIndex[i] = classonetype[j]
          expectedVector[i] = 45 + 15*specClassIndex[i]
          classTrigger = TRUE
        }
      }
    }
    dummyVector[[i]] = na.omit(dblDummyVector)
    dummyVector[[i]] = dummyVector[[i]] - expectedVector[i]
    if (meanVsMedian == "mean") {
      statDiscrepancy[i] = mean(dummyVector[[i]])
    } else if (meanVsMedian == "median") {
      statDiscrepancy[i] = median(dummyVector[[i]])
    } else if (graphType == "barplot") {
      return("need mean or median spec for barplot")
    }
    
    
  }
  
  if (length(dummyVector) == 1) {
    workClassTitle = paste("Discrepancy for", clNames[1], "(n =", paste(length(dummyVector[[1]]), ")", sep = ""), sep = " ")
    if (graphType == "hist") {
      hist(dummyVector[[i]], main = workClassTitle)
    } else if (graphType == "boxplot") {
      boxplot(dummyVector, xlab = "time discrepancy from expected (minutes)", horizontal = TRUE)
      title(workClassTitle)
      print(summary(dummyVector[[1]]))
    } else if (graphType == "barplot") {
      barplot(statDiscrepancy, main = workClassTitle)
      print(statDiscrepancy)
    } else {
      return("only hist or boxplot functionality")
    }
  } else if (graphType == "boxplot") {
    if (graphTitle == "potato") {
      workClassTitle = paste("Workload Discrepancy for", length(dummyVector), "classes", sep = " ")
    } else {
      workClassTitle = paste("Workload Discrepancy for", graphTitle, sep = " ")
    }
    
    boxplot(dummyVector, names = clNames, las = 1, xlab = "time discrepancy from expected (minutes)", horizontal = TRUE)
    title(workClassTitle)
  } else if (graphType == "barplot") {
    if (graphTitle == "potato") {
      workClassTitle = paste("Workload Discrepancy for", length(dummyVector), "classes", sep = " ")
    } else {
      workClassTitle = paste("Workload Discrepancy for", graphTitle, sep = " ")
    }
    barplot(statDiscrepancy, names.arg = clNames, col = c("mediumseagreen", "olivedrab2"), main = workClassTitle, cex.names = min(1, 0.9^(length(clNames)-2)))
    print(statDiscrepancy)
  } else {
    return("only box/bar functionality")
  }
}