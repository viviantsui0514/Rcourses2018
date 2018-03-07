course.student.number <- c(1:30)
csn <- length(course.student.number)
course.student.grade <- sample(x = c(55:100), size = csn)
names(course.student.grade) <- course.student.number

csg.mean <- mean(course.student.grade)
csg.max <- max(course.student.grade)
csg.min <- min(course.student.grade)
csg.over.80 <- course.student.grade >= 80
csg.over.80

course.student.grade[csg.over.80]

print(paste("全班人數:", csn))
print(paste("全班平均：", csg.mean))
print(paste("全班最高：", csg.max))
print(paste("全班最低：", csg.min))

print(paste("高於80分總人數：", length(course.student.grade[csg.over.80])))
print(paste("高於80分座號：", names(course.student.grade[csg.over.80])))
