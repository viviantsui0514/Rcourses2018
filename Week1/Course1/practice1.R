my.height.cm <- 161
my.weight.kg <- 53
my.height.m <- my.height.cm / 100
my.bmi <- my.weight.kg / (my.height.m) ^ 2

if (my.bmi >= 35) {
  print(paste("Your bmi: ", my.bmi))
  print("重度肥胖!")
} else if (my.bmi >= 30) {
  print(paste("Your bmi: ", my.bmi))
  print("中度肥胖!")
} else if (my.bmi >= 27) {
  print(paste("Your bmi: ", my.bmi))
  print("輕度肥胖!")
} else if (my.bmi >= 24) {
  print(paste("Your bmi: ", my.bmi))
  print("過重!")
} else if (my.bmi >= 18.5) {
  print(paste("Your bmi: ", my.bmi))
  print("正常範圍")
} else {
  print(paste("Your bmi: ", my.bmi))
  print("過輕!")
}
