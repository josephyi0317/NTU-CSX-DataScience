### practice_1_question

# Craete variable called my.height.cm with your actual height in cm 
my.height.cm <- 180
  
  # Craete variable called my.weight.kg with your actual weight in kg
  my.weight.kg <- 66
  
  # Create my.height.m transfered by my.height.cm  
  my.height.m <- my.height.cm/ 100
  
  # Create my.bmi with BMI(Body Mass Index) formula
  my.bmi <- my.weight.kg / (my.height.m) ^ 2
  
  # Use if-else to print matched information
  if (my.bmi >= 35) {
    print(paste("Your bmi: ", my.bmi))
    print("���תέD!")
  } else if (my.bmi >= 30) {
    print(paste("Your bmi: ", my.bmi))
    print("���תέD!")
  } else if (my.bmi >= 27) {
    print(paste("Your bmi: ", my.bmi))
    print("���תέD!")
  } else if (my.bmi >= 24) {
    print(paste("Your bmi: ", my.bmi))
    print("�L��!")
  } else if (my.bmi >= 18.5) {
    print(paste("Your bmi: ", my.bmi))
    print("���`�d��")
  } else {
    print(paste("Your bmi: ", my.bmi))
    print("�L��!")
  }
  
    
  
  