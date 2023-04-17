library(dplyr)
library(rstatix)
make.ona.plot.six.pairs <- function(set, 
                          plot.name
                          # node_size_multiplier, 
                          # edge_size_multiplier
                          ){
  color <- c("red", "orange", "green", "blue", "purple", "grey")
  plot_all_pairs <- plot(set, title = paste0(plot.name," (all pairs)")) %>%
    edges(
      weights =set$line.weights,
      edge_color = c("black")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("grey")) 
    # units(
    #   points=set$points[set$points$Patient.Student.Pair == "Lucas & Rakshya" & set$points$model_unit == TRUE,],
    #   points_color = c("red"),
    #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    # units(
    #   points=set$points[set$points$Patient.Student.Pair == "Lucas & Ryleigh" & set$points$model_unit == TRUE,],
    #   points_color = c("orange"),
    #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    # units(
    #   points=set$points[set$points$Patient.Student.Pair == "Quan & Rakshya" & set$points$model_unit == TRUE,],
    #   points_color = c("green"),
    #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    # units(
    #   points=set$points[set$points$Patient.Student.Pair == "Quan & Ryleigh" & set$points$model_unit == TRUE,],
    #   points_color = c("blue"),
    #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    # units(
    #   points=set$points[set$points$Patient.Student.Pair == "Regina & Rakshya" & set$points$model_unit == TRUE,],
    #   points_color = c("purple"),
    #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    # units(
    #   points=set$points[set$points$Patient.Student.Pair == "Regina & Ryleigh" & set$points$model_unit == TRUE,],
    #   points_color = c("grey"),
    #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE)
  print(plot_all_pairs)
  
  # index <- 1
  # for(patient_student in unique(set$points$Patient.Student.Pair)){
  #   
  #   plot_Patient.Student.Pair <- plot(set, title = paste0(plot.name," (Patient.Student.Pair: ", patient_student,")")) %>%
  #     units(
  #       points=set$points[set$points$Patient.Student.Pair == patient_student & set$points$model_unit == TRUE,],
  #       points_color = c(color[index]),
  #       show_mean = FALSE, show_points = TRUE, with_ci = FALSE) %>%
  #     edges(
  #       weights =set$line.weights[set$line.weights$Patient.Student.Pair == patient_student & set$points$model_unit == TRUE,],
  #       # edge_size_multiplier = edge_size_multiplier,
  #       # edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
  #       # node_position_multiplier = node_position_multiplier,
  #       edge_color = c(color[index])) %>%
  #     nodes(
  #       # node_size_multiplier = node_size_multiplier,
  #       # node_position_multiplier = node_position_multiplier,
  #       self_connection_color = c(color[index]))
  #   index <- index+1
  #   print(plot_Patient.Student.Pair)
  # }
}

make.ona.plot.compare.two.students <- function(set, 
                          plot.name
                          # node_size_multiplier, 
                          # edge_size_multiplier
){
  color <- c("red", "blue")
  student_Rakshya <- plot(set, title = "ONA plot for Rakshya") %>%
    edges(
      weights = set$line.weights$Participant$Rakshya,
      edge_color = c("red")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("red")) %>%
    units(
      points=set$points[set$points$Participant == "Rakshya",],
      points_color = c("red"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(student_Rakshya)
  
  student_Ryleigh <- plot(set, title = "ONA plot for Ryleigh") %>%
    edges(
      weights = set$line.weights$Participant$Ryleigh,
      edge_color = c("blue")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("blue")) %>%
    units(
      points=set$points[set$points$Participant == "Ryleigh",],
      points_color = c("blue"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE)
  print(student_Ryleigh)
  
  # FirstGame and SecondGame subtracted plot
  sub <- plot(set, title = "Difference: Rakshya (red) vs Ryleigh (blue)") %>%
    units(
      points = set$points$Participant$Rakshya, 
      points_color = "red",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    units(
      points = set$points$Participant$Ryleigh, 
      points_color = "blue",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    edges(
      weights = (colMeans(set$line.weights$Participant$Rakshya) - colMeans(set$line.weights$Participant$Ryleigh)),
      edge_size_multiplier = 5,
      edge_arrow_saturation_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      edge_color = c("red","blue")) %>%
    nodes(
      node_size_multiplier = 0.5,
      # node_position_multiplier = node_position_multiplier,
      self_connection_color = c("red","blue"))
  print(sub)
  print(t.test(set$points$Participant$Rakshya$MR1, set$points$Participant$Ryleigh$MR1))
  data_rakshya <- as.data.frame(set$points$Participant$Rakshya$MR1) %>% 
    mutate(points = `set$points$Participant$Rakshya$MR1`, ID = "Rakshya") %>%
    select(-`set$points$Participant$Rakshya$MR1`)
  print(data_rakshya)
  data_ryleigh <- as.data.frame(set$points$Participant$Ryleigh$MR1) %>%
    mutate(points = `set$points$Participant$Ryleigh$MR1`, ID = "Ryleigh") %>%
    select(-`set$points$Participant$Ryleigh$MR1`)
  data <- rbind(data_rakshya, data_ryleigh)
  # Effect sizes for each pairwise t test
  print(cohens_d(data, points ~ ID))
}

make.ona.plot.compare.two.cases <- function(set, 
                                            plot.name
                                            # node_size_multiplier, 
                                            # edge_size_multiplier
){
  color <- c("orange", "purple")
  case_Quan <- plot(set, title = "ONA plot for Contact Tracing") %>%
    edges(
      edge_size_multiplier = 0.5,
      weights = set$line.weights[set$line.weights$cases == "Quan" & set$line.weights$model_unit == TRUE,],
      edge_color = c("orange")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("orange")) %>%
    units(
      points=set$points[set$points$cases == "Quan",],
      # points=set$points[set$points$cases == "Quan" & set$points$model_unit == TRUE,],
      points_color = c("orange"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(case_Quan)
  
  case_non_Quan <- plot(set, title = "ONA plot for Focused Exam") %>%
    edges(
      edge_size_multiplier = 0.5,
      weights = set$line.weights[set$line.weights$cases == "Non-Quan" & set$line.weights$model_unit == TRUE,],
      edge_color = c("purple")) %>%
    nodes(
      node_size_multiplier = 0.5,
      self_connection_color = c("purple")) %>%
    units(
      points=set$points[set$points$cases == "Non-Quan",],
      # points=set$points[set$points$cases == "Non-Quan" & set$points$model_unit == TRUE,],
      points_color = c("purple"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE)
  print(case_non_Quan)
  
  # Difference plot
  sub <- plot(set, title = "Difference: Focused Exam (purple) vs Contact Tracing (orange) ") %>%
    units(
      points = set$points[set$points$cases == "Quan",], 
      # points = set$points[set$points$cases == "Quan" & set$points$model_unit == TRUE,], 
      points_color = "orange",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    units(
      points = set$points[set$points$cases == "Non-Quan",], 
      # points = set$points[set$points$cases == "Non-Quan" & set$points$model_unit == TRUE,], 
      points_color = "purple",
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
    edges(
      edge_size_multiplier = 0.5,
      weights = (colMeans(set$line.weights[set$line.weights$cases == "Quan" & set$line.weights$model_unit == TRUE,]) - colMeans(set$line.weights[set$line.weights$cases == "Non-Quan" & set$line.weights$model_unit == TRUE,])),
      # edge_size_multiplier = 3,
      # edge_arrow_saturation_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      edge_color = c("orange","purple")) %>%
    nodes(
      node_size_multiplier = 0.5,
      # node_position_multiplier = node_position_multiplier,
      self_connection_color = c("orange", "purple"))
      # self_connection_color = c("orange", "purple"))
  print(sub)
  print(t.test(set$points[set$points$cases == "Quan" & set$points$model_unit == TRUE,]$MR1, set$points[set$points$cases == "Non-Quan" & set$points$model_unit == TRUE,]$MR1))
  data_quan <- as.data.frame(set$points[set$points$cases == "Quan" & set$points$model_unit == TRUE,]$MR1) %>% 
    mutate(points = set$points[set$points$cases == "Quan" & set$points$model_unit == TRUE,]$MR1, ID = "Quan") %>%
    select(-c(1))
  print(data_quan)
  data_non_quan <- as.data.frame(set$points[set$points$cases == "Non-Quan" & set$points$model_unit == TRUE,]$MR1) %>%
    mutate(points = set$points[set$points$cases == "Non-Quan" & set$points$model_unit == TRUE,]$MR1, ID = "Non-Quan") %>%
    select(-c(1))
  data <- rbind(data_quan, data_non_quan)
  # Effect sizes for each pairwise t test
  print(cohens_d(data, points ~ ID))
}