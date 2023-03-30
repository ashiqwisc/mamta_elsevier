library(dplyr)
make.ona.plot <- function(set, 
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
      # node_size_multiplier = node_size_multiplier,
      self_connection_color = c("black")) %>%
    units(
      points=set$points[set$points$Patient.Student.Pair == "Lucas & Rakshya" & set$points$model_unit == TRUE,],
      points_color = c("red"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE) %>%
    units(
      points=set$points[set$points$Patient.Student.Pair == "Lucas & Ryleigh" & set$points$model_unit == TRUE,],
      points_color = c("orange"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE) %>%
    units(
      points=set$points[set$points$Patient.Student.Pair == "Quan & Rakshya" & set$points$model_unit == TRUE,],
      points_color = c("green"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE) %>%
    units(
      points=set$points[set$points$Patient.Student.Pair == "Quan & Ryleigh" & set$points$model_unit == TRUE,],
      points_color = c("blue"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE) %>%
    units(
      points=set$points[set$points$Patient.Student.Pair == "Regina & Rakshya" & set$points$model_unit == TRUE,],
      points_color = c("purple"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE) %>%
    units(
      points=set$points[set$points$Patient.Student.Pair == "Regina & Ryleigh" & set$points$model_unit == TRUE,],
      points_color = c("grey"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE)
  print(plot_all_pairs)
  
  index <- 1
  for(patient_student in unique(set$points$Patient.Student.Pair)){
    
    plot_Patient.Student.Pair <- plot(set, title = paste0(plot.name," (Patient.Student.Pair: ", patient_student,")")) %>%
      units(
        points=set$points[set$points$Patient.Student.Pair == patient_student & set$points$model_unit == TRUE,],
        points_color = c(color[index]),
        show_mean = FALSE, show_points = TRUE, with_ci = FALSE) %>%
      edges(
        weights =set$line.weights[set$line.weights$Patient.Student.Pair == patient_student & set$points$model_unit == TRUE,],
        # edge_size_multiplier = edge_size_multiplier,
        # edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
        # node_position_multiplier = node_position_multiplier,
        edge_color = c(color[index])) %>%
      nodes(
        # node_size_multiplier = node_size_multiplier,
        # node_position_multiplier = node_position_multiplier,
        self_connection_color = c(color[index]))
    index <- index+1
    print(plot_Patient.Student.Pair)
  }
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
      # node_size_multiplier = node_size_multiplier,
      self_connection_color = c("red")) %>%
    units(
      points=set$points[set$points$Participant == "Rakshya",],
      points_color = c("red"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE) 
  print(student_Rakshya)
  
  student_Ryleigh <- plot(set, title = "ONA plot for Ryleigh") %>%
    edges(
      weights = set$line.weights$Participant$Ryleigh,
      edge_color = c("blue")) %>%
    nodes(
      # node_size_multiplier = node_size_multiplier,
      self_connection_color = c("blue")) %>%
    units(
      points=set$points[set$points$Participant == "Ryleigh",],
      points_color = c("blue"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE)
  print(student_Ryleigh)
  
  # FirstGame and SecondGame subtracted plot
  sub <- plot(set, title = "Difference: Rakshya (red) vs Ryleigh (blue)") %>%
    units(
      points = set$points$Participant$Rakshya, 
      points_color = "red",
      show_mean = TRUE, show_points = TRUE, with_ci = FALSE) %>%
    units(
      points = set$points$Participant$Ryleigh, 
      points_color = "blue",
      show_mean = TRUE, show_points = TRUE, with_ci = FALSE) %>%
    edges(
      weights = (colMeans(set$line.weights$Participant$Rakshya) - colMeans(set$line.weights$Participant$Ryleigh)),
      edge_size_multiplier = 10,
      edge_arrow_saturation_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      edge_color = c("red","blue")) %>%
    nodes(
      # node_size_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      self_connection_color = c("red","blue"))
  print(sub)
}


make.ona.plot.compare.two.cases <- function(set, 
                                            plot.name
                                            # node_size_multiplier, 
                                            # edge_size_multiplier
){
  color <- c("red", "blue")
  case_Quan <- plot(set, title = "ONA plot for Quan") %>%
    edges(
      weights = set$line.weights$cases$Quan,
      edge_color = c("red")) %>%
    nodes(
      # node_size_multiplier = node_size_multiplier,
      self_connection_color = c("red")) %>%
    units(
      points=set$points[set$points$cases == "Quan",],
      points_color = c("red"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE) 
  print(case_Quan)
  
  case_non_Quan <- plot(set, title = "ONA plot for non-Quan") %>%
    edges(
      weights = set$line.weights$cases$`Non-Quan`,
      edge_color = c("blue")) %>%
    nodes(
      # node_size_multiplier = node_size_multiplier,
      self_connection_color = c("blue")) %>%
    units(
      points=set$points[set$points$cases == "Non-Quan",],
      points_color = c("blue"),
      show_mean = FALSE, show_points = TRUE, with_ci = FALSE)
  print(case_non_Quan)
  
  # Difference plot
  sub <- plot(set, title = "Difference: Quan (red) vs non-Quan (blue)") %>%
    units(
      points = set$points$cases$Quan, 
      points_color = "red",
      show_mean = TRUE, show_points = TRUE, with_ci = FALSE) %>%
    units(
      points = set$points$cases$`Non-Quan`, 
      points_color = "blue",
      show_mean = TRUE, show_points = TRUE, with_ci = FALSE) %>%
    edges(
      weights = (colMeans(set$line.weights$cases$Quan) - colMeans(set$line.weights$cases$`Non-Quan`)),
      edge_size_multiplier = 5,
      edge_arrow_saturation_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      edge_color = c("red","blue")) %>%
    nodes(
      # node_size_multiplier = 2,
      # node_position_multiplier = node_position_multiplier,
      self_connection_color = c("red","blue"))
  print(sub)
}


