#packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("janitor")
library(ggplot2)
library(dplyr)
library(janitor)

#loading the data 
data <- read.csv("question-5-data/Cui_etal2014.csv", header = TRUE) #the 1st column is just the row number 

#changing the data to make it more R friendly 
virus_data <- clean_names(data)

#plotting the raw data 
volume_genome_plot <- ggplot(data = virus_data, aes(x=genome_length_kb, y=virion_volume_nm_nm_nm))+
  geom_point()+
  theme_bw()+
  labs(x="Genome Length (kb)", y="Virion volume (nm^3)", title = "The relationship between virus genome length and volume")
volume_genome_plot

#plotting the logged data 
log_volume_plot <- ggplot(data=virus_data, aes(x=log(genome_length_kb), y=log(virion_volume_nm_nm_nm)))+
  geom_point()+
  theme_bw()+
  labs(x="log[Genome length (kb)]", y="log[Virion volume(nm3)]", title = "The relationship between virus log(genome length) and log(volume)")
log_volume_plot

#fitting a linear model to the data 
virus_data_log <- virus_data %>% mutate(volume_log = log(virion_volume_nm_nm_nm), length_log= log(genome_length_kb)) 
volume_lm <- lm(volume_log ~ length_log, virus_data_log)
summary(volume_lm)

#plotting the data with a logistic model
logistic_fun <- function(genome_length_kb) {
  
  virion_volume_nm_nm_nm <- beta*genome_length_kb^alpha
  
  return(virion_volume_nm_nm_nm)
  
}

#adding in the estimates of the scaling factor and exponent derived from the linear model 
beta <-  exp(7.0748)
alpha <-  1.5152

#graph with the raw data and equation
volume_genome_model_plot <- ggplot(data = virus_data_log, aes(x=genome_length_kb, y=virion_volume_nm_nm_nm))+
  geom_point()+
  theme_bw()+
  geom_function(fun=logistic_fun, colour= "red")+
  labs(x="Genome Length (kb)", y="Virion volume (nm^3)", title = "The relationship between virus genome length and volume")
volume_genome_model_plot

#code to make the figure
figure <- ggplot(data=virus_data_log, aes(x=length_log, y=volume_log))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  labs(x="log[Genome length (kb)]", y="log[Virion volume(nm3)]")
figure





