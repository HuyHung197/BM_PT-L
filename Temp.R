# Introduction to R ---- 
# Change the formula to calculate 2 plus 2
2+2
# my change
2+2
# import the database: sử dụng lệnh install.package(), hoặc sử dụng chức năng tools -> install packages
install.packages("datasauRus")
install.packages("gganimate")
install.packages("ggplot2")
install.packages("purrr")
install.packages("rmarkdown")
install.packages("tidyverse")
install.packages("magrittr")


Hung
# applying the library in this working file: sử dụng lệnh library() để sử dụng thư viện code trong file làm việc hiện tại
# 1 cách khác là sử dụng packages tab ở cửa sổ góc dưới bên phải
library(datasauRus)
library(gganimate)
library(ggplot2)
library(purrr)
library(rmarkdown)
library(tidyverse)
library(dplyr)
library(magrittr)

# Wrangling #### 2 trang web giải thích về cách thức sử dụng MC Simulation trong phân tích ra quyết định
browseVignettes(package = "tidyverse")
browseVignettes(package = "decisionSupport")
# How to import data from other source/ dùng lệnh read.csv() hoặc 1 cách khác là import từ đường link url
participants_data <- read.csv("participants_data.csv")
# dùng lệnh để import từ url
urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"
participants_data <- read.csv(url(urlfile))
# Section 3 Impact pathway Building ####
# We use the "mermaid" function from DiagrammeR lib to draw impact path
install.packages("DiagrammeR")
library(DiagrammeR)
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px")
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1px
        CM(Management cost)-->F; linkStyle 4 stroke: red, stroke-width:1px")
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost"),
                              lower = c(6000, 3, 500),
                              median = NA,
                              upper = c(14000, 8, 1000),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season"))

input_estimates
library(decisionSupport)
model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  # Estimate the final results from the model
  final_result <- income - Labor_cost
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function (<- dấu này mang ý nghĩa define biến)

example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 800,
                                      functionSyntax = "plainNames")

example_mc_simulation
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
# Shw the result of Montecarlo Simulation
print(summary(example_mc_simulation, prob=c(0.05,0.50,0.95)))
hist(example_mc_simulation, xlab="Hung")
# Lệnh để nhìn chỉ 1 số hàng nhất định, n là số hàng
head(participants_data,
     n=4)
head(participants_data,
     n=7)
