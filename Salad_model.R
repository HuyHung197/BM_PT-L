# Salad and diet diversity of schoolchildren
# 
library(decisionSupport)
input_estimates <- data.frame(variable = c("Taste", "Safety", "Health", "Existing_diet_div"),
                              lower = c(.10, .3, .50, .30),
                              median = NA,
                              upper = c(.90, .88, .99, .80),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                              label = c("Taste (percent)", "Safety (percent)", "Health (percent)", "Existing diversity (percent)"),
                              Description = c("Percentage of the salad that is tasty",
                                              "Percentage of the salad that is safe",
                                              "Percentage of the salad that is healthy", 
                                              "Percentage of  diets that are currently diverse"))

# Model ####
model_function <- function(){
  
  
  final_result_salad <- Existing_diet_div + Existing_diet_div * (sum(10*(Taste), Safety, Health))
  final_result_no_salad <- Existing_diet_div
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result_salad = final_result_salad, 
              final_result_no_salad = final_result_no_salad))
}

# Run the Monte Carlo simulation using the model function
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 800,
                                      functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = c("final_result_salad", "final_result_no_salad"),
                   method = "smooth_simple_overlay",
                   old_names = c("final_result_salad", "final_result_no_salad"),
                   new_names = c("Outcome with salad", "Outcome without salad"))
