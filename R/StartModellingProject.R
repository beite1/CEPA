#The idea for this function is to create the structure for a modelling project.
#It will in the future download the AIMMS pack as well.

StartModellingProject <- function(Model = "200408_UCED_ZN_AU"){

  dir.create("!Model")
  dir.create("Assumptions")
  dir.create("Inputs")
  dir.create("Inputs/Generation")
  dir.create("Inputs/Zonal")
  dir.create("Inputs/Network")
  dir.create("Inputs/System_Costs")
  dir.create("Inputs/Other")
  dir.create("ModelOutputs")
  dir.create("ModelOutputs/Scenario1")
  dir.create("ModelOutputs/Scenario2")
  dir.create("ModelOutputs/Scenario3")
  dir.create("Outputs")
  dir.create("Dashboard")
  dir.create("R")


}
