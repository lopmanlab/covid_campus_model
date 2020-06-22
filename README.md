# COVID19 Campus Model Summary
The COVID19 Campus Model is a tool to guide mitigation strategies and resource needs for universities considering in-person instruction for Fall 2020. We developed a susceptible-exposed-infectious-recovered (SEIR) type of deterministic compartmental model that captures transmission process, allowing for estimates of direct and indirect (transmission-mediated) effects of control strategies. 

Code in this repository was used for the simulations and results presented our paper. An interactive software tool is available for additional simulations. Links below:

* [Paper] (Insert link)
* [Interactive tool] (Insert link)

# Code description
## Model runs and sensitivity analysis

| File                   | Description |
| ---------------------- | ------------- |
| [Screening scenarios](.../1_screen_explore_plot.R)           |Explores a range of screening intervals and with an outcome of total and cumulative cases in staff and students|
| [Testing scenarios](.../2_test_explore_plot.R)        |Explores a range of testing interval (time from symptom-onset to testing and subsequent isolation)|
| [Screen and test scenarios](.../3_testandscreen_explore_plot.R) |Explores combinations of screening and testing intervals |
| [Transmission scenarios](.../4_transmission_explore_plot.R)| Explores different transmission scenarios |
| [Probabilistic sensitivity analysis (PSA)](.../99_psa_parm.R)         |Probabilistic sensitivty analysis where distributions are assigned to key parameters and latin-hypercube sampling is used to sample from the parameter space. Produces outputs for base and three intervention scenarios |

## Functions, parameters and initials

| File                   | Description |
| ---------------------- | ------------- |
| [Dependencies](.../99_dependencies.R)           | Loads relevant packages  |
| [Model function](.../99_model_func.R)        | Loads function for transmission model   |
| [Parameters & initials](.../99_parm_init_control.R) | Loads paramemeters, initial conditions and control settings  |
| [Distribution optimizer](.../99_psa_optimizedistr.R)| Loads function used to optimize beta distribution for the probabilistic senstivity analysis (PSA) |
| [PSA parameters](.../99_psa_parm.R)         | Loads parameters and distributions used for the PSA  |
| [PSA plot function](.../99_psa_plot.R)      | Loads plot function used for the PSA  |

## Outputs
| File                   | Description |
| ---------------------- | ------------- |
| [Plots](.../Plots.R)         | Folder containing figures from model outputs  |
