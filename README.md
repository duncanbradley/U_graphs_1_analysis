# Interpretation of Univariate Scatterplots

## Background

In this experiment, 82 partcipants viewed univariate scatterplots, each with three dot clusters. Following each graph, participants moved a marker on a slider to indicate where they perceived the mean for each cluster.

## Data Pre-Processing

The data provided here is presented in the most unprocessed format possible, however some pre-processing has taken place. This is because the two original data files contained Prolific.co ID codes, which should not be shared publicly, according to Prolific.co guidelines. However, I include the script which details how data in these files were anonymised (`replace_participants_IDs.R`). The output from this script (`U_graphs_1_data.csv`) is used throughout the main analysis. 

## Analysis

Script: `analysis_script.R`

This script has three main sections:

- Data wrangling
- Model-building and visualisation
- Supplementary visualisations for report
 
## Data Files

- `U_graphs_1_data.csv` contains participant response data and demographic information
- The other data files contain information concerning the stimuli:
     - `graphs_book1` in the `graph_generation` folder contains the population-level statistics used for sampling when building stimuli visualisations
     - `all_summary` in the `summary_stats` folder (inside `graph_generation`) contains the sample-level means, SDs, min and max values created when building stimuli visualisations 
     - `axis_summary` and `blank_axis_summary` in the `summary_stats` folder (inside `graph_generation`) contain the upper and lower y-axis limits used in stimuli visualisations (both populated graphs used during presentation, and also blank graphs used response)

# Stimuli Generation

- `graph_generator.R` in the `graph_generation` folder generates all graphs used for stimuli and responses. 
     - This script uses population-level statistics from `graphs_book1.csv` as input for samples plotted in the graphs
     - This script produces summary files in the `summary_stats` folder (inside `graph_generation`)
- `graph_lists.R` produces .csv files for use in PsychoPy to control presentation of stimuli
- `tutorial_and_prac_graphs.R` produces graphs used in the tutorial and practice phases of the experiment

# Experiment Script

- The file `U_graphs_1.psyexp` is the PsychoPy script which runs the experiment.
