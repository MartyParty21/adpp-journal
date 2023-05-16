#!/bin/bash

# list of environments
ENVS="empty-hall-r22-docks warehouse-r25-docks ubremen-r27-docks "

# run the experiment for each environment

for ENV in $ENVS
do
    echo "-------------------------------------------------------"
    echo " Preparing Plots for $ENV"
    echo "-------------------------------------------------------"

    # run R script to generate the plots
      Rscript make-plots.r $ENV

      echo "-------------------------------------------------------"
      echo " PDF with plot has been generated to file plots/$ENV.pdf"
      echo "-------------------------------------------------------"
done
