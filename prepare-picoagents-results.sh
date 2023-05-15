#!/bin/bash

# list of environments
ENVS="empty-hall-r22-docks warehouse-r25-docks ubremen-r27-docks "

# run the experiment for each environment

for ENV in $ENVS
do
    echo "-------------------------------------------------------"
    echo " Processing results for $ENV"
    echo "-------------------------------------------------------"

    # add head row to the generated csv file
    cat "instances/$ENV/head" > "instances/$ENV/data.out.head"
    cat "instances/$ENV/data.out" >> "instances/$ENV/data.out.head"
done
