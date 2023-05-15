#!/bin/bash

# how many instances will be generated for each data point (i.e. for each number of robots in each instanceset)
N=20

SUFFIX="-Distributed"

# list of environments
ENVS="empty-hall-r22-docks warehouse-r25-docks ubremen-r27-docks "

# run the experiment for each environment

for ENV in $ENVS
do
    # cleanup
    rm -fr instances/$ENV
    mkdir -p instances/$ENV

    cp admap-solver/target/admap-1.0-SNAPSHOT-jar-with-dependencies.jar solver.jar # copy the binaries

    echo "-------------------------------------------------------"
    echo " Generating instances for $ENV instanceset"
    echo "-------------------------------------------------------"

    # generate N random instances for each number of robots
    instanceset-generators/$ENV.sh $N $SUFFIX

done

echo "----------------------------------------------------------"
echo " Done! Instances have been generated to instances/ directory."
echo "-----------------------------------------------------------"






