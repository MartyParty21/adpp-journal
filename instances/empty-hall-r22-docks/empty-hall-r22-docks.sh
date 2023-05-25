#!/bin/bash

instancesetname="empty-hall-r22-docks"
denvname="empty-hall-r25-docks"
denvxml="d-envs/$denvname.xml"
instancefolder="instances/$instancesetname"
maxtime=15000
timeout=240000

# run the java conflict generator	

mkdir -p $instancefolder
rm $instancefolder/*
cp $0 $instancefolder/

instance=0

for radius in "22"
do  
    for nagents in "1" "2" "3" "5" "7" "9" "10" "12" "15" #"10" "12" "15" "20" #"25" "30" "35" "40" "45" "48" # "1" "2" "4" "6" "8" "10" "12" "14" "16" "18" "20" "22" "24" "26" "28" "30"
    do        
        for seed in $(seq 1 $1)
        do
	        # create a problem instance file
	        instancename="$instance"
	        instancefile=$instancefolder/$instancename.xml
		    timestep=$radius

	        ## ConflictGenerator
	        java -XX:+UseSerialGC -cp solver.jar -Dlog4j.configuration="file:$PWD/log4j.custom" tt.jointeuclid2ni.probleminstance.generator.GenerateEAInstance -env $denvxml -nagents $nagents -radius $radius -seed $seed -outfile $instancefile -sgnooverlap

	        # add instance to data.in
	        for alg in "ADPP"
	        do
   				activitylog=""
			
			    summaryprefix="$instancesetname;$instance;$nagents;$radius;$seed;$maxtime;$alg$2;"
		        echo -method $alg -problemfile $instancefile -maxtime $maxtime -timestep $timestep -timeout $timeout -summary -summaryprefix "$summaryprefix" $activitylog >> $instancefolder/data.in           
	        done

	        echo Finished instance no $instance. Agents: $nagents. Gridstep: $gridstep Radius: $radius. Seed: $seed.
	        let instance=instance+1  
        done 
     done
done

echo "env;instance;nagents;radius;seed;maxtime;alg;cost;status;simtime;time;msgs;expansions;clusters;replans;planningTime" > $instancefolder/head
mkdir $instancefolder/figs
