



####################### CHECK IN ####################### 
# see the sbatches
cd /home/groups/manishad/IWN/sbatch_files

sbatch -p qsu,owners,normal /home/groups/manishad/IWN/sbatch_files/1.sbatch

# check on my running or pending jobs
squeue -u mmathur -t RUNNING
squeue -u mmathur -t PENDING

# /home/groups/manishad/IWN/sbatch_files/1.sbatch


# see the datasets
vim /home/groups/manishad/IWN/sim_results/long/long_results_job_1_.csv
cd /home/groups/manishad/IWN/sim_results/long
ls -l . | egrep -c '^-'

###### See the Errors #####
vim /home/groups/manishad/IWN/sbatch_files/rm_1507.err

# see the scen parameters
nano /home/groups/manishad/IWN/scen_params.csv

# see the stitched results
nano /home/groups/manishad/IWN/sim_results/overall_stitched/sti*
  
  
  
# CODE -> SHERLOCK ----------------------------------

# push all Sherlock code
scp /Users/mmathur/Dropbox/Personal\ computer/Independent\ studies/2023/\*IWN\ \(Imputation\ without\ nightMARs\)/Simulation\ code/* mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IWN



# SHERLOCK -> DESKTOP ----------------------------------

scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IWN/stitched_results/stitched.csv ~/Desktop


scp mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IWN/* ~/Desktop


####################### SHERLOCK -> DESKTOP (DEBUGGING) ####################### 

# move error file to Desktop
scp -r mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IWN/sbatch_files/rm_1250.err ~/Desktop

# move one sbatch file to Desktop
scp -r mmathur@login.sherlock.stanford.edu:/home/groups/manishad/IWN/sbatch_files/2296.sbatch ~/Desktop


####################### RUN SBATCH ####################### 

# run one of them
sbatch -p qsu,normal,owners /home/groups/manishad/IWN/sbatch_files/1.sbatch




####################### RESULTS -> DESKTOP FOR ANALYSIS ####################### 

scp mmathur@login.sherlock.stanford.edu /home/groups/manishad/IWN/results/overall_stitched/stitched.csv ~/Desktop

####################### CLEAN UP ####################### 


  # DELETE ALL LONG RESULTS
  mkdir /home/groups/manishad/IWN/empty_dir
rsync -a --delete /home/groups/manishad/IWN/empty_dir/ /home/groups/manishad/IWN/long_results/
  
  # delete stitched results
  rm /home/groups/manishad/IWN/overall_stitched/*
  rm /home/groups/manishad/IWN/stitched_results/*
  
  # delete "rm" files
  rm /home/users/mmathur/rm_*
  rm /home/groups/manishad/IWN/sbatch_files/rm_*
  
  
  # DELETE ALL SBATCHES
  # https://unix.stackexchange.com/questions/37329/efficiently-delete-large-directory-containing-thousands-of-files
  # https://superuser.com/questions/156664/what-are-the-differences-between-the-rsync-delete-options
  mkdir /home/groups/manishad/IWN/empty_dir
rsync -a --delete /home/groups/manishad/IWN/empty_dir/ /home/groups/manishad/IWN/sbatch_files/
  
  
  