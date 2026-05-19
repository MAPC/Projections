import os
#import numpy as np
import pandas as pd
import h5py

# set working directory

#Most recent MAPC Model run
run = 'run_243'

#Most recent SWM run
#run = 's102'

#In-Office directory
#os.chdir('K:/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/UrbanSim_Outputs/' + run)
#os.chdir('/mnt/k/DataServices/Projects/Current_Projects/Projections/Data/03_UrbanSim/UrbanSim_Outputs/' + run)

#Remote Directory
os.chdir('/mnt/s/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/UrbanSim_Outputs/' + run)

#os.chdir('/mnt/s/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/UrbanSim_Outputs/' + run)
#os.chdir('/mnt/s/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/UrbanSim_Outputs/test')

#os.chdir('/mnt/k/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/UrbanSim_Outputs/' + run)

#os.chdir('/mnt/cygdrive/k/DataServices/Projects/Current_Projects/Projections/Projections_2023/Data/03_UrbanSim/UrbanSim_Outputs/' + run)
#Years in the H5 file
#years = ['2010','2019','2029','2034']
#years = ['2019','2023','2029','2034','2039','2044','2049']
years = ['2019','2024']
for yr in years:
        if not os.path.exists(yr):
            os.makedirs(yr)

file_path = 'results_mapc_' + run + '_run_results.h5'
#file_path = 'run_results.h5'
# Reopen the file in read mode within a 'with' statement for automatic closing
#with pd.HDFStore(file_path, mode='r') as store:
#    store.close()
        
#h5=pd.HDFStore('run_results.h5')
#h5=pd.HDFStore('results_mapc_' + run + '_run_results.h5')
with pd.HDFStore(file_path) as h5:
        keys = h5.keys()
        for key in keys:
                out = h5[key]
                out.to_csv('.' + key + ".csv", header = True, index = False)
                print("export of " + key + " complete")
        h5.close()
        pass

