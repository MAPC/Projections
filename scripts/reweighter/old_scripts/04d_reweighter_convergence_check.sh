#!/bin/bash

cd /k/DataServices/Projects/Current_Projects/Projections/Reweighter/Municipal_Controls_2010/Outputs2/
cd /c/Users/cgately/Desktop/Outputs2/
grep -Ril "Maximum number of iterations reached" . > incomplete_munis.csv

find . -type f -name '*2010.txt' -exec cat {} + > all_muni_logfile.txt
echo "NOT CONVERGED: Max iterations reached = "
grep -o "Maximum number of iterations reached" all_muni_logfile.txt | wc -l
echo "NOT CONVERGED: No change in weights = "
grep -o "No change in weights stopping condition met" all_muni_logfile.txt | wc -l
echo "Convergence Achieved = "
grep -o "Maximum deviation from target stopping condition achieved." all_muni_logfile.txt | wc -l


