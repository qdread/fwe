# -*- coding: utf-8 -*-
"""
Created on Thu Dec  6 14:53:08 2018

@author: qread
"""

# Script to run the three different scenarios and look at total impacts

import useeiopy
import iomb
import pandas
import iomb.demand2dict as dd

eeio0 = useeiopy.assemble("USEEIO2012_scen0") # Baseline

eeio1 = useeiopy.assemble("USEEIO2012_scen1")

eeio2 = useeiopy.assemble("USEEIO2012_scen2")

eeio3 = useeiopy.assemble("USEEIO2012_scen3")

drc = eeio0.drc_matrix.copy()

demand2012_file = 'C:/Users/qread/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'

# Results across all sectors of the economy.

demand_dict2012 = dd.demandtodict('2012_US_Consumption', demand2012_file)
demand_dict2012_corrected = correct_demand_names(demand_dict2012, drc)

result0 = iomb.calculate(eeio0, demand_dict2012_corrected)
result1 = iomb.calculate(eeio1, demand_dict2012_corrected)
result2 = iomb.calculate(eeio2, demand_dict2012_corrected)
result3 = iomb.calculate(eeio3, demand_dict2012_corrected)

result0.lcia_total
result1.lcia_total
result2.lcia_total
result3.lcia_total

all_results = pandas.concat([result0.lcia_total, result1.lcia_total, result2.lcia_total, result3.lcia_total], axis = 1)
all_results.columns = ['result0','result1','result2','result3']

# Write the output.
all_results.to_csv('Q:/IO_output/structural_scenarios_2012.csv') # Remote version
all_results.to_csv('C:/Users/qread/Dropbox/projects/foodwaste/Data/structural_scenarios_2012.csv') # Local version

# Results accounting for food system only.
# ----------------------------------------

# Join classification of sectors to the demand vector so we can get only the ones with demand >0
sector_classification = pandas.read_csv('C:/Users/qread/Dropbox/projects/foodwaste/Data/useeio_sector_classification.csv')
demand2012_table = pandas.read_csv(demand2012_file)
merge_on = ['BEA_389_code', 'BEA_389_def']
sector_classification = pandas.merge(sector_classification, demand2012_table, how = 'left', left_on = merge_on, right_on = merge_on)

foodsystem_sectors = sector_classification[(sector_classification['food_system'] == 'y') & (sector_classification['2012_US_Consumption']) > 0]

ag_sectors = sector_classification[(sector_classification['stage'] == 'agriculture') & (sector_classification['2012_US_Consumption']) > 0]

foodsystem_sectors = sector_classification[(sector_classification['stage'] == 'y') & (sector_classification['2012_US_Consumption']) > 0]

foodsystem_sectors = sector_classification[(sector_classification['food_system'] == 'y') & (sector_classification['2012_US_Consumption']) > 0]

# Get short names of dictionary keys
demand_dict2012_shortnames = dict((key[0:6], value) for (key, value) in demand_dict2012_corrected.items())

demand_dict_foodsystem = dict( (x.lower(), demand_dict2012_shortnames[x.lower()]) for x in foodsystem_sectors['BEA_389_code'].values)
