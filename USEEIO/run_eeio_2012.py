# -*- coding: utf-8 -*-
"""
Created on Fri Nov  2 09:12:55 2018

@author: qread
"""

# Script to run different demand scenarios in USEEIO 2012
# Version created 14 Nov 2018 by QDR to use the new input output table
# Converted into 2013 dollars

import useeiopy
import iomb
import pandas
import iomb.demand2dict as dd

useeio1pt1 = useeiopy.assemble("USEEIO2012")
drc = useeio1pt1.drc_matrix.copy()

# Define function to get LCIA total from a demand scenario
def get_impacts( scenario_name, scenario_csv ):
    demand_dict = dd.demandtodict(scenario_name, scenario_csv)
    demand_dict_corrected = correct_demand_names(demand_dict, drc)
    result = iomb.calculate(useeio1pt1, demand_dict_corrected)
    return(result.lcia_total)

# Define function to match names in demand vector with names of DRC matrix    
def correct_demand_names( demand_dict, drc ):
    # Check key names versus model matrix dimension names
    key_demand = list(demand_dict.keys())
    drc_index = list(drc.index)
    
    key_demand_codes = [i[0:6] for i in key_demand]
    drc_index_codes = [i[0:6] for i in drc_index]
    
    # Change the names of the demand dicts to match the names in the model.
    matches_idx = [i for i, j in enumerate(drc_index_codes) if j in key_demand_codes]
    
    # Sort DRC index by the matches
    drc_index_sorted = [drc_index[i] for i in matches_idx]
    
    # Create dictionary to map the old and new name
    key_mapping = dict(zip(key_demand, drc_index_sorted))
    
    # Correct names in original dictionary
    dict_corrected = dict((key_mapping[key], value) for (key, value) in demand_dict.items())
    return(dict_corrected)

# Test on full scenario
csv2 = 'C:/Users/qread/Dropbox/projects/foodwaste/Code/USEEIO-master/useeiopy/Model Builds/USEEIO2012/USEEIO2012_FinalDemand.csv'
all_impacts = get_impacts('2012_US_Consumption', csv2)
    
scen_csv = 'C:/Users/qread/Dropbox/projects/foodwaste/Data/demand_scenarios_2012.csv'     
baseline_freshfruit = get_impacts('baseline_freshfruit', scen_csv)
baseline_freshveg = get_impacts('baseline_freshveg', scen_csv)
baseline_cannedfruitveg = get_impacts('baseline_cannedfruitveg', scen_csv)
scenario2_freshfruit = get_impacts('scenario2_freshfruit', scen_csv)
scenario2_freshveg = get_impacts('scenario2_freshveg', scen_csv)
scenario2_cannedfruitveg = get_impacts('scenario2_cannedfruitveg', scen_csv)

alltypes_results = pandas.concat([baseline_freshfruit, baseline_freshveg, baseline_cannedfruitveg,
                                  scenario2_freshfruit, scenario2_freshveg, scenario2_cannedfruitveg], axis = 1)
    
alltypes_results.columns = ['baseline_freshfruit', 'baseline_freshveg', 'baseline_cannedfruitveg',
                            'scenario2_freshfruit', 'scenario2_freshveg', 'scenario2_cannedfruitveg']    

alltypes_results.to_csv('Q:/IO_output/twoscenarios_bytype_2012.csv') # Remote version
alltypes_results.to_csv('C:/Users/qread/Dropbox/projects/foodwaste/Data/twoscenarios_bytype_2012.csv') # Local version
