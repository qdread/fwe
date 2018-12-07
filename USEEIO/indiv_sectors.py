# -*- coding: utf-8 -*-
"""
Created on Tue Dec  4 08:50:53 2018

@author: qread
"""

# Script to look at environmental impacts at different stages of supply chain
# Use Dairy as an example

import useeiopy
import iomb
import pandas
import iomb.demand2dict as dd
import re

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


# Get names of the sectors
sector_names = drc.index.values.tolist()

# Get dairy code
dairy_code = [x for x in sector_names if re.search('dairies', x)]
dairy_result = iomb.calculate(useeio1pt1, {dairy_code[0]: 1})

# Get inputs and outputs of dairy sector
dairy_inputs = drc.loc[:, dairy_code[0]]
dairy_outputs = drc.loc[dairy_code[0], :]

# Milk products that use output from dairies
dairyproducts_code = [x for x in sector_names if re.search('^3115', x)]
cheese_result = iomb.calculate(useeio1pt1, {dairyproducts_code[0]: 1})

# Inputs and outputs of cheese sector
cheese_inputs = drc.loc[:, dairyproducts_code[0]] # 33 cents of dairy output is required to produce 1 dollar of cheese.
cheese_outputs = drc.loc[dairyproducts_code[0], :] # 1/2 cent of cheese output is required to produce 1 dollar of restaurant value

