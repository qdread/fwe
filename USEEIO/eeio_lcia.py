# -*- coding: utf-8 -*-
"""
Created on Fri Dec  7 09:56:30 2018

@author: qread
"""

# FUNCTION TO GET THE RESULTS FROM AN EEIO MODEL
# ----------------------------------------------

# Inputs: 
# 1. Path to folder containing built model
# 2. Final demand vector (unnamed numeric)
# 3. Sector ID vector corresponding to demand vector (character vector)

import useeiopy
import iomb

def eeio_lcia(model_path, demand_values, demand_codes):
    eeio = useeiopy.assemble(model_path)
    demand_dict = dict(zip(demand_codes, demand_values))
    result = iomb.calculate(eeio, demand_dict)
    lcia = result.lcia_total
    return lcia
    