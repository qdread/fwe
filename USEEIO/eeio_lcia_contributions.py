# -*- coding: utf-8 -*-
"""
Created on July 17 2019

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

def eeio_lcia_contributions(model_path, demand_values, demand_codes):
    eeio = useeiopy.assemble(model_path)
    demand_dict = dict(zip(demand_codes, demand_values))
    result = iomb.calculate(eeio, demand_dict)
    lcia_contr = result.lcia_contributions
    return lcia_contr
    