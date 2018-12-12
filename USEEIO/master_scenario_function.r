# EEIO Scenario Master Function
# -----------------------------

# There are two inputs that can be edited here.
# One is the structure of the DRC matrix itself. The user inputs a csv with 3 columns: the sector providing output, the sector receiving input, and the (additive) increment by which the cell should be changed relative to the 2012 baseline. If no change, input nothing.
# Two is the final demand vector. The user inputs a csv with 2 columns: the sector to be changed and the (additive) increment by which it should be changed relative to the 2012 baseline. If no change, input nothing.

# Another user option would be to save the intermediate files or not, and if so, what location to save them to.

# Workflow
# --------

# 1. Load the 2012 baseline use table and edit it based on user's input.
# 2. Write the edited use table to a csv.
# 3. Build USEEIO with the new table and write the resulting files to a directory.
# 4. Create a demand vector with the user's input.
# 5. Run the python script from R to calculate the LCIA results (and possibly other results) from the built model and demand vector.
# 6. Delete the intermediate files (edited use table and model build folder) unless user specifies not to delete.
# 7. Return the results.