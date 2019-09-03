from rasterstats import zonal_stats
from sys import argv
import pandas as pd

script, vector_file, raster_file, output_file = argv

output = zonal_stats(vector_file, raster_file, categorical = True)

outputdf = pd.DataFrame(output)

outputdf.to_csv(output_file)
