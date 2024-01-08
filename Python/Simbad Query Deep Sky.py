"""
************************************************************************************************************************
*                                       Query Deep Sky from the Simbad Database                                        *
************************************************************************************************************************
*                              (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
*                                                www.white-elephant.ch                                                 *
*                                                                                                                      *
*      This program is free software; you can redistribute it and/or modify it under the terms of the GNU General      *
*      Public License as published by the Free Software Foundation; either version 2 of the License, or                *
*      (at your option) any later version.                                                                             *
*                                                                                                                      *
*      This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the      *
*      implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License     *
*      for more details.                                                                                               *
*                                                                                                                      *
*      You should have received a copy of the GNU General Public License along with this program; if not, write to     *
*      the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                 *
************************************************************************************************************************
"""
from astroquery.simbad import Simbad
import pandas as pd
import sys

def get_data(lower_magnitude, upper_magnitude):
    """
    Query the Simbad database for deep sky objects within a specified magnitude range.

    Parameters:
    - lower_magnitude (float): Lower limit of the magnitude range.
    - upper_magnitude (float): Upper limit of the magnitude range.

    Returns:
    - result_table (astropy.table.Table): Table containing the query results.
    """
    # Set up the Simbad query
    customSimbad = Simbad()
    customSimbad.TIMEOUT = 1000  # Set the timeout for the query (in seconds)
    customSimbad.remove_votable_fields('coordinates')
    customSimbad.add_votable_fields('id(name)', 'id(M)', 'id(NGC)', 'id(IC)', 'id(Ocl)', 'ra(d)', 'dec(d)', 'plx', 'otype(otypes)', 'flux(V)')

    # Define the query to retrieve deep sky objects with a magnitude in the specified range
    query = f"Vmag>={lower_magnitude} & Vmag<{upper_magnitude} & otype in ('G', 'ISM', 'Cl*', 'As*')"

    # Execute the query
    try:
        result_table = customSimbad.query_criteria(query)
    except Exception as e:
        print(f"Error during Simbad query: {e}")
        return None

    return result_table


"""
   Get symbad objects in chunks of magnitude ranges
   ================================================
"""

Output_File = "deep_sky.csv"

Min_Magnitude          =  0.0
First_Upper_Magnitude  =  9.0
Max_Magnitude          = 16.0

max_table_length = 20000

# Values in centi magnitudes
first_upper_value = int(First_Upper_Magnitude * 100)
last_upper_value  = int(Max_Magnitude * 100)
increment         = 5

all_dataframes  = []
lower_magnitude = Min_Magnitude
print("generate object table from magnitude", lower_magnitude)
for centi_magnitude in range(first_upper_value, last_upper_value, increment):
    upper_magnitude = float(centi_magnitude / 100.0)
    print(".. to magnitude", upper_magnitude)
    object_table = get_data(lower_magnitude, upper_magnitude)
    if object_table:
        dataframe = object_table.to_pandas()
        table_length = len(object_table)
        if table_length >= max_table_length:
            print ("object table incomplete - table length:", table_length)
            sys.exit()
        all_dataframes.append(dataframe)
    else:
        print ("object table empty")
        sys.exit()
    lower_magnitude = upper_magnitude

# Process data
if all_dataframes:
    # Concatenate all DataFrames into a single DataFrame
    result_dataframe = pd.concat(all_dataframes, ignore_index=True)

    # Select only the relevant columns
    relevant_columns = ['MAIN_ID', 'ID_name', 'ID_M', 'ID_NGC', 'ID_IC', 'ID_Ocl', 'RA_d', 'DEC_d', 'PLX_VALUE', 'OTYPE_otypes', 'FLUX_V']

    # Save to CSV without index
    result_dataframe[relevant_columns].to_csv(Output_File, index=False)
    print("Data saved to " + Output_File)
else:
    print("No objects found.")
