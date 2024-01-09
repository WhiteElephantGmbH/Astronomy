"""
************************************************************************************************************************
*                                         Query Stars from the Simbad Database                                         *
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
    Query the Simbad database for stars within a specified magnitude range.

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
    customSimbad.add_votable_fields('id(name)', 'id(HD)', 'id(HIP)', 'id(HR)', 'ra(d)', 'dec(d)', 'pmra', 'pmdec',
                                    'plx', 'otype(otypes)', 'sp', 'flux(V)')

    # Define the query to retrieve stars with a magnitude in the specified range
    query = f"Vmag>={lower_magnitude} & Vmag<{upper_magnitude} & otype in ('*')"

    # Execute the query
    try:
        result_table = customSimbad.query_criteria(query)
    except Exception as e:
        print(f"Error during Simbad query: {e}")
        return None

    return result_table


def get_hr_data():
    """
    Query the Simbad database for stars from the HR catalog range.

    Returns:
    - result_table (astropy.table.Table): Table containing the query results.
    """
    # Set up the Simbad query
    customSimbad = Simbad()
    customSimbad.TIMEOUT = 1000  # Set the timeout for the query (in seconds)
    customSimbad.remove_votable_fields('coordinates')
    customSimbad.add_votable_fields('id(name)', 'id(HD)', 'id(HIP)', 'id(HR)', 'ra(d)', 'dec(d)', 'pmra', 'pmdec',
                                    'plx', 'otype(otypes)', 'sp', 'flux(V)')

    # Define the query to retrieve stars
    query = f"cat='HR'"

    # Execute the query
    try:
        result_table = customSimbad.query_criteria(query)
    except Exception as e:
        print(f"Error during Simbad query: {e}")
        return None

    return result_table


"""
   Get symbad stars
   ================
"""

Output_File = "stars.csv"

Min_Magnitude          = -2.0
First_Upper_Magnitude  =  3.0
Max_Star_Magnitude     =  9.0

max_table_length = 20000

all_dataframes  = []

print("generate HR catalogue star table")
star_table = get_hr_data()
if star_table:
    dataframe = star_table.to_pandas()
    table_length = len(star_table)
    if table_length >= max_table_length:
        print ("star table incomplete - table length:", table_length)
        sys.exit()
    all_dataframes.append(dataframe)
else:
    print ("star table empty")
    sys.exit()

# Values in centi magnitudes
first_upper_value = int(First_Upper_Magnitude * 100)
last_upper_value  = int(Max_Star_Magnitude * 100)
increment         = 5

lower_magnitude = Min_Magnitude
print("generate star table from magnitude", lower_magnitude)
for centi_magnitude in range(first_upper_value, last_upper_value, increment):
    upper_magnitude = float(centi_magnitude / 100.0)
    print(".. to magnitude", upper_magnitude)
    star_table = get_data(lower_magnitude, upper_magnitude)
    if star_table:
        dataframe = star_table.to_pandas()
        table_length = len(star_table)
        if table_length >= max_table_length:
            print ("star table incomplete - table length:", table_length)
            sys.exit()
        all_dataframes.append(dataframe)
    else:
        print ("star table empty")
        sys.exit()
    lower_magnitude = upper_magnitude

# Process data
if all_dataframes:
    # Concatenate all DataFrames into a single DataFrame
    result_dataframe = pd.concat(all_dataframes, ignore_index=True)

    # Select only the relevant columns
    relevant_columns = ['MAIN_ID', 'ID_name', 'ID_HD', 'ID_HIP', 'ID_HR', 'RA_d', 'DEC_d', 'PMRA', 'PMDEC',
                        'PLX_VALUE', 'OTYPE_otypes', 'SP_TYPE', 'FLUX_V']

    # Save to CSV without index
    result_dataframe[relevant_columns].to_csv(Output_File, index=False)
    print("Data saved to " + Output_File)
else:
    print("No stars found.")
