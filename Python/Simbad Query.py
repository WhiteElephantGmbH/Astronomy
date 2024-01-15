"""
************************************************************************************************************************
*                                         Query Data from the Simbad Database                                          *
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

"""
    Query the Simbad database for deep sky objects within a specified ra range and catalogues
    ------------------------------------------------------------------------------------------
    Parameters:
    - lower_ra: Lower limit of the right ascension in degrees.
    - upper_ra: Upper limit of the right ascension in degrees.

    Returns:
    - result_table (astropy.table.Table): Table containing the query results.
"""
def get_data(lower_ra, upper_ra, catalogs):
    # Set up the Simbad query
    customSimbad = Simbad()
    customSimbad.TIMEOUT = 180 # Set the timeout 3 min for the query (in seconds)
    customSimbad.remove_votable_fields('coordinates')
    customSimbad.add_votable_fields('id(name)', 'id(HD)', 'id(HIP)', 'id(HR)', 'id(M)', 'id(NGC)', 'id(IC)', 'id(Ocl)',
                                    'ra(d)', 'dec(d)', 'pmra', 'pmdec', 'plx', 'distance', 'otype(otypes)', 'sp',
                                    'flux(R)', 'flux(G)', 'flux(B)', 'flux(V)')

    # Define the query to retrieve deep sky objects
    query = f"Ra>={lower_ra} & Ra<{upper_ra} & cat in ({catalogs})"

    # Execute the query
    try:
        result_table = customSimbad.query_criteria(query)
    except Exception as e:
        print(f"Error during Simbad query: {e}")
        return None

    return result_table


"""
   Get symbad cataloges in chunks of Ra ranges
   -------------------------------------------
"""
max_table_length = 20000

all_dataframes  = []

def get_catalog(catalog, chunks):
    step_size = int(360 / chunks)
    print ("catalog(s):", catalog)
    for lower_ra in range(0, 360, step_size):
        from_ra = lower_ra
        to_ra = lower_ra + step_size
        print("from RA:", from_ra, "to RA:", to_ra)
        object_table = get_data(from_ra, to_ra, catalog)
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

"""
   Get cataloge data and store the result
   ======================================
"""

Output_File = "data.csv"

get_catalog ("'*','HD','HIP','HR', 'IC','M','NGC','OCL'", 360)

# Process data
if all_dataframes:
    # Concatenate all DataFrames into a single DataFrame
    result_dataframe = pd.concat(all_dataframes, ignore_index=True)

    # Select only the relevant columns
    relevant_columns = ['MAIN_ID', 'ID_name', 'ID_HD', 'ID_HIP', 'ID_HR', 'ID_M', 'ID_NGC', 'ID_IC', 'ID_Ocl',
                        'RA_d', 'DEC_d', 'PMRA', 'PMDEC', 'PLX_VALUE', 'Distance_distance', 'Distance_unit',
                        'OTYPE_otypes', 'SP_TYPE', 'FLUX_R', 'FLUX_G', 'FLUX_B', 'FLUX_V']

    # Save to CSV without index
    result_dataframe[relevant_columns].to_csv(Output_File, index=True)
    print("Data saved to " + Output_File)
else:
    print("No objects found.")
