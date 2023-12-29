from astroquery.simbad import Simbad
import pandas as pd
import sys

def get_data(lower_magnitude, upper_magnitude, upper_star_magnitude):
    """
    Query the Simbad database for astronomical objects within a specified magnitude range.

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
    customSimbad.add_votable_fields('id(name)', 'id(HD)', 'id(HIP)', 'id(HR)', 'id(M)', 'id(NGC)', 'ra(d)', 'dec(d)', 'pmra', 'pmdec', 'plx', 'otype', 'sp', 'flux(V)')

    # Define the query to retrieve objects with a magnitude in the specified range
    if upper_magnitude < upper_star_magnitude:
        query = f"Vmag>={lower_magnitude} & Vmag<{upper_magnitude} & otype in ('G', 'ISM', 'Cl*', 'As*', '*')"
    else:
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

Min_Magnitude          = -2.0
First_Upper_Magnitude  =  3.0
Max_Star_Magnitude     = 10.0
Max_Deap_Sky_Magnitude = 15.0

max_table_length = 20000

# Values in centi magnitudes
first_upper_value = int(First_Upper_Magnitude * 100)
last_upper_value  = int(Max_Deap_Sky_Magnitude * 100)
increment         = 5

all_dataframes  = []
lower_magnitude = Min_Magnitude
print("generate object table from magnitude", lower_magnitude)
for centi_magnitude in range(first_upper_value, last_upper_value, increment):
    upper_magnitude = float(centi_magnitude / 100.0)
    print(".. to magnitude", upper_magnitude)
    object_table = get_data(lower_magnitude, upper_magnitude, Max_Star_Magnitude)
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
    relevant_columns = ['MAIN_ID', 'ID_name', 'ID_HD', 'ID_HIP', 'ID_HR', 'ID_M', 'ID_NGC', 'RA_d', 'DEC_d', 'PMRA', 'PMDEC', 'PLX_VALUE', 'OTYPE', 'SP_TYPE', 'FLUX_V']

    # Save to CSV without index
    result_dataframe[relevant_columns].to_csv('simbad_data.csv', index=False)
    print("Data saved to 'simbad_data.csv'")
else:
    print("No objects found.")
