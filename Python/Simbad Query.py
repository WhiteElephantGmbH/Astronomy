from astroquery.simbad import Simbad
import pandas as pd
import sys

def get_data(lower_magnitude, upper_magnitude):
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
    customSimbad.add_votable_fields('id(name)', 'id(HD)', 'id(NGC)', 'ra(d)', 'dec(d)', 'pmra', 'pmdec', 'plx', 'otype', 'sp', 'flux(V)')

    # Define the query to retrieve objects with a magnitude in the specified range
    if upper_magnitude < 10.0:
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

# Get object tables within the specified magnitude range
max_table_length = 20000
lower_magnitude = -4.00
all_dataframes = []

print("generate object table from magnitude", lower_magnitude)
for deci_magnitude in range(500, 1500, 5):
    upper_magnitude = float(deci_magnitude / 100.0)
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
    relevant_columns = ['MAIN_ID', 'ID_name', 'ID_HD', 'ID_NGC', 'RA_d', 'DEC_d', 'PMRA', 'PMDEC', 'PLX_VALUE', 'OTYPE', 'SP_TYPE', 'FLUX_V']

    # Save to CSV without index
    result_dataframe[relevant_columns].to_csv('simbad_data.csv', index=False)
    print("Data saved to 'simbad_data.csv'")
else:
    print("No objects found.")
