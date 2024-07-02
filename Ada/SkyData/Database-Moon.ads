-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-- Generated Moon feature information, obtained from the web page https://planetarynames.wr.usgs.gov

pragma Style_White_Elephant;

pragma Restrictions (No_Elaboration_Code);

package Database.Moon is

  type Feature_Name is (Copernicus,
                        Kepler,
                        Vallis_Alpes);

  type Feature is record
    Kind      : Moon_Feature_Type;
    Latitude  : Feature_Latitude;
    Longitude : Feature_Longitude;
    Size      : Feature_Size;
  end record;

  type Features is array (Feature_Name) of Feature;
  
  type Data is array (Feature_Name) of Feature;

  List : constant Data := [
--   Kind    Latitude    Longitude   Size
    (Crater,   9.620945, 339.921379,   96.0699), -- Copernicus
    (Crater,   8.120985, 321.991275,   29.4895), -- Kepler
    (Vallis,  49.208780,   3.631381,  155.417)]; -- Vallis Alpes

end Database.Moon;