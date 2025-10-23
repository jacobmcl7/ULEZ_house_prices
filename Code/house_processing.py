# This script processes all the data for the ULEZ project

# It needs to take place within the ArcGIS project titled 'house_processing'

import arcpy

# ensure the basemap XY coordinate system is set to WGS 1984 Web Mercator (auxiliary sphere)

# we need to access a locator to geocode the addresses. To do this, we use ESRI's free UK locator, which we access as follows
# go Insert > Connections > Server > New ArcGIS Server, and then paste in the following URL: https://datahub.esriuk.com/arcgis/rest/services/gb_locators/os_open_names_locator/GeocodeServer
# this must be done first, before starting the geocoding loop

#---------------------------------#
# Import/make the shapefiles
#---------------------------------#

# Make the ULEZ borders - first import the ULEZ shapefiles
ULEZ_2023 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\London-wide_ULEZ_expansion\LEZ_Boundary_20071113.shp"
ULEZ_2023 = arcpy.management.MakeFeatureLayer(ULEZ_2023, "ULEZ_2023")

ULEZ_2021 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\InnerUltraLowEmissionZone\InnerUltraLowEmissionZone.shp"
ULEZ_2021 = arcpy.management.MakeFeatureLayer(ULEZ_2021, "ULEZ_2021")

ULEZ_2019 = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\ULEZCentral_CongestionChargingZone\Shapefile\UltraLowEmissionsZoneBoundary(ULEZ).shp"
ULEZ_2019 = arcpy.management.MakeFeatureLayer(ULEZ_2019, "ULEZ_2019")


# Now get the borders of the ULEZs
ULEZ_2023_border = arcpy.management.PolygonToLine(ULEZ_2023, "ULEZ_2023_border")
ULEZ_2021_border = arcpy.management.PolygonToLine(ULEZ_2021, "ULEZ_2021_border")
ULEZ_2019_border = arcpy.management.PolygonToLine(ULEZ_2019, "ULEZ_2019_border")


#---------------------------------#
# Now start the processing
#---------------------------------#

years = ["2025", "2024", "2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015"]

for year in years:
    # Import the csv of Price Paid data
    Price_Paid = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Temp\Price_Paid_Data\pp_"+year+".csv"
    Price_Paid = arcpy.management.MakeTableView(Price_Paid, "Price_Paid")

    # Now geocode the addresses (using the locator)
    arcpy.geocoding.GeocodeAddresses(
        in_table="Price_Paid",
        address_locator=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing\GeocodeServer on datahub.esriuk.com.ags\gb_locators\os_open_names_locator.GeocodeServer",
        in_address_fields="Full_Location <None> VISIBLE NONE;PlaceName Field8 VISIBLE NONE;Street Field10 VISIBLE NONE;PopulatedPlace Field12 VISIBLE NONE;DistrictBorough Field13 VISIBLE NONE;CountyUnitary Field14 VISIBLE NONE;Postcode Field4 VISIBLE NONE",
        out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing\house_processing.gdb\Price_Paid_GeocodeAddresses",
        out_relationship_type="STATIC",
        country=None,
        location_type="",
        category=None,
        output_fields=""
    )

    # Now perform a spatial join to record which ULEZ zone each point is in
    arcpy.analysis.SpatialJoin(
        target_features="Price_Paid_GeocodeAddresses",
        join_features="ULEZ_2023",
        out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing\house_processing.gdb\Price_Paid_2023_join",
        join_operation="JOIN_ONE_TO_ONE",
        join_type="KEEP_ALL",
        field_mapping=None,
        match_option="WITHIN",
        search_radius=None,
        distance_field_name="",
        match_fields=None
    )

    # Rename the field 'Join_Count' to 'Join_Count_2023' in the Price_Paid_2023_join feature class
    arcpy.management.AlterField(
        in_table="Price_Paid_2023_join",
        field="Join_Count",
        new_field_name="Join_Count_2023",
        new_field_alias="Join_Count_2023"
    )

    # Now do the same for 2021
    arcpy.analysis.SpatialJoin(
        target_features="Price_Paid_2023_join",
        join_features="ULEZ_2021",
        out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing\house_processing.gdb\Price_Paid_2023_2021_join",
        join_operation="JOIN_ONE_TO_ONE",
        join_type="KEEP_ALL",
        field_mapping=None,
        match_option="WITHIN",
        search_radius=None,
        distance_field_name="",
        match_fields=None
    )

    # Rename the field 'Join_Count' to 'Join_Count_2021' in the Price_Paid_2023_2021_join feature class
    arcpy.management.AlterField(
        in_table="Price_Paid_2023_2021_join",
        field="Join_Count",
        new_field_name="Join_Count_2021",
        new_field_alias="Join_Count_2021"
    )

    # Now do the same for 2019
    arcpy.analysis.SpatialJoin(
        target_features="Price_Paid_2023_2021_join",
        join_features="ULEZ_2019",
        out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\house_processing\house_processing.gdb\Price_Paid_2023_2021_2019_join",
        join_operation="JOIN_ONE_TO_ONE",
        join_type="KEEP_ALL",
        field_mapping=None,
        match_option="WITHIN",
        search_radius=None,
        distance_field_name="",
        match_fields=None
    )

    # Rename the field 'Join_Count' to 'Join_Count_2019' in the Price_Paid_2023_2021_2019_join feature class
    arcpy.management.AlterField(
        in_table="Price_Paid_2023_2021_2019_join",
        field="Join_Count",
        new_field_name="Join_Count_2019",
        new_field_alias="Join_Count_2019"
    )

    # Now calculate the distance from each point to each ULEZ border - first 2023
    arcpy.analysis.Near(
        in_features="Price_Paid_2023_2021_2019_join",
        near_features=f"ULEZ_2023_border",
        search_radius=None,
        location="NO_LOCATION",
        angle="NO_ANGLE",
        method="PLANAR",
        field_names=f"NEAR_FID Near_ID;NEAR_DIST Dist_2023",
        distance_unit="Kilometers"
    )

    # Now 2021
    arcpy.analysis.Near(
        in_features="Price_Paid_2023_2021_2019_join",
        near_features=f"ULEZ_2021_border",
        search_radius=None,
        location="NO_LOCATION",
        angle="NO_ANGLE",
        method="PLANAR",
        field_names=f"NEAR_FID Near_ID;NEAR_DIST Dist_2021",
        distance_unit="Kilometers"
    )

    # Now 2019
    arcpy.analysis.Near(
        in_features="Price_Paid_2023_2021_2019_join",
        near_features=f"ULEZ_2019_border",
        search_radius=None,
        location="NO_LOCATION",
        angle="NO_ANGLE",
        method="PLANAR",
        field_names=f"NEAR_FID Near_ID;NEAR_DIST Dist_2019",
        distance_unit="Kilometers"
    )

    #---------------------------------#
    # Export the processed attribute table
    #---------------------------------#

    # Export the table
    arcpy.conversion.TableToTable(
        in_rows="Price_Paid_2023_2021_2019_join",
        out_path=r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Temp",
        out_name=r"pp_"+year+"_processed.csv"
    )