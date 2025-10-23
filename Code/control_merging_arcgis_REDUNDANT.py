# this file gets the air pollution reading nearest the given postcode sector

# we first need to get each file from the air quality download into the right format, by deleting the rows before the observations start
# this is done manually in Excel, and then saved as a csv file with the same name

# Import the postcode sector borders
postcode_sectors = r"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\GB_Postcodes\PostalSector.shp"
postcode_sectors = arcpy.management.MakeFeatureLayer(postcode_sectors, "postcode_sectors")


# Now loop the code to get nearest pollution figure to pcsector centroid, over the pollutants available

for pollutant in ["pm102019g", "pm252019g", "nox2019", "no22019"]:

    # Get centroids of the postcode sectors
    arcpy.management.FeatureToPoint(
        in_features="PostalSector",
        out_feature_class=r"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\control_merging_arcgis\control_merging_arcgis.gdb\PostalSector_FeatureToPoint",
        point_location="CENTROID"
    )

    # Import the environmental data from excel
    arcpy.conversion.ExcelToTable(
        Input_Excel_File=fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Input\Extra controls\Air quality\map{pollutant}.xlsx",
        Output_Table=fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\control_merging_arcgis\control_merging_arcgis.gdb\pollution_table_{pollutant}",
        Sheet=f"map{pollutant}",
        field_names_row=6,
        cell_range="A6:D500000"
    )

    # Geocode the pollution data
    arcpy.management.XYTableToPoint(
        in_table=f"pollution_table_{pollutant}",
        out_feature_class=fr"C:\Users\jpmcl\OneDrive\Documents\ArcGIS\Projects\control_merging_arcgis\control_merging_arcgis.gdb\pollution_table_{pollutant}_XYTableToPoint",
        x_field="x",
        y_field="y",
        z_field=None,
        coordinate_system='PROJCS["Transverse_Mercator",GEOGCS["GCS_Airy 1830",DATUM["D_unknown",SPHEROID["airy",6377563.396,299.3249753150316]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["false_easting",400000.0],PARAMETER["false_northing",-100000.0],PARAMETER["central_meridian",-2.0],PARAMETER["scale_factor",0.9996012717],PARAMETER["latitude_of_origin",49.0],UNIT["Meter",1.0]];-5220400 -15524400 450481592.767097;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision'
    )

    # Get the ID of the point nearest to the centroid of each postcode sector, and add this to the postcode sector data
    arcpy.analysis.Near(
        in_features="PostalSector_FeatureToPoint",
        near_features=f"pollution_table_{pollutant}_XYTableToPoint",
        search_radius=None,
        location="NO_LOCATION",
        angle="NO_ANGLE",
        method="PLANAR",
        field_names="NEAR_FID nearest_ID;NEAR_DIST nearest_distance",
        distance_unit="Kilometers"
    )

    # Get the air quality reading corresponding to this point
    arcpy.management.JoinField(
        in_data="PostalSector_FeatureToPoint",
        in_field="nearest_ID",
        join_table=f"pollution_table_{pollutant}_XYTableToPoint",
        join_field="OBJECTID",
        fields=None,
        fm_option="NOT_USE_FM",
        field_mapping=None,
        index_join_fields="NO_INDEXES"
    )

    # Remove all fields other than those of interest, and export
    arcpy.management.DeleteField(
        in_table="PostalSector_FeatureToPoint",
        drop_field="SectID;GISSect;StrSect;PostDist;PostArea;DistNum;SecNum;PCCnt;AnomCnt;RefPC;x;y;Sprawl;Locale;pcsect_ori;pcsect_in_;pcsect_in1;pcsect_i_1;pcsect_imd;pcsect_pop;pcsect_sal;pcsect_sha;pcsect_s_1;pcsect_s_2;pcsect_s_3;pcsect_s_4;pcsect_s_5;pcsect_s_6;pcsect_s_7;pcdist_sha;pcdist_s_1;pcdist_s_2;pcdist_s_3;pcdist_s_4;pcdist_s_5;pcdist_s_6;pcdist_s_7;pcsect_avg;pcdist_avg;pcsect_o_1;pcsect_i_2;pcsect_i_3;pcsect_i_4;pcsect_i_5;pcsect_p_1;pcsect_s_8;pcsect_s_9;pcsect__10;pcsect__11;pcsect__12;pcsect__13;pcsect__14;pcsect__15;pcsect__16;pcdist_s_8;pcdist_s_9;pcdist__10;pcdist__11;pcdist__12;pcdist__13;pcdist__14;pcdist__15;pcsect_a_1;pcdist_a_1;ORIG_FID;nearest_ID;nearest_distance;ukgridcode;x_1;y_1",
        method="DELETE_FIELDS"
    )

    arcpy.conversion.TableToExcel(
        Input_Table="PostalSector_FeatureToPoint",
        Output_Excel_File=fr"C:\Users\jpmcl\OneDrive\Documents\Economics\Papers\ULEZ on house prices\Data\Temp\{pollutant}_processed.xls",
        Use_field_alias_as_column_header="NAME",
        Use_domain_and_subtype_description="CODE"
    )

# these files can now be used in the control merging