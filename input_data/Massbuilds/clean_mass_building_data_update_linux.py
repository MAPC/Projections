import pandas as pd
import numpy as np
import geopandas as gpd
from geopandas import GeoDataFrame
from shapely.geometry import Point
import os

# Windows Path
#os.chdir("K:\\DataServices\\Projects\\Current_Projects\\Projections\\Data\\03_UrbanSim\\MassBuilds")

# Linux path
os.chdir("/mnt/k/DataServices/Projects/Current_Projects/Projections/Data/03_UrbanSim/MassBuilds/")
#os.chdir("/mnt/s/Network Shares/K Drive/DataServices/Projects/Current_Projects/Projections/Data/03_UrbanSim/MassBuilds/")

# Set output region (MAPC vs. State)
# region_extent = 'MAPC'
region_extent = 'Statewide'

# Code for vintage of massbuilds data
vintage = '20230401'

# Give output names
final_res_dev = 'Massbuilds_inputs_' + vintage + '/' + region_extent + '/' + vintage + '_residential'
final_nonres_dev = 'Massbuilds_inputs_' + vintage + '/' + region_extent + '/' + vintage + '_non_residential'

os.makedirs('Massbuilds_inputs_' + vintage + '/' + region_extent, exist_ok=True)

# Load necessary data
mb = pd.read_csv('https://api.massbuilds.com/developments.csv') #get current snapshot of massbuilds.com 
#mb = pd.read_csv('Massbuilds_Inputs_' + vintage + '/massbuilds-' + vintage + '.csv')

comm_type = pd.read_csv('commtype.csv') #comm type from spatial data browser. CS transforms "municipal" to proper case to match with mb.
comm_type['municipal'] = comm_type['municipal'].str.title()


mapc_blocks = gpd.read_file('mapc_blocks_20191026/mapc_blocks_20191026.shp') #shapefile from mapc extent


### Apply first filter of data

mb = pd.merge(mb, comm_type, left_on = 'municipal', right_on = 'municipal', how = 'left')

cols_with_nulls = ['comm_type', 'Comm_low', 'Comm_high', 'Edu_publ', 'Industrial']
for cols in cols_with_nulls:
    print ('*** There are {} rows with null values for {} and they will be eliminated ***'.format(len(mb[mb[cols].isnull()]), cols))

mb = mb[~(mb['comm_type'].isnull())].copy()
mb = mb[~(mb['Comm_low'].isnull())].copy()
mb = mb[~(mb['Comm_high'].isnull())].copy()
mb = mb[~(mb['Edu_publ'].isnull())].copy()
mb = mb[~(mb['Industrial'].isnull())].copy()

fillna_cols = ['ret_sqft', 'ofcmd_sqft', 'indmf_sqft', 'whs_sqft', 'rnd_sqft', 'ei_sqft', 'other_sqft', 'hotel_sqft',
               'commsf', 'rptdemp', 'hu', 'singfamhu']

for col in fillna_cols:
    mb[col].fillna(0, inplace = True)

mb['commtot'] = mb['ret_sqft'] + mb['ofcmd_sqft'] + mb['indmf_sqft'] + mb['whs_sqft'] + mb['rnd_sqft'] +\
                mb['ei_sqft'] + mb['other_sqft'] + mb['hotel_sqft']

mb['empRE8'] = (((mb['ret_sqft'] + mb['ofcmd_sqft'] + mb['other_sqft'] + mb['hotel_sqft']) +(mb['commsf'] - mb['commtot']))/2) / mb['Comm_low']

mb['empRE9'] = (((mb['ret_sqft'] + mb['ofcmd_sqft'] + mb['other_sqft'] + mb['hotel_sqft']) +(mb['commsf'] - mb['commtot']))/2) / mb['Comm_high']

mb['empRE10'] = (mb['rnd_sqft'] + mb['ei_sqft'])/ mb['Edu_publ']

mb['empRE11'] = (mb['indmf_sqft'] + mb['whs_sqft'])/ mb['Industrial']

mb.loc[mb['rptdemp']>0, 'emp'] = mb.loc[mb['rptdemp']>0, 'rptdemp']
mb.loc[~(mb['rptdemp']>0), 'emp'] = mb.loc[~(mb['rptdemp']>0)].apply(lambda row: round(row['empRE8'] + row['empRE9'] +\
                                                                     row ['empRE10'] + row['empRE11'], 0), axis = 1)

mb['aux'] = mb['hu'] - mb['singfamhu']

mb.loc[:, 'bldtyp'] = 5
mb.loc[mb['aux'] >= 1, 'bldtyp'] = 4
mb.loc[mb['singfamhu'] > 0, 'bldtyp'] = 1

mb['status2'] = 0
mb.loc[mb['status'] == 'completed', 'status2'] = 'Completed'
mb.loc[mb['status'] == 'in_construction', 'status2'] = 'Committed'
mb.loc[(mb['status'] == 'projected') | (mb['status'] == 'planning'), 'status2'] = 'Proposed'

mb['buildings_count'] = 1
mb.loc[mb['singfamhu'] > 0, 'buildings_count'] = mb.loc[mb['singfamhu'] > 0, 'singfamhu']

mb['floors'] = 1
mb.loc[mb['stories'] > 0, 'floors'] = mb.loc[mb['stories'] > 0, 'stories']

mb['unitsz'] = 1000
mb.loc[mb['bldtyp'] == 1, 'unitsz'] = 2000

mb['buildingprogram'] = 'Other'
mb.loc[mb['ovr55'] == True, 'buildingprogram'] = 'Senior'

mb['gq'] = ""
mb.loc[mb['gqpop'] > 0, 'gp'] = 'Other'

mb['aux_2'] = mb['aff_u30']+ mb['aff_30_50'] + mb['aff_50_80']

mb['aff_80'] = mb['aff_80p']
mb.loc[((mb['aux_2'] == 0) & (mb['affrd_unit'] > 0)), 'aff_80'] = mb['affrd_unit']

print ('*** There are {} projects with no status and they will be deleted ***'.format(len(mb[mb['status2']==0])))

mb = mb[mb['status2']!=0].copy()

# take = from the zip_code
mb.loc[~(mb['zip_code'].isnull()), 'zip_code'] = mb.loc[~(mb['zip_code'].isnull()), 'zip_code'] .apply(lambda x: x[2:-1])

# add missing information
mb.loc[:, 'duration'] = 1
mb.loc[:, 'tags'] = mb['status2'].apply(lambda x: x + '; ' + 'all')
mb.loc[:, 'phased'] = 0
mb.loc[:, 'State'] = 'MA'
mb.loc[:, 'market_rate_units'] = mb['hu'] - mb['affrd_unit']
mb.loc[:, 'affordable_program'] = 'Both'
mb['zip_code'].fillna("", inplace = True) # fill NaNs zip codes with ""

required_columns = ['longitude', 'latitude', 'name', 'bldtyp', 'year_compl', 'duration', 'stalled', 'status2',\
                    'tags', 'phased', 'buildings_count', 'floors', 'nhood', 'address', 'municipal', 'state',\
                    'zip_code', 'id', 'devlper', 'user_id', 'descr', 'hu', 'unitsz', 'market_rate_units', 'affrd_unit',\
                    'affordable_program', 'buildingprogram', 'gq', 'gqpop', 'emp', 'aff_u30', 'aff_30_50', 'aff_50_80',\
                    'aff_80', 'singfamhu', 'smmultifam', 'lgmultifam' ]

mb_to_export = mb[required_columns].copy()

mb_to_export.rename(columns = {
                                'longitude': 'x',
                                'latitude': 'y',
                                'bldtyp': 'building_type_id',
                                'year_compl': 'start_year',
                                'status2': 'status',
                                'nhood': 'descriptive_location',
                                'address': 'street',
                                'municipal': 'city',
                                'zip_code': 'zip',
                                'id': 'identification',
                                'devlper': 'developer',
                                'user_id': 'source',
                                'descr': 'notes',
                                'hu': 'residential_units',
                                'unitsz': 'average_unit_size',
                                'affrd_unit': 'affordable_units',
                                'buildingprogram': 'building_program',
                                'gq': 'group_quarters',
                                'gqpop': 'population_or_beds',
                                'emp': 'employment_capacity',
                                'aff_u30': 'ami_u30_units',
                                'aff_30_50': 'ami_3050_units',
                                'aff_50_80': 'ami_5080_units',
                                'aff_80': 'ami_80p_units',
                                'singfamhu': 'SF',
                                'smmultifam': 'smMF',
                                'lgmultifam': 'lrgMF'
                            }, inplace = True)

# Second filtering
data = mb_to_export.copy()

data[data['name']=='Village at Reed Meadows']

pd.set_option('display.max_columns', 500)

# check if there are other types
data[(data['residential_units']==0)&(data['employment_capacity']==0)].shape

data['employment_capacity'] = data['employment_capacity'].astype('float')

data = data[data['start_year']>=2010]

# Divide data
res = data[data['residential_units']>0].copy()
non_res = data[data['employment_capacity']>0].copy()

## Residential Units

res['building_type_id'].unique()

res.head(2)

res['SF'].fillna(0, inplace = True)
res['smMF'].fillna(0, inplace = True)
res['lrgMF'].fillna(0, inplace = True)
res['population_or_beds'].fillna(0, inplace = True)
res['ami_u30_units'].fillna(0, inplace = True)
res['ami_3050_units'].fillna(0, inplace = True)
res['ami_5080_units'].fillna(0, inplace = True)
res['ami_80p_units'].fillna(0, inplace = True)

res_sf_only = res[(res['SF']>0)&(res['smMF']==0)&(res['lrgMF']==0)].copy()
res_sf_only.loc[:,'building_type_id'] = 1
res_mf_only = res[(res['SF']==0)&((res['smMF']>0)|(res['lrgMF']>0))].copy()
res_mf_only.loc[:,'building_type_id'] = 4

res_mixed = res[(res['SF']>0)&((res['smMF']>0)|(res['lrgMF']>0))].copy()

res_mixed.loc[:,'per_sf'] = res_mixed.apply(lambda row: row['SF']/(row['SF'] + row['smMF'] +\
                                                                   row['lrgMF']), axis=1)
res_mixed.loc[:,'per_mf'] = res_mixed.apply(lambda row: (row['smMF']+row['lrgMF'])/(row['SF'] +\
                                                            row['smMF'] + row['lrgMF']), axis=1)

res_mixed_sf = res_mixed[res_mixed['SF']>0].copy()
res_mixed_sf['residential_units'] = res_mixed_sf['per_sf']*res_mixed_sf['residential_units']
res_mixed_sf['residential_units'] = res_mixed_sf['residential_units'].apply(lambda x: np.round(x, 0))
res_mixed_sf.loc[:,'building_type_id'] = 1

res_mixed_mf = res_mixed[(res_mixed['smMF']>0)|(res_mixed['lrgMF']>0)].copy()
res_mixed_mf['residential_units'] = res_mixed_mf['per_mf']*res_mixed_mf['residential_units']
res_mixed_mf['residential_units'] = res_mixed_mf['residential_units'].apply(lambda x: np.round(x, 0))
res_mixed_mf.loc[:,'building_type_id'] = 4

res_unknown = res[(res['SF']==0)&(res['smMF']==0)&(res['lrgMF']==0)].copy()
res_unknown.loc[:,'building_type_id'] = 4

res_final = pd.concat([res_sf_only,res_mf_only,res_mixed_sf, res_mixed_mf, res_unknown], sort = True)

res_final.reset_index(inplace = True)

res_final.head(2)

# Check if we missed data
missed = data['residential_units'].sum() - res_final['residential_units'].sum()
print ('*** A number of {} residential units were missed in the process ***'.format(missed))

#Format the data

res_final['x'] = res_final['x'].astype('float')
res_final['y'] = res_final['y'].astype('float')
res_final['name'] = res_final['name'].astype('str')
res_final['building_type_id'] = res_final['building_type_id'].astype('int')
res_final['start_year'] = res_final['start_year'].astype('int')
res_final['duration'] = res_final['duration'].astype('int')
res_final['status'] = res_final['status'].astype('str')
res_final['redevelopment'] = False
res_final['tags'] = res_final['tags'].astype('str')
res_final['phased'] = False
res_final['buildings_count'] = res_final['buildings_count'].astype('int')
res_final['floors'] = res_final['floors'].astype('int')
res_final['descriptive_location'] = res_final['descriptive_location'].astype('str')
res_final['street'] =res_final['street'].astype('str')
res_final['city'] =res_final['city'].astype('str')
res_final['state'] =res_final['state'].astype('str')
res_final['zip'] =res_final['zip'].astype('str')
res_final['identification'] =res_final['identification'].astype('str')
res_final['source'] =res_final['source'].astype('str')
res_final['notes'] =res_final['notes'].astype('str')

res_final['residential_units'] =res_final['residential_units'].astype('int')
res_final['average_unit_size'] =res_final['average_unit_size'].astype('int')
res_final['market_rate_units'] =res_final['residential_units'].astype('int')
res_final['affordable_units'] = 0
res_final['affordable_program'] =res_final['affordable_program'].astype('str')
res_final['own_units'] =res_final['residential_units'].astype('int')
res_final['rent_units'] =0
res_final['building_program'] =res_final['building_program'].astype('str')
res_final['group_quarters'] =res_final['group_quarters'].astype('str')
res_final['population_or_beds'] = res_final['population_or_beds'].astype('int')
res_final['ami_3050_units']=res_final['ami_3050_units'].astype('int')
res_final['ami_5080_units']=res_final['ami_5080_units'].astype('int')
res_final['ami_80p_units']=res_final['ami_80p_units'].astype('int')
res_final['ami_u30_units']=res_final['ami_u30_units'].astype('int')

res_final.rename(columns={'ami_u30_units':'ami_30_units', 'ami_80p_units':'ami_80_units'}, inplace = True)

res_final.drop(['index', 'SF', 'per_sf', 'per_mf', 'smMF', 'lrgMF', 'employment_capacity'], inplace = True, axis =1)

res_final = res_final[~(res_final['start_year'].isin([20185,20207]))].copy()
res_final = res_final[['x','y','name','building_type_id','start_year','duration',\
'stalled','status','tags','phased','buildings_count',\
'floors','descriptive_location','street','city','state','zip','identification',\
'source','notes','residential_units','average_unit_size','market_rate_units',\
'affordable_units','affordable_program','own_units','rent_units',\
'building_program','group_quarters','population_or_beds','ami_3050_units',\
'ami_5080_units','ami_80_units','ami_30_units']].copy()
cols_res = res_final.columns

## Non Residential Units

non_res['x'] = non_res['x'].astype('float')
non_res['y'] = non_res['y'].astype('float')
non_res['name'] = non_res['name'].astype('str')
non_res['building_type_id'] = int(5)
non_res['start_year'] = non_res['start_year'].astype('int')
non_res['duration'] = non_res['duration'].astype('int')
non_res['status'] = non_res['status'].astype('str')
non_res['redevelopment'] = False
non_res['tags'] = non_res['tags'].astype('str')
non_res['phased'] = False
non_res['buildings_count'] = non_res['buildings_count'].astype('int')
non_res['floors'] = non_res['floors'].astype('int')
non_res['descriptive_location'] = non_res['descriptive_location'].astype('str')
non_res['street'] =non_res['street'].astype('str')
non_res['city'] =non_res['city'].astype('str')
non_res['state'] =non_res['state'].astype('str')
non_res['zip'] =non_res['zip'].astype('str')
non_res['identification'] =non_res['identification'].astype('str')
non_res['source'] =non_res['source'].astype('str')
non_res['notes'] =non_res['notes'].astype('str')

non_res['employment_capacity'] = non_res['employment_capacity'].astype('int')

non_res_final = non_res[['x','y','name','building_type_id','start_year','duration',\
'stalled','status','tags','phased','buildings_count','floors',\
'descriptive_location','street','city','state','zip','identification',\
'source','notes','employment_capacity']].copy()

non_res_final = non_res_final[~(non_res_final['start_year'].isin([3030, 220149, 20207]))]
cols_nonres = non_res_final.columns
if region_extent == 'Statewide':
    res_final.set_index('x').to_csv('{}.csv'.format(final_res_dev))
    non_res_final.set_index('x').to_csv('{}.csv'.format(final_nonres_dev))

if region_extent == 'MAPC':
    ### Filter development projects with mapc blocks. Comment out if this is run for MA-State
    ### make the df into gdf
    geometry = [Point(xy) for xy in zip(res_final.x, res_final.y)]
    #crs = 'epsg:4326'
    crs = {'init': 'epsg:4326'}
    res_dev_gdf = GeoDataFrame(res_final, crs=crs, geometry=geometry)

    geometry = [Point(xy) for xy in zip(non_res_final.x, non_res_final.y)]
    #  crs = 'epsg:4326'
    crs = {'init': 'epsg:4326'}
    non_res_gdf = GeoDataFrame(non_res_final, crs=crs, geometry=geometry)

    mapc_latlon = mapc_blocks.to_crs(crs)
    res_mapc = gpd.sjoin(res_dev_gdf, mapc_latlon, op= 'intersects')
    non_res_mapc = gpd.sjoin(non_res_gdf, mapc_latlon, op= 'intersects')

    res_mapc[cols_res].set_index('x').to_csv('{}.csv'.format(final_res_dev))
    non_res_mapc[cols_nonres].set_index('x').to_csv('{}.csv'.format(final_nonres_dev))
