import pandas as pd
import numpy as np
import copy
import os.path
from datetime import datetime
from sqlalchemy import cast,Date,text
import psql_15_sec

class NicaraguaData(object):
    """  
    This class represents the dataset for Nicaragua on a national level.
    """

    def __init__(self, data_dir,
                    start_date=datetime(2013,1,1), end_date=datetime(2020,1,1),
                    real_time=False,posdepacho=True, posdepacho_costs=True, posdepacho_spot=True,
                    posdepacho_colors=True, predespacho=True, hydro_heights=True,add_group_labels=True):
        '''
        Initializes a datasource object. Reliable data is available from Jan 1 2013.
        '''
        self.data_dir = data_dir
        self.generation_groupings = {}

        self.generation_groupings['bunker_fuel'] = ['PCG1','PCG2','PCG3','PCG4','PCG5','PCG6','PCG7','PCG8','PCG9',
        'PHC1','PHC2','CEN','EEC20','EEC','TPC','PLB1','PLB2','PMG3','PMG4','PMG5','PCHN','PNI1','PNI2','GSR']#23 + PCHN
        self.generation_groupings['interconnect'] = ['LNI-L9040','SND-L9090','AMY-L9030','TCPI-L9150'] #4
        self.generation_groupings['hydro'] = ['PCA1','PCA2','PCF1','PCF2','HPA1','HPA2','HEM','PHL1','PHL2','PHD']#10
        self. generation_groupings['geothermal'] = ['PEN1 y 2','PEN3','PEN4','PMT1','PMT2','PMT3','MTL']#6 + PEN1 y 2
        self.generation_groupings['biomass'] = ['MTR','NSL'] #2
        self.generation_groupings['wind'] = ['AMY1','AMY2','ABR','EOL','PBP']#5

        self.generation_groupings['agc'] = ['CEN','EEC','PCA1','PCA2','TPC']
        self.generation_groupings['albanisa'] = ['PCG1','PCG2','PCG3','PCG4','PCG5','PCG6','PCG7','PCG8','PCG9','PHC1','PHC2']
        self.generation_groupings['peak'] = ['PLB1','PLB2','PHC1','PHC2']
        self.generation_groupings['all_gen'] = []
        self.generation_groupings['all_gen'].extend(self.generation_groupings['bunker_fuel'])
        self.generation_groupings['all_gen'].extend(self.generation_groupings['hydro'])
        self.generation_groupings['all_gen'].extend(self.generation_groupings['geothermal'])
        self.generation_groupings['all_gen'].extend(self.generation_groupings['biomass'])
        self.generation_groupings['all_gen'].extend(self.generation_groupings['wind'])
        self.generation_groupings['all_gen'].extend(self.generation_groupings['interconnect'])
        if real_time:
            print 'Loading Real-Time 20-Second Data...(real_time)'
            self.real_time = self._get_real_time_df(start_date,end_date)
        if posdepacho:
            print 'Loading Posdepacho Data... (posdepacho)'
            self.posdepacho = self._get_posdepacho_df(start_date,end_date)
        if posdepacho_costs:
            print 'Loading Posdepacho Cost Data... (posdepacho_costs)'
            self.posdepacho_costs = self._get_posdepacho_cost_df(start_date,end_date)

        if posdepacho_spot:
            print 'Loading Posdepacho Spot Price Data... (posdepacho_spot)'
            self.posdepacho_spot = self._get_posdepacho_spot_price_df(start_date,end_date)
        if posdepacho_colors:
            print 'Loading Posdepacho Color Data... (posdepacho_colors)'
            self.posdepacho_colors = self._get_posdepacho_color_df(start_date,end_date)
        if predespacho:
            print 'Loading Predespacho Data... (predespacho)'
            self.predespacho = self._get_predespacho_df(start_date,end_date)
        if hydro_heights:
            print 'Loading Hydro Height Data... (hdyro_heights)'
            self.hydro_heights = self._get_hydropower_height_df(start_date,end_date)

        print 'Loading overall plant characteristics dataframe... (plant_characteristics_df)'
        self.plant_characteristics_df = self.get_plant_characteristics_df(start_date,end_date,printDate=False)
        
        if add_group_labels:
            self.posdepacho = self._add_group_labels(self.posdepacho)
        if predespacho and posdepacho:
            print 'Combining pre and pos depacho into new dataframe ... (pre_and_pos_depacho)'
            self.pre_and_pos_depacho = self._get_pre_and_pos_merged_df(self.predespacho,self.posdepacho)
        if posdepacho and posdepacho_costs:
            print 'Combining posdepacho and posdepacho costs into new dataframe ... (posdepacho_and_costs)'
            self.posdepacho_and_costs = self._get_pos_and_pos_cost_merged_df(self.posdepacho,self.posdepacho_costs)
        self.capacity_dict = self.get_capacity_dict()
        print 'Done.'

    def _get_real_time_df(self,start_date,end_date):
        metadata = psql_15_sec.get_metadata()
        table_ref = psql_15_sec.setup_tables(metadata)
        values = table_ref.select().\
                where(cast(table_ref.c.datetime,Date)>=start_date).\
                where(cast(table_ref.c.datetime,Date)<end_date)
        result = values.execute()
        df_15_sec = pd.DataFrame(result.fetchall(),columns=result.keys())
        df_15_sec = df_15_sec.drop_duplicates(subset='datetime').dropna()
        df_15_sec = df_15_sec.set_index('datetime')
        #We store all values in the database as integers, but they are really decimals (135 is 13.5)
        df_15_sec = df_15_sec/10 
        #There is one generator that I called gen_Unknown because it is a number on the map isn't near
        #any of the words, but I wanted to collect it anyway so we could eventually identify it.
        df_15_sec = df_15_sec.sort_index()
        return df_15_sec
    
    def _get_posdepacho_df(self,start_date,end_date):
        dfs = []
        num_months = (end_date.year-start_date.year)*12+end_date.month-start_date.month
        for val in range(-1,num_months):
            year = str(start_date.year+(start_date.month+val)/12)
            month = str((start_date.month+val)%12+1)
            if os.path.isfile(self.data_dir+'/NatData/'+str(year)+'_'+str(month)+'.csv'):
                dfs.append(pd.read_csv(self.data_dir+'/NatData/'+str(year)+'_'+str(month)+'.csv'))

        df_actual = pd.concat(dfs)
        datetimes = []
        for val in df_actual.iterrows():
            date = val[1]['Date'].split('/')
            hora = val[1]['HORA']
            datetimes.append(datetime(int(date[2]),int(date[1]),int(date[0]),int(hora)))
        df_actual['datetime'] = datetimes
        df_actual = df_actual.set_index('datetime',drop=True)
        df_actual['LNI-L9040'] = df_actual['LNI-L9040']*-1
        df_actual['SND-L9090'] = df_actual['SND-L9090']*-1
        df_actual['AMY-L9030'] = df_actual['AMY-L9030']*-1
        df_actual['TCPI-L9150'] = df_actual['TCPI-L9150']*-1
        df_actual = df_actual.drop('Unnamed: 0',axis=1)
        return df_actual[start_date:end_date]

    def _get_posdepacho_cost_df(self,start_date,end_date):
        dfs = []
        num_months = (end_date.year-start_date.year)*12+end_date.month-start_date.month
        for val in range(-1,num_months):
            year = str(start_date.year+(start_date.month+val)/12)
            month = str((start_date.month+val)%12+1)
            if os.path.isfile(self.data_dir+'/NatDataCosts/'+str(year)+'_'+str(month)+'_cost.csv'):
                df = pd.read_csv(self.data_dir+'/NatDataCosts/'+str(year)+'_'+str(month)+'_cost.csv')
                if df.keys()[1] != 'HORA':
                    print str(month) +' ' + str(year)
                dfs.append(df)

        df_actual_cost = pd.concat(dfs)
        datetimes = []
        for val in df_actual_cost.iterrows():
            try:
                date = val[1]['Date'].split('/')
            except:
                print val[1]
            hora = val[1]['HORA']
            datetimes.append(datetime(int(date[2]),int(date[1]),int(date[0]),int(hora)))
        df_actual_cost['datetime'] = datetimes
        df_actual_cost = df_actual_cost.set_index('datetime',drop=True)
        df_actual_cost = df_actual_cost.drop('Unnamed: 0',axis=1)
        df_actual_cost = df_actual_cost.drop('Date',axis=1)
        df_actual_cost = df_actual_cost.drop('HORA',axis=1)
        return df_actual_cost[start_date:end_date]

    def _get_posdepacho_spot_price_df(self,start_date,end_date):
        '''
        Loads a dataframe containing the hourly spot price between 'start_date' and 'end_date'. 
        Data is taken from the NatPosDepacho_Spot_Market folder, which should contain a seperate csv
        file for each month, which was downloaded as zip files that need to be unzipped from 
        www.cndc.org.ni > Descargas > Comercial > Transacciones Economicas > 
        Precios Energia y Potencia
        '''
        concatanated_price_dataframe = pd.DataFrame()
        num_months = (end_date.year-start_date.year)*12+end_date.month-start_date.month
        for val in range(-1,num_months):
            year = str(start_date.year+(start_date.month+val)/12)
            month = str((start_date.month+val)%12+1)
            if os.path.exists(self.data_dir+'/NatPosDepacho_Spot_Market/'+str(year)+'_'+str(month)+'.csv'): 
                ops = pd.read_csv(self.data_dir+'/NatPosDepacho_Spot_Market/'+str(year)+'_'+str(month)+'.csv') 
                #Keeping only rows with the values we need
                row_hours = ops.loc[(ops['Unnamed: 0'].isin(['1:00','2:00','3:00','4:00','5:00','6:00',
                    '7:00','8:00','9:00','10:00','11:00','12:00','13:00','14:00','15:00','16:00','17:00',
                    '18:00','19:00','20:00','21:00','22:00','23:00','24:00']) 
                | ops['Unnamed: 0'].isin(['HRS']) | ops['Unnamed: 0'].isin(['Horas']) 
                | ops['Unnamed: 0'].isin(['HORAS']) | ops['Unnamed: 1'].isin(['1:00','2:00','3:00',
                    '4:00','5:00','6:00','7:00','8:00','9:00','10:00','11:00','12:00','13:00',
                    '14:00','15:00','16:00','17:00','18:00','19:00','20:00','21:00','22:00','23:00','24:00']) 
                | ops['Unnamed: 1'].isin(['HRS']) | ops['Unnamed: 1'].isin(['Horas']) | ops['Unnamed: 1'].isin(['HORAS']))]

                #Reset Index and Reshaping the data frame
                row_hours=row_hours.dropna(axis=1,how='all')
                row_hours.columns = row_hours.iloc[0]
                row_hours.drop(row_hours.index[:1], inplace=True)

                #Creating a list of variables to use for the melt
                day_list = [None] * (len(row_hours.columns))
                count=-1
                for i,val in enumerate(row_hours.columns):
                    if val == 'HRS':
                        pass
                    elif val == 'HORAS':
                        pass
                    elif val == 'Horas':
                        pass
                    else:
                        count= count+1
                        day_list[count]=val


                #Checking if there is a duplicate vallue of the 'horas' column
                if row_hours.columns[-1] == 'HRS':
                   row_hours.columns.values[len(row_hours.columns)-1] = 'FALSE ROW'
                   row_hours = row_hours.drop(row_hours.columns[[len(row_hours.columns)-1]], axis=1)
                elif row_hours.columns[-1] == 'HORAS':
                   row_hours.columns.values[len(row_hours.columns)-1] = 'FALSE ROW'
                   row_hours = row_hours.drop(row_hours.columns[[len(row_hours.columns)-1]], axis=1)
                elif row_hours.columns[-1] == 'Horas':
                   row_hours.columns.values[len(row_hours.columns)-1] = 'FALSE ROW'
                   row_hours = row_hours.drop(row_hours.columns[[len(row_hours.columns)-1]], axis=1)
                else:
                    pass

                month_df = pd.melt(row_hours, id_vars= row_hours.columns[0], value_vars= day_list)
                month_df.columns = ['hour', 'date','price']
                month_df['year'] = year
                month_df=month_df.dropna() #Get rid of NAs again after the reshape
                #print month_df
                
                concatanated_price_dataframe = pd.concat([concatanated_price_dataframe,month_df])
        spanish_months_dict = ['Ene','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
        datetime_column = []
        price_column = []
        for row in concatanated_price_dataframe.iterrows():
            #print row[1]
            date_split = row[1]['date'].split('-')
            hour_number = int(row[1]['hour'].split(':')[0])
            price = float(row[1]['price'])
            try:
                day_number = int(date_split[0])
                month_number = spanish_months_dict.index(date_split[1])+1
                year_number = int(row[1]['year'])
                datetime_object =  (datetime(year_number,month_number,day_number,hour_number))
                datetime_column.append(datetime_object)
                price_column.append(price)
            except:
                #There are some rows with things like 31-June and 0-January.
                pass
        return pd.DataFrame(price_column,index=datetime_column,columns=['price'])[start_date:end_date]


    def _get_posdepacho_color_df(self,start_date,end_date):
        dfs = []
        num_months = (end_date.year-start_date.year)*12+end_date.month-start_date.month
        for val in range(-1,num_months):
            year = str(start_date.year+(start_date.month+val)/12)
            month = str((start_date.month+val)%12+1)
            if os.path.isfile(self.data_dir+'/NatDataColor/'+str(year)+'_'+str(month)+'_color.csv'):
                df = pd.read_csv(self.data_dir+'/NatDataColor/'+str(year)+'_'+str(month)+'_color.csv')
                if df.keys()[1] != 'HORA':
                    print str(month) +' ' + str(year)
                dfs.append(df)

        df_actual_color = pd.concat(dfs)
        datetimes = []
        for val in df_actual_color.iterrows():
            try:
                date = val[1]['Date'].split('/')
            except:
                print val[1]
            hora = val[1]['HORA']
            datetimes.append(datetime(int(date[2]),int(date[1]),int(date[0]),int(hora)))
            old_val = val
        df_actual_color['datetime'] = datetimes
        df_actual_color = df_actual_color.set_index('datetime',drop=True)
        df_actual_color = df_actual_color.drop('Date',axis=1)
        df_actual_color = df_actual_color.drop('Demanda',axis=1)
        df_actual_color = df_actual_color.drop('HORA',axis=1)
        df_actual_color = df_actual_color.drop('Unnamed: 0',axis=1)
        df_actual_color = df_actual_color.drop('Unnamed: 24',axis=1)
        return df_actual_color[start_date:end_date]

    def _get_hydropower_height_df(self,start_date,end_date):
        dfs = []
        num_months = (end_date.year-start_date.year)*12+end_date.month-start_date.month
        for val in range(-1,num_months):
            year = str(start_date.year+(start_date.month+val)/12)
            month = str((start_date.month+val)%12+1)
            if os.path.isfile(self.data_dir+'/NatDataHydroHeight/'+str(year)+'_'+str(month)+'hydro_height.csv'):  
                df = pd.read_csv(self.data_dir+'/NatDataHydroHeight/'+str(year)+'_'+str(month)+'hydro_height.csv')
                if df.keys()[len(df.keys())-2] != 'HORA':
                    columns = list(df.keys()[2:])
                    columns.insert(len(df.keys())-3,'HORA')
                    df = df.drop('Unnamed: 0',axis=1)
                    df.columns = columns
                dfs.append(df)

        df_hydro_height = pd.concat(dfs)
        datetimes = []
        for val in df_hydro_height.iterrows():
            try:
                date = val[1]['Date'].split('/')
            except:
                print val[1]
            hora = val[1]['HORA']
            datetimes.append(datetime(int(date[2]),int(date[1]),int(date[0]),int(hora)))
        df_hydro_height['datetime'] = datetimes
        df_hydro_height = df_hydro_height.set_index('datetime',drop=True)
        df_hydro_height = df_hydro_height.drop('Unnamed: 0',axis=1)
        df_hydro_height = df_hydro_height.drop('Date',axis=1)
        df_hydro_height = df_hydro_height.drop('HORA',axis=1)
        for plant in df_hydro_height.keys():
            df_hydro_height = df_hydro_height[abs(df_hydro_height[plant].diff())
                    <np.percentile(df_hydro_height[plant].diff().dropna(),99.9)]
        return df_hydro_height[start_date:end_date]

    def _get_predespacho_df(self,start_date,end_date):
        '''
        Returns a dataframe between the specified dates from the predicted_hourly.csv file that was created from scraping
        www.cndc.org.ni > Predespacho
        '''
        df_predict = pd.read_csv(self.data_dir+'/predicted_hourly.csv')
        df_predict['datetime'] = df_predict['Unnamed: 0']
        df_predict = df_predict.drop('Unnamed: 0',axis=1)
        df_predict = df_predict.drop('HORA',axis=1)
        df_predict = df_predict.set_index('datetime')
        df_predict = df_predict.dropna()
        return df_predict

    def _get_pre_and_pos_merged_df(self,df_actual,df_predict):
        df_predict_columns_renamed = copy.deepcopy(df_predict)
        df_predict_columns_renamed.columns = [column+'_pred' for column in df_predict.keys()]
        df_gen_predict = df_predict_columns_renamed.join(df_actual,how='inner')
        for val in df_gen_predict.keys():
            if val+'_pred' in df_gen_predict.keys():
                df_gen_predict[val+'_diff'] = df_gen_predict[val]-df_gen_predict[val+'_pred']
        return df_gen_predict

    def _get_pos_and_pos_cost_merged_df(self,df_actual,df_cost):
        df_cost_columns_renamed = copy.deepcopy(df_cost)
        df_cost_columns_renamed.columns = [column+'_cost' for column in df_cost_columns_renamed.keys()]
        df_gen_cost = df_cost_columns_renamed.join(df_actual,how='inner')
        df_gen_cost = df_gen_cost.fillna(method='ffill')
        return df_gen_cost

    def _add_group_labels(self,df_for_grouping):
        all_gen = []
        for key in self.generation_groupings.keys():
            groupings_to_load = [plant for plant in self.generation_groupings[key] if plant in df_for_grouping]
            df_for_grouping[key] = df_for_grouping[groupings_to_load].sum(axis=1)
        df_for_grouping['net_Demanda'] = df_for_grouping['Demanda'] - df_for_grouping['wind']-df_for_grouping['interconnect']
        df_for_grouping['dispatch'] = df_for_grouping['all_gen']-df_for_grouping['wind']-df_for_grouping['interconnect']
        return df_for_grouping

    @staticmethod
    def get_capacity_dict():
        cap_dict = {}
        cap_dict['CEN'] = 57
        cap_dict['PCG1'] = 19.2
        cap_dict['PCG2'] = 19.2
        cap_dict['PCG3'] = 19.2
        cap_dict['PCG4'] = 19.2
        cap_dict['PCG5'] = 18.6
        cap_dict['PCG6'] = 12.9
        cap_dict['PCG7'] = 38
        cap_dict['PCG8'] = 25.3
        cap_dict['PCG9'] = 45.3
        cap_dict['PHC1'] = 15
        cap_dict['PHC2'] = 45
        cap_dict['EEC20'] = 18.5
        cap_dict['EEC'] = 50
        cap_dict['TPC'] = 50.9
        cap_dict['PLB1'] = 23
        cap_dict['PLB2'] = 35
        cap_dict['PMG3'] = 42
        cap_dict['PMG4'] = 5.5
        cap_dict['PMG5'] = 5.5
        cap_dict['PNI1'] = 50
        cap_dict['PNI2'] = 50
        cap_dict['GSR'] = 5.4

        #Wind
        cap_dict['ABR'] = 39.6
        cap_dict['AMY1'] = 39.9
        cap_dict['AMY2'] = 23.1
        cap_dict['EOL'] = 44.4
        cap_dict ['PBP'] = 39.6

        #Hydro
        cap_dict['PCA1'] = 25
        cap_dict['PCA2'] = 25
        cap_dict['PCF1'] = 25
        cap_dict['PCF2'] = 25
        cap_dict['HPA1'] = 6.5
        cap_dict['HPA2'] = 6.5
        cap_dict['PHL1'] = 8.5
        cap_dict['PHL2'] = 8.5

        #Geothermal
        cap_dict['PEN1 y 2'] = 5
        cap_dict['PEN3'] = 36
        cap_dict['PEN4'] = 36
        cap_dict['PMT1'] = 30
        cap_dict['PMT2'] = 30
        cap_dict['PMT3'] = 6.4
        cap_dict['PCHN'] = 15
        cap_dict['MTL'] = 38

        #Biomass
        cap_dict['MTR'] = 35
        cap_dict['NSL'] = 30
        return cap_dict


    def get_cfs(self,start_date,end_date):
        cap_fact = []
        cap_dict = {}
        cap_dict = self.get_capacity_dict()
        keys = []
        for val in self.posdepacho.keys():
            if val in cap_dict:
                cf = float(sum(self.posdepacho[val][start_date:end_date])/\
                    (cap_dict[val]*len(self.posdepacho[start_date:end_date])))
                cap_dict[val] = cf
                cap_fact.append(cf)
                keys.append(val)
        df_cf = pd.DataFrame([keys,cap_fact]).T
        df_cf.columns = ['Plant','CF']
        df_cf.sort_values('CF',ascending=False)
        return cap_dict

    def get_plant_characteristics_df(self,start_date,end_date,sort_key='% Used',ascending=False,printDate=True):
        generation_type_list = ['bunker_fuel','interconnect','hydro','geothermal','biomass','wind']
        gens = self.posdepacho.keys()
        cf_dict = self.get_cfs(start_date,end_date)
        cap_dict = self.get_capacity_dict()
        cfs = []
        costs = []
        perc_maint = []
        capacities = []
        gens_for_df = []
        plant_types = []
        for gen in gens:
            if gen in self.posdepacho_colors and gen in self.posdepacho_costs:
                perc_maint.append(float(len(self.posdepacho_colors[start_date:end_date][gen]\
                    [self.posdepacho_colors[start_date:end_date][gen]=='255 255 153']))/\
                    float(len(self.posdepacho_colors[start_date:end_date][gen])))
                costs.append(self.posdepacho_costs[start_date:end_date][gen].mean()/1000)
                cfs.append(cf_dict[gen])
                plant_type = 'None'
                for generation_type in generation_type_list:
                    if gen in self.generation_groupings[generation_type]:
                        plant_type = generation_type
                plant_types.append(plant_type)
                capacities.append(cap_dict[gen])
                gens_for_df.append(gen)
        df_gen_cost_total = pd.DataFrame([gens_for_df,costs,cfs,perc_maint,capacities,plant_types]).T
        df_gen_cost_total.columns = ['Plant','$/kWh','CF','FOR','Capacity MW','technology_type']
        df_gen_cost_total = df_gen_cost_total[df_gen_cost_total['FOR']<1]
        df_gen_cost_total = df_gen_cost_total[df_gen_cost_total['CF']>0]
        df_gen_cost_total['% Used'] = df_gen_cost_total['CF']/(1-df_gen_cost_total['FOR'])
        df_gen_cost_total.fillna(0,inplace=True)
        if sort_key not in df_gen_cost_total:
            sort_key = '% Used'
        if printDate:
            print str(start_date.month)+'/'+str(start_date.year) + ' to ' + str(end_date.month)+'/'+str(end_date.year)
            print 'Sorted by '+sort_key
        return df_gen_cost_total.sort_values(sort_key,ascending=ascending)
