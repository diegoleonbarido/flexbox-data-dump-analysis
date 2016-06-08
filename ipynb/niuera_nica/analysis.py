import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import pandas as pd
from niuera_nica import object_builder as nica_data

def plot_supply_curve(nica_data_object,start_date,end_date):
    plt.figure()
    peak_demand = max(nica_data_object.posdepacho[start_date:end_date]['Demanda'])
    plant_characteristics_df = nica_data_object.get_plant_characteristics_df(start_date,end_date)
    generation_type_list = ['bunker_fuel','interconnect','hydro','geothermal','biomass','wind']
    generation_type_color_list = {'bunker_fuel':'#FF4500',
                                  'wind':'#F0F8FF',
                                  'biomass':'#228B22',
                                  'geothermal':'#DAA520',
                                  'hydro':'#483D8B',}
    plant_types = []
    plant_types_colors = []
    for row in plant_characteristics_df.iterrows():
        for generation_type in generation_type_list:
            if row[1]['Plant'] in nica_data_object.generation_groupings[generation_type]:
                plant_types.append(generation_type)
                plant_types_colors.append(generation_type_color_list[generation_type])
    plant_characteristics_df['technology_type'] = plant_types
    plant_characteristics_df['technology_type_color'] = plant_types_colors

    cumulative_generation = []
    cumulative_generation_value = 0
    
    plant_characteristics_df.sort_values("$/kWh",ascending=True,inplace=True)
    for plant in plant_characteristics_df.iterrows():
        cumulative_generation_value+= plant[1]['Capacity MW']
        cumulative_generation.append(cumulative_generation_value)
    plant_characteristics_df['cum_gen'] = cumulative_generation

    arial = {'fontname':'Arial'}
    hfont = {'fontname':'Helvetica'}
    plt.tick_params(labelsize=7)

    # Plot Supply Curve
    plt.bar(plant_characteristics_df['cum_gen'],plant_characteristics_df['$/kWh'], 
            color=plant_characteristics_df['technology_type_color'],align='edge',
            width=-1*plant_characteristics_df['Capacity MW'])
    plt.xlabel('Capacity MW',**arial)
    plt.ylabel('$/kWh',**arial)
    plt.title('Supply Curve for 2015 - Average Hourly Generation by Plant and Technology',**arial)

    #Plot legend
    legend_patches = []
    for generation_type in generation_type_color_list:
        legend_patches.append(mpatches.Patch(color=generation_type_color_list[generation_type],label=generation_type))
    plt.legend(handles=legend_patches,loc=0)

    # Plot Peak Demand and Average Cost of Generation
    plt.axvline(peak_demand,color='black',linestyle='--')
    plt.axhline(plant_characteristics_df['$/kWh'].mean(),color='black',linestyle='--')

    # Add Text
    plt.text(peak_demand, 0.27, 'Peak Demand', 
             rotation='vertical',verticalalignment='top', horizontalalignment='right', color='black', fontsize=8,**arial)
    plt.text(100, plant_characteristics_df['$/kWh'].mean() + 0.01,'Average Cost of Generation', 
             rotation='horizontal',verticalalignment='top', horizontalalignment='left', color='black', fontsize=8,**arial)
    plt.show()