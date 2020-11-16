# Setup
import pandas as pd

maps_df = pd.read_excel('data\maps_data.xlsx')
pillar_df = pd.read_csv('data\pillar mark.csv')
pillar_dict = pillar_df.to_dict()

# Goal is to append pillar mark data to maps as a separate column

def assign_pillar(map):
    pillar_mark = pillar_df[pillar_df['Map Name'] == map]['Pillar']
    return pillar_mark

maps_df['pillar_label'] = maps_df['Map Name'].apply(assign_pillar)

maps_df.to_csv('maps_data.csv')