from sqlalchemy import create_engine
import pandas as pd
from datetime import datetime
#%matplotlib inline
import matplotlib.pyplot as plt

url = "postgresql://{}:{}@{}:{}/{}".format('flexbox','flexbox','cosmos.niuera.co',5432,'flexbox_db_server_compiled')
con = create_engine(url,client_encoding='utf8')


for i in range(1,31):
	print(datetime.now())
	#Pre Implementation
	path_import_pre = "select * from fridge_power where datetime < '2016-07-01' and hostname = 'flxbxD" + str(i) + "'"
	path_export_pre = '/Users/diego/Desktop/Data/flexbox_event_analysis/pre_intervention/' + 'flxbxD'+ str(i) + '.csv'

	df_test_pre = pd.read_sql(path_import_pre,con)
	df_test_pre.to_csv(path_export_pre)

	#Post Implementation
	path_import_post = "select * from fridge_power where datetime >= '2016-07-01' and hostname = 'flxbxD" + str(i) + "'"
	path_export_post = '/Users/diego/Desktop/Data/flexbox_event_analysis/post_intervention/' + 'flxbxD'+ str(i) + '.csv'

	df_test_post = pd.read_sql(path_import_post,con)
	df_test_post.to_csv(path_export_post)

	print(i)
	print(datetime.now())
    
    

for i in range(1,31):
	print(datetime.now())

	path_import_dr = "select * from demand_response where hostname = 'flxbxD" + str(i) + "'"
	path_export_dr = '/Users/diego/Desktop/Data/flexbox_event_analysis/dr/' + 'flxbxD'+ str(i) + '.csv'

	df_dr = pd.read_sql(path_import_dr,con)
	df_dr.to_csv(path_export_dr)
	print(datetime.now())
