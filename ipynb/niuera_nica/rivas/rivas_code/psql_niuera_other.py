from sqlalchemy import create_engine
from sqlalchemy import MetaData, Column, Table
from sqlalchemy import Integer, String, DateTime, Boolean, Float, func
from sqlalchemy.orm import Session, sessionmaker
from sqlalchemy.ext.declarative import declarative_base
import datetime



def setup_tables(metadata):
    '''
    This is where the tables for the PSQL database are defined.
    If columns/tables need to be changed, this is where it must happen.
    '''
    table_dict = {}
    rivas_weather_table = Table('rivas_weather', metadata,
                    Column('datetime', DateTime,primary_key=True,default=datetime.datetime.utcnow),
                    Column('pressure',Float),
                    Column('altimeter',Float),
                    Column('outTemp',Float),
                    Column('outHumidity',Float),
                    Column('windSpeed',Float),
                    Column('windDir',Float),
                    Column('windGust',Float),
                    Column('windGustDir',Float),
                    Column('rainRate',Float),
                    Column('rain',Float),
                    Column('dewpoint',Float),
                    Column('modem_MB_usage',Float),
                    )

    table_dict['rivas_weather_table'] = rivas_weather_table
    return table_dict

def setup_schema():
    '''
    This method clears the database by dropping and readding the tables to the db.
    '''
    metadata = get_metadata()
    table_dict = setup_tables(metadata)
    # create or drops tables in database
    metadata.drop_all()
    metadata.create_all()
    return table_dict

def add_values_to_table(table_ref,value_dict):
    value = table_ref.insert()
    value.execute(value_dict)

def get_last_row():
    engine = create_engine('postgresql://niuera_analyzer:analysis@localhost/niuera_other_db')
    metadata = MetaData(bind=engine)
    table_dict = setup_tables(metadata)
    result =  table_dict['rivas_weather_table'].select().\
        order_by(table_dict['rivas_weather_table'].c.datetime.desc()).execute()
    output_dict = {}
    result_tuple = result.fetchone()
    for i,column_name in enumerate(result.keys()):
        output_dict[column_name] = result_tuple[i]
    return output_dict
    

def get_metadata(batchEngine=True):
    engine = create_engine('postgresql://niuera_analyzer:analysis@localhost/niuera_other_db')
    metadata = MetaData(bind=engine)
    return metadata

def get_session():
    engine = create_engine('postgresql://niuera_analyzer:analysis@localhost/niuera_other_db')
    Session = sessionmaker(bind=engine)
    session = Session()
    return session