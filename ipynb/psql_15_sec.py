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
    table = Table('cndc_15_sec_table', metadata,
                    Column('id', Integer, primary_key=True,autoincrement=True),
                    Column('datetime', DateTime,default=func.now()),
                    Column('gen_all',Integer),
                    Column('inter_all',Integer),
                    Column('load_all',Integer),
                    Column('gen_EOLO',Integer),
                    Column('gen_PCG1',Integer),
                    Column('gen_PCG2',Integer),
                    Column('gen_PCG3',Integer),
                    Column('gen_PCG4',Integer),
                    Column('gen_PCG5',Integer),
                    Column('gen_PCG6',Integer),
                    Column('gen_PCG7',Integer),
                    Column('gen_PCG8',Integer),
                    Column('gen_PCG9',Integer),
                    Column('gen_PHC1',Integer),
                    Column('gen_PHC2',Integer),
                    Column('gen_PLB',Integer),
                    Column('gen_alba_rivas',Integer),
                    Column('gen_amayo',Integer),
                    Column('gen_blue_power',Integer),
                    Column('gen_c_fonesca',Integer),
                    Column('gen_canal',Integer),
                    Column('gen_censa_amfels',Integer),
                    Column('gen_centroamerica',Integer),
                    Column('gen_hidropantasma',Integer),
                    Column('gen_larreynega',Integer),
                    Column('gen_managua',Integer),
                    Column('gen_momotombo',Integer),
                    Column('gen_monte_rosa',Integer),
                    Column('gen_planta_corinto',Integer),
                    Column('gen_planta_nicaragua',Integer),
                    Column('gen_san_jacinto',Integer),
                    Column('gen_tipitapa',Integer),
                    Column('gen_unknown',Integer),
                    Column('inter_norte1',Integer),
                    Column('inter_norte2',Integer),
                    Column('inter_sur1',Integer),
                    Column('inter_sur2',Integer),
                    Column('load_BZN',Integer),
                    Column('load_CHG',Integer),
                    Column('load_acoyapa',Integer),
                    Column('load_altamira',Integer),
                    Column('load_amerrisque',Integer),
                    Column('load_asososca',Integer),
                    Column('load_asturias',Integer),
                    Column('load_batahola',Integer),
                    Column('load_bluefields',Integer),
                    Column('load_boaco',Integer),
                    Column('load_chinandega',Integer),
                    Column('load_corinto',Integer),
                    Column('load_corocito',Integer),
                    Column('load_diriamba',Integer),
                    Column('load_el_mojon',Integer),
                    Column('load_el_periodista',Integer),
                    Column('load_el_tuma',Integer),
                    Column('load_el_viejo',Integer),
                    Column('load_enacal',Integer),
                    Column('load_esteli',Integer),
                    Column('load_granada',Integer),
                    Column('load_la_esperanza',Integer),
                    Column('load_la_gateada',Integer),
                    Column('load_las_banderas',Integer),
                    Column('load_leon_1',Integer),
                    Column('load_leon_2',Integer),
                    Column('load_los_brasiles',Integer),
                    Column('load_malpaisillo',Integer),
                    Column('load_managua',Integer),
                    Column('load_masatepe',Integer),
                    Column('load_matagalpa',Integer),
                    Column('load_matiguas_mulukuku_siuna',Integer),
                    Column('load_nadaime',Integer),
                    Column('load_oriental',Integer),
                    Column('load_portezuelo',Integer),
                    Column('load_punta_huete',Integer),
                    Column('load_rivas',Integer),
                    Column('load_san_benito',Integer),
                    Column('load_san_miguelito',Integer),
                    Column('load_san_ramon',Integer),
                    Column('load_sandino',Integer),
                    Column('load_santa_clara',Integer),
                    Column('load_sebaco',Integer),
                    Column('load_ticuantepe_2',Integer),
                    Column('load_tipitapa',Integer),
                    Column('load_yalaguina',Integer),
                    )
 
    return table

def setup_schema():
    '''
    This method clears the database by dropping and readding the tables to the db.
    '''
    metadata = get_metadata()
    table = setup_tables(metadata)
    # create or drops tables in database
    metadata.drop_all()
    metadata.create_all()
    return table

def add_values_to_table(table_ref,value_dict):
    value = table_ref.insert()
    value.execute(value_dict)

def get_metadata(batchEngine=True):
    if batchEngine:
        engine = create_engine('postgresql://cndc_15_sec:cndc_15_sec@localhost/cndc_15_sec_db')
    else:
        engine = create_engine('postgresql://cndc_15_sec:cndc_15_sec@localhost/cndc_15_sec_db')

    metadata = MetaData(bind=engine)
    return metadata
