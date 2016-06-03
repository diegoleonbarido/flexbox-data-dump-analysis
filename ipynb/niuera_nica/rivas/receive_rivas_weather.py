from datetime import datetime
import weewx_pb2
from sqlalchemy import create_engine, text, cast, DateTime

def save_message(message):
    incoming_message = weewx_pb2.weather_message()
    incoming_message.ParseFromString(message.SerializeToString())
    output_dict = {}
    output_dict['datetime'] = datetime.fromtimestamp(incoming_message.datetime)
    output_dict['pressure'] = incoming_message.pressure
    output_dict['altimeter'] = incoming_message.altimeter
    output_dict['outTemp'] = incoming_message.outTemp
    output_dict['outHumidity'] = incoming_message.outHumidity
    output_dict['windSpeed'] = incoming_message.windSpeed
    output_dict['windDir'] = incoming_message.windDir
    output_dict['windGust'] = incoming_message.windGust
    output_dict['windGustDir'] = incoming_message.windGustDir
    output_dict['rainRate'] = incoming_message.rainRate
    output_dict['rain'] = incoming_message.rain
    output_dict['dewpoint'] = incoming_message.dewpoint
    output_dict['modem_MB_usage'] = incoming_message.modem_MB_usage
    for key in output_dict:
        if output_dict[key] != output_dict[key]:
            output_dict[key] = None
            
    engine = create_engine('postgresql://niuera_analyzer:analysis@localhost/niuera_other_db')
    metadata = psql.get_metadata()
    table_dict = psql.setup_tables(metadata)
    result = table_dict['rivas_weather_table'].select().\
        where(cast(table_dict['rivas_weather_table'].c.datetime,DateTime)==output_dict['datetime']).execute()
    if not result.fetchone():
        psql.add_values_to_table(table_dict['rivas_weather_table'],output_dict)

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

server_address = ('0.0.0.0', 10000)
print('Starting up on %s port %s' % server_address)
sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
sock.bind(server_address)

sock.listen(1)

while True:
    print('waiting for a connection')
    connection, client_address = sock.accept()

    try:
        print('connection from: {0}'.format((client_address,)))
        data = connection.recv(1000)
        print('data length: {0}'.format(len(data),))
        if data:
            save_message(data)
            print('Sending ACK back to the client')
            connection.sendall('ACK')
    except:
        print(traceback.print_exc())
    finally:
        connection.close()

