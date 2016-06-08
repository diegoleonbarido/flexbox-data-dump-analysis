from sqlalchemy import create_engine, text, cast, DateTime
import weewx_pb2

def get_modem_usage():
    try:
        r = requests.get('http://192.168.8.1/api/monitoring/traffic-statistics')
        tree = ElementTree.fromstring(r.content)
        data_used_bytes = (int(tree.find('TotalDownload').text)+int(tree.find('TotalUpload').text))
        data_used_mb = data_used_bytes/1048576.0
        return data_used_mb
    except:
        return float("nan")
 
def create_message():   
    engine = create_engine('mysql://weewx:weewx@localhost/weewx')
    result = engine.execute(text("select * from archive order by dateTime desc"))
    output_dict = {}
    result_tuple = result.fetchone()
    for i,column_name in enumerate(result.keys()):
        output_dict[column_name] = result_tuple[i]
        
    if not output_dict['windDir']:
        output_dict['windDir'] = float("nan")
    if not output_dict['windGustDir']:
        output_dict['windGustDir'] = float("nan")
    message = weewx_pb2.weather_message()  
    message.datetime = output_dict['dateTime']
    message.pressure = output_dict['pressure']
    message.altimeter = output_dict['altimeter']
    message.outTemp = output_dict['outTemp']
    message.outHumidity = output_dict['outHumidity']
    message.windSpeed = output_dict['windSpeed']
    message.windDir = output_dict['windDir']
    message.windGust = output_dict['windGust']
    message.windGustDir = output_dict['windGustDir']
    message.rainRate = output_dict['rainRate']
    message.rain = output_dict['rain']
    message.dewpoint = output_dict['dewpoint']
    message.modem_MB_usage = get_modem_usage()
    return message


def send_packet(message):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # Connect the socket to the port where the server is listening
    server_address = ('cooljoule.javirosa.com', 10000)
    #print('connecting to {0} port {0}'.format((server_address,)))

    try:
        sock.connect(server_address)
        try:
            # Send data
            sys.stdout.write('o')
            sys.stdout.flush()
            sock.sendall(message)

            # Look for the response
            amount_received = 0
            amount_expected = 3
            '''
            while amount_received < amount_expected:
                data = sock.recv(3)
                amount_received += len(data)
                #print >>sys.stderr, 'received "%s"' % data
            print
            '''
        finally:
            sys.stdout.write('.')
            sys.stdout.flush()
            sock.close()
    except (socket.error, socket.herror, socket.gaierror, socket.timeout) as e:
        print(e)

def main():
    print('Starting hearbeat.')
    message = create_message().SerializeToString()
    #print 'MFI: ' + str(mfi_val)
    overhead = time.time() - start
    #print(overhead)
    print message
    send_packet(message)
if __name__ == '__main__':
    main()
