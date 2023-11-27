"""------------------------------------------------------------------------------------------------------------------
   -- This Python module wraps the calls and responses provided by the HTTP API exposed by CDK_PWI4 (SkyTracker). --
   ------------------------------------------------------------------------------------------------------------------
"""

import json
import requests

class CDK_MOUNT:
    """
    Mount information of the CDK700 telescope.
    """
    def __init__(self, mount):
        self.mount = mount
            
    def exists(self):
        return self.mount["exists"]
    
    def speed(self):
        return self.mount["speed"]


class CDK_M3:
    """
    M3 information of the CDK700 telescope.
    """
    def __init__(self, m3):
        self.m3 = m3
            
    def exists(self):
        return self.m3["exists"]
    
    def position(self):
        return self.m3["position"]


class CDK_INFO:
    """
    Information of the CDK700 telescope.
    """
    def __init__(self, data):
        self.info = json.loads(data)
        
    def mount(self):
        return CDK_MOUNT(self.info["mount"])

    def m3(self):
        return CDK_MOUNT(self.info["M3"])


class CDK_PWI4:
    """
    Client to the CDK700 telescope control application.
    """
    def __init__(self, host="localhost", port=9000):
        self.host = host
        self.port = port

    ### High-level methods ###


    def mount_command(self, command):
        """
        one of the following items can be specified as a command argument:
            move_left, move_right, move_up, move_down, end_command, go_back, next_speed, previous_speed, stop
        """
        return self.request_with_status("/mount/" + command)
        
    def info(self):
        return CDK_INFO(data=self.request_with_status("/information"))

    ### Low-level method for issuing requests ###


    def request_with_status(self, command):
        url = "http://" + self.host + ":" + str(self.port) + command
        '''debug: print('url: ', url)'''
        try:
            response = requests.get(url)
            if response.status_code == 200:
                '''debug: print('response: ', response.text)'''
                return response.text
            else:
                print('request failed')
                
        except:
            print('no connection')
            raise

