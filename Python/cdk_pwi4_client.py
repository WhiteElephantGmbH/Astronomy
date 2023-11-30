"""------------------------------------------------------------------------------------------------------------------
   -- This Python module wraps the calls and responses provided by the HTTP API exposed by CDK_PWI4 (SkyTracker). --
   ------------------------------------------------------------------------------------------------------------------
"""
import json
import requests

class Mount:
    """
    Mount information of the CDK700 telescope.
    """
    def __init__(self, mount):
        self.mount = mount
            
    def exists(self):
        return self.mount["exists"]
    
    def speed(self):
        return self.mount["speed"]


class M3:
    """
    M3 information of the CDK700 telescope.
    """
    def __init__(self, m3):
        self.m3 = m3
            
    def exists(self):
        return self.m3["exists"]
    
    def at_camera(self):
        return self.m3["at_camera"]
    
    def position(self):
        return self.m3["position"]


class Information:
    """
    Information of the CDK700 telescope.
    """
    def __init__(self, data):
        self.info = json.loads(data)
        
    def mount(self):
        return Mount(self.info["mount"])

    def m3(self):
        return M3(self.info["m3"])


class Client:
    """
    Client to the CDK700 telescope control application.
    """
    def __init__(self, host="localhost", port=9000):
        self.host = host
        self.port = port

    ### High-level methods ###

    """ Mount Commands
        --------------
    """
    move_left      = 'move_left'
    move_right     = 'move_right'
    move_up        = 'move_up'
    move_down      = 'move_down'
    end_command    = 'end_command'
    go_back        = 'go_back'
    next_speed     = 'next_speed'
    previous_speed = 'previous_speed'
    stop           = 'stop'

    def mount_command(self, command):
        return self.request_with_status("/mount/" + command)

    """ M3 Command
        ----------
    """
    rotate = 'rotate'

    def m3_command(self, command):
        return self.request_with_status("/m3/" + command)

    """ Info
        ----
    """
    def info(self):
        return Information(data=self.request_with_status("/information"))

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

