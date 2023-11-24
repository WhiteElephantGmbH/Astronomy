"""------------------------------------------------------------------------------------------------------------------
   -- This Python module wraps the calls and responses provided by the HTTP API exposed by CDK_PWI4 (SkyTracker). --
   ------------------------------------------------------------------------------------------------------------------
"""

import requests

class CDK_PWI4:
    """
    Client to the CDK700 telescope control application.
    """
    def __init__(self, host="localhost", port=9000):
        self.host = host
        self.port = port

    ### High-level methods #################################

    def information(self):
        return self.request_with_status("/information")

    def mount_command(self, command):
        """
        one of the following commands can be specified as a keyword argument:
            move_left, move_right, move_up, move_down, end_command, go_back, stop
        """
        return self.request_with_status("/mount/" + command)
        
    ### Low-level method for issuing requests ##################

    def request_with_status(self, command):
        url = "http://" + self.host + ":" + str(self.port) + command
        print('url: ', url)
        try:
            response = requests.get(url)
            print('response: ', response)
            return response
        except:
            print('no connection to CDK_PWI4')
        return 'failed'

