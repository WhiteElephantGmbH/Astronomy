"""
************************************************************************************************************************
*                      HTTP API exposed by HPS (SkyTracker) implemented using the json protocoll                       *
************************************************************************************************************************
*                              (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
*                                                www.white-elephant.ch                                                 *
*                                                                                                                      *
*      This program is free software; you can redistribute it and/or modify it under the terms of the GNU General      *
*      Public License as published by the Free Software Foundation; either version 2 of the License, or                *
*      (at your option) any later version.                                                                             *
*                                                                                                                      *
*      This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the      *
*      implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License     *
*      for more details.                                                                                               *
*                                                                                                                      *
*      You should have received a copy of the GNU General Public License along with this program; if not, write to     *
*      the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                 *
************************************************************************************************************************
"""
import json
import requests

""" Control Information
    ===================
"""
class Control:
    def __init__(self, control):
        self.control = control

    def window_minimized(self):
        return self.control["window_minimized"]


""" Mount Information
    =================
"""
class Mount:
    def __init__(self, mount):
        self.mount = mount

    def exists(self):
        return self.mount["exists"]

    def speed(self):
        return self.mount["speed"]


""" Focuser Information
    ===================
"""
class Focuser:
    def __init__(self, focuser):
        self.focuser = focuser

    def exists(self):
        return self.focuser["exists"]

    def moving(self):
        return self.focuser["moving"]

    def position(self):
        return self.focuser["position"]


""" Information from SkyTracker
    ***************************
"""
class Information:
    def __init__(self, data):
        self.info = json.loads(data)

    def control(self):
        return Control(self.info["control"])

    def mount(self):
        return Mount(self.info["mount"])

    def focuser(self):
        return Focuser(self.info["focuser"])


"""
    **************
    * HPS Client *
    **************
"""
class Client:
    def __init__(self, host="localhost", port=9001):
        self.host = host
        self.port = port

    ### High-level methods ###

    """ Mount Command
        -------------
    """
    move_left      = 'move_left'
    move_right     = 'move_right'
    move_up        = 'move_up'
    move_down      = 'move_down'
    end_command    = 'end_command'
    go_back        = 'go_back'
    next_speed     = 'next_speed'
    previous_speed = 'previous_speed'

    def mount_command(self, command):
        return self.request_with_status("/mount/" + command)


    """ Focuser Commands
        ----------------
    """
    move_in  = 'move_in'
    move_out = 'move_out'

    def focuser_move_in(self, value):
        return self.request_with_status("/focuser/move_in/?speed=" + str(value))

    def focuser_move_out(self, value):
        return self.request_with_status("/focuser/move_out/?speed=" + str(value))

    def focuser_stop(self):
        return self.request_with_status("/focuser/stop")


    """ Information
        -----------
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
