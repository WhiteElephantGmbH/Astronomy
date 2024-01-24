"""
************************************************************************************************************************
*                    HTTP API exposed by CDK_PWI4 (SkyTracker) implemented using the json protocoll                    *
************************************************************************************************************************
*                              (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

    def axis0(self):
        return self.mount["axis0"]

    def axis1(self):
        return self.mount["axis1"]

    def points(self):
        return self.mount["points"]


""" M3 Information
    ==============
"""
class M3:
    def __init__(self, m3):
        self.m3 = m3

    def exists(self):
        return self.m3["exists"]

    def at_camera(self):
        return self.m3["at_camera"]

    def position(self):
        return self.m3["position"]


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

    def max_position(self):
        return self.focuser["max_position"]

    def position(self):
        return self.focuser["position"]


""" Rotator Information
    ===================
"""
class Rotator:
    def __init__(self, rotator):
        self.rotator = rotator

    def moving(self):
        return self.rotator["moving"]

    def slewing(self):
        return self.rotator["slewing"]

    def field_angle(self):
        return self.rotator["field_angle"]

    def mech_position(self):
        return self.rotator["mech_position"]


""" Information from SkyTracker
    ***************************
"""
class Information:
    def __init__(self, data):
        self.info = json.loads(data)

    def mount(self):
        return Mount(self.info["mount"])

    def m3(self):
        return M3(self.info["m3"])

    def focuser(self):
        return Focuser(self.info["focuser"])

    def rotator(self):
        return Rotator(self.info["rotator"])


"""
    *******************
    * CDK_PWI4 Client *
    *******************
"""
class Client:
    def __init__(self, host="localhost", port=9000):
        self.host = host
        self.port = port

    ### High-level methods ###

    """ Mount Commands
        --------------
    """
    add_point              = 'add_point'
    move_left              = 'move_left'
    move_right             = 'move_right'
    move_up                = 'move_up'
    move_down              = 'move_down'
    end_command            = 'end_command'
    go_back                = 'go_back'
    spiral_offset_center   = 'spiral_offset_center'
    spiral_offset_next     = 'spiral_offset_next'
    spiral_offset_previous = 'spiral_offset_previous'
    next_speed             = 'next_speed'
    previous_speed         = 'previous_speed'
    stop                   = 'stop'
    goto_field             = 'goto_field'
    goto_mech              = 'goto_mech'
    goto_offset            = 'goto_offset'
    start                  = 'start'

    def mount_command(self, command):
        return self.request_with_status("/mount/" + command)

    """ M3 Command
        ----------
    """
    rotate = 'rotate'

    def m3_rotate(self):
        return self.request_with_status("/m3/rotate")

    """ Focuser Command
        ---------------
    """
    def focuser_set_position(self, value):
        return self.request_with_status("/focuser/set/?position=" + str(value))

    """ Rotator Command
        ---------------
    """
    def rotator_goto_field_angle(self, value):
        return self.request_with_status("/rotator/goto_field/?angle=" + str(value))

    def rotator_goto_mech_position(self, value):
        return self.request_with_status("/rotator/goto_mech/?position=" + str(value))

    def rotator_goto_offset(self, value):
        return self.request_with_status("/rotator/goto/?offset=" + str(value))

    def rotator_start(self):
        return self.request_with_status("/rotator/start")

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
