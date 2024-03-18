"""
************************************************************************************************************************
*                                            Handbox Simulator for 10micron                                            *
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
import cdk_pwi4_client as cc
import PySimpleGUI     as sg
import sys


def main():
    args = len(sys.argv) - 1
    if args == 1:
        ip_address = sys.argv[1]
    else:
        ip_address = "localhost"

    client = cc.Client(host=ip_address)

    #events
    go_back        = client.go_back
    move_left      = client.move_left
    move_right     = client.move_right
    move_up        = client.move_up
    move_down      = client.move_down
    end_command    = client.end_command
    next_speed     = client.next_speed
    previous_speed = client.previous_speed

    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]

    speed = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key=previous_speed),
              sg.Text(size=(8,1), key='-SPEED-', pad=(0,0), font='Ani 13',
                      justification='c', background_color=color0, text_color=color1),
              sg.RealtimeButton(sg.SYMBOL_RIGHT, key=next_speed)]]

    move = [[sg.RealtimeButton(sg.SYMBOL_UP, key=move_up)],
            [sg.RealtimeButton(sg.SYMBOL_LEFT, key=move_left),
             sg.RealtimeButton(sg.SYMBOL_CIRCLE, key=go_back),
             sg.RealtimeButton(sg.SYMBOL_RIGHT, key=move_right)],
            [sg.RealtimeButton(sg.SYMBOL_DOWN, key=move_down)]]

    handbox = [[sg.Frame('Speed', font='Ani 8', layout=speed, element_justification='c')],
               [sg.Frame('', layout=move, element_justification='c')]]

    layout = [[sg.Frame('', layout=handbox, element_justification='c')]]

    window = sg.Window('CDK700 Control',
                       layout=layout,
                       no_titlebar=True,
                       finalize=True,
                       element_justification='c',
                       location=(0,0),
                       size=(245,186))
    count = 0
    pressed = False
    minimized = False
    while True:
        try:
            event, values = window.read(timeout=100)
            if event != sg.TIMEOUT_EVENT:
                # if not a timeout event, then it's a button that's being held down
                if not pressed:
                    pressed = True
                    release_action = event in (move_up,  move_down, move_left, move_right)
                    response = client.mount_command (command = event)
            else:
                # A timeout signals that all buttons have been released
                if pressed:
                    pressed = False
                    if release_action:
                        response = client.mount_command (command = end_command)
                else:
                    count += 1
                    if count == 5: # every half second
                        count = 0;
                        info = client.info()
                        control = info.control()
                        if control.window_minimized():
                            if not minimized:
                                window.Hide()
                                minimized = True
                        else:
                            if minimized:
                                window.UnHide()
                                minimized = False
                        mount = info.mount()
                        if mount.exists():
                            window['-SPEED-'].update(mount.speed())
                        else:
                            window['-SPEED-'].update("")
        except:
            break
    window.close()

if __name__ == '__main__':
    sg.theme('DarkAmber')
    main()
