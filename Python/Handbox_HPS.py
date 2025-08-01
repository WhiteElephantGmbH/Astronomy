"""
************************************************************************************************************************
*                                            Handbox Simulator for 10micron                                            *
************************************************************************************************************************
*                           (c) 2024 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                         *
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
import hps_client  as cc
import PySimpleGUI as sg
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
    decrease_rate  = client.decrease_rate
    increase_rate  = client.increase_rate
    move_in        = client.move_in
    move_out       = client.move_out
    stop           = client.stop

    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]
    
    speed = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key=previous_speed),
              sg.Text(size=(8,1), key='-SPEED-', pad=(0,0), font='Ani 12',
                      justification='c', background_color=color0, text_color=color1),
              sg.RealtimeButton(sg.SYMBOL_RIGHT, key=next_speed)]]

    move = [[sg.RealtimeButton(sg.SYMBOL_UP, key=move_up)],
            [sg.RealtimeButton(sg.SYMBOL_LEFT, key=move_left),
             sg.RealtimeButton(sg.SYMBOL_CIRCLE, key=go_back),
             sg.RealtimeButton(sg.SYMBOL_RIGHT, key=move_right)],
            [sg.RealtimeButton(sg.SYMBOL_DOWN, key=move_down)]]

    handbox = [[sg.Frame('Speed', font='Ani 8', layout=speed, element_justification='c')],
               [sg.Frame('', layout=move, element_justification='c')]]

    rate = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key=decrease_rate),
             sg.Text(size=(1,1), key='-RATE-', pad=(0,0), font='Ani 12',
                     justification='c', background_color=color0, text_color=color1),
             sg.RealtimeButton(sg.SYMBOL_RIGHT, key=increase_rate)]]
    position = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key=move_in),
                 sg.Text(size=(5,1), key='-POSITION-', pad=(0,0), font='Ani 12',
                         justification='c', background_color=color0, text_color=color1),
                 sg.RealtimeButton(sg.SYMBOL_RIGHT, key=move_out)]]

    focuser = [[sg.Frame('Speed', font='Ani 8', layout=rate, element_justification='c'),
               sg.Frame('Position', font='Ani 8', layout=position, element_justification='c')]]

    layout = [[sg.Frame('', layout=handbox, element_justification='c')],
              [sg.Frame('Focuser', font='Ani 8', key='-FFO-', layout=focuser, element_justification='c',
                        visible=False)]]

    window = sg.Window('HPS Control',
                       layout=layout,
                       no_titlebar=True,
                       finalize=True,
                       element_justification='c',
                       location=(0,0),
                       size=(255,265))
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
                    mount_release_action = event in (move_up, move_down, move_left, move_right)
                    focuser_release_action = event in (move_in,  move_out)
                    if event in (decrease_rate, increase_rate, move_in, move_out):
                        response = client.focuser_command (command = event)
                    else:
                        response = client.mount_command (command = event)
            else:
                # A timeout signals that all buttons have been released
                if pressed:
                    pressed = False
                    if mount_release_action:
                        response = client.mount_command (command = end_command)
                    if focuser_release_action:
                        response = client.focuser_command (command = stop)
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
                focuser = info.focuser()
                if mount.exists():
                    window['-SPEED-'].update(mount.speed())
                    if focuser.exists():
                        window['-FFO-'].update(visible=True)
                        window['-POSITION-'].update(value=focuser.position())
                        window['-RATE-'].update(value=focuser.rate())
                    else:
                        window['-FFO-'].update(visible=False)
                else:
                    window['-SPEED-'].update("")
        except:
            break
    window.close()

if __name__ == '__main__':
    sg.theme('DarkAmber')
    main()
