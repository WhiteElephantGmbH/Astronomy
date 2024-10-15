"""
************************************************************************************************************************
*                                             Handbox Simulator for CDK700                                             *
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

    focuser_min = 1
    focuser_max = 10000 # default
    zoom_size   = 100   # default

    #events
    go_back                = client.go_back
    move_left              = client.move_left
    move_right             = client.move_right
    move_up                = client.move_up
    move_down              = client.move_down
    end_command            = client.end_command
    next_speed             = client.next_speed
    previous_speed         = client.previous_speed
    rotate                 = client.rotate
    goto_mech              = client.goto_mech
    goto_offset            = client.goto_offset

    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]
    red    = 'IndianRed1'

    focuser_position      = focuser_min
    last_focuser_position = focuser_position
    rotator_position      = 0
    last_rotator_position = rotator_position

    lower_limit = focuser_min
    upper_limit = focuser_max

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

    m3 = [[sg.RealtimeButton(sg.SYMBOL_RIGHT, key=rotate),
           sg.Text(size=(11,1), key='-M3-', pad=(0,0), font='Ani 14',
                   justification='c', background_color=color0, text_color=color1)]]

    focuser = [[sg.RealtimeButton('🔎', key='zoom', font='Ani 11', size=(2,1)),
                sg.Slider(key='focus', range=(lower_limit,upper_limit), default_value=focuser_position,
                          size=(18,18), orientation='horizontal', font='Ani 12',
                          enable_events=True)]]

    rotator = [[sg.Slider(key='angle', range=(-60,60), default_value=0,
                          size=(20,18), orientation='horizontal', font='Ani 12',
                          enable_events=True)]]

    layout = [[sg.Frame('', layout=handbox, element_justification='c')],
              [sg.Frame('M3', font='Ani 8', key='-FM3-', layout=m3, element_justification='c',
                        visible=False)],
              [sg.Frame('Focuser', font='Ani 8', key='-FFO-', layout=focuser, element_justification='c',
                        visible=False)],
              [sg.Frame('Rotator', font='Ani 8', key='-FRO-', layout=rotator, element_justification='c',
                        visible=False)]]

    window = sg.Window('CDK700 Control',
                       layout,
                       no_titlebar=True,
                       finalize=True,
                       element_justification='c',
                       location=(0,0),
                       size=(253,380))
    count = 0
    pressed = False
    zoomed = False
    minimized = False
    startup = True
    while True:
        try:
            event, values = window.read(timeout=100)
            if event == 'focus':
                focuser_position = values['focus']
            elif event == 'angle':
                rotator_position = values['angle']
            elif event != sg.TIMEOUT_EVENT:
                # if not a timeout event, then it's a button that's being held down
                if not pressed:
                    pressed = True
                    release_action = event in (move_up,  move_down, move_left, move_right)
                    if event == 'zoom':
                        if zoomed:
                            zoomed = False
                            lower_limit = focuser_min
                            upper_limit = focuser_max
                            window['zoom'].update(text='🔎')
                        else:
                            zoomed = True
                            upper_limit = focuser_position + (zoom_size / 2)
                            if upper_limit > focuser_max:
                                lower_limit = focuser_max - zoom_size
                                upper_limit = focuser_max
                            else:
                                lower_limit = focuser_position - (zoom_size / 2)
                                if lower_limit < focuser_min:
                                    lower_limit = focuser_min
                                    upper_limit = focuser_min + zoom_size
                            window['zoom'].update(text='⇔')
                        window['focus'].update(range=(lower_limit,upper_limit))
                    elif event == rotate:
                        response = client.m3_rotate()
                    else:
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
                        m3 = info.m3()
                        focuser = info.focuser()
                        rotator = info.rotator()
                        if mount.exists():
                            window['-SPEED-'].update(mount.speed())
                            if m3.exists():
                                window['-FM3-'].update(visible=True)
                                window['-M3-'].update(m3.position())
                                if focuser.exists() and m3.at_camera():
                                    window[rotate].update(sg.SYMBOL_LEFT)
                                    window['-FFO-'].update(visible=True)
                                    window['-FRO-'].update(visible=True)
                                    if startup:
                                        focuser_max = focuser.max_position()
                                        zoom_size = focuser.zoom_size()
                                        upper_limit = focuser_max
                                        window['focus'].update(range=(lower_limit,upper_limit))
                                        focuser_position = focuser.position()
                                        last_focuser_position = focuser_position
                                        window['focus'].update(value=focuser_position)
                                        rotator_position = 0
                                        last_rotator_position = rotator_position
                                        startup = False
                                    else:
                                        if focuser_position != last_focuser_position:
                                            last_focuser_position = focuser_position;
                                            response = client.focuser_set_position (int(focuser_position))
                                            window['zoom'].update(button_color=(color0, color1))
                                        else:
                                            if focuser.moving():
                                                window['zoom'].update(button_color=(red, color1))
                                            else:
                                                window['focus'].update(value=focuser.position())
                                                window['zoom'].update(button_color=(color0, color1))
                                        if rotator_position != last_rotator_position:
                                            last_rotator_position = rotator_position;
                                            response = client.rotator_goto_mech_position(180 + rotator_position)
                                        elif not rotator.moving():
                                            response = client.rotator_goto_offset(0)
                                else:
                                    response = client.rotator_goto_mech_position(180)
                                    window['angle'].update(value=0)
                                    window[rotate].update(sg.SYMBOL_RIGHT)
                                    window['-FFO-'].update(visible=False)
                                    window['-FRO-'].update(visible=False)
                            else:
                                window['-FM3-'].update(visible=False)
                        else:
                            window['-SPEED-'].update("")
                            window['-FM3-'].update(visible=False)
                            window['-FFO-'].update(visible=False)
                            window['-FRO-'].update(visible=False)
        except:
            break
    window.close()

if __name__ == '__main__':
    sg.theme('DarkAmber')
    main()
