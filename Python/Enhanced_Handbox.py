"""
************************************************************************************************************************
*                                        Enhanced Handbox Simulator for CDK700                                         *
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
    zoom_delta  = 150

    #events
    go_back                = client.go_back
    move_left              = client.move_left
    move_right             = client.move_right
    move_up                = client.move_up
    move_down              = client.move_down
    end_command            = client.end_command
    spiral_offset_center   = client.spiral_offset_center
    spiral_offset_next     = client.spiral_offset_next
    spiral_offset_previous = client.spiral_offset_previous
    next_speed             = client.next_speed
    previous_speed         = client.previous_speed
    add_point              = client.add_point
    rotate                 = client.rotate
    goto_field             = client.goto_field
    goto_mech              = client.goto_mech
    goto_offset            = client.goto_offset

    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]

    value      = focuser_min
    last_value = value

    lower_limit = focuser_min
    upper_limit = focuser_max

    speed = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key=previous_speed),
              sg.Text(size=(8,1), key='-SPEED-', pad=(0,0), font='Ani 13',
                      justification='c', background_color=color0, text_color=color1),
              sg.RealtimeButton(sg.SYMBOL_RIGHT, key=next_speed)]]

    spiral = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key=spiral_offset_previous),
               sg.RealtimeButton(sg.SYMBOL_CIRCLE, key=spiral_offset_center),
               sg.RealtimeButton(sg.SYMBOL_RIGHT, key=spiral_offset_next)]]

    move = [[sg.RealtimeButton(sg.SYMBOL_UP, key=move_up)],
            [sg.RealtimeButton(sg.SYMBOL_LEFT, key=move_left),
             sg.RealtimeButton(sg.SYMBOL_CIRCLE, key=go_back),
             sg.RealtimeButton(sg.SYMBOL_RIGHT, key=move_right)],
            [sg.RealtimeButton(sg.SYMBOL_DOWN, key=move_down)]]

    handbox = [[sg.Frame('Speed', font='Ani 8', layout=speed, element_justification='c')],
               [sg.Frame('', layout=move, element_justification='c')],
               [sg.Frame('Spiral Offset', font='Ani 8', layout=spiral, element_justification='c')]]

    axis = [[sg.Text(size=(14,1), key='-AXIS0-', pad=(0,0), font='Ani 12',
                     justification='r', background_color=color0, text_color=color1)],
            [sg.Text(size=(14,1), key='-AXIS1-', pad=(0,0), font='Ani 12',
                     justification='r', background_color=color0, text_color=color1)]]

    model = [[sg.RealtimeButton('Add Point', key=add_point),
              sg.Text(size=(6,1), key='-POINTS-', pad=(0,0), font='Ani 13',
                      justification='c', background_color=color0, text_color=color1)]]

    mount = [[sg.Frame('', layout=handbox, element_justification='c')],
             [sg.Frame('Axis 0/1', font='Ani 8', layout=axis, element_justification='c')],
             [sg.Frame('Model', font='Ani 8', layout=model, element_justification='c')]]

    m3 = [[sg.RealtimeButton(sg.SYMBOL_RIGHT, key=rotate),
           sg.Text(size=(12,1), key='-M3-', pad=(0,0), font='Ani 14',
                   justification='c', background_color=color0, text_color=color1)]]

    focuser = [[sg.RealtimeButton('🔎', key='zoom', font='Ani 11', size=(2,1)),
                sg.Slider(key='focus', range=(lower_limit,upper_limit), default_value=value,
                          size=(18,18), orientation='horizontal', font='Ani 12',
                          enable_events=True)]]

    field_angle = [[sg.Text(size=(9,1), key='-FIA-', pad=(0,0), font='Ani 12',
                            justification='r', background_color=color0, text_color=color1)]]

    mech_position = [[sg.Text(size=(9,1), key='-MPO-', pad=(0,0), font='Ani 12',
                              justification='r', background_color=color0, text_color=color1)]]

    rotator = [[sg.Frame('Field Angle', font='Ani 8', layout=field_angle, element_justification='c'),
                sg.Frame('Mech Position', font='Ani 8', layout=mech_position, element_justification='c')],
               [sg.RealtimeButton('>>Field', key=goto_field),
                sg.RealtimeButton('>>Mech', key=goto_mech),
                sg.RealtimeButton('>>Offset', key=goto_offset)],
               [sg.Slider(key='angle', range=(-180,180), default_value=0,
                          size=(20,18), orientation='horizontal', font='Ani 12')]]

    layout = [[sg.Frame('Mount', font='Ani 8', layout=mount, element_justification='c')],
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
                       size=(253,663))
    count = 0
    pressed = False
    zoomed = False
    startup = True
    while True:
        try:
            event, values = window.read(timeout=100)
            if event == 'focus':
                value = values['focus']
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
                            upper_limit = value + (zoom_delta / 2)
                            if upper_limit > focuser_max:
                                lower_limit = focuser_max - zoom_delta
                                upper_limit = focuser_max
                            else:
                                lower_limit = value - (zoom_delta / 2)
                                if lower_limit < focuser_min:
                                    lower_limit = focuser_min
                                    upper_limit = focuser_min + zoom_delta
                            window['zoom'].update(text='⇔')
                        window['focus'].update(range=(lower_limit,upper_limit))
                    elif event == rotate:
                        response = client.m3_rotate()
                    elif event == goto_field:
                       response = client.rotator_goto_field_angle(values['angle'])
                    elif event == goto_mech:
                        response = client.rotator_goto_mech_position(values['angle'] + 180)
                    elif event == goto_offset:
                        response = client.rotator_goto_offset(values['angle'])
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
                        mount = info.mount()
                        m3 = info.m3()
                        focuser = info.focuser()
                        rotator = info.rotator()
                        if mount.exists():
                            window['-SPEED-'].update(mount.speed())
                            window['-AXIS0-'].update(mount.axis0())
                            window['-AXIS1-'].update(mount.axis1())
                            window['-POINTS-'].update(mount.points())
                            if m3.exists():
                                window['-FM3-'].update(visible=True)
                                window['-M3-'].update(m3.position())
                                if focuser.exists() and m3.at_camera():
                                    window[rotate].update(sg.SYMBOL_LEFT)
                                    window['-FFO-'].update(visible=True)
                                    window['-FRO-'].update(visible=True)
                                    if startup:
                                        focuser_max = focuser.max_position()
                                        upper_limit = focuser_max
                                        window['focus'].update(range=(lower_limit,upper_limit))
                                        startup = False
                                    if value != last_value:
                                        last_value = value;
                                        response = client.focuser_set_position (int(last_value))
                                    else:
                                        if not focuser.moving():
                                            window['focus'].update(value=focuser.position())
                                    window['-FIA-'].update(rotator.field_angle())
                                    window['-MPO-'].update(rotator.mech_position())
                                else:
                                    last_value = value
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
