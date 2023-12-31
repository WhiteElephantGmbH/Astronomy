"""----------------------------------
   -- Handbox Simulator for CDK700 --
   ----------------------------------
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
    go_back        = client.go_back
    move_left      = client.move_left
    move_right     = client.move_right
    move_up        = client.move_up
    move_down      = client.move_down
    end_command    = client.end_command
    next_speed     = client.next_speed
    previous_speed = client.previous_speed
    add_point      = client.add_point
    rotate         = client.rotate

    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]

    value      = focuser_min
    last_value = value

    lower_limit = focuser_min
    upper_limit = focuser_max
    
    control = [[sg.RealtimeButton(sg.SYMBOL_UP, key=move_up)],
               [sg.RealtimeButton(sg.SYMBOL_LEFT, key=move_left),
                sg.RealtimeButton(sg.SYMBOL_CIRCLE, key=go_back),
                sg.RealtimeButton(sg.SYMBOL_RIGHT, key=move_right)],
               [sg.RealtimeButton(sg.SYMBOL_DOWN, key=move_down)]]

    position = [[sg.Text(size=(14,1), key='-AXIS0-', pad=(0,0), font='Ani 12', justification='r', background_color=color0, text_color=color1)],
                [sg.Text(size=(14,1), key='-AXIS1-', pad=(0,0), font='Ani 12', justification='r', background_color=color0, text_color=color1)]]

    model = [[sg.RealtimeButton('Add', key=add_point),
              sg.Text(size=(8,1), key='-POINTS-', pad=(0,0), font='Ani 14', justification='c', background_color=color0, text_color=color1)]]

    mount = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key=previous_speed),
              sg.Text(size=(8,1), key='-SPEED-', pad=(0,0), font='Ani 14', justification='c', background_color=color0, text_color=color1),
              sg.RealtimeButton(sg.SYMBOL_RIGHT, key=next_speed)],
             [sg.Frame('', control, element_justification='c')],
             [sg.Frame('Axis 0/1', position, element_justification='c')],
             [sg.Frame('Model', model, element_justification='c')]]

    m3 = [[sg.RealtimeButton(sg.SYMBOL_RIGHT, key=rotate),
           sg.Text(size=(11,1), key='-M3-', pad=(0,0), font='Ani 14', justification='c', background_color=color0, text_color=color1)]]

    focuser = [[sg.RealtimeButton('🔎', key='zoom', font='Ani 11', size=(2,1)),
                sg.Slider(key='focus', range=(lower_limit,upper_limit), default_value=value,
                          size=(18,18), orientation='horizontal', font='Ani 12',
                          enable_events=True)]]

    layout = [[sg.Frame('', mount, element_justification='c')],
              [sg.Frame('M3', font='Ani 8', key='-FM3-', layout=m3, element_justification='c', visible=False)],
              [sg.Frame('Focuser', font='Ani 8', key='-FFO-', layout=focuser, element_justification='c', visible=False)]]

    window = sg.Window('CDK700 Control',
                       layout,
                       no_titlebar=True,
                       finalize=True,
                       element_justification='c',
                       location=(0,0),
                       size=(245,355))
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
                    if event == rotate:
                        response = client.m3_rotate()
                    elif event == 'zoom':
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
                        if mount.exists():
                            window['-SPEED-'].update(mount.speed())
                            window['-AXIS0-'].update(mount.axis0())
                            window['-AXIS1-'].update(mount.axis1())
                            window['-POINTS-'].update(mount.points())
                            if m3.exists():
                                window['-FM3-'].update(visible=True)
                                window['-M3-'].update(m3.position())
                                if focuser.connected() and m3.at_camera():
                                    window[rotate].update(sg.SYMBOL_LEFT)
                                    window['-FFO-'].update(visible=True)
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
                                else:
                                    last_value = value
                                    window[rotate].update(sg.SYMBOL_RIGHT)
                                    window['-FFO-'].update(visible=False)
                            else:
                                window['-FM3-'].update(visible=False)
                        else:
                            window['-SPEED-'].update("")                        
                            window['-FM3-'].update(visible=False)
                            window['-FFO-'].update(visible=False)
        except:
            break
    window.close()

if __name__ == '__main__':
    sg.theme('DarkAmber')
    main()
