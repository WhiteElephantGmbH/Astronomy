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
    focuser_max = 10000
    zoom_delta  = 100
    
    #events
    go_back        = client.go_back
    move_left      = client.move_left
    move_right     = client.move_right
    move_up        = client.move_up
    move_down      = client.move_down
    end_command    = client.end_command
    next_speed     = client.next_speed
    previous_speed = client.previous_speed
    rotate         = client.rotate

    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]

    value      = focuser_max/2
    last_value = 0

    lower_limit = focuser_min
    upper_limit = focuser_max
    
    control = [[sg.RealtimeButton(sg.SYMBOL_UP, key=move_up)],
               [sg.RealtimeButton(sg.SYMBOL_LEFT, key=move_left),
                sg.RealtimeButton(sg.SYMBOL_CIRCLE, key=go_back),
                sg.RealtimeButton(sg.SYMBOL_RIGHT, key=move_right)],
               [sg.RealtimeButton(sg.SYMBOL_DOWN, key=move_down)]]

    mount = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key=previous_speed),
              sg.Text(size=(8,1), key='-SPEED-', pad=(0,0), font='Ani 14', justification='c', background_color=color0, text_color=color1),
              sg.RealtimeButton(sg.SYMBOL_RIGHT, key=next_speed)],
             [sg.Frame('', control, element_justification='c')]]

    m3 = [[sg.RealtimeButton(sg.SYMBOL_RIGHT, key=rotate),
           sg.Text(size=(11,1), key='-M3-', pad=(0,0), font='Ani 14', justification='c', background_color=color0, text_color=color1)]]

    focuser = [[sg.Slider(key='focus', range=(lower_limit,upper_limit), default_value=value,
                          size=(13,18), orientation='horizontal', font='Ani 14',
                          enable_events=True),
                sg.RealtimeButton('🔎', key='zoom', font='Ani 12', size=(1,1))]]

    layout = [[sg.Frame('', mount, element_justification='c')],
              [sg.Frame('M3', font='Ani 8', key='-FM3-', layout=m3, element_justification='c', visible=False)],
              [sg.Frame('Focuser', font='Ani 8', key='-FFO-', layout=focuser, element_justification='c', visible=False)]]

    window = sg.Window('CDK700 Control',
                       layout,
                       no_titlebar=True,
                       finalize=True,
                       element_justification='c',
                       location=(0,0),
                       size=(245,290))
    count = 0
    pressed = False
    while True:
        try:
            event, values = window.read(timeout=100)
            if event == 'focus':
                value = values['focus']
                if (value == upper_limit) | (value == lower_limit):
                    lower_limit = focuser_min
                    upper_limit = focuser_max
                    window['focus'].update(range=(lower_limit,upper_limit))
            elif event != sg.TIMEOUT_EVENT:
                # if not a timeout event, then it's a button that's being held down
                if not pressed:
                    pressed = True
                    release_action = event in (move_up,  move_down, move_left, move_right)
                    if event == rotate:
                        response = client.m3_rotate()
                    elif event == 'zoom':
                        upper_limit = value + (zoom_delta / 2)
                        if upper_limit > focuser_max:
                            lower_limit = focuser_max - zoom_distance
                            upper_limit = focuser_max
                        else:
                            lower_limit = value - (zoom_delta / 2)
                            if lower_limit < focuser_min:
                                lower_limit = focuser_min
                                upper_limit = focuser_min + zoom_distance                           
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
                        if mount.exists():
                            window['-SPEED-'].update(mount.speed())
                            if m3.exists():
                                window['-FM3-'].update(visible=True)
                                window['-M3-'].update(m3.position())
                                if m3.at_camera():
                                    window[rotate].update(sg.SYMBOL_LEFT)
                                    window['-FFO-'].update(visible=True)
                                    if value != last_value:
                                        last_value = value;
                                        response = client.focuser_set_position (int(last_value))
                                else:
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
