"""----------------------------------
   -- Handbox Simulator for CDK700 --
   ----------------------------------
"""
import PySimpleGUI as sg
import sys

from cdk_pwi4_client import CDK_PWI4, CDK_INFO, CDK_MOUNT

def main():
    args = len(sys.argv) - 1
    if args == 1:
        ip_address = sys.argv[1]
    else:
        ip_address = "localhost"

    rotate_m3 = 'rotate_m3'
    
    client = CDK_PWI4(host=ip_address)
    
    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]

    control = [[sg.RealtimeButton(sg.SYMBOL_UP, key='move_up')],
               [sg.RealtimeButton(sg.SYMBOL_LEFT, key='move_left'),
                sg.RealtimeButton(sg.SYMBOL_SQUARE, key='go_back', size=(2,1)),
                sg.RealtimeButton(sg.SYMBOL_RIGHT, key='move_right')],
               [sg.RealtimeButton(sg.SYMBOL_DOWN, key='move_down')]]

    mount = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key='previous_speed'),
              sg.Text(size=(8,1), key='-SPEED-', pad=(0,0), font='Ani 14', justification='c', background_color=color0, text_color=color1),
              sg.RealtimeButton(sg.SYMBOL_RIGHT, key='next_speed')],
             [sg.Frame('', control, element_justification='c')]]

    m3 = [[sg.RealtimeButton(sg.SYMBOL_RIGHT, key=rotate_m3),
           sg.Text(size=(14,1), key='-M3-', pad=(0,0), font='Ani 14', background_color=color0, text_color=color1)]]

    layout = [[sg.Frame('', mount, element_justification='c')],
              [sg.Frame('', m3, element_justification='c')]]

    window = sg.Window('CDK700 Control',
                       layout,
                       no_titlebar=True,
                       finalize=True,
                       element_justification='c',
                       location=(0,0),
                       size=(245,200))
    count = 0
    pressed = False

    while True:
        try:
            event, values = window.read(timeout=100)
            if event != sg.TIMEOUT_EVENT:
                # if not a timeout event, then it's a button that's being held down
                if not pressed:
                    response = client.mount_command (command = event)
                    pressed = True
                    release_action = event in ('move_up',  'move_down', 'move_left', 'move_right')
            else:
                # A timeout signals that all buttons have been released
                if pressed:
                    pressed = False
                    if release_action:
                        response = client.mount_command (command = 'end_command')
                else:
                    count += 1
                    if count == 5: # every half second
                        count = 0;
                        info = client.info()
                        mount = info.mount()
                        if mount.exists():
                            window['-SPEED-'].update(mount.speed())
                        else:
                            window['-SPEED-'].update("")                        
                        m3 = info.m3()
                        if m3.exists():
                            window['-M3-'].update(m3.position())
                            if m3.at_camera():
                                window[rotate_m3].update(sg.SYMBOL_LEFT)
                            else:
                                window[rotate_m3].update(sg.SYMBOL_RIGHT)
                        else:
                            window['-M3-'].update("")
        except:
            break
    window.close()

if __name__ == '__main__':
    sg.theme('DarkAmber')
    main()
