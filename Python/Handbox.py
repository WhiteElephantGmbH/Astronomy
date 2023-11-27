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

    client = CDK_PWI4(host=ip_address)
    
    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]

    control = [[sg.RealtimeButton(sg.SYMBOL_UP, key='move_up')],
               [sg.RealtimeButton(sg.SYMBOL_LEFT, key='move_left'),
                sg.RealtimeButton(sg.SYMBOL_SQUARE, key='go_back', size=(2,1)),
                sg.RealtimeButton(sg.SYMBOL_RIGHT, key='move_right')],
               [sg.RealtimeButton(sg.SYMBOL_DOWN, key='move_down')]]

    mount = [[sg.RealtimeButton(sg.SYMBOL_LEFT, key='previous_speed'),
              sg.Text(size=(8,1), key='-SPEED-', pad=(0,0), font='Ani 14', justification='c', background_color=color1, text_color=color0),
              sg.RealtimeButton(sg.SYMBOL_RIGHT, key='next_speed')],
             [sg.Frame('', control, element_justification='c')]]

    layout = [[sg.Frame('CDK700', mount, font='Any 12', title_color=color1, element_justification='c')],
              [sg.RealtimeButton('', size=(14,1), key='-M3-', font='Any 14')]]

    window = sg.Window('CDK700 Control',
                       layout,
                       no_titlebar=True,
                       finalize=True,
                       element_justification='c',
                       location=(0,0),
                       size=(180,180))
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
                    go_back = event == "go_back"                
            else:
                # A timeout signals that all buttons have been released
                if pressed:
                    pressed = False
                    if not go_back:
                        response = client.mount_command (command = 'end_command')
                else:
                    count += 1
                    if count == 5:
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
                        else:
                            window['-M3-'].update("")
        except:
            break
    window.close()

if __name__ == '__main__':
    sg.theme('DarkAmber')
    main()
