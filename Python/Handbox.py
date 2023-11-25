"""----------------------------------
   -- Handbox Simulator for CDK700 --
   ----------------------------------
"""
import PySimpleGUI as sg
import requests

from cdk_pwi4_client import CDK_PWI4

def main():
    client = CDK_PWI4()
    
    color0 = sg.theme_button_color()[0]
    color1 = sg.theme_button_color()[1]

    layout = [[sg.Text('CDK700 Control')],
              [sg.RealtimeButton(sg.SYMBOL_LEFT, key='previous_speed'),
               sg.Text(size=(7,1), key='-SPEED-', pad=(0,0), background_color=color1, text_color=color0),
               sg.RealtimeButton(sg.SYMBOL_RIGHT, key='next_speed')],
              [sg.Text('     '),
               sg.RealtimeButton(sg.SYMBOL_UP, key='move_up')],
              [sg.RealtimeButton(sg.SYMBOL_LEFT, key='move_left'),
               sg.RealtimeButton(sg.SYMBOL_SQUARE, key='go_back', size=(2,1)),
               sg.RealtimeButton(sg.SYMBOL_RIGHT, key='move_right')],
              [sg.Text('     '),
               sg.RealtimeButton(sg.SYMBOL_DOWN, key='move_down')]]

    window = sg.Window('CDK700 Control',
                       layout,
                       no_titlebar=True,
                       finalize=True,
                       location=(0,0))
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
                        response = client.information()
                        window['-SPEED-'].update(response.text)
                        count = 0;
        except:
            break
    window.close()

if __name__ == '__main__':
    sg.theme('DarkAmber')
    main()