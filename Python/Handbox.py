"""----------------------------------
   -- Handbox Simulator for CDK700 --
   ----------------------------------
"""
import PySimpleGUI as sg
import requests

from cdk_pwi4_client import CDK_PWI4

def main():
    client = CDK_PWI4()

    # The Quit button is being placed in the bottom right corner and the colors are inverted, just for fun
    layout = [[sg.Text('CDK700 Control')],
              [sg.Text('     '),
               sg.RealtimeButton(sg.SYMBOL_UP, key='move_up')],
              [sg.RealtimeButton(sg.SYMBOL_LEFT, key='move_left'),
               sg.RealtimeButton(sg.SYMBOL_SQUARE, key='go_back', size=(2,1)),
               sg.RealtimeButton(sg.SYMBOL_RIGHT, key='move_right')],
              [sg.Text('     '),
               sg.RealtimeButton(sg.SYMBOL_DOWN, key='move_down')],
              [sg.Column([[sg.Quit(button_color=(sg.theme_button_color()[1], sg.theme_button_color()[0]), focus=True)]],
               justification='r')]]

    window = sg.Window('CDK700 Control',
                       layout,
                       no_titlebar=True,
                       grab_anywhere=True,
                       location=sg.user_settings_get_entry('-location-', (None, None)))
    pressed : bool = False
    while True:
        event, values = window.read(timeout=100)
        if event in (sg.WIN_CLOSED, 'Quit'):
            sg.user_settings_set_entry('-location-', window.current_location())
            break

        if event != sg.TIMEOUT_EVENT:
            # if not a timeout event, then it's a button that's being held down
            if not pressed:
                print(client.mount_command (command = event))
                pressed = True
                go_back = event == "go_back"                
        else:
            # A timeout signals that all buttons have been released
            if pressed:
                pressed = False
                if not go_back: #!= True:
                    print(client.mount_command (command = 'end_command'))

    window.close()

if __name__ == '__main__':
    sg.theme('DarkAmber')
    main()