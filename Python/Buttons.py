"""
    CDK700 - Realtime Buttons

    Note that your reaction latency will be the same as the timeout value.
"""
import PySimpleGUI as sg

def main():
    # The Quit button is being placed in the bottom right corner and the colors are inverted, just for fun
    layout = [[sg.Text('CDK700 Control')],
              [sg.Text('     '),
               sg.RealtimeButton(sg.SYMBOL_UP, key='Move_Up')],
              [sg.RealtimeButton(sg.SYMBOL_LEFT, key='Move_Left'),
               sg.RealtimeButton(sg.SYMBOL_SQUARE, key='Go_Back', size=(2,1)),
               sg.RealtimeButton(sg.SYMBOL_RIGHT, key='Move_Right')],
              [sg.Text('     '),
               sg.RealtimeButton(sg.SYMBOL_DOWN, key='Move_Down')],
              [sg.Column([[sg.Quit(button_color=(sg.theme_button_color()[1], sg.theme_button_color()[0]), focus=True)]], justification='r')]]

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
            if pressed == False:
                print('Event ', event)
                pressed = True
                back = event == "Go_Back"
        else:
            # A timeout signals that all buttons have been released
            if pressed == True:
                if back != True:
                    print('Event ', "End_Move")
                pressed = False

    window.close()

if __name__ == '__main__':
    sg.theme('dark red')
    main()