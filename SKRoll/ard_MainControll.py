import state
from arduino_funcs import *

class SawBase(state.StateFuncs):
    _initial_ = 'M_WAIT'
    _properties_ = {'num_sk': 0}
    _read_pins_ = {'p_start': ...}
    def M_WAIT_main(ctrlr):
        if digital_read(ctrlr, 'START'):
            state.set_property(ctrlr, 'num_sk', 0)
            state.goto_master(ctrlr, 'M_START')

    def M_START_init(ctrlr):
        state.increment_property(ctrlr, 'num_sk')
    def M_START_main(ctrlr):
        if state.get_property(ctrlr, 'num_sk') <= 3:
            state.set_sequence(ctrlr,
                               'PUSH_SAW',
                               'FORWARD',
                               'PULL_SAW',
                               'REACHED',
                               'BACKWARD',
                               'ROT_SHAFT',
                               'M_START',
                               )
            state.next_state(ctrlr)
        else:
            state.goto_master(ctrlr, 'M_WAIT')

    def PUSH_SAW_init(ctrlr):
        digital_write(ctrlr, 'SawArm', True)
    def PUSH_SAW_main(ctrlr):
        if state.statetime(ctrlr) > 1:
            digital_write(ctrlr, 'Saw', True)
            state.next_state(ctrlr)

    def FORWARD_init(ctrlr):
        digital_write(ctrlr, 'SawBase_FOR', True)
    def FORWARD_main(ctrlr):
        if digital_read(ctrlr, 'SawBaseEndTouch'):
            digital_write(ctrlr, 'SawBase_FOR', False)
            state.next_state(ctrlr)

    def PULL_SAW_init(ctrlr):
        digital_write(ctrlr, 'SawArm', False)
    def PULL_SAW_main(ctrlr):
        if state.statetime(ctrlr) > 1:
            digital_write(ctrlr, 'Saw', False)
            state.next_state(ctrlr)

    def REACHED_main(ctrlr):
        if state.statetime(ctrlr) > 1:
            state.next_state(ctrlr)

    def BACKWARD_init(ctrlr):
        digital_write(ctrlr, 'SawBase_BACK', True)
    def BACKWARD_main(ctrlr):
        if digital_read(ctrlr, 'SawBaseStartTouch'):
            digital_write(ctrlr, 'SawBase_BACK', False)
            state.next_state(ctrlr)

    def ROT_SHAFT_init(ctrlr):
        act = ctrlr.actuators['Shaft_ROT']
        act.body = '120'
        digital_write(ctrlr, 'Shaft_ROT', True)
    def ROT_SHAFT_main(ctrlr):
        if digital_read(ctrlr, 'Shaft_ROTATED'):
            state.next_state(ctrlr)

loop = SawBase()
