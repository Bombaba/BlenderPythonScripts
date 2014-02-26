import state

class Shaft(state.StateFuncs):
    _initial_ = 'M_WAIT'
    _properties_ = {'angle': 0}
    def M_WAIT_main(ctrlr):
        sensor = ctrlr.sensors['ROT']
        if sensor.positive:
            state.set_property(ctrlr, 'angle', int(sensor.bodies[0], 10))
            state.set_sequence(ctrlr,
                               'ROTATING',
                               'M_WAIT',
                               )
            state.next_state(ctrlr)
    def ROTATING_init(ctrlr):
        ctrlr.activate(ctrlr.actuators['rotation'])
    def ROTATING_main(ctrlr):
        if state.stateframe(ctrlr) > state.get_property(ctrlr, 'angle'):
            ctrlr.deactivate(ctrlr.actuators['rotation'])
            ctrlr.activate(ctrlr.actuators['Shaft_ROTATED'])
            print(ctrlr.owner.localOrientation.to_euler('XYZ'))
            state.next_state(ctrlr)

loop = Shaft()
