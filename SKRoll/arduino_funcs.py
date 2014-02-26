__all__ = [
    "digital_read", "digital_write",
]

def digital_read(ctrlr, sensor_name):
    """
    setup::
        pinMode({pin}, INPUT)
    main::
        digitalRead({pin})
    """
    return ctrlr.sensors[sensor_name].positive

def digital_write(ctrlr, actuator_name, value):
    if value is True:
        ctrlr.activate(ctrlr.actuators[actuator_name])
    else:
        ctrlr.deactivate(ctrlr.actuators[actuator_name])
