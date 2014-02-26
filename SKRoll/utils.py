def sensors_any(ctrlr):
    for sensor in ctrlr.sensors:
        if sensor.positive:
            return True
    else:
        return False
def sensors_all(ctrlr):
    for sensor in ctrlr.sensors:
        if not sensor.positive:
            return False
    else:
        return True

def or_activate(ctrlr):
    if sensors_any(ctrlr):
        for act in ctrlr.actuators:
            ctrlr.activate(act)

def and_activate(ctrlr):
    if sensors_all(ctrlr):
        for act in ctrlr.actuators:
            ctrlr.activate(act)

def or_deactivate(ctrlr):
    if sensors_any(ctrlr):
        for act in ctrlr.actuators:
            ctrlr.deactivate(act)

def and_deactivate(ctrlr):
    if sensors_all(ctrlr):
        for act in ctrlr.actuators:
            ctrlr.deactivate(act)
