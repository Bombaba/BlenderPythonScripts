from bge import logic
from bge import render
from bge import events
from bge import types
from bge import constraints
import mathutils

from . import utilities
from .utilities import ANGLE_0, ANGLE_180, AXIS_Z
import math

class MouseLook(types.KX_Camera):
    speed = 0.2
    sensitivity = 0.001
    upper_limit = ANGLE_180
    lower_limit = ANGLE_0
    limit_threshold = 0.01
    def __init__(self, old_owner):
        types.KX_Camera.__init__(self)
        #render.showMouse(True)
        logic.mouse.position = .5, .5
        self.phys_parent = constraints.getCharacter(self.parent)
        self.walk_direction = mathutils.Vector()

    def main(self):
        self.look()
        self.move()

    def look(self):
        sense = self.sensitivity
        x = round(0.5 - logic.mouse.position[0], 2) * sense \
                * render.getWindowWidth()
        y = round(0.5 - logic.mouse.position[1], 2) * sense \
                * render.getWindowHeight()

        laxis_z = self.localOrientation.transposed()[2]
        laxis_z.normalize()
        angle = math.acos(laxis_z.dot(AXIS_Z))
        upper_limit = self.upper_limit - self.limit_threshold
        lower_limit = self.lower_limit + self.limit_threshold
        if angle + y > upper_limit:
            y = upper_limit - angle
        elif angle + y < lower_limit:
            y = lower_limit - angle

        self.parent.applyRotation((0, 0, x), False)
        self.applyRotation((y, 0, 0), True)

        logic.mouse.position = .5, .5

    def move(self):
        key = logic.keyboard

        walk_direction = self.walk_direction
        walk_direction[:] = (0, 0, 0)
        speed = self.speed
        phys_parent = self.phys_parent

        if key.events[events.WKEY]:
            walk_direction[1] += 1
        if key.events[events.SKEY]:
            walk_direction[1] -= 1
        if key.events[events.DKEY]:
            walk_direction[0] += 1
        if key.events[events.AKEY]:
            walk_direction[0] -= 1
        walk_direction.normalize()
        walk_direction = speed * self.parent.localOrientation * walk_direction
        phys_parent.walkDirection = walk_direction

        if key.events[events.SPACEKEY]:
            phys_parent.jump()

        if key.events[events.EKEY]:
            self.applyMovement((0, 0, speed), False)
        if key.events[events.CKEY]:
            self.applyMovement((0, 0, -speed), False)

def register(cont):
    utilities.register(MouseLook, cont)
