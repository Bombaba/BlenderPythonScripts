from bge import logic
from bge.logic import KX_INPUT_NONE, KX_INPUT_JUST_ACTIVATED, KX_INPUT_ACTIVE, KX_INPUT_JUST_RELEASED
from bge import render
from bge import events
from bge import types
from bge import constraints
from mathutils import Vector
from time import perf_counter

from . import utilities
from .utilities import ANGLE_0, ANGLE_90, ANGLE_180, ANGLE_360, AXIS_Z
from math import cos, sin, acos

class MouseLook(types.KX_GameObject):
    sensitivity = 0.001
    _error = 0.01
    upper_limit = ANGLE_180
    lower_limit = ANGLE_0

    speed = 10.0
    body_height = 0.2
    body_width = 1.6

    jump_speed = 10.0
    jump_threshold = 3.0

    def __init__(self, old_owner):
        types.KX_GameObject.__init__(self)
        render.showMouse(True)
        logic.mouse.position = .5, .5

        self.phys_id = self.getPhysicsId()
        if self.phys_id:
            self.phys_chara = constraints.getCharacter(self)
        else:
            self.phys_chara = None

        self.head = self.foot = None
        for c in self.children:
            if "HEAD" in c:
                self.head = c
            elif "FOOT" in c:
                self.foot = c
                self.foot_sensor = c.sensors["FOOT"]

        self.walk_direction = Vector()

        self.jumping = False

    def main(self):
        self.look()
        self.move()

    def look(self):
        sense = self.sensitivity
        x = (0.5 - logic.mouse.position[0]) * render.getWindowWidth()
        y = (0.5 - logic.mouse.position[1]) * render.getWindowHeight()
        if abs(y) <= 1.0:
            y = 0.0
        x *= sense
        y *= sense

        head = self.head or self
        laxis_z = head.localOrientation.transposed()[2]
        laxis_z.normalize()
        #laxis_z = self.getAxisVect(AXIS_Z)
        angle = acos(laxis_z.dot(AXIS_Z))
        upper_limit = self.upper_limit - self._error
        lower_limit = self.lower_limit + self._error
        if angle + y > upper_limit:
            y = upper_limit - angle
        elif angle + y < lower_limit:
            y = lower_limit - angle

        self.applyRotation((0, 0, x), False)
        head.applyRotation((y, 0, 0), True)

        logic.mouse.position = .5, .5

    def move(self):
        key = logic.keyboard

        walk_direction = self.walk_direction
        walk_direction[:] = (0, 0, 0)
        speed = self.speed

        if key.events[events.WKEY]:
            walk_direction[1] += 1
        if key.events[events.SKEY]:
            walk_direction[1] -= 1
        if key.events[events.DKEY]:
            walk_direction[0] += 1
        if key.events[events.AKEY]:
            walk_direction[0] -= 1
        walk_direction.normalize()
        walk_direction *= speed

        phys_chara = self.phys_chara
        if phys_chara:
            walk_direction = self.localOrientation * walk_direction
            phys_chara.walkDirection = walk_direction

            if key.events[events.SPACEKEY]:
                phys_chara.jump()
        elif self.phys_id != 0:
            hit = self.foot_sensor.hitObject
            velo_z = self.getLinearVelocity(True)[2]
            walk_direction[2] = velo_z
            space = key.events[events.SPACEKEY]
            if velo_z < self.jump_threshold:
                self.jumping = False

            if self.jumping:
                if not space:
                    walk_direction[2] = velo_z / 2
                    self.jumping = False
            elif space and hit:
                walk_direction[2] = max(velo_z, self.jump_speed)
                self.jumping = True

            self.setLinearVelocity(walk_direction, True)
        else:
            if key.events[events.EKEY]:
                walk_direction[2] += speed
            if key.events[events.CKEY]:
                walk_direction[2] -= speed
            self.applyMovement(walk_direction, True)

def register(cont):
    utilities.register(MouseLook, cont)
