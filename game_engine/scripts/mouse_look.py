from bge import logic
from bge.logic import KX_INPUT_NONE, KX_INPUT_JUST_ACTIVATED, KX_INPUT_ACTIVE, KX_INPUT_JUST_RELEASED
from bge import render
from bge import events
from bge import types
from bge import constraints
from mathutils import Matrix, Vector, Quaternion
from time import perf_counter

from . import utilities
from .utilities import ANGLE_0, ANGLE_90, ANGLE_180, ANGLE_360, AXIS_Z
from math import cos, sin, acos, pi

MAT_IDENTITY3 = Matrix.Identity(3)

class MouseLook(types.KX_GameObject):
    sensitivity = 0.001
    _error = 0.01
    upper_limit = ANGLE_180
    lower_limit = ANGLE_0

    speed = 10.0
    body_height = 1.8
    body_width = 1.6

    jump_speed = 10.0
    jump_threshold = 3.0

    division_onground = 8
    division_angle = 2 * pi / division_onground

    def __init__(self, old_owner):
        types.KX_GameObject.__init__(self)
        render.showMouse(True)
        logic.mouse.position = .5, .5

        self.phys_id = self.getPhysicsId()

        self.head = self.foot = None
        for c in self.children:
            if "HEAD" in c:
                self.head = c
            elif "FOOT" in c:
                self.foot = c

        self.walk_direction = Vector.Fill(3, .0)
        self.walk_delta = Vector.Fill(3, .0)
        self.walk_key = [0, 0, 0]

        self.jumping = False

    def main(self):
        self.detect_onground()
        self.look()
        self.move()

    def detect_onground(self):
        pos_w = self.worldPosition.copy()
        ori = self.worldOrientation.col
        waxis_ly = ori[1]
        waxis_lz = ori[2]
        pos_w += waxis_lz * 0.2
        ground = self.rayCast(pos_w - waxis_lz, pos_w, .5)[2]
        
        if not ground:
            angle = self.division_angle
            quat = Quaternion(waxis_lz, angle)
            v = waxis_ly * (self.body_width / 2)
            for i in range(self.division_onground):
                vec_from = pos_w + v
                ground = self.rayCast(vec_from - waxis_lz, vec_from, .5)[2]
                if ground:
                    break
                v.rotate(quat)

        self.ground = ground

    def rotate_foot(self):
        if self.ground:
            normal = self.worldOrientation.inverted() * self.ground
            quat = AXIS_Z.rotation_difference(normal)
            self.foot.localOrientation = quat
        else:
            self.foot.localOrientation = MAT_IDENTITY3

    def look(self):
        sense = self.sensitivity
        x = (0.5 - logic.mouse.position[0]) * render.getWindowWidth()
        y = (0.5 - logic.mouse.position[1]) * render.getWindowHeight()
        if abs(y) <= 1.0:
            y = 0.0
        x *= sense
        y *= sense

        head = self.head or self
        laxis_z = head.localOrientation.col[2]
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

        #walk_key = self.walk_key
        walk_direction = self.walk_direction
        walk_direction[:] = (0, 0, 0)
        walk_delta = self.walk_delta
        speed = self.speed

        if key.events[events.WKEY]:
            walk_direction[1] += 1.0
        elif key.events[events.SKEY]:
            walk_direction[1] -= 1.0

        if key.events[events.DKEY]:
            walk_direction[0] += 1.0
        elif key.events[events.AKEY]:
            walk_direction[0] -= 1.0

        walk_direction.normalize()
        walk_direction *= speed

        if self.phys_id != 0:
            self.rotate_foot()

            space = key.events[events.SPACEKEY]
            #if velo_z < self.jump_threshold:
            #    self.jumping = False

            #if self.jumping:
            #    if not space:
            #        walk_direction[2] = velo_z / 2
            #        self.jumping = False
            #elif space and hit:
            #    walk_direction[2] = max(velo_z, self.jump_speed)
            #    self.jumping = True

            #self.setLinearVelocity(walk_direction, True)

            #walk_direction[:] = self.walk.worldOrientation * walk_direction
            #velo = self.getLinearVelocity(False) + walk_direction
            #for i in range(3):
            #    v, d = velo[i], walk_direction[i]
            #    if v * d > 0.0 and abs(v) > abs(d):
            #        velo[i] = walk_direction[i]

            #if space and hit:
            #    walk_direction[2] = self.jump_speed
            #    self.jumping = True
            walk_direction[:] = self.foot.localOrientation * walk_direction
            velo = self.getLinearVelocity(True)
            for i in range(3):
                v, w = velo[i], walk_direction[i]
                if v >= 0.0 and w > 0.0:
                    walk_direction[i] = max(v, w)
                elif v <= 0.0 and w < 0.0:
                    walk_direction[i] = min(v, w)
                else:
                    walk_direction[i] += v

            self.setLinearVelocity(walk_direction, True)
        else:
            if key.events[events.EKEY]:
                walk_direction[2] += speed
            if key.events[events.CKEY]:
                walk_direction[2] -= speed

            walk_direction = self.walk.worldOrientation * walk_direction
            self.applyMovement(walk_direction, False)

def register(cont):
    utilities.register(MouseLook, cont)
