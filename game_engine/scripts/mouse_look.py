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
        self.walk_key = [0, 0, 0]
        #self.walk_lz_prev = 0

        self.jumping = False

    def main(self):
        self.detect_onground()
        self.look()
        self.move()

    def detect_onground(self):
        offset = 0.2
        dist = 0.2001
        pos_w = self.worldPosition.copy()
        ori = self.worldOrientation.col
        waxis_ly = ori[1]
        waxis_lz = ori[2]
        pos_w += waxis_lz * offset
        ground = self.rayCast(pos_w - waxis_lz, pos_w, dist)[2]
        
        if not ground:
            quat = Quaternion(waxis_lz, self.division_angle)
            v = waxis_ly * (self.body_width / 2)
            rayCast = self.rayCast
            for i in range(self.division_onground):
                vec_from = pos_w + v
                ground = rayCast(vec_from - waxis_lz, vec_from, dist)[2]
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
        upper_limit = self.upper_limit - 0.01
        lower_limit = self.lower_limit + 0.01
        if angle + y > upper_limit:
            y = upper_limit - angle
        elif angle + y < lower_limit:
            y = lower_limit - angle

        self.applyRotation((0, 0, x), False)
        head.applyRotation((y, 0, 0), True)

        logic.mouse.position = .5, .5

    #def detect_col(self, vec_to, division=3):
    #    if vec_to.length < 0.1:
    #        return vec_to
    #    offset = 0.05
    #    dist = self.body_width / 2 + 0.01
    #    vec_from = self.worldPosition.copy()
    #    waxis_lz = self.worldOrientation.col[2]
    #    vec_from += waxis_lz * offset
    #    vec_delta = waxis_lz * ((self.body_height - offset * 2) / division)
    #    rayCast = self.rayCast
    #    for i in range(division):
    #        vec_from += vec_delta
    #        normal = rayCast(vec_from + vec_to, vec_from, dist)[2]
    #        if normal:
    #            return vec_to - normal * vec_to.dot(normal)
    #    return vec_to

    def move(self):
        key = logic.keyboard

        #walk_key = self.walk_key
        walk_direction = self.walk_direction
        walk_direction[:] = (0, 0, 0)
        speed = self.speed

        if key.events[events.WKEY]:
            walk_direction[1] = 1.0
            #walk_key[1] = 1.0
        if key.events[events.SKEY]:
            walk_direction[1] = -1.0
            #walk_key[1] = -1.0
        #elif walk_key[1]:
        #    walk_direction[1] = -walk_key[1]
        #    walk_key[1] = 0

        if key.events[events.DKEY]:
            walk_direction[0] = 1.0
            #walk_key[0] = 1.0
        if key.events[events.AKEY]:
            walk_direction[0] = -1.0
            #walk_key[0] = -1.0
        #elif walk_key[0]:
        #    walk_direction[0] = -walk_key[0]
        #    walk_key[0] = 0

        walk_direction.normalize()

        if self.phys_id != 0:
            walk_direction *= speed
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
            walk_lz = walk_direction[2]
            velo_lz = self.getLinearVelocity(True)[2]
            ground = self.ground
            if ground:
                walk_direction -= self.worldOrientation.inverted() * ground * 5
            else:
                walk_direction[2] = velo_lz
            #walk_lz_prev = self.walk_lz_prev
            #if -0.05 < walk_lz < 0.05:
            #    walk_direction[2] = velo_lz - walk_lz_prev

            self.walk_lz_prev = walk_lz

            self.setLinearVelocity(walk_direction, True)
        else:
            if key.events[events.EKEY]:
                walk_direction[2] += speed
            if key.events[events.CKEY]:
                walk_direction[2] -= speed

            walk_direction = self.foot.worldOrientation * walk_direction
            self.applyMovement(walk_direction, False)

def register(cont):
    utilities.register(MouseLook, cont)
