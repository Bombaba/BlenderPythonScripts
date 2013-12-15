import math
import mathutils

ANGLE_0 = .0
ANGLE_1 = math.pi / 180
ANGLE_90 = math.pi / 2
ANGLE_180 = math.pi
ANGLE_270 = math.pi * 3 / 2
ANGLE_360 = math.pi * 2

AXIS_X = mathutils.Vector((1, 0, 0))
AXIS_Y = mathutils.Vector((0, 1, 0))
AXIS_Z = mathutils.Vector((0, 0, 1))


def register(cls, cont):
    cls(cont.owner)
    cont.script = "scripts.utilities.main"

def main(cont):
    cont.owner.main()
