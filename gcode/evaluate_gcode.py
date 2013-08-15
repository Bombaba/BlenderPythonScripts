import bpy

def parse_gcode(gcode):
    c = bpy.data.curves.new("gcode_path", 'CURVE')
    c.dimensions = '3D'
    add_line(c, 2, (0, 0, 0), (1, 0, 0))
    #for l in gcode.lines:
    #    print(l.body)
    #    print("---")

    ob = bpy.data.objects.new("gcode_path", c)
    bpy.context.scene.objects.link(ob)
    bpy.context.scene.update()

def add_line(curve, num_points, *pcoords):
    sp = curve.splines.new('POLY')
    print(len(sp.points))
    #sp.points.add(num_points-1)
    #for point, pcoord in zip(sp.points, coords):
    #    point.co = tuple(pcoord) + (.0,)

def main():
    for t in bpy.data.texts:
        if t.name.endswith(".gcode"):
            parse_gcode(t)
            break

