# ***** BEGIN GPL LICENSE BLOCK *****
#
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ***** END GPL LICENCE BLOCK *****

# <pep8 compliant>

bl_info = {
    "name": "Offset Edges",
    "author": "Hidesato Ikeya",
    "version": (0, 2, 1),
    "blender": (2, 70, 0),
    "location": "VIEW3D > Edge menu(CTRL-E) > Offset Edges",
    "description": "Offset Edges",
    "warning": "",
    "wiki_url": "http://wiki.blender.org/index.php/Extensions:2.6/Py/Scripts/Modeling/offset_edges",
    "tracker_url": "",
    "category": "Mesh"}

import math
from math import sin, cos, pi
import bpy
import bmesh
from mathutils import Vector
#from time import perf_counter

X_UP = Vector((1.0, .0, .0))
Y_UP = Vector((.0, 1.0, .0))
Z_UP = Vector((.0, .0, 1.0))
ZERO_VEC = Vector((.0, .0, .0))
ANGLE_90 = pi / 2
ANGLE_180 = pi
ANGLE_360 = 2 * pi


def calc_normal_from_verts(verts, fallback=Z_UP):
    # Calculate normal from verts using Newell's method.
    normal = ZERO_VEC.copy()

    verts_2 = verts[1:]
    if verts[0] is not verts[-1]:
        # Half loop.
        verts_2.append(verts[0])
    for v1, v2 in zip(verts, verts_2):
        v1co, v2co = v1.co, v2.co
        normal.x += (v1co.y - v2co.y) * (v1co.z + v2co.z)
        normal.y += (v1co.z - v2co.z) * (v1co.x + v2co.x)
        normal.z += (v1co.x - v2co.x) * (v1co.y + v2co.y)

    normal.normalize()
    if normal == ZERO_VEC:
        normal = fallback

    return normal

def get_corner_type(vec_up, vec_right2d, vec_left2d, threshold=1.0e-4):
    # vec_right2d and vec_left2d should be perpendicular to vec_up.
    # All vectors in parameters should have been normalized.
    if vec_right2d == vec_left2d == ZERO_VEC:
        return 'FOLDING'
    elif vec_right2d == ZERO_VEC or vec_left2d == ZERO_VEC:
        return 'STRAIGHT'

    angle = vec_right2d.angle(vec_left2d)
    if angle < threshold:
        return 'FOLDING'
    elif angle > ANGLE_180 - threshold:
        return 'STRAIGHT'
    elif vec_right2d.cross(vec_left2d).dot(vec_up) > threshold:
        return 'CONVEX'
    else:
        return 'CONCAVE'

def calc_tangent(vec_up, vec_right, vec_left, threshold=1.0e-4):
    vec_right2d = vec_right- vec_right.project(vec_up)
    vec_right2d.normalize()
    vec_left2d = vec_left- vec_left.project(vec_up)
    vec_right2d.normalize()

    corner = get_corner_type(vec_up, vec_right2d, vec_left2d, threshold)
    if corner == 'FOLDING':
        vec_tangent = ZERO_VEC
    elif corner == 'STRAIGHT':
        if vec_right2d.length >= vec_left2d.length:
            vec_longer = vec_right2d
        else:
            vec_longer = -vec_left2d
        vec_tangent = vec_longer.cross(vec_up)
    elif corner == 'CONVEX':
        vec_tangent = vec_right2d + vec_left2d
        vec_tangent *= -1
    elif corner == 'CONCAVE':
        vec_tangent = vec_right2d + vec_left2d

    vec_tangent.normalize()

    return vec_tangent

def get_factor(vec_direction, vec_right, vec_left, func=max):
    if vec_direction == ZERO_VEC:
        return .0

    denominator = func(sin(vec_direction.angle(vec_right)), sin(vec_direction.angle(vec_left)))
    if denominator != .0:
        return 1.0 / denominator
    else:
        return .0

def collect_edges(bm):
    set_edges_orig = set()
    for e in bm.edges:
        if e.select:
            co_faces_selected = 0
            for f in e.link_faces:
                if f.select:
                    co_faces_selected += 1
                    if co_faces_selected == 2:
                        break
            else:
                set_edges_orig.add(e)

    if not set_edges_orig:
        self.report({'WARNING'},
                    "No edges selected.")
        return None

    return set_edges_orig

def collect_loops(set_edges_orig):
    set_edges_orig = set_edges_orig.copy()

    loops = []  # [v, e, v, e, ... , e, v]
    while set_edges_orig:
        edge_start = set_edges_orig.pop()
        v_left, v_right = edge_start.verts
        lp = [v_left, edge_start, v_right]
        reverse = False
        while True:
            edge = None
            for e in v_right.link_edges:
                if e in set_edges_orig:
                    if edge:
                        # Overlap detected.
                        return None
                    edge = e
                    set_edges_orig.remove(e)
            if edge:
                v_right = edge.other_vert(v_right)
                lp.extend((edge, v_right))
                continue
            else:
                if v_right is v_left:
                    # Real loop.
                    loops.append(lp)
                    break
                elif reverse is False:
                    # Right side of half loop.
                    # Reversing the loop to operate same procedure on the left side.
                    lp.reverse()
                    v_right, v_left = v_left, v_right
                    reverse = True
                    continue
                else:
                    # Half loop, completed.
                    loops.append(lp)
                    break
    return loops

def reorder_loop(verts, edges, normal, adj_faces):
    for i, adj_f in enumerate(adj_faces):
        if adj_f is None:
            continue
        v1, v2 = verts[i], verts[i+1]
        e = edges[i]
        fv = tuple(adj_f.verts)
        if fv[fv.index(v1)-1] is v2:
            # Align loop direction
            verts.reverse()
            edges.reverse()
            adj_faces.reverse()
        if normal.dot(adj_f.normal) < .0:
            normal *= -1
        break
    return verts, edges, normal, adj_faces

def get_adj_ix(ix_start, vec_edges, half_loop):
    # Get adjacent edge index, skipping zero length edges
    len_edges = len(vec_edges)
    if half_loop:
        if ix_start == 0:
            # Left most.
            range_right = range(0, len_edges)
            range_left = None
        elif ix_start == len_edges:
            # Right most.
            range_right = None
            range_left = range(len_edges - 1, -1, -1)
        else:
            range_right = range(ix_start, len_edges)
            range_left = range(ix_start-1, -1, -1)
    else:
        range_right = range(ix_start, ix_start+len_edges)
        range_left = range(ix_start-1, ix_start-1-len_edges, -1)

    ix_right = ix_left = None
    if range_right:
        for i in range_right:
            # Right
            i %= len_edges
            if vec_edges[i] != ZERO_VEC:
                ix_right = i
                break
    if range_left is None:
        # Left most
        ix_left = ix_right
    else:
        for i in range_left:
            # Left
            i %= len_edges
            if vec_edges[i] != ZERO_VEC:
                ix_left = i
                break
    if range_right is None:
        # Right most
        ix_right = ix_left

    return ix_right, ix_left

def get_normals(lp_normal, ix_r, ix_l, adj_faces):
    normal_r = normal_l = None
    if adj_faces:
        f_r, f_l = adj_faces[ix_r], adj_faces[ix_l]
        if f_r:
            normal_r = f_r.normal
        if f_l:
            normal_l = f_l.normal

    if normal_r and normal_l:
        vec_up = (normal_r + normal_l).normalized()
        if vec_up == ZERO_VEC:
            vec_up = lp_normal.copy()
    elif normal_r or normal_l:
        vec_up = (normal_r or normal_l).copy()
    else:
        vec_up = lp_normal.copy()

    return vec_up, normal_r, normal_l


def get_offset_verts(verts, edges, geom_ex):
    if verts[0] is verts[-1]:
        # Real loop
        verts = verts[:-1]
    if geom_ex:
        geom_s = geom_ex['side']
        verts_ex = []
        for v in verts:
            for e in v.link_edges:
                if e in geom_s:
                    verts_ex.append(e.other_vert(v))
                    break
        #assert len(verts) == len(verts_ex)
        verts = verts_ex
    return verts

def get_adj_faces(edges):
    adj_faces = []
    adj_exist = False
    for e in edges:
        face = None
        for f in e.link_faces:
            # Search an adjacent face.
            # Selected face has precedance.
            if not f.hide and f.normal != ZERO_VEC:
                face = f
                adj_exist = True
                if f.select: break
        adj_faces.append(face)
    if adj_exist:
        return adj_faces
    else:
        return None

def get_edge_rail(vert, set_edges_orig):
    co_edge =  0
    vec_inner = None
    for e in vert.link_edges:
        if not e.hide and e not in set_edges_orig:
            v1, v2 = e.verts
            vec = v1.co - v2.co
            if vec != ZERO_VEC:
                co_edge += 1
                vec_inner = vec
                if co_edge == 2:
                    return None
    else:
        return vec_inner

def get_cross_rail(vec_tan, vec_edge_r, vec_edge_l, normal_r, normal_l, threshold=1.0e-4):
    # Cross rail is a cross vector between normal_r and normal_l.
    angle = normal_r.angle(normal_l)
    if angle < threshold:
        # normal_r and normal_l are almost same, no cross vector.
        return None

    vec_cross = normal_r.cross(normal_l)
    vec_cross.normalize()
    if vec_cross.dot(vec_tan) < .0:
        vec_cross *= -1
    cos_min = min(vec_tan.dot(vec_edge_r), vec_tan.dot(vec_edge_l))
    cos = vec_tan.dot(vec_cross)
    if cos >= cos_min:
        return vec_cross
    else:
        return None

def do_offset(width, depth, verts, vec_directions):
    for v, (t, u) in zip(verts, vec_directions):
        v.co += width * t + depth * u

def extrude_edges(bm, set_edges_orig):
    extruded = bmesh.ops.extrude_edge_only(bm, edges=list(set_edges_orig))['geom']
    n_edges = n_faces = len(set_edges_orig)
    n_verts = len(extruded) - n_edges - n_faces

    geom = dict()
    geom['verts'] = verts = set(extruded[:n_verts])
    geom['edges'] = edges = set(extruded[n_verts:n_verts + n_edges])
    geom['faces'] = set(extruded[n_verts + n_edges:])
    geom['side'] = set(e for v in verts for e in v.link_edges if e not in edges)

    return geom

def clean_geometry(bm, mode, set_edges_orig, geom_ex=None):
    for f in bm.faces:
        f.select = False
    if geom_ex:
        for e in geom_ex['edges']:
            e.select = True
        if mode == 'offset':
            lis_geom = list(geom_ex['side']) + list(geom_ex['faces'])
            bmesh.ops.delete(bm, geom=lis_geom, context=2)
    else:
        for e in set_edges_orig:
            e.select = True


class OffsetEdges(bpy.types.Operator):
    """Offset Edges."""
    bl_idname = "mesh.offset_edges"
    bl_label = "Offset Edges"
    bl_options = {'REGISTER', 'UNDO'}

    geometry_mode = bpy.props.EnumProperty(
        items=[('offset', "Offset", "Offset edges"),
               ('extrude', "Extrude", "Extrude edges"),
               ('move', "Move", "Move selected edges")],
        name="Geometory mode", default='offset')
    width = bpy.props.FloatProperty(
        name="Width", default=.2, precision=4, step=1)
    flip_width = bpy.props.BoolProperty(
        name="Flip Width", default=False,
        description="Flip width direction")
    depth = bpy.props.FloatProperty(
        name="Depth", default=.0, precision=4, step=1)
    flip_depth = bpy.props.BoolProperty(
        name="Flip Depth", default=False,
        description="Flip depth direction")
    angle_mode = bpy.props.BoolProperty(
        name="Angle Mode", default=True,
        description="Offset based on angle")
    angle = bpy.props.FloatProperty(
        name="Angle", default=0, step=.1, subtype='ANGLE',
        description="Angle")
    flip_angle = bpy.props.BoolProperty(
        name="Flip Angle", default=False,
        description="Flip Angle")
    follow_face = bpy.props.BoolProperty(
        name="Follow Face", default=False,
        description="Offset along faces around")
    edge_rail = bpy.props.BoolProperty(
        name="Edge Rail", default=False,
        description="Align vertices along inner edges")
    edge_rail_only_end = bpy.props.BoolProperty(
        name="Edge Rail Only End", default=False,
        description="Apply edge rail to end verts only")
    threshold = bpy.props.FloatProperty(
        name="Threshold", default=1.0e-4, step=.1, subtype='ANGLE',
        description="Angle threshold which determines straight or folding edges",
        options={'HIDDEN'})

    @classmethod
    def poll(self, context):
        return context.mode == 'EDIT_MESH'

    def draw(self, context):
        layout = self.layout
        layout.prop(self, 'geometry_mode', text="")

        layout.prop(self, 'angle_mode')
        layout.prop(self, 'width')
        layout.prop(self, 'flip_width')
        if self.angle_mode:
            layout.prop(self, 'angle')
            layout.prop(self, 'flip_angle')
        else:
            layout.prop(self, 'depth')
            layout.prop(self, 'flip_depth')

        layout.prop(self, 'follow_face')

        layout.prop(self, 'edge_rail')
        if self.edge_rail:
            layout.prop(self, 'edge_rail_only_end')

    @staticmethod
    def get_view_vecs(context):
        mat_view = context.region_data.view_matrix.to_3x3()
        vec_right = Vector(mat_view[0])
        vec_up = Vector(mat_view[1])
        vec_front = Vector(mat_view[2])
        return vec_right, vec_up, vec_front

    def execute(self, context):
        #time_start = perf_counter()

        edit_object = context.edit_object
        me = edit_object.data

        bpy.ops.object.editmode_toggle()
        bm = bmesh.new()
        bm.from_mesh(me)


        #vec_front = Vector(context.region_data.view_matrix[2][:3])
        vec_front = Z_UP
        # vec_front is used when loop normal cannot be calculated.
        vec_upward = (X_UP + Y_UP + Z_UP).normalized()
        # vec_upward is used to unify loop normals when follow_face is off.


        set_edges_orig = collect_edges(bm)
        if set_edges_orig is None:
            bm.free()
            bpy.ops.object.editmode_toggle()
            return {'CANCELLED'}

        loops = collect_loops(set_edges_orig)
        if loops is None:
            self.report({'WARNING'},
                        "Overlap detected. Select non-overlap edge loops")
            bm.free()
            bpy.ops.object.editmode_toggle()
            return {'CANCELLED'}

        if self.geometry_mode == 'move':
            geom_ex = None
        else:
            geom_ex = extrude_edges(bm, set_edges_orig)

        follow_face = self.follow_face
        edge_rail = self.edge_rail
        er_only_end = self.edge_rail_only_end
        threshold = self.threshold

        if not self.angle_mode:
            width = self.width if not self.flip_width else -self.width
            depth = self.depth if not self.flip_depth else -self.depth
        else:
            w = self.width if not self.flip_width else -self.width
            angle = self.angle if not self.flip_angle else -self.angle
            width = w * cos(angle)
            depth = w * sin(angle)

        for lp in loops:
            verts, edges = lp[::2], lp[1::2]
            lp_normal = calc_normal_from_verts(verts, fallback=vec_front)

            ##### Loop order might be changed below.
            if lp_normal.dot(vec_upward) < .0:
                # Keep consistent loop normal.
                verts.reverse()
                edges.reverse()
                lp_normal *= -1

            if follow_face:
                adj_faces = get_adj_faces(edges)
                if adj_faces:
                    verts, edges, lp_normal, adj_faces = \
                        reorder_loop(verts, edges, lp_normal, adj_faces)
            else:
                adj_faces = None
            ##### Loop order might be changed above.

            vec_edges = [(e.other_vert(v).co - v.co).normalized() for v, e in zip(verts, edges)]

            if verts[0] is verts[-1]:
                # Real loop.
                len_verts = len(verts) -1
                HALF_LOOP = False
            else:
                # Half loop
                len_verts = len(verts)
                HALF_LOOP = True
            directions = []
            for i in range(len_verts):
                ix_r, ix_l = get_adj_ix(i, vec_edges, HALF_LOOP)
                if ix_r is None:
                    break
                vec_edge_r = vec_edges[ix_r]
                vec_edge_l = -vec_edges[ix_l]

                vec_up, normal_r, normal_l =  get_normals(lp_normal, ix_r, ix_l, adj_faces)
                vec_tan = calc_tangent(vec_up, vec_edge_r, vec_edge_l, threshold)

                rail = None
                if edge_rail:
                    # Get edge rail.
                    # edge rail is a vector of inner edge.
                    if (not er_only_end or
                        (HALF_LOOP and (i == 0 or i == len_verts-1))):
                        rail = get_edge_rail(verts[i], set_edges_orig)
                if (not rail) and normal_r and normal_l:
                    # Get cross rail.
                    # Cross rail is a cross vector between normal_r and normal_l.
                    rail = get_cross_rail(vec_tan, vec_edge_r, vec_edge_l,
                                          normal_r, normal_l, threshold)
                if rail:
                    vec_tan = vec_tan.project(rail)
                    vec_tan.normalize()

                vec_tan *= get_factor(vec_tan, vec_edge_r, vec_edge_l)
                vec_up *= get_factor(vec_up, vec_edge_r, vec_edge_l)
                directions.append((vec_tan, vec_up))

            if directions:
                verts = get_offset_verts(verts, edges, geom_ex)
                do_offset(width, depth, verts, directions)

        clean_geometry(bm, self.geometry_mode, set_edges_orig, geom_ex)

        bm.to_mesh(me)
        bm.free()
        bpy.ops.object.editmode_toggle()

        #print("Time of offset_edges: ", perf_counter() - time_start)
        return {'FINISHED'}

    def invoke(self, context, event):
        edit_object = context.edit_object
        me = edit_object.data
        bpy.ops.object.editmode_toggle()
        for p in me.polygons:
            if p.select:
                self.follow_face = True
                break
        bpy.ops.object.editmode_toggle()

        return self.execute(context)


def draw_item(self, context):
    self.layout.operator_context = 'INVOKE_DEFAULT'
    self.layout.operator_menu_enum('mesh.offset_edges', 'geometry_mode')


def register():
    bpy.utils.register_class(OffsetEdges)
    bpy.types.VIEW3D_MT_edit_mesh_edges.append(draw_item)


def unregister():
    bpy.utils.unregister_class(OffsetEdges)
    bpy.types.VIEW3D_MT_edit_mesh_edges.remove(draw_item)


if __name__ == '__main__':
    register()
