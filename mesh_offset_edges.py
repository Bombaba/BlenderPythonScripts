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
    "version": (0, 1, 3),
    "blender": (2, 68, 0),
    "location": "VIEW3D > Edge menu(CTRL-E) > Offset Edges",
    "description": "Offset Edges",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Mesh"}

import math
from math import sin, pi
import bpy
import bmesh
from mathutils import Vector

Z_UP = Vector((.0, .0, 1.0))
ANGLE_90 = pi / 2
ANGLE_180 = pi
ANGLE_360 = 2 * pi


class OffsetEdges(bpy.types.Operator):
    """Offset Edges."""
    bl_idname = "mesh.offset_edges"
    bl_label = "Offset Edges"
    bl_options = {'REGISTER', 'UNDO'}

    width = bpy.props.FloatProperty(
        name="Width", default=.2, precision=3, step=0.05)
    geometry_mode = bpy.props.EnumProperty(
        items=[('offset', "Offset", "Offset edges"),
               ('extrude', "Extrude", "Extrude edges"),
               ('move', "Move", "Move selected edges")],
        name="Geometory mode", default='offset')
    follow_face = bpy.props.BoolProperty(
        name="Follow Face", default=False,
        description="Offset along faces around")
    flip = bpy.props.BoolProperty(
        name="Flip", default=False,
        description="Flip Direction")

    threshold = bpy.props.FloatProperty(
        name="Threshold", default=1.0e-4, step=1.0e-5,
        description="Angle threshold which determins folding edges",
        options={'HIDDEN'})

    @classmethod
    def poll(self, context):
        return context.mode == 'EDIT_MESH'

    def draw(self, context):
        layout = self.layout
        layout.prop(self, 'geometry_mode', text="")

        layout.prop(self, 'width')
        layout.prop(self, 'flip')
        layout.prop(self, 'follow_face')

    def create_edgeloops(self, bm):
        selected_edges = []
        for e in bm.edges:
            if e.select:
                co_faces_selected = 0
                for f in e.link_faces:
                    if f.select:
                        co_faces_selected += 1
                else:
                    if co_faces_selected <= 1:
                        selected_edges.append(e)

        if not selected_edges:
            self.report({'WARNING'},
                        "No edges selected.")
            return None

        selected_verts = set(v for e in selected_edges for v in e.verts)

        v_es_pairs = dict()
        end_verts = set()
        for v in selected_verts:
            selected_link_edges = \
                tuple(e for e in v.link_edges if e in selected_edges)
            len_link = len(selected_link_edges)

            if len_link:
                if len_link == 2:
                    v_es_pairs[v] = selected_link_edges
                elif len_link == 1:
                    v_es_pairs[v] = selected_link_edges
                    end_verts.add(v)
                elif len_link >= 3:
                    self.report({'WARNING'},
                                "Select non-branching edge chains")
                    return None

        edge_loops = selected_edges.copy()

        self.extended_verts = set()
        while end_verts:
            v_start = end_verts.pop()
            e_start = v_es_pairs[v_start][0]
            edge_chain = [(v_start, e_start)]
            v_current = e_start.other_vert(v_start)
            e_prev = e_start
            while v_current not in end_verts:
                for e in v_es_pairs[v_current]:
                    if e != e_prev:
                        edge_chain.append((v_current, e))
                        v_current = e.other_vert(v_current)
                        e_prev = e
                        break
            end_verts.remove(v_current)

            geom = bmesh.ops.extrude_vert_indiv(bm, verts=[v_start, v_current])
            self.extended_verts.update(geom['verts'])
            edge_loops += geom['edges']
            for ex_v in geom['verts']:
                link_edge = ex_v.link_edges[0]
                if link_edge.other_vert(ex_v) is edge_chain[0][0]:
                    for v, e in edge_chain:
                        if e.calc_length() != 0.0:
                            ex_v.co += v.co - e.other_vert(v).co
                            break
                else:
                    for v, e in reversed(edge_chain):
                        if e.calc_length() != 0.0:
                            ex_v.co += e.other_vert(v).co - v.co
                            break
            edge_loops.append(bm.edges.new(geom['verts']))

        return edge_loops

    def create_geometry(self, bm, e_loops):
        geom_extruded = bmesh.ops.extrude_edge_only(bm, edges=e_loops)['geom']

        self.offset_verts = offset_verts = \
            [e for e in geom_extruded if isinstance(e, bmesh.types.BMVert)]
        self.offset_edges = offset_edges = \
            [e for e in geom_extruded if isinstance(e, bmesh.types.BMEdge)]
        self.side_faces = side_faces = \
            [f for f in geom_extruded if isinstance(f, bmesh.types.BMFace)]
        bmesh.ops.recalc_face_normals(bm, faces=side_faces)
        self.side_edges = side_edges = \
            [e.link_loops[0].link_loop_next.edge for e in offset_edges]

        for f in side_faces:
            f.select = True

        extended_verts = self.extended_verts
        self.v_v_pairs = v_v_pairs = dict()  # keys are offset verts,
                                             # values are original verts
        for e in side_edges:
            v1, v2 = e.verts
            if v1 in offset_verts:
                v_offset, v_orig = v1, v2
            else:
                v_offset, v_orig = v2, v1
            v_v_pairs[v_offset] = v_orig

            if v_orig in extended_verts:
                extended_verts.add(v_offset)

        self.faces = faces = bmesh.ops.edgeloop_fill(
            bm, edges=offset_edges, mat_nr=0, use_smooth=False)['faces']

        self.l_fn_pairs = l_fn_pairs = dict()  # loop - face normal pairs.
        for face in faces:
            face.loops.index_update()
            if face.normal.dot(v_v_pairs[face.verts[0]].normal) < .0:
                face.normal_flip()

            for fl in face.loops:
                edge = \
                    fl.link_loop_radial_next.link_loop_next.link_loop_next.edge

                co = 0
                normal = Vector((.0, .0, .0))
                for f in edge.link_faces:
                    if (f not in side_faces and not f.hide
                       and f.normal.length):
                        normal += f.normal
                        co += 1
                        if f.select:
                            l_fn_pairs[fl] = f.normal.copy()
                            break
                else:
                    if co:
                        normal.normalize()
                        l_fn_pairs[fl] = normal
                # Be careful, if you flip face normal after
                # this line, l_fn_pairs won't work as you expect
                # because face.normal_flip() changes loop order in
                # the face.

        return faces

    def clean_geometry(self, bm):
        bm.normal_update()

        offset_verts = self.offset_verts
        offset_edges = self.offset_edges
        side_edges = self.side_edges
        side_faces = self.side_faces
        extended_verts = self.extended_verts
        v_v_pairs = self.v_v_pairs
        l_fn_pairs = self.l_fn_pairs

        # Align extruded face normals
        if self.geometry_mode == 'extrude':
            for f in self.faces:
                for l in f.loops:
                    side_face = l.link_loop_radial_next.face
                    direction = l_fn_pairs.get(l)
                    if direction:
                        if side_face.normal.dot(direction) < .0:
                            side_face.normal_flip()
                    elif side_face.normal.dot(Z_UP) < .0:
                            side_face.normal_flip()

        bmesh.ops.delete(bm, geom=self.faces, context=1)


        if self.geometry_mode != 'extrude':
            if self.geometry_mode == 'offset':
                bmesh.ops.delete(bm, geom=side_edges+side_faces, context=1)
            elif self.geometry_mode == 'move':
                for v_target, v_orig in v_v_pairs.items():
                    v_orig.co = v_target.co
                bmesh.ops.delete(
                    bm, geom=side_edges+side_faces+offset_edges+offset_verts,
                    context=1)
                extended_verts -= set(offset_verts)

        extended = extended_verts.copy()
        for v in extended_verts:
            extended.update(v.link_edges)
            extended.update(v.link_faces)
        bmesh.ops.delete(bm, geom=list(extended), context=1)

    @staticmethod
    def skip_zero_width_edges(f_loop, normal=None, reverse=False):
        if normal:
            normal = normal.normalized()
        skip_co = 0
        length = f_loop.edge.calc_length()
        if length and normal:
            # length which is perpendicular to normal
            edge = f_loop.vert.co - f_loop.link_loop_next.vert.co
            edge -= edge.dot(normal) * normal
            length = edge.length

        while length == 0:
            f_loop = (f_loop.link_loop_next if not reverse
                      else f_loop.link_loop_prev)
            skip_co += 1
            length = f_loop.edge.calc_length()
            if length and normal:
                edge = f_loop.vert.co - f_loop.link_loop_next.vert.co
                edge -= edge.dot(normal) * normal
                length = edge.length

        return f_loop, skip_co

    @staticmethod
    def get_vector(loop_act, loop_prev,
                    f_normal_act=None, f_normal_prev=None,
                    threshold=1.0e-4):
        flags = set()

        vec_edge_act = loop_act.link_loop_next.vert.co - loop_act.vert.co
        vec_edge_act.normalize()

        vec_edge_prev = loop_prev.vert.co - loop_prev.link_loop_next.vert.co
        vec_edge_prev.normalize()

        if f_normal_act and f_normal_prev:
            vec_normal = f_normal_act + f_normal_prev
            vec_normal.normalize()
        elif f_normal_act or f_normal_prev:
            vec_normal = (f_normal_act or f_normal_prev).normalized()
        else:
            vec_normal = loop_act.face.normal.copy()
            if vec_normal.length == .0:
                vec_normal = Vector((.0, .0, 1.0))

        # 2d edge vectors are perpendicular to vec_normal
        vec_edge_act2d = \
            vec_edge_act - vec_edge_act.dot(vec_normal) * vec_normal
        vec_edge_act2d.normalize()

        vec_edge_prev2d = \
            vec_edge_prev - vec_edge_prev.dot(vec_normal) * vec_normal
        vec_edge_prev2d.normalize()

        vec_angle2d = vec_edge_act2d.angle(vec_edge_prev2d)
        if vec_angle2d < threshold:
            # FOLD edges
            flags.add('FOLD')
            vec_tangent = vec_edge_act2d
            vec_angle2d = ANGLE_360
        elif vec_angle2d > ANGLE_180 - threshold:
            # STRAIGHT edges
            flags.add('STRAIGHT')
            vec_tangent = vec_edge_act2d.cross(vec_normal)
            vec_angle2d = ANGLE_180
        else:
            direction = vec_edge_act2d.cross(vec_edge_prev2d).dot(vec_normal)
            if direction > .0:
                # CONVEX corner
                flags.add('CONVEX')
                vec_tangent = -(vec_edge_act2d + vec_edge_prev2d).normalized()
                vec_angle2d = vec_edge_act2d.angle(vec_edge_prev2d)
            else:
                # CONCAVE corner
                flags.add('CONCAVE')
                vec_tangent = (vec_edge_act2d + vec_edge_prev2d).normalized()
                vec_angle2d = ANGLE_360 - vec_edge_act2d.angle(vec_edge_prev2d)

        if vec_tangent.dot(vec_normal):
            # Make vec_tangent perpendicular to vec_normal
            vec_tangent -= vec_tangent.dot(vec_normal) * vec_normal
            vec_tangent.normalize()

        if f_normal_act and f_normal_prev:
            f_angle = f_normal_act.angle(f_normal_prev)
            if f_angle > threshold:
                f_tangent = f_normal_act.cross(f_normal_prev)
                f_tangent.normalize()
                #print("vec_tangent:", vec_normal, "\nf_tangent:", f_tangent)
                if vec_tangent.dot(f_tangent) < .0:
                    f_tangent *= -1
                vec_tangent = f_tangent
                flags.add('FACE_CROSS_LINE')

        if 'FOLD' in flags:
            # This case occures when edges are folding
            factor_act = factor_prev = 0
        elif 'FACE_CROSS_LINE' in flags:
            angle_a = vec_tangent.angle(vec_edge_act2d)
            angle_p = vec_tangent.angle(vec_edge_prev2d)
            angle_sum = vec_angle2d + angle_a + angle_p
            if (angle_a < threshold or angle_p < threshold
               or angle_sum > ANGLE_360 + threshold):
                # For the case in which vec_tangent is not
                # between vec_edge_act2d and vec_edge_prev2d.
                    vec_tangent = vec_edge_act + vec_edge_prev
                    # Using original vector is more intuitive
                    # than using 2d edges.
                    vec_tangent.normalize()
                    if 'CONVEX' in flags:
                        vec_tangent *= -1
                    factor_act = factor_prev = \
                        1.0 / sin(vec_tangent.angle(vec_edge_act))
            else:
                factor_act = 1. / sin(vec_tangent.angle(vec_edge_act))
                factor_prev = 1. / sin(vec_tangent.angle(vec_edge_prev))

        else:
            factor_act = 1. / sin(vec_tangent.angle(vec_edge_act))
            factor_prev = 1. / sin(vec_tangent.angle(vec_edge_prev))

        return vec_tangent, factor_act, factor_prev

    def execute(self, context):
        edit_object = context.edit_object
        me = edit_object.data
        #bm = bmesh.from_edit_mesh(me)  # This method causes blender crash
                                        # if an error occured during script
                                        # execution.
        bpy.ops.object.editmode_toggle()
        bm = bmesh.new()
        bm.from_mesh(me)

        e_loops = self.create_edgeloops(bm)
        if e_loops is None:
            bm.free()
            bpy.ops.object.editmode_toggle()
            return {'CANCELLED'}

        fs = self.create_geometry(bm, e_loops)

        width = self.width if not self.flip else -self.width
        threshold = self.threshold
        if self.follow_face:
            l_fn_pairs = self.l_fn_pairs

        for f in fs:
            f_normal = f.normal.normalized()

            move_vectors = []

            for f_loop in f.loops:
                if self.follow_face:
                    act_loop, skip_next_co = \
                        self.skip_zero_width_edges(f_loop, reverse=False)

                    prev_loop = f_loop.link_loop_prev
                    prev_loop, skip_prev_co = \
                        self.skip_zero_width_edges(prev_loop, reverse=True)

                    n1 = l_fn_pairs.get(act_loop)
                    n2 = l_fn_pairs.get(prev_loop)
                    vectors = self.get_vector(
                        act_loop, prev_loop, n1, n2, threshold)
                else:
                    act_loop, skip_next_co = \
                        self.skip_zero_width_edges(f_loop, f_normal, reverse=False)

                    prev_loop = f_loop.link_loop_prev
                    prev_loop, skip_prev_co = \
                        self.skip_zero_width_edges(prev_loop, f_normal, reverse=True)

                    vectors = self.get_vector(
                        act_loop, prev_loop, threshold=threshold)

                move_vectors.append(vectors)

            for f_loop, vecs in zip(f.loops, move_vectors):
                vec_tan, factor_act, factor_prev = vecs
                f_loop.vert.co += \
                    width * min(factor_act, factor_prev) * vec_tan

        self.clean_geometry(bm)

        #bmesh.update_edit_mesh(me)
        bm.to_mesh(me)
        bm.free()
        bpy.ops.object.editmode_toggle()
        return {'FINISHED'}

    def invoke(self, context, event):
        me = context.edit_object.data
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
    #bpy.types.VIEW3D_PT_tools_meshedit.append(draw_item)


def unregister():
    bpy.utils.unregister_class(OffsetEdges)
    bpy.types.VIEW3D_MT_edit_mesh_edges.remove(draw_item)
    #bpy.types.VIEW3D_PT_tools_meshedit.remove(draw_item)


if __name__ == '__main__':
    register()
