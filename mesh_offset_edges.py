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
    "version": (0, 1, 11),
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
from mathutils import Vector, Quaternion

X_UP = Vector((1.0, .0, .0))
Y_UP = Vector((.0, 1.0, .0))
Z_UP = Vector((.0, .0, 1.0))
ZERO_VEC = Vector((.0, .0, .0))
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
    detect_hole = bpy.props.BoolProperty(
        name="Detect Hole", default=True,
        description="Detect edges around holes and flip direction")
    flip = bpy.props.BoolProperty(
        name="Flip", default=False,
        description="Flip direction")
    mirror_modifier = bpy.props.BoolProperty(
        name="Mirror Modifier", default=False,
        description="Take into account for Mirror modifier")

    threshold = bpy.props.FloatProperty(
        name="Threshold", default=1.0e-4, step=1.0e-5,
        description="Angle threshold which determines folding edges",
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

        if self.follow_face:
            layout.prop(self, 'detect_hole')

        for m in context.edit_object.modifiers:
            if m.type == 'MIRROR':
                layout.prop(self, 'mirror_modifier')
                break

    def create_edgeloops(self, bm, mirror_planes):
        selected_edges = []
        self.mirror_v_p_pairs = mirror_v_p_pairs = dict()
        # key is vert, value is the mirror plane to which the vert belongs.
        for e in bm.edges:
            if e.select:
                co_faces_selected = 0
                for f in e.link_faces:
                    if f.select:
                        co_faces_selected += 1
                else:
                    if co_faces_selected <= 1:
                        selected_edges.append(e)
                        if mirror_planes:
                            v1, v2 = e.verts
                            v1_4d = v1.co.to_4d()
                            v2_4d = v2.co.to_4d()
                            for plane, threshold in mirror_planes:
                                if (abs(v1_4d.dot(plane)) < threshold
                                   and abs(v2_4d.dot(plane)) < threshold):
                                    # This edge is on the mirror plane
                                    selected_edges.pop()
                                    mirror_v_p_pairs[v1] = \
                                        mirror_v_p_pairs[v2] = plane
                                    break

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

            if len_link == 2:
                v_es_pairs[v] = selected_link_edges
            elif len_link == 1:
                v_es_pairs[v] = selected_link_edges
                end_verts.add(v)
            elif len_link >= 3:
                self.report({'WARNING'},
                            "Select non-branching edge chains")
                return None

        if mirror_planes:
            for v in end_verts:
                if v not in mirror_v_p_pairs:
                    for plane, threshold in mirror_planes:
                        if abs(v.co.to_4d().dot(plane)) < threshold:
                            # This vert is on the mirror plane
                            mirror_v_p_pairs[v] = plane
                            break

        self.edge_loops = edge_loops = selected_edges

        self.extended_verts = extended_verts = set()
        while end_verts:
            v_start = end_verts.pop()
            e_start = v_es_pairs[v_start][0]
            edge_chain = [(v_start, e_start)]
            v_current = e_start.other_vert(v_start)
            e_prev = e_start
            while v_current not in end_verts:
                e1, e2 = v_es_pairs[v_current]
                e = e1 if e1 != e_prev else e2
                edge_chain.append((v_current, e))
                v_current = e.other_vert(v_current)
                e_prev = e
            end_verts.remove(v_current)

            geom = bmesh.ops.extrude_vert_indiv(bm, verts=[v_start, v_current])
            extended_verts.update(geom['verts'])
            edge_loops += geom['edges']
            for ex_v in geom['verts']:
                ex_edge = ex_v.link_edges[0]
                delta = .0
                if ex_edge.other_vert(ex_v) is v_start:
                    v_orig = v_start
                    for v, e in edge_chain:
                        if e.calc_length() != 0.0:
                            delta = v.co - e.other_vert(v).co
                            break
                else:
                    v_orig = v_current
                    for v, e in reversed(edge_chain):
                        if e.calc_length() != 0.0:
                            delta = e.other_vert(v).co - v.co
                            break

                ex_v.co += delta
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

        extended_verts = self.extended_verts
        mirror_v_p_pairs = self.mirror_v_p_pairs
        mirror_v_p_pairs_new = dict()
        self.v_v_pairs = v_v_pairs = dict()  # keys is offset vert,
                                             # values is original vert.
        for e in side_edges:
            v1, v2 = e.verts
            if v1 in offset_verts:
                v_offset, v_orig = v1, v2
            else:
                v_offset, v_orig = v2, v1
            v_v_pairs[v_offset] = v_orig

            if v_orig in extended_verts:
                extended_verts.add(v_offset)
            plane = mirror_v_p_pairs.get(v_orig)
            if plane:
                # Offsetted vert should be on the mirror plane.
                mirror_v_p_pairs_new[v_offset] = plane
        self.mirror_v_p_pairs = mirror_v_p_pairs_new

        self.img_faces = img_faces = bmesh.ops.edgeloop_fill(
            bm, edges=offset_edges, mat_nr=0, use_smooth=False)['faces']

        self.e_e_pairs = {
            fl.edge: fl.link_loop_radial_next.link_loop_next.link_loop_next.edge
            for face in img_faces for fl in face.loops}

        if self.follow_face:
            self.e_lp_pairs = e_lp_pairs = dict()
            for e_offset, e_orig in self.e_e_pairs.items():
                t = tuple(loop for loop in e_orig.link_loops
                          if (loop.face.select
                              and loop.face not in side_faces
                              and not loop.face.hide
                              and loop.face.normal != ZERO_VEC))
                if t:
                    e_lp_pairs[e_offset] = t
                    continue
                t = tuple(loop for loop in e_orig.link_loops
                          if (loop.face not in side_faces
                              and not loop.face.hide
                              and loop.face.normal != ZERO_VEC))
                e_lp_pairs[e_offset] = t or tuple()

            # Calculate normals
            self.calc_average_fnorm()
            e_fn_pairs = self.e_fn_pairs
            for face in img_faces:
                #face.loops.index_update()
                for fl in face.loops:
                    fn = e_fn_pairs[fl.edge]
                    if fn:
                        if face.normal.dot(fn) < .0:
                            face.normal_flip()
                        break
        else:
            for face in img_faces:
                if face.normal[2] < .0:
                    face.normal_flip()

        return img_faces

    def calc_average_fnorm(self):
        self.e_fn_pairs = e_fn_pairs = dict()
        # edge:average_face_normal pairs.
        e_lp_pairs = self.e_lp_pairs

        for e in self.offset_edges:
            loops = e_lp_pairs[e]
            if loops:
                normal = Vector()
                for lp in loops:
                    normal += lp.face.normal
                normal.normalize()
                e_fn_pairs[e] = normal
            else:
                e_fn_pairs[e] = None

    def get_inner_vec(self, floop):
        """Get inner edge vector connecting to floop.vert"""
        adj_loop = self.e_lp_pairs[floop.edge]
        if len(adj_loop) != 1:
            return None
        adj_loop = adj_loop[0]

        vert = self.v_v_pairs[floop.vert]
        if adj_loop.vert is vert:
            inner_edge = adj_loop.link_loop_prev.edge
        else:
            inner_edge = adj_loop.link_loop_next.edge
        edge_vector = inner_edge.other_vert(vert).co - vert.co
        if edge_vector == ZERO_VEC:
            return None
        edge_vector.normalize()
        return edge_vector

    def is_hole(self, floop, tangent):
        edge = self.e_e_pairs[floop.edge]
        adj_loop = self.e_lp_pairs[floop.edge]
        if len(adj_loop) != 1:
            return None
        adj_loop = adj_loop[0]

        vec_edge = edge.verts[0].co - edge.verts[1].co
        vec_adj = adj_loop.calc_tangent()
        vec_adj -= vec_adj.project(vec_edge)
        if vec_adj == ZERO_VEC:
            return None
        if vec_adj.dot(tangent) > .0:
            # Hole
            return True
        else:
            return False

    def clean_geometry(self, bm):
        bm.normal_update()

        img_faces = self.img_faces
        offset_verts = self.offset_verts
        offset_edges = self.offset_edges
        side_edges = self.side_edges
        side_faces = self.side_faces
        extended_verts = self.extended_verts
        v_v_pairs = self.v_v_pairs

        if self.geometry_mode == 'extrude':
            for face in img_faces:
                flip = True if self.flip else False

                lp = face.loops[0]
                side_lp = lp.link_loop_radial_next
                if lp.vert is not side_lp.vert:
                    # imaginary face normal and side faces normal
                    # should inconsistent.
                    flip = not flip

                if face in self.should_flip:
                    flip = not flip

                if flip:
                    sides = (
                        lp.link_loop_radial_next.face for lp in face.loops)
                    for sf in sides:
                        sf.normal_flip()

        bmesh.ops.delete(bm, geom=img_faces, context=1)

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
    def skip_zero_length_edges(floop, normal=None, reverse=False):
        floop_orig = floop
        if normal:
            normal = normal.normalized()
        skip_co = 0
        length = floop.edge.calc_length()
        if length and normal:
            # length which is perpendicular to normal
            edge = floop.vert.co - floop.link_loop_next.vert.co
            edge -= edge.project(normal)
            length = edge.length

        while length == 0:
            floop = (floop.link_loop_next if not reverse
                     else floop.link_loop_prev)
            if floop is floop_orig:
                # All edges is zero length
                return None, None
            skip_co += 1
            length = floop.edge.calc_length()
            if length and normal:
                edge = floop.vert.co - floop.link_loop_next.vert.co
                edge -= edge.project(normal)
                length = edge.length

        return floop, skip_co

    @staticmethod
    def get_mirror_planes(edit_object):
        mirror_planes = []
        e_mat_inv = edit_object.matrix_world.inverted()
        for m in edit_object.modifiers:
            if (m.type == 'MIRROR' and m.use_mirror_merge
               and m.show_viewport and m.show_in_editmode):
                mthreshold = m.merge_threshold
                if m.mirror_object:
                    xyz_mat = e_mat_inv * m.mirror_object.matrix_world
                    x, y, z, w = xyz_mat.adjugated()
                    loc = xyz_mat.to_translation()
                    for axis in (x, y, z):
                        axis[0:3] = axis.to_3d().normalized()
                        dist = -axis.to_3d().dot(loc)
                        axis[3] = dist
                else:
                    x, y, z = X_UP.to_4d(), Y_UP.to_4d(), Z_UP.to_4d()
                    x[3] = y[3] = z[3] = .0
                if m.use_x:
                    mirror_planes.append((x, mthreshold))
                if m.use_y:
                    mirror_planes.append((y, mthreshold))
                if m.use_z:
                    mirror_planes.append((z, mthreshold))
        return mirror_planes

    def apply_mirror(self):
        # Crip or extend edges to the mirror planes
        side_edges, extended_verts = self.side_edges, self.extended_verts
        for v, plane in self.mirror_v_p_pairs.items():
            for e in v.link_edges:
                if e in side_edges or e.other_vert(v) in extended_verts:
                    continue
                point = v.co.to_4d()
                direction = e.verts[0].co - e.verts[1].co
                direction = direction.to_4d()
                direction[3] = .0
                t = -plane.dot(point) / plane.dot(direction)
                v.co = (point + t * direction)[:3]
                break

    @staticmethod
    def decompose_vector(vec, vec_s, vec_t):
        det_xy = vec_s.x * vec_t.y - vec_s.y * vec_t.x
        if det_xy:
            s = (vec.x * vec_t.y - vec.y * vec_t.x) / det_xy
            t = (-vec.x * vec_s.y + vec.y * vec_s.x) / det_xy
        else:
            det_yz = vec_s.y * vec_t.z - vec_s.z * vec_t.y
            if det_yz:
                s = (vec.x * vec_t.z - vec.y * vec_t.y) / det_yz
                t = (-vec.x * vec_s.z + vec.y * vec_s.y) / det_yz
            else:
                det_zx = vec_s.z * vec_t.x - vec_s.x * vec_t.z
                s = (vec.x * vec_t.x - vec.y * vec_t.z) / det_zx
                t = (-vec.x * vec_s.x + vec.y * vec_s.z) / det_zx
        return s, t

    def get_tangent(self, loop_act, loop_prev,
                    f_normal_act=None, f_normal_prev=None,
                    threshold=1.0e-4):
        vec_edge_act = loop_act.link_loop_next.vert.co - loop_act.vert.co
        vec_edge_act.normalize()

        vec_edge_prev = loop_prev.vert.co - loop_prev.link_loop_next.vert.co
        vec_edge_prev.normalize()

        if f_normal_act:
            if f_normal_act != ZERO_VEC:
                f_normal_act = f_normal_act.normalized()
            else:
                f_normal_act = None
        if f_normal_prev:
            if f_normal_prev != ZERO_VEC:
                f_normal_prev = f_normal_prev.normalized()
            else:
                f_normal_prev = None

        vec_tangent = None
        f_cross = None
        if f_normal_act and f_normal_prev:
            f_angle = f_normal_act.angle(f_normal_prev)
            if threshold < f_angle < ANGLE_180 - threshold:
                vec_normal = f_normal_act + f_normal_prev
                vec_normal.normalize()
                f_cross = f_normal_act.cross(f_normal_prev)
                f_cross.normalize()
            elif f_angle > ANGLE_90:
                inner = self.get_inner_vec(loop_act)
                if inner:
                    vec_tangent = -inner
                    corner_type = 'FACE_FOLD'
                else:
                    vec_normal = f_normal_act
            else:
                vec_normal = f_normal_act
        elif f_normal_act or f_normal_prev:
            vec_normal = f_normal_act or f_normal_prev
        else:
            vec_normal = loop_act.face.normal.copy()
            if vec_normal == ZERO_VEC:
                if threshold < vec_edge_act.angle(Z_UP) < ANGLE_180 - threshold:
                    vec_normal = Z_UP - Z_UP.project(vec_edge_act)
                    vec_normal.normalize()
                else:
                    # vec_edge is parallel to Z_UP
                    vec_normal = Y_UP.copy()

        if vec_tangent is None:
            # 2d edge vectors are perpendicular to vec_normal
            vec_edge_act2d = vec_edge_act - vec_edge_act.project(vec_normal)
            vec_edge_act2d.normalize()

            vec_edge_prev2d = vec_edge_prev - vec_edge_prev.project(vec_normal)
            vec_edge_prev2d.normalize()

            angle2d = vec_edge_act2d.angle(vec_edge_prev2d)
            if angle2d < threshold:
                # folding corner
                corner_type = 'FOLD'
                vec_tangent = vec_edge_act2d
                vec_angle2d = ANGLE_360
            elif angle2d > ANGLE_180 - threshold:
                # straight corner
                corner_type = 'STRAIGHT'
                vec_tangent = vec_edge_act2d.cross(vec_normal)
                vec_angle2d = ANGLE_180
            else:
                direction = vec_edge_act2d.cross(vec_edge_prev2d).dot(vec_normal)
                if direction > .0:
                    # convex corner
                    corner_type = 'CONVEX'
                    vec_tangent = -(vec_edge_act2d + vec_edge_prev2d)
                    vec_angle2d = angle2d
                else:
                    # concave corner
                    corner_type = 'CONCAVE'
                    vec_tangent = vec_edge_act2d + vec_edge_prev2d
                    vec_angle2d = ANGLE_360 - angle2d

            if vec_tangent.dot(vec_normal):
                # Make vec_tangent perpendicular to vec_normal
                vec_tangent -= vec_tangent.project(vec_normal)

            vec_tangent.normalize()

        if f_cross:
            if vec_tangent.dot(f_cross) < .0:
                f_cross *= -1

            if corner_type == 'FOLD' or corner_type == 'STRAIGHT':
                vec_tangent = f_cross
            else:
                f_cross2d = f_cross - f_cross.project(vec_normal)
                s, t = self.decompose_vector(
                    f_cross2d, vec_edge_act2d, vec_edge_prev2d)
                if s * t < threshold:
                    # For the case in which vec_tangent is not
                    # between vec_edge_act2d and vec_edge_prev2d.
                    # Probably using 3d edge vectors is
                    # more intuitive than 2d edge vectors.
                    if corner_type == 'CONVEX':
                        vec_tangent = -(vec_edge_act + vec_edge_prev)
                    else:
                        # CONCAVE
                        vec_tangent = vec_edge_act + vec_edge_prev
                    vec_tangent.normalize()
                else:
                    vec_tangent = f_cross

        if corner_type == 'FOLD':
            factor_act = factor_prev = 0
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

        mirror_planes = None
        if self.mirror_modifier:
            mirror_planes = self.get_mirror_planes(edit_object)

        e_loops = self.create_edgeloops(bm, mirror_planes)
        if e_loops is None:
            bm.free()
            bpy.ops.object.editmode_toggle()
            return {'CANCELLED'}

        fs = self.create_geometry(bm, e_loops)
        self.should_flip = set()  # includes faces, side faces around which
                                  # should flip its normal
                                  # later in clean_geometry()

        follow_face = self.follow_face
        if follow_face:
            e_fn_pairs = self.e_fn_pairs
            detect_hole = self.detect_hole
        else:
            detect_hole = False
        threshold = self.threshold

        for f in fs:
            width = self.width if not self.flip else -self.width
            normal = f.normal if not self.follow_face else None
            move_vectors = []
            co_hole_check = 5  # The largest number of test
                               # whether edges are around a hole or not
            for floop in f.loops:
                loop_act, skip_next_co = \
                    self.skip_zero_length_edges(floop, normal, reverse=False)
                if loop_act is None:
                    # All edges is zero length
                    break

                loop_prev = floop.link_loop_prev
                loop_prev, skip_prev_co = \
                    self.skip_zero_length_edges(loop_prev, normal, reverse=True)

                if not follow_face:
                    n1, n2 = None, None
                else:
                    n1 = e_fn_pairs[loop_act.edge]
                    n2 = e_fn_pairs[loop_prev.edge]

                tangent = self.get_tangent(
                    loop_act, loop_prev, n1, n2, threshold)

                if detect_hole and co_hole_check:
                    co_hole_check -= 1
                    hole = self.is_hole(loop_act, tangent[0])
                    if hole is not None:
                        co_hole_check = 0
                        if hole:
                            width *= -1
                            # side face normals should be flipped
                            self.should_flip.add(f)

                move_vectors.append(tangent)

            for floop, vecs in zip(f.loops, move_vectors):
                vec_tan, factor_act, factor_prev = vecs
                floop.vert.co += \
                    width * min(factor_act, factor_prev) * vec_tan

        if self.mirror_modifier:
            self.apply_mirror()

        self.clean_geometry(bm)

        #bmesh.update_edit_mesh(me)
        bm.to_mesh(me)
        bm.free()
        bpy.ops.object.editmode_toggle()
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

        self.mirror_modifier = False
        for m in edit_object.modifiers:
            if (m.type == 'MIRROR' and m.use_mirror_merge
               and m.show_viewport and m.show_in_editmode):
                self.mirror_modifier = True
                break

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
