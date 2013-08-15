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

bl_info = {
        "name": "Normalize Deform Weight",
        "author": "Hidesato Ikeya",
        "version": (1, 1),
        "blender": (2, 68, 0),
        "location": "View3D > ToolShelf > Normalize Deform",
        "description": "Normalize vertex weight to which vertices belongs"
                       "deformed by active armature",
        "warning": "",
        "wiki_url": "",
        "tracker_url": "",
        "category": "Mesh"}

import bpy

class _Base:
    bl_options = {'REGISTER', 'UNDO'}
    # If total of vertex weights is under this value,
    # vertex weight isn't normalized.
    threshold = bpy.props.FloatProperty(
        name="threshold", default=0.000001, min=0.0000001, max=1.0,
        step=0.000001, precision=6)

    lock_active = bpy.props.BoolProperty(name="lock active", default=False)

    def _normalize(self, context, selected):
        obj = context.weight_paint_object
        if not context.active_pose_bone:
            self.report({'ERROR'}, "Active pose bone must be selected")
            return {'CANCELLED'}

        path = repr(context.active_pose_bone)
        # path should be "bpy.data.objects['ArmatureName'].pose.bones['BoneName']"
        armature_name = path[path.find("[")+2:path.find("]")-1]
        armature = context.scene.objects[armature_name]

        deform_bones = set()
        for bone in armature.data.bones:
            if bone.use_deform:
                deform_bones.add(bone.name)

        deform_groups = set()
        locked_deform_groups = set()
        for group in obj.vertex_groups:
            if group.name in deform_bones:
                if group.lock_weight:
                    locked_deform_groups.add(group.index)
                elif self.lock_active and \
                   group.index == obj.vertex_groups.active_index:
                    locked_deform_groups.add(group.index)
                else:
                    deform_groups.add(group.index)

        lock_overrange = False
        for v in obj.data.vertices:
            if selected and not v.select:
                continue
            weight_total = 0
            locked_weight_total = 0
            for grp in v.groups:
                if grp.group in locked_deform_groups:
                    locked_weight_total += grp.weight
                elif grp.group in deform_groups:
                    weight_total += grp.weight

            if weight_total < self.threshold:
                continue

            factor = 1.0 - locked_weight_total
            if factor < 0.0:
                lock_overrange = True
                factor = 0
            for g in v.groups:
                if g.group in deform_groups:
                    g.weight = (g.weight / weight_total) * factor

        if lock_overrange:
            self.report({'WARNING'}, "Some locked weight total is over 1.0")
        return {'FINISHED'}


class VertexWeightNormalizeDeform(_Base, bpy.types.Operator):
    bl_idname = 'mesh.vertex_weight_normalize_deform'
    bl_label = "Normalize Deform Weight"

    @classmethod
    def poll(cls, context):
        return context.mode == 'PAINT_WEIGHT'
    def execute(self, context):
        return self._normalize(context, False)
    def invoke(self, context, event):
        return self.execute(context)

class VertexWeightNormalizeDeformSelected(_Base, bpy.types.Operator):
    bl_idname = 'mesh.vertex_weight_normalize_deform_selected'
    bl_label = "Normalize Deform Weight Selected"

    @classmethod
    def poll(cls, context):
        me = context.weight_paint_object.data
        return context.mode == 'PAINT_WEIGHT' and \
                (me.use_paint_mask or me.use_paint_mask_vertex)
    def execute(self, context):
        return self._normalize(context, True)
    def invoke(self, context, event):
        return self.execute(context)


def panel_draw(self, context):
    layout = self.layout
    layout.label("Normalize Deform:")
    row = layout.row(align=True)
    row.operator('mesh.vertex_weight_normalize_deform',
                 text="ALL")
    row.operator('mesh.vertex_weight_normalize_deform_selected',
                 text="Selected")


def register():
    bpy.utils.register_class(VertexWeightNormalizeDeform)
    bpy.utils.register_class(VertexWeightNormalizeDeformSelected)
    bpy.types.VIEW3D_PT_tools_weightpaint.append(panel_draw)

def unregister():
    bpy.utils.unregister_class(VertexWeightNormalizeDeform)
    bpy.utils.unregister_class(VertexWeightNormalizeDeformSelected)
    bpy.types.VIEW3D_PT_tools_weightpaint.remove(panel_draw)

if __name__ == '__main__':
    register()
