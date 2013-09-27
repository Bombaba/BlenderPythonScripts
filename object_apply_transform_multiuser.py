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
        "name": "Apply Transform MultiUser",
        "author": "Hidesato Ikeya",
        "version": (1, 2),
        "blender": (2, 68, 0),
        "location": "View3D > CTRL-A > Multiuser",
        "description": "Apply transform to multi user objects",
        "warning": "",
        "wiki_url": "",
        "tracker_url": "",
        "category": "Object"}

import bpy
from mathutils import *

class ApplyTransformMultiUser(bpy.types.Operator):
    bl_idname = "object.apply_transform_multiuser"
    bl_label = "Apply Transform MultiUser"
    bl_options = {'REGISTER', 'UNDO'}

    only_selected = bpy.props.BoolProperty(
        name="Only selected", default=False,
        description="Only selected objects sharing same data is affected")

    keep_visual = bpy.props.BoolProperty(
        name="Keep visual", default=False,
        description="Tries to keep visual proportion as much as possible")

    remove_original = bpy.props.BoolProperty(
        name="Remove Original Data", default=True,
        description="Remove original data if it is used by no user.")
    
    location = bpy.props.BoolProperty(name="Location", default=False)
    rotation = bpy.props.BoolProperty(name="Rotation", default=False)
    scale = bpy.props.BoolProperty(name="Scale", default=False)

    def draw(self, context):
        layout = self.layout

        layout.prop(self, 'only_selected')
        layout.prop(self, 'keep_visual')
        layout.prop(self, 'remove_original')

        layout.separator()

        layout.prop(self, 'location')
        layout.prop(self, 'rotation')
        layout.prop(self, 'scale')

    @classmethod
    def poll(self, context):
        obtype = context.active_object.type
        return context.mode == 'OBJECT' and obtype != 'EMPTY'

    def execute(self, context):
        if not context.active_object:
            self.report({'ERROR'}, "No active object")
            return {'CANCELLED'}
        active = context.active_object
        selected_objects = set(context.selected_objects)

        if self.keep_visual:
            scale_factor = Vector(1.0 / s for s in active.scale)
            m = active.matrix_basis.copy()
            m.invert()
            location_factor = m.to_translation()
            rot_factor = m.to_quaternion()

        bpy.ops.object.select_all(action='DESELECT')
        context.scene.objects.active = active
        bpy.ops.object.select_linked(extend=False, type='OBDATA')
        linked_objects = set(ob for ob in context.selected_objects)
        linked_objects.remove(active)
        if self.only_selected:
            linked_objects &= selected_objects
        bpy.ops.object.select_all(action='DESELECT')

        orig_data = active.data
        new_data = orig_data.copy()
        active.data = new_data
        active.select = True
        bpy.ops.object.transform_apply(
            location=self.location, rotation=self.rotation, scale=self.scale)

        for ob in linked_objects:
            ob.data = new_data
            if self.keep_visual:
                if self.location:
                    ob.location += ob.matrix_basis.to_3x3() * location_factor
                if self.rotation:
                    rot_mode_orig = ob.rotation_mode
                    ob.rotation_mode = 'QUATERNION'
                    ob.rotation_quaternion *= rot_factor
                    ob.rotation_mode = rot_mode_orig
                if self.scale:
                    ob.scale[0] *= scale_factor[0]
                    ob.scale[1] *= scale_factor[1]
                    ob.scale[2] *= scale_factor[2]
            ob.select = True

        if self.remove_original and orig_data.users == 0:
            context.blend_data.meshes.remove(orig_data)
        
        return {'FINISHED'}
    def invoke(self, context, event):
        return self.execute(context)

class ApplyTransformMultiUserMenu(bpy.types.Menu):
    bl_label = "MultiUser"
    bl_idname = "VIEW3D_MT_object_apply_transform_multiuser"

    def draw(self, context):
        l = self.layout.operator(
            "object.apply_transform_multiuser",
            text="Location")
        l.location = True
        l.scale = False
        l.rotation = False

        r = self.layout.operator(
            "object.apply_transform_multiuser",
            text="Rotation")
        r.location = False
        r.scale = False
        r.rotation = True

        s = self.layout.operator(
            "object.apply_transform_multiuser",
            text="Scale")
        s.location = False
        s.scale = True
        s.rotation = False

def draw_item(self, context):
    self.layout.menu(ApplyTransformMultiUserMenu.bl_idname)

def register():
    bpy.utils.register_class(ApplyTransformMultiUser)
    bpy.utils.register_class(ApplyTransformMultiUserMenu)
    bpy.types.VIEW3D_MT_object_apply.append(draw_item)

def unregister():
    bpy.utils.unregister_class(ApplyTransformMultiUser)
    bpy.utils.unregister_class(ApplyTransformMultiUserMenu)
    bpy.types.VIEW3D_MT_object_apply.remove(draw_item)

if __name__ == '__main__':
    register()
