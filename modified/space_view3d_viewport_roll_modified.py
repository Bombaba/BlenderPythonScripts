import bpy
from mathutils import *
from math import *


bl_info = {
    "name": "Viewport Roll",
    "author": "Jace Priester",
    "version": (1, 0, 1),
    "blender": (2, 6, 3),
    "api": 45996,
    "location": "View 3D -> Ctrl-Shift-Mousewheel and Alt-Middle-Drag",
    "description": "Adds roll capability to the 3d viewport",
    "warning": "",
    "wiki_url": "http://wiki.blender.org/index.php/Extensions:2.5/Py/"\
        "Scripts/My_Script",
    "tracker_url": "http://projects.blender.org/tracker/index.php?"\
        "func=detail&aid=<number>",
    "category": "3D View"}

CAMNORMAL = Vector((0, 0, -1))

class RollViewportCW(bpy.types.Operator):
    '''Roll the viewport clockwise.'''
    bl_idname = "view3d.roll_viewport_cw"
    bl_label = "Roll the viewport clockwise"
    bl_options = {'GRAB_POINTER'}

    def execute(self, context):
        v3d = context.space_data
        rv3d = v3d.region_3d
        if rv3d.view_perspective == 'CAMERA':
            rv3d.view_perspective = 'PERSP'
        #camNormal=Vector((0,0,-1))
        angle_diff=bpy.context.user_preferences.view.rotation_angle*pi/180.0
        q=Quaternion(CAMNORMAL,-angle_diff)
        rv3d.view_rotation*=q
        
        return {'FINISHED'}

class RollViewportCCW(bpy.types.Operator):
    '''Roll the viewport counterclockwise.'''
    bl_idname = "view3d.roll_viewport_ccw"
    bl_label = "Roll the viewport counterclockwise"
    bl_options = {'GRAB_POINTER'}

    def execute(self, context):
        v3d = context.space_data
        rv3d = v3d.region_3d
        if rv3d.view_perspective == 'CAMERA':
            rv3d.view_perspective = 'PERSP'
        #camNormal=Vector((0,0,-1))
        angle_diff=bpy.context.user_preferences.view.rotation_angle*pi/180.0
        q=Quaternion(CAMNORMAL,angle_diff)
        rv3d.view_rotation*=q
        
        return {'FINISHED'}
    
class RollViewport(bpy.types.Operator):
    '''Roll the viewport.'''
    bl_idname = "view3d.roll_viewport"
    bl_label = "Roll the viewport"
    bl_options = {'GRAB_POINTER'}

    initial_angle=0
    angle_now=0
    initial_rotation=Vector((0,0,0))

    def execute(self, context):
        v3d = context.space_data
        rv3d = v3d.region_3d

        #camNormal=Vector((0,0,-1))
        angle_diff=self.angle_now-self.initial_angle
        q=Quaternion(CAMNORMAL,angle_diff)
        rv3d.view_rotation=self.initial_rotation*q
        
        return {'FINISHED'}

    def modal(self, context, event):
        v3d = context.space_data
        rv3d = v3d.region_3d

        if event.type == 'MOUSEMOVE':
            mouseloc=Vector((event.mouse_region_x,event.mouse_region_y))
            mouseloc_centered=mouseloc-self.center
            self.angle_now=atan2(mouseloc_centered.y,mouseloc_centered.x)
            self.execute(context)
            #context.area.header_text_set("Offset %.4f %.4f %.4f" % tuple(self.offset))

        elif event.type in {'LEFTMOUSE', 'MIDDLEMOUSE'}:
            context.area.header_text_set()
            return {'FINISHED'}

        elif event.type in {'RIGHTMOUSE', 'ESC'}:
            rv3d.view_rotation = self.initial_rotation
            context.area.header_text_set()
            return {'CANCELLED'}

        return {'RUNNING_MODAL'}

    def invoke(self, context, event):

        if context.space_data.type == 'VIEW_3D':
            v3d = context.space_data
            rv3d = v3d.region_3d

            context.window_manager.modal_handler_add(self)

            if rv3d.view_perspective == 'CAMERA':
                rv3d.view_perspective = 'PERSP'

            
            self.windowsize=Vector((context.region.width,context.region.height))
            self.center=self.windowsize/2
            mouseloc=Vector((event.mouse_region_x,event.mouse_region_y))
            mouseloc_centered=mouseloc-self.center
            self.initial_rotation=rv3d.view_rotation.copy()
            self.initial_angle=atan2(mouseloc_centered.y,mouseloc_centered.x)
            self.angle_now=self.initial_angle

            return {'RUNNING_MODAL'}
        else:
            self.report({'WARNING'}, "Active space must be a View3d")
            return {'CANCELLED'}


def register():
    bpy.utils.register_class(RollViewport)
    bpy.utils.register_class(RollViewportCW)
    bpy.utils.register_class(RollViewportCCW)
    
    wm = bpy.context.window_manager
    km = wm.keyconfigs.addon.keymaps.new(name="3D View", space_type='VIEW_3D')
    kmi = km.keymap_items.new('view3d.roll_viewport', 'MIDDLEMOUSE', 'PRESS', shift=False, ctrl=False, alt=True)
    kmi = km.keymap_items.new('view3d.roll_viewport_ccw', 'WHEELUPMOUSE', 'PRESS', shift=True, ctrl=True, alt=False)
    kmi = km.keymap_items.new('view3d.roll_viewport_cw', 'WHEELDOWNMOUSE', 'PRESS', shift=True, ctrl=True, alt=False)
    kmi = km.keymap_items.new('view3d.roll_viewport_ccw', 'NUMPAD_4', 'PRESS', shift=False, ctrl=False, alt=True)
    kmi = km.keymap_items.new('view3d.roll_viewport_cw', 'NUMPAD_6', 'PRESS', shift=False, ctrl=False, alt=True)


def unregister():
    bpy.utils.unregister_class(RollViewport)
    bpy.utils.unregister_class(RollViewportCW)
    bpy.utils.unregister_class(RollViewportCCW)
    
    wm = bpy.context.window_manager
    km = wm.keyconfigs.addon.keymaps['3D View']
    for kmi in km.keymap_items:
        if kmi.idname == 'view3d.roll_viewport':
            km.keymap_items.remove(kmi)
        elif kmi.idname == 'view3d.roll_viewport_ccw':
            km.keymap_items.remove(kmi)
        elif kmi.idname == 'view3d.roll_viewport_cw':
            km.keymap_items.remove(kmi)


if __name__ == "__main__":
    register()
