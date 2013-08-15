
bl_info = {
    "name": "Set smooth menu",
    "author": "Hidesato Ikeya",
    "version": (0, 1),
    "blender": (2, 67, 0),
    "location": "View3D > Header",
    "description": "Set smooth menu on View3D header",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "3D View"}

import bpy

def menu_draw(self, context):
    #mode_string = context.mode
    row = self.layout.row(align=True)
    row.operator("object.shade_smooth", text='S')
    row.operator("object.shade_flat", text='F')

    act = context.active_object
    if act:
        self.layout.row().prop(act, 'show_wire', text='W')

def register():
    bpy.types.VIEW3D_HT_header.append(menu_draw)

def unregister():
    bpy.types.VIEW3D_HT_header.remove(menu_draw)


if __name__ == '__main__':
    register()
