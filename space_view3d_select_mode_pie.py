bl_info = {
    "name" : "Select Mode Pie Menu",
    "author" : "Stan Pancakes",
    "version" : (0, 1, 0),
    "blender" : (2, 72, 0),
    "description" : "Custom Pie Menus",
    "category" : "3D View",}

import bpy

class SelectModeCombined(bpy.types.Operator):
    bl_label = "Combined Select Mode"
    bl_idname = "edit.mesh_select_mode_combined"
    bl_options = {'INTERNAL'}    
    
    mode = bpy.props.BoolVectorProperty(size=3)
          
    def execute(self, context):
        context.tool_settings.mesh_select_mode[:] = self.mode
        return {'FINISHED'}

class MeshSelectMode(bpy.types.Menu):
    bl_label = "Select Mode"
    bl_idname = "VIEW3D_MT_edit_mesh_select_mode_pie"
    
    def draw(self, context):
        layout = self.layout
        pie = layout.menu_pie()
        
        # left
        pie.operator("edit.mesh_select_mode_combined", text='Vertex', icon='VERTEXSEL').mode=(True, False, False)
        # right
        pie.operator("edit.mesh_select_mode_combined", text='Face', icon='FACESEL').mode=(False, False, True)
        # bottom
        pie.operator("edit.mesh_select_mode_combined", text='Vertex & Edge & Face', icon='UV_FACESEL').mode=(True, True, True)
        # top
        pie.operator("edit.mesh_select_mode_combined", text='Edge', icon='EDGESEL').mode=(False, True, False)
        # topleft
        pie.operator("edit.mesh_select_mode_combined", text='Vertex & Edge', icon='UV_EDGESEL').mode=(True, True, False)
        # topright
        pie.operator("edit.mesh_select_mode_combined", text='Edge & Face', icon='SPACE2').mode=(False, True, True)
        # bottomleft
        pie.prop(context.space_data, "use_occlude_geometry", text='Limit to Visible')
        # bottomright
        pie.operator("edit.mesh_select_mode_combined", text='Vertex & Face', icon='LOOPSEL').mode=(True, False, True)
        
def register():   
    bpy.utils.register_class(SelectModeCombined)   
    bpy.utils.register_class(MeshSelectMode)

def unregister():
    bpy.utils.unregister_class(MeshSelectMode)
    bpy.utils.unregister_class(SelectModeCombined)
