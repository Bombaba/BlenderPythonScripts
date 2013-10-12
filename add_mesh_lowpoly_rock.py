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
    "name": "LowPoly Rock",
    "author": "Hidesato Ikeya",
    "version": (0, 1, 5),
    "blender": (2, 68, 0),
    "location": "VIEW3D > ADD > Mesh",
    "description": "LowPoly Rock",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Mesh"}

import bpy
import bmesh
from mathutils import Matrix
from math import radians

ROCK_NAME = "LowPolyRock"
ORIGIN_NAME = ROCK_NAME + "DisplaceOrigin"
TEXTURE_NAME = ROCK_NAME + "Texture"
ANGLE_MAX = radians(90)


def get_basemesh(context, subdiv=5, radius=1.0, ratio=(1., 1., 1.)):
    me = context.blend_data.meshes.new('tempmeshname')
    bm = bmesh.new()
    bm.from_mesh(me)
    mat = Matrix()
    mat[0][0], mat[1][1], mat[2][2] = ratio
    bmesh.ops.create_icosphere(
        bm, subdivisions=subdiv, diameter=radius, matrix=mat)
    bm.to_mesh(me)
    return me

def get_texture(context, name, size=1.0, brightness=.8, contrast=.8,
                weights=(1.0, .3, .0)):
    tex = context.blend_data.textures.new(name, 'VORONOI')
    tex.noise_scale = size
    tex.intensity = brightness
    tex.contrast = contrast
    tex.weight_1, tex.weight_2, tex.weight_3 = weights
    tex.use_color_ramp = True

    ramp = tex.color_ramp
    ramp.interpolation = 'B_SPLINE'
    ramp.elements[0].color = (.0, .0, .0, 1.0)
    ramp.elements[0].position = .5
    return tex

class LowPolyRock(bpy.types.Operator):
    """LowPoly Rock"""
    bl_idname = "mesh.lowpoly_rock_add"
    bl_label = "LowPoly Rock"
    bl_options = {'REGISTER', 'UNDO', 'PRESET'}

    keep_modifiers = bpy.props.BoolProperty(
        name="Keep Modifiers", default=False,
        description="Keep modifiers")
    triangulate = bpy.props.BoolProperty(
        name="Triangulate", default=False,
        description="Triangulate and Shade smooth")
    size = bpy.props.FloatProperty(
        name="Size", min=.0, default=1.0, precision=3, step=0.01)
    displace_center = bpy.props.FloatVectorProperty(
        name="Displace Center", step=0.1, subtype='TRANSLATION', size=3,
        description="Displacement texture origin")
    simplicity = bpy.props.FloatProperty(
        name="Simplicity", min=.0, max=1.0, default=0.25,
        precision=2, step=0.1, description="Reduce polygons")
    sharpness = bpy.props.FloatProperty(
        name="Sharpness", min=.0, max=2.0, default=.8, precision=3, step=0.1)
    advanced_menu = bpy.props.BoolProperty(
        name="Advanced", default=False,
        description="Display advanced menu")
    voronoi_weights = bpy.props.FloatVectorProperty(
        name="Voronoi Weights", min=-1.0, max=1.0, size=3,
        default=(1.,.3,.0), step=0.1, description="Voronoi Weights")
    ico_subdiv = bpy.props.IntProperty(
        name="Ico Subdivision", min=1, max=7, default=5, options={'HIDDEN'},
        description="Icosphere subdivision")
    collapse_ratio = bpy.props.FloatProperty(
        name="Collapse Ratio", min=.0, max=1.0, default=.06, precision=3, step=0.01,
        options={'HIDDEN'})
    size_ratio = bpy.props.FloatVectorProperty(
        name="Size Ratio", size=3, min=.0, default=(1., 1., 1.),
        subtype='TRANSLATION', step=0.1, precision=2, description="Size ratio")

    @classmethod
    def poll(self, context):
        return context.mode == 'OBJECT'

    def draw(self, context):
        layout = self.layout
        box = layout.box()
        box.prop(self, 'keep_modifiers')
        box.prop(self, 'triangulate')
        box.prop(self, 'size')
        box.prop(self, 'simplicity')
        box.prop(self, 'sharpness')
        box.prop(self, 'displace_center')
        layout.prop(self, 'advanced_menu')
        if self.advanced_menu:
            box = layout.box()
            box.prop(self, 'size_ratio')
            box.prop(self, 'voronoi_weights')
            box.prop(self, 'ico_subdiv')
            box.prop(self, 'collapse_ratio')

    def execute(self, context):
        bpy.ops.object.select_all(action='DESELECT')
        me = get_basemesh(context, self.ico_subdiv, self.size, self.size_ratio)
        rock = context.blend_data.objects.new(ROCK_NAME, me)
        rock.show_all_edges = True
        ix_dot = rock.name.rfind('.')
        if ix_dot != -1:
            number = rock.name[ix_dot:]
        else:
            number = ""
        context.scene.objects.link(rock)
        context.scene.objects.active = rock
        rock.select = True

        # Displacement
        displace_origin = \
            context.blend_data.objects.new(ORIGIN_NAME + number, None)
        displace_origin.location = self.displace_center
        displace_origin.location *= self.size
        context.scene.objects.link(displace_origin)
        disp = rock.modifiers.new('displace', 'DISPLACE')
        disp.direction = 'NORMAL'
        disp.mid_level = 0.5
        disp.strength = self.size
        disp.texture_coords = 'OBJECT'
        disp.texture_coords_object  = displace_origin
        tex = get_texture(
            context, TEXTURE_NAME + number, size=self.size,
            contrast=self.sharpness, weights=self.voronoi_weights)
        disp.texture = tex

        # Collapse
        collapse = rock.modifiers.new('collapse', 'DECIMATE')
        collapse.decimate_type = 'COLLAPSE'
        collapse.ratio = self.collapse_ratio

        # Planer
        planer = rock.modifiers.new('planer', 'DECIMATE')
        planer.decimate_type = 'DISSOLVE'
        planer.angle_limit = self.simplicity * ANGLE_MAX
        planer.use_dissolve_boundaries = True

        if self.keep_modifiers:
            rock.location = context.scene.cursor_location
            displace_origin.location += rock.location
        else:
            context.scene.update()
            rock.data = rock.to_mesh(context.scene, True, 'PREVIEW')
            context.blend_data.meshes.remove(me)
            rock.modifiers.clear()
            rock.location = context.scene.cursor_location
            context.scene.objects.unlink(displace_origin)
            context.blend_data.objects.remove(displace_origin)
            context.blend_data.textures.remove(tex)

        if self.triangulate:
            bpy.ops.object.shade_smooth()
            # Split
            split = rock.modifiers.new('split', 'EDGE_SPLIT')
            split.use_edge_angle = True
            split.use_edge_sharp = False
            split.split_angle = .0

            # Triangulate
            triangulate = rock.modifiers.new('triangulate', 'TRIANGULATE')
            triangulate.use_beauty = True

        rock.data.name = rock.name
        return {'FINISHED'}

    def invoke(self, context, event):
        return self.execute(context)


def draw_item(self, context):
    self.layout.operator_context = 'INVOKE_DEFAULT'
    self.layout.operator(LowPolyRock.bl_idname, text="LowPoly Rock", icon="PLUGIN")

def register():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_mesh_add.append(draw_item)

def unregister():
    bpy.utils.unregister_module(__name__)
    bpy.types.INFO_MT_mesh_add.remove(draw_item)

if __name__ == '__main__':
    register()
