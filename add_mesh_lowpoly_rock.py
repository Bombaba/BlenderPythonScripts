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
    "version": (0, 1, 9),
    "blender": (2, 68, 0),
    "location": "VIEW3D > ADD > Mesh",
    "description": "LowPoly Rock",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Add Mesh"}

import bpy
import bmesh
from mathutils import Matrix
from math import radians
from random import seed, uniform

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

def create_rock(context, subdiv, radius, size_ratio,
                noise_center, noise_size, noise_brightness,
                sharpness, displace_midlevel, displace_strength,
                voronoi_weights, simplicity, collapse_ratio):
    me = get_basemesh(context, subdiv, radius, size_ratio)
    rock = context.blend_data.objects.new(ROCK_NAME, me)
    rock.show_all_edges = True
    ix_dot = rock.name.rfind('.')
    if ix_dot != -1:
        number = rock.name[ix_dot:]
    else:
        number = ""
    context.scene.objects.link(rock)
    context.scene.objects.active = rock

    # Displacement
    noise_origin = \
        context.blend_data.objects.new(ORIGIN_NAME + number, None)
    noise_origin.location = noise_center
    noise_origin.location *= radius
    context.scene.objects.link(noise_origin)
    disp = rock.modifiers.new('displace', 'DISPLACE')
    disp.direction = 'NORMAL'
    disp.mid_level = displace_midlevel
    disp.strength = radius * displace_strength
    disp.texture_coords = 'OBJECT'
    disp.texture_coords_object  = noise_origin
    disp.texture = get_texture(
        context, TEXTURE_NAME + number, size=radius * noise_size,
        brightness=noise_brightness,
        contrast=sharpness, weights=voronoi_weights)

    # Collapse
    collapse = rock.modifiers.new('collapse', 'DECIMATE')
    collapse.decimate_type = 'COLLAPSE'
    collapse.ratio = collapse_ratio

    # Planer
    planer = rock.modifiers.new('planer', 'DECIMATE')
    planer.decimate_type = 'DISSOLVE'
    planer.angle_limit = simplicity * ANGLE_MAX
    planer.use_dissolve_boundaries = True

    return rock, noise_origin


class LowPolyRock(bpy.types.Operator):
    """LowPoly Rock"""
    bl_idname = "mesh.lowpoly_rock_add"
    bl_label = "LowPoly Rock"
    bl_options = {'REGISTER', 'UNDO', 'PRESET'}

    num_rock = bpy.props.IntProperty(
        name="Number", min=1, max=9, default=1,
        description="Number of rocks")
    size = bpy.props.FloatProperty(
        name="Size", min=.0, default=1.0, precision=3, step=0.01)
    size_ratio = bpy.props.FloatVectorProperty(
        name="Size Ratio", size=3, min=.0, default=(1., 1., 1.),
        subtype='TRANSLATION', step=0.1, precision=2, description="Size ratio")
    displace_midlevel = bpy.props.FloatProperty(
        name="Midlevel", min=.0, max=1.0, default=.5, precision=3, step=0.1)
    noise_center = bpy.props.FloatVectorProperty(
        name="Noise Center", size=3, step=0.1, subtype='TRANSLATION',
        description="Displacement noise texture origin")
    simplicity = bpy.props.FloatProperty(
        name="Simplicity", min=.0, max=1.0, default=0.25,
        precision=2, step=0.1, description="Reduce polygons")
    sharpness = bpy.props.FloatProperty(
        name="Sharpness", min=.0, max=2.0, default=.8, precision=3, step=0.1)
    edge_split = bpy.props.BoolProperty(
        name="Edge Split", default=True,
        description="Shade smooth and add edge split modifier")

    random_seed = bpy.props.IntProperty(
        name="Random Seed", min=-1, default=0,
        description="Randome seed (set -1 to use system clock)")
    size_min = bpy.props.FloatProperty(
        name="Size Min", min=-1.0, max=.0, default=-.3, precision=3, step=0.01)
    size_max = bpy.props.FloatProperty(
        name="Size Max", min=.0, default=.3, precision=3, step=0.01)
    size_ratio_min = bpy.props.FloatVectorProperty(
        name="Ratio Min", size=3, min=-1.0, max=.0, default=(-.2, -.2, -.2),
        precision=3, step=0.01)
    size_ratio_max = bpy.props.FloatVectorProperty(
        name="Ratio Max", size=3, min=.0, default=(.2, .2, .2),
        precision=3, step=0.01)

    keep_modifiers = bpy.props.BoolProperty(
        name="Keep Modifiers", default=False,
        description="Keep modifiers")
    advanced_menu = bpy.props.BoolProperty(
        name="Advanced Menu", default=False,
        description="Display advanced menu")
    voronoi_weights = bpy.props.FloatVectorProperty(
        name="Voronoi Weights", min=-1.0, max=1.0, size=3,
        default=(1.,.3,.0), step=0.1, description="Voronoi Weights")
    displace_strength = bpy.props.FloatProperty(
        name="Strength", min=.0, default=1.0, precision=3, step=0.1)
    noise_size = bpy.props.FloatProperty(
        name="Nsize", min=.0, default=1.0, precision=3, step=0.1)
    noise_brightness = bpy.props.FloatProperty(
        name="Nbright", min=.0, max=1.5, default=.8, precision=3, step=0.1)
    subdiv = bpy.props.IntProperty(
        name="Subdivision", min=1, max=7, default=5,
        description="Icosphere subdivision")
    collapse_ratio = bpy.props.FloatProperty(
        name="Collapse Ratio", min=.0, max=1.0, default=.06,
        precision=3, step=0.01,)

    @classmethod
    def poll(self, context):
        return context.mode == 'OBJECT'

    def draw(self, context):
        layout = self.layout

        basic = layout.box()
        basic.label("Basic Settings:")
        basic.prop(self, 'num_rock')
        col = basic.column(align=True)
        col.prop(self, 'size')
        col.label("Size Ratio:")
        col.prop(self, 'size_ratio', text="")
        col.prop(self, 'displace_midlevel')

        col = basic.column(align=True)
        col.label("Noise Center:")
        row = col.row()
        row.prop(self, 'noise_center', text="")

        basic.prop(self, 'simplicity')
        basic.prop(self, 'sharpness')
        basic.prop(self, 'edge_split')

        random = layout.box()
        random.label("Random Settings:")
        random.prop(self, 'random_seed')

        col = random.column(align=True)
        col.label('Size Range:')
        row = col.row(align=True)
        row.prop(self, 'size_min', text='min')
        row.prop(self, 'size_max', text='max')

        col = random.column(align=True)
        col.label('Size Ratio Range:')
        row = col.row()
        col = row.column(align=True)
        col.prop(self, 'size_ratio_min', text="")
        col = row.column(align=True)
        col.prop(self, 'size_ratio_max', text="")

        advanced = layout.box()
        advanced.prop(self, 'advanced_menu', text="Advanced Settings:")
        if self.advanced_menu:
            advanced.prop(self, 'keep_modifiers')
            advanced.prop(self, 'displace_strength')
            advanced.prop(self, 'voronoi_weights')
            advanced.prop(self, 'noise_size')
            advanced.prop(self, 'noise_brightness')
            advanced.prop(self, 'subdiv')
            advanced.prop(self, 'collapse_ratio')

    def execute(self, context):
        bpy.ops.object.select_all(action='DESELECT')

        radius = self.size
        size_ratio = self.size_ratio.copy()
        noise_center = self.noise_center.copy()

        random_seed = self.random_seed
        if random_seed == -1:
            random_seed = None
        seed(random_seed)

        location = context.scene.cursor_location.copy()
        rocks = [None] * self.num_rock
        for n in range(self.num_rock):
            rock, noise_origin = create_rock(
                context, self.subdiv, radius, size_ratio,
                noise_center, self.noise_size, self.noise_brightness,
                self.sharpness, self.displace_midlevel, self.displace_strength,
                self.voronoi_weights, self.simplicity, self.collapse_ratio)
            rocks[n] = rock

            if self.keep_modifiers:
                rock.location = location
                noise_origin.location += location
            else:
                context.scene.update()
                me_orig = rock.data
                tex = rock.modifiers['displace'].texture
                rock.data = rock.to_mesh(context.scene, True, 'PREVIEW')
                context.blend_data.meshes.remove(me_orig)
                rock.modifiers.clear()
                rock.location = location
                context.scene.objects.unlink(noise_origin)
                context.blend_data.objects.remove(noise_origin)
                context.blend_data.textures.remove(tex)

            if self.edge_split:
                rock.select = True
                bpy.ops.object.shade_smooth()
                split = rock.modifiers.new('split', 'EDGE_SPLIT')
                split.use_edge_angle = True
                split.use_edge_sharp = False
                split.split_angle = .0
                rock.select = False

            rock.data.name = rock.name

            radius = self.size * (1.0 + uniform(self.size_min, self.size_max))
            for i in range(3):
                noise_center[i] = self.noise_center[i] + uniform(-1000, 1000)
                size_ratio[i] = self.size_ratio[i] * \
                    (1.0 + uniform(self.size_ratio_min[i], self.size_ratio_max[i]))
            if n % 2 == 0:
                location.x = context.scene.cursor_location.x \
                    + self.size * 1.6 * (n // 2 + 1)
            else:
                location.x = context.scene.cursor_location.x \
                    - self.size * 1.6 * (n // 2 + 1)

        for rock in rocks:
            rock.select = True

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
