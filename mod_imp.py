MODULE_NAME = "io_curve_gcode"

import sys
import os
import bpy
import imp

mod = sys.modules.get(MODULE_NAME, None)
if mod:
    mod.unregister()
    imp.reload(mod)
else:
    blend_dir = os.path.dirname(bpy.data.filepath)
    if blend_dir not in sys.path:
        sys.path.append(blend_dir)
    mod = __import__(MODULE_NAME)
mod.register()
