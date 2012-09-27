
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2012 Esteban Tovagliari.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#

import os
import subprocess
import time

import bpy

from .project_writer import ProjectWriter

def init( engine):
    pass

def free( engine):
    pass

def update_preview( engine, data, scene):
    pass

def render_preview( engine, scene):
    pass

def update_scene( engine, data, scene):
    writer = ProjectWriter( scene)
    engine.render_info = writer.write()

def render_scene( engine, scene):

    return

    DELAY = 0.1

    project_file = engine.render_info["project_file"]
    render_output = engine.render_info["render_output"]
    width = engine.render_info["width"]
    height = engine.render_info["height"]

    try:
        os.remove( render_output)
    except:
        pass

    cmd = "appleseed.cli"
    cdir = os.path.dirname( project_file)
    process = subprocess.Popen( cmd, cwd = cdir, stdout=subprocess.PIPE)

    # Wait for the file to be created
    while not os.path.exists( render_output):
        if engine.test_break():
            try:
                process.kill()
            except:
                pass
            break

        if process.poll() != None:
            engine.update_stats("", "Appleseed: Error")
            break

        time.sleep( DELAY)

    if os.path.exists(render_output):
        engine.update_stats("", "Appleseed: Rendering")

        prev_size = -1

        def update_image():
            result = engine.begin_result( 0, 0, width, height)
            lay = result.layers[0]
            # possible the image wont load early on.
            try:
                lay.load_from_file( render_output)
            except:
                pass

            engine.end_result( result)

        # Update while rendering
        while True:
            if process.poll() is not None:
                update_image()
                break

            # user exit
            if engine.test_break():
                try:
                    process.kill()
                except:
                    pass
                break

            # check if the file updated
            new_size = os.path.getsize( render_output)

            if new_size != prev_size:
                update_image()
                prev_size = new_size

            time.sleep( DELAY)

def update( engine, data, scene):
    if engine.is_preview:
        update_preview( engine, data, scene)
    else:
        update_scene( engine, data, scene)

def render( engine, scene):
    if engine.is_preview:
        render_preview( engine, scene)
    else:
        render_scene( engine, scene)
