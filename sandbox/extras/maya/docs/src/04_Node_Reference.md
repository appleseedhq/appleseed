Mayaseed docs
=============


Node reference
--------------

This section contains information on the nodes that make up the Mayaseed plugin and their atributes.


###ms_renderSettings node###

The ms\_renderSettings (render settings) node is the biggest node in the Mayaseed plugin and contains most of the attributes that you will use to control your export. 

It is also one of the places where you can start your export from, at the top of the render settings node's attribute editor there is a blue button marked **Export** use this button to start your export.

The following section is a list of attributes in the render settings node and a description of their functions.

>Note: The ms_renderSettings node's attributes are organised in a way that mirrors the internal file structure, so if an attribute seems like its in a strange place there is a good reason. By using Mayaseed you are also learning about appleseed at the same time.


####Export settings section####

#####Output Directory#####

This is where you tell Mayaseed where to export the appleseed scene file and other relevant files to.

#####Output File#####

This is where you choose the name of your output file, the `#` character will be replaced with the frame number padded to 5 characters. So `my_scene.#.appleseed` will export as my_scene.00001.appleseed`.

#####Convert Shading Nodes To Textures#####

Use this checkbox to convert maya shading networks to textures on export. If a color attribute has a shading network attached Mayaseed will check to see the shading network is a texture node, if not with this option checked Mayaseed will bake the shading network to an exr image. 

#####Overwrite Existing EXRs#####

If this checkbox is checked then Maya will convert and overwrite texture files with every export.

#####Export Motion Blur#####

This attribute is only a placeholder at this point.

#####Shutter Open Time#####

This attribute is only a placeholder at this point.

#####Shutter Close Time#####

This attribute is only a placeholder at this point.

#####Export Animation#####

This attribute is only a placeholder at this point.

#####Animation Start Frame#####

This attribute is only a placeholder at this point.

#####Animation End Frame#####

This attribute is only a placeholder at this point.


####Environment Settings section####

#####Environment#####

A Maya scene can contain many environment nodes, here you can select which environment node to use in your export and also create new ones.


####Camera Settings Section####

#####Export All Cameras#####

Although appleseed can only use one camera at a time it is possible to have more than one included in the scene file, use this checkbox to export all the maya cameras. 

#####Export All Cameras As Thinlens#####

Appleseed has two types of cameras: **Pinhole** and **Thinlens**, the main difference being that a Thinlens camera can simulate depth of field and the Pinhole cannot. By default Mayaseed will export cameras with depth of field turned off as Pinhole and with depth of field turned on as Thinlens. Use this option to force Mayaseed to export all cameras as Thinlens.


####Assembly Settings Section####

> Note: Appleseed uses the concept of Assembles to divide up the scene into smaller components. 

#####Interpret Sets As Assemblies#####

With this option checked Mayaseed will export any sets containing geometry as an assembly.

#####Double Sided Shading#####

Use this option to turn on double shading in your appleseed scene file. Double shading causes geometry to be rendered on both sides of the shading normal. This can help reduce rendering artifacts especially on low poly geometry with smoothed normals.


####Output Settings Section####

#####Camera#####

Use this attribute to select the camera you would like to export.

> Note: If you do not select a camera the export will fail.

#####Resolution Width#####

This attribute sets the width of the framebuffer.

#####Resolution Height#####

This attribute sets the height of the framebuffer.

#####Color Space#####

This sets the color space that appleseed will use, the default is **sRGB**.


####Configuration Settings Section####

Appleseed configurations contain information on the rendering method and quality setting of a render. Appleseed can have an arbitrarily high number of these render settings but must contain at least two, an **Interactive Config** and a **Final config**. These configurations control the quality of appleseed's default interactive render and final render. Without checking **Export Custom Interactive COnfig** or **Export Custom Final Config** Mayaseed will export a default settings. 

Both Interactive Config and Final COnfig have the following attributes.

#####Lighting Engine#####

This drop-down menu currently has two options: **Path Tracing** and **Distributed Ray Tracing**. Path tracing is more physically accurate and will compute color bleeding and caustics whereas distributed ray tracing is slower but less accurate.

#####Min Samples#####

Use this attribute to set the minimum render samples.

#####Max Samples#####

Use this attribute to set the maximum render samples.

#####Max Ray Depth#####

Max Ray depth controls the maximum number of bounces that a ray can go through. Higher numbers are more accurate but slower to render.

#####Light Samples#####

This attribute controls the number of samples per light.



###ms_environment node###

The ms\_environment (environment) node is used to control the environment settings for your export. Right now it could easily be implemented in the render settings node but when environment transformations are implemented in appleseed it will be useful to have the environment node as a separate entity with its own transform node.

Below is a list of the node's attributes and their functions.

#####Model#####

This drop down menu contains the different types of environment models that appleseed provides(below).

+ Constant Environment
+ Gradient Environment
+ Latitude Longitude Map
+ Mirrorball Map

#####Constant Exitance#####

Use this attribute if you have selected **Constant Environment** as your model. 

> Note: only modify the color of this attribute, connecting a shading network will have no effect.

#####Gradient Horizon Exitance#####

Use this attribute to set the horizon color of the evironment if you have selected **Gradient Environment** as your model. 

> Note: only modify the color of this attribute, connecting a shading network will have no effect.

#####Gradient Zenith Exitance#####

Use this attribute to set the zenith (highest point) color of the environment if you have selected **Gradient Environment** as your model. 

> Note: only modify the color of this attribute, connecting a shading network will have no effect

#####Latitude Longitude Exitance#####

Attach a texture node to this attribute if you have selected **Latitude Longitude Map** as your environment model. The image should be in latitude longitude format for correct results.

#####Mirror Ball Exitance#####

Attach a texture node to this attribute if you have selected **Mirror Ball Map** as your environment model.


###Maya shaders###

Mayaseed will automatically translate maya shaders as best as possible to appleseed shaders but this often isn't perfect, when automatic shader translation isn't enough you can add a **Custom Shader Translation**. With an object or shader selected you can Choose **Mayaseed -> Add Custom Shader Translation**to add some Mayaseed specific attributes to your shader.

> Note: Custom shader translation is an experimental feature and is still limited in functionality.

Once you have added a custom shader translation to your shader you will now find three new attributes in the **Extra Attributes** section of the Shaders attribute editor, the following is a list of the attributes and their functions.

####Mayaseed BSDF####

The **Mayaseed BSDF** drop-down menu lets you choose the BSDF model that Mayaseed will translate your shader to on export. BSDF stands for Bidirectional scattering distribution function and controls how light is reflected off the surface of an object. Although appleseed contains more BSDF options Mayaseed only supports the following BSDF's.

+ Labertian
+ Ashikhmin-Shirley
+ Kleeman
+ Specular_BSDF
+ \<None>

####Mayaseed EDF####

The **Mayaseed EDF** drop-down menu lets you choose the EDF model that Mayaseed will translate your shader to on export. EDF stands for **emittance distribution function** and controls how light is emitted from a surface. Mayaseed has the following 2 options available:

+ \<None\>
+ Diffuse

####Mayaseed Surface Shader####

The **Mayaseed Surface Shader** controls how an object is rendered when it is directly visible to the camera. By default this is set to **Physical** which means that the object will be rendered according to the BSDF, this is usually the most physically accurate setting. Mayaseed has the following options available:

+ Physical
+ Constant
+ \<None>


Menu reference
--------------
The Mayaseed menu will appear when the plugin is correctly installed, if you cant see it make sure the plugin installed correctly and is enabled.

The following section contains all the items in the Mayaseed menu and their uses.

###Add Render Settings Node###

Use this to create a new instance of the ms\_renderSettings node.

###Select Render Settings Node###

Use this to select any ms\_renderSettings nodes that exist in the maya scene.

###Add Environment Node###

Use this to create a new instance of the ms\_environment node.

###Select Environment Node###

Use this to select any ms\_environment nodes that exist in the maya scene.

###Add Custom Shader Translation###

With a shader or an object selected use this to add a custom shader translation to a shader, more in the **Maya Shaders** section of the 

###Remove Custom Shader Translation###

Use this to remove a custom shader translation from a Maya shader

###About ###

Show information about Mayaseed.
