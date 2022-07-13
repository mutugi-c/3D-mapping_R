# install.packages("devtools")
# devtools::install_github("tylermorganwall/rayshader")

# install.packages("magick") # package implements functionality for manipulating 
# high-dimensional arrays using efficient vectorised methods
# install.packages("av")


library(magick)
library(rayshader)
library(raster)
library(rayrender)
library(av)


# import downloaded raster
DEM <- raster::raster("54081_dem_1m.tif")

# check CRS
crs(DEM)

# plot DEM
plot(DEM,col = terrain.colors(n=50))

# Create the polygon to clip DEM to the area of interest
polygon <- as(extent(682000, 682600, 6400750, 6401350), 'SpatialPolygons')

# assign projection to polygon
crs(polygon) <- crs(DEM)
crs(polygon)

# crop DEM using polygon
DEM_crop <- crop(DEM, polygon)

# plot cropped DEM
plot(DEM_crop,col = terrain.colors(n=50))

# convert DEM from raster to matrix
DEM_matrix  <-  raster_to_matrix(DEM_crop)

# Plot to preview
DEM_matrix %>%
  sphere_shade(texture = "desert", sunangle = 45) %>% 
  plot_map()

DEM_matrix %>%
  sphere_shade(sunangle = 315, texture = "unicorn") %>%
  plot_map()

DEM_matrix %>%
  sphere_shade(texture=create_texture("#FFA900",
                                      "#FF7600",
                                      "#A2CDCD",
                                      "#CD113B",
                                      "black"),
               colorintensity = 5) %>%
  plot_map()


# Plot DEM in new window
DEM_matrix %>%
  sphere_shade(texture = "unicorn", 
               sunangle = 10) %>%
plot_3d(DEM_matrix, 
        baseshape = "hex", # Shape of the base: "rectangle","circle","hex"
        solidcolor = 'black', # Base color
        solidlinecolor = '#191A19', #Base edge line color
        background = '#1F1D36',# Color of the background
        zscale = 0.4, # ratio between the x and y spacing
        fov = 0, # field-of-view angle
        theta = 60, # Rotation around z-axis
        zoom = 0.7, # Zoom factor
        phi = 45, # Azimuth angle
        windowsize = c(500, 500)) 

Sys.sleep(0.2)
render_snapshot()


# Visualise with render_highquality() function

render_highquality(lightdirection = 100, #position of the light angle around the scene
                   lightaltitude = 45, #Angle above the horizon that the light is located.
                   lightintensity = 400, # Intensity of the light.
                   clamp_value = 10, 
                   title_text = "Suur-Munam?gi",
                   title_color = "white",
                   title_bar_color = "black", scene_elements = NULL,
                   camera_location = NULL, camera_lookat = NULL,
                   parallel = TRUE, width = 1000, height = 1000, 
                   samples = 200) # depending on your computer use from 200 to 2000
rgl::rgl.close() 


# load RGB orthophoto
image  <-  stack("54081.tif")

# plot orthophoto
plotRGB(image)

# crop orthophoto
image_crop <- raster::crop(image, polygon)

# plot cropped orthphoto
plotRGB(image_crop)

# Convert raster to the matrix
names(image_crop) <-  c("r","g","b")

image_crop_R <-  rayshader::raster_to_matrix(image_crop$r) # Red band
image_crop_G <-  rayshader::raster_to_matrix(image_crop$g) # Green band
image_crop_B <-  rayshader::raster_to_matrix(image_crop$b) # Blue band

image_crop_RGB <-  array(0,dim=c(nrow(image_crop_R),ncol(image_crop_R),3))

image_crop_RGB[,,1] <-  image_crop_R #Red layer
image_crop_RGB[,,2] <-  image_crop_G #Blue layer
image_crop_RGB[,,3] <-  image_crop_B #Green layer

# transpose the image_crop_RGB
image_crop_RGB <-  aperm(image_crop_RGB, c(2,1,3))

image_crop_RGB <-  scales::rescale(image_crop_RGB,to=c(0,1)) # rescale image

plot_map(image_crop_RGB)


# plot RGB image above the DEM
plot_3d(image_crop_RGB, # RGB image
        DEM_matrix, # DEM 
        windowsize = c(700,600),
        zscale = 0.6, 
        shadowdepth = -50,
        zoom=0.7, phi=45,theta=-45,fov=70, 
        background = "#D5D5D5", 
        shadowcolor = "#5e5e5e")

# add labels to the plot
render_label(DEM_matrix,x = 335, y = 325,
             altitude=30, zscale=0.6, 
             text = "Top of the peak",
             textcolor = "white",
             linecolor="black",
             dashed = TRUE,
             alpha = 0.9,
             textsize = 1.5)

render_label(DEM_matrix,x = 50, y = 190,
             altitude=60, zscale=0.6, 
             text = "Parking place,",
             textcolor = "white",
             linecolor="black",
             dashed = TRUE,
             alpha = 0.9,
             textsize = 1.5)

render_label(DEM_matrix,x = 50, y = 400,
             altitude=60, zscale=0.6, 
             text = "Cafe Suur Muna",
             textcolor = "white",
             linecolor="black",
             dashed = TRUE,
             alpha = 0.9,
             textsize = 1.5)

render_snapshot(title_text = "Suur-Munam?gi \nData: Land Board ",
                title_color = "lightgray", 
                title_bar_alpha = 1)

# create 360 degrees images for each degree of view
angles <- seq(0,360,length.out = 361)[-1] # one image for one degree (360 in total)

for(i in 1:360) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("Suur_Munamagi%i.png", i), 
                  title_text = "Suur Munam?gi \nData: Land Board",
                  title_color = "white", 
                  title_bar_alpha = 1)
}

rgl::rgl.close()

# create a video with the generated images
av::av_encode_video(sprintf("Suur_Munamagi%d.png",seq(1,360,by=1)), 
                    framerate = 30, output = "Suur_Munamagi.mp4")

