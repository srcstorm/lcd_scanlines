# LCD Scanlines

Dynamic scanline generation for LCD


### Introduction
LCD Scanlines is an image upscaler that takes advantage of scanlines to increase resolution of images.

It demonstrates a dynamic scanline generation algorithm where brightness of each black line pixel is calculated by summing the illumination coming from adjacent active pixels. The resulting image is suitable for viewing on most flat-panel displays and high-resolution projectors.


### The Algorithm
The ones who are only willing to see how algorithm works need to check **dynscanlines.pas** file. Look for **TDynScanlines.DSImageEffect** method.


### License
Although this application is licensed under the GPL 2.0+, the algorithm itself has no restrictions. It can be used in proprietary products, and any enhancements made to the algorithm can remain private. When it is used in a project, an attribution will be appreciated.
