# minecraft_stronghold

Locate Minecraft stronghold from Eye of Ender throws by least-square intersection of lines

Repo includes:

* stronghold.R
  * R script where coordinates and headings can be manually edited in the code
  * run in RStudio console
  * df_p will contain the [x,z] coordinates for the least-squares solution for the stronhold
  
* app.R (in /stronhold_triangulator/)
  * R shiny app for interactive calculations
  * can open app.R in RStudio and then Run App for interactive use
  * enter rows of X coordinate, Z coordinate, heading and click Submit button
  * will plot the rays, least-sqaures solution, and text annotation of the solution coordinates

