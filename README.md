# Calculate Laterals for Carlson

This program is **free** and **open source**. No licenses apply to this program.

This is a custom application for Carlson with IntelliCAD 2026. It calculates the station, offset, and finished grade elevation of all laterals at a given offset from a given centerline. It then parses and reorganizes all of the results in a human-readable format that can be quickly imported to many different industry-standard tools for stakeout.

## To load this application:
In Carlson, use the command ```APPLOAD``` to load custom applications. The "Load Application Files" window will open. Select "Add File". In the bottom-right corner of the file explorer pop-up, open the file extension drop-down menu and select "LISP Application (*.lsp)". Find and select the "LATERALS.lsp" script and click "Open". <br>
Now, in the "Load Application Files" window, highlight the "LATERALS.lsp" file you added and select "Load". The custom command ```LATERALS``` is now available.

## To use the LATERALS command:
Before running the command, isolate any layers containing linework for the laterals you want to calculate the location of.

Then, use the command ```LATERALS``` from your command line. First, select the centerline file used to reference for stations and offsets. The centerline will be automatically drawn on the currently selected layer. Then, enter the offset from the selected centerline for detection of laterals. The drawn centerline will eventually be automatically offset that distance in either direction for detecting intersections with lateral linework.

Now, draw a window to select all relevant lateral linework. It is fine to include the reference centerline; it will be automatically excluded by the application.

Then, navigate to and select a reference TIN surface for getting surface elevations.

Finally, navigate to the directory for the report to be written into and give it a file name. The directory the report was written into will be repeated into the command line.