# Progress report on trace functionality 

## 1. Creation, definition and testing of the trace structure

Started by creating a ocaml structure capable of holding the trace contained in a JSON file.
This includes : position (xyz, yaw, pitch and roll), a region with its type (circle, square, etc ...) and radius, an element that has an ID a type and its position and region defined. Finally we have an event that has it's respective ID, time and element. A list of this events contitutes the trace.

After having created the structure I tested to see if the code correctly read and parsed the JSON and transformed it into our structure. The test consisted in reading a JSON file parsing it into our structure and printing it to verify it is correct.

## 2. Using the Trace to create expressions

### 2.1 Creating expressions for circle type elements

After having a working structure to hold the trace. I begun by creating a small file to test the creation for circle type elements. Using Z3 I defined an expression (for each element) based on the data in the trace. 
