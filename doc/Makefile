
# required doxygen and perl 5.22.1 with packages:
#  - File::Spec
#  - GraphViz2
#  - Hash::FieldHash
#  - lib::abs
#  - Moose
#  - namespace::autoclean
#  - XML::Rabbit

all:
	doxygen config
	perl5.22.1  doxygraph/doxygraph xml/index.xml rtemlib_class_diagram.dot
	doxygen config
