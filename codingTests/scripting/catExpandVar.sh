#!/bin/bash

#does cat bash expand variables:


foo="hello"

echo $foo

echo "$foo"

cat>foo.txt<<EOF

$foo
"$foo"
${foo}

EOF


#bas expands variables before writing issuing cat
