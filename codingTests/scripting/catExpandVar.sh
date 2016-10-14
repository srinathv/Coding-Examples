#!/bin/bash

#does cat bash expand variables:


foo="hello"

echo $foo

echo "$foo"

cat>foo.txt<<EOF
#can I have a comment in file
$foo
"$foo"
${foo}

EOF


#bas expands variables before writing issuing cat
