#! /bin/csh -f

set COMPILER1 = intel
set COMPILER2 = intelmic

echo "COMPILER1 = $COMPILER1"
echo "COMPILER2 = $COMPILER2"

set word = 'mic'
if ($COMPILER1:q =~ *$word:q*) echo "COMPILER1 has mic in it"
if ($COMPILER2:q =~ *$word:q*) echo "COMPILER2 has mic in it"
