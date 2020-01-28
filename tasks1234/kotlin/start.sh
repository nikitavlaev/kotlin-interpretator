#!/bin/bash
read -p "Write 'i' to start interpretator, 't' to start translator: " Input
Name="Main"
InFolder="test"
OutFolder="jsmClasses"
ProgramName="test_program.kt"
stack run -- $Input $InFolder $Name $ProgramName
if [ "$Input" == "t" ]; then 
if [ -f "$OutFolder\$Name.class" ] #if exists
then rm "$OutFolder\$Name.class"
fi
java -jar jasmin-2.4/jasmin.jar "$InFolder/$Name.j"
java "$OutFolder/$Name"
fi
