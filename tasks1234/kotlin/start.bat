@echo off
set /p Input=Write 'i' to start interpretator, 't' to start translator: 
set Name="test_jasmin"
set InFolder="test"
set OutFolder="jsmClasses"
stack run -- %Input% %InFolder%/%Name%
IF %Input% == t (
  IF EXIST %OutFolder%\%Name%.class (del %OutFolder%\%Name%.class)
  java -jar jasmin-2.4/jasmin.jar %InFolder%/%Name%.j
  java %OutFolder%/%Name%
)
