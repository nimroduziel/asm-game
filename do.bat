@echo off

set arg1=%1
tasm /zi %arg1%
tlink /v %arg1%
