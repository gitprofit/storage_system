@echo off
set projdir="P:\_studia_done\agh\semestr5\tw\projekt\storage_system"
set mountpoint="P:\ds_mount"
set origin="P:\test_input"

echo %time%
xcopy %origin%\automaty %mountpoint%\automaty\ /Q /S
xcopy %origin%\mownit %mountpoint%\mownit\ /Q /S
echo %time%
