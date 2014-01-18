@echo off
set projdir="P:\_studia_done\agh\semestr5\tw\projekt\storage_system"
set mountpoint="P:\ds_mount"
set workdir="P:\local_ds_meta"

echo cleaning %mountpoint%
cd /d %mountpoint%
for /F "delims=" %%i in ('dir /b') do (rmdir "%%i" /s/q || del "%%i" /s/q)
cd /d %projdir%

echo cleaning %workdir%
cd /d %workdir%
for /F "delims=" %%i in ('dir /b') do (rmdir "%%i" /s/q || del "%%i" /s/q)
cd /d %projdir%
