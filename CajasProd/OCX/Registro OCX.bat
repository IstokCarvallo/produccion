@echo off
move  /y mscomctl.ocx c:\windows\system32\mscomctl.ocx
move  /y mscomct2.ocx c:\windows\system32\mscomct2.ocx
move  /y mscomm32.ocx c:\windows\system32\mscomm32.ocx
regsvr32.exe /s c:\windows\system32\mscomctl.ocx
regsvr32.exe /s c:\windows\system32\mscomct2.ocx
regsvr32.exe /s c:\windows\system32\mscomm32.ocx
Licenciaocx.reg
del Licenciaocx.reg
@exit