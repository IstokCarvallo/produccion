$PBExportHeader$lab01.sra
$PBExportComments$Generated Application Object
forward
global type lab01 from application
end type
global transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global error error
global message message
end forward

global type lab01 from application
string appname = "lab01"
string themepath = "C:\Program Files (x86)\Appeon\Shared\PowerBuilder\theme190"
string themename = "Do Not Use Themes"
long richtextedittype = 2
long richtexteditversion = 1
string richtexteditkey = ""
string appruntimeversion = "22.0.0.1900"
end type
global lab01 lab01

on lab01.create
appname="lab01"
message=create message
sqlca=create transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create error
end on

on lab01.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

event open;Open(w_inicio)
end event

