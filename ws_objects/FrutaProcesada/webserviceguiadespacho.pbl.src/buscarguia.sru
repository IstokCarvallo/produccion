$PBExportHeader$buscarguia.sru
$PBExportComments$Proxy imported from Web service using Web Service Proxy Generator.
forward
    global type buscarGuia from nonvisualobject
    end type
end forward

global type buscarGuia from nonvisualobject
end type

type variables
    string guia
end variables

on buscarGuia.create
call super::create
TriggerEvent( this, "constructor" )
end on

on buscarGuia.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

