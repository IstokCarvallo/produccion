$PBExportHeader$array.sru
$PBExportComments$Proxy imported from Web service using Web Service Proxy Generator.
forward
    global type Array from nonvisualobject
    end type
end forward

global type Array from nonvisualobject
end type

type variables
    any ws_Any[]
    string arrayType
    string offset
    boolean offsetSpecified
    string id
    string href
    any AnyAttr[]
end variables

on Array.create
call super::create
TriggerEvent( this, "constructor" )
end on

on Array.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

