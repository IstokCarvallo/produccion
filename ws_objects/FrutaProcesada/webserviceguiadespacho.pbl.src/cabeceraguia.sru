$PBExportHeader$cabeceraguia.sru
$PBExportComments$Proxy imported from Web service using Web Service Proxy Generator.
forward
    global type cabeceraGuia from nonvisualobject
    end type
end forward

global type cabeceraGuia from nonvisualobject
end type

type variables
    string instEmbarque
    long numGuia
    long codRecibidor
    long codPlanta
    string patTransporte
    long codEspecie
    string condFumi
    string numPallet
    long codEtiq
    string fhEmbalaje
    long codProductor
    string codEmbalaje
    long codVariedad
    string calibre
    long cantCajas
    string categoria
    string tipoCamion
    long codPuertoEmbarque
    long codPack
    string numTermografo
    long codPallet
    string codInsp
    string actPallet
    string numContenedor
    string sellos
    string chofer
    string celular
    string acoplado
    long hsPartida
    long minPartida
    long codPuertoDestino
    string idUsuario
    string nomEquipo
    string hsFecha
end variables

on cabeceraGuia.create
call super::create
TriggerEvent( this, "constructor" )
end on

on cabeceraGuia.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

