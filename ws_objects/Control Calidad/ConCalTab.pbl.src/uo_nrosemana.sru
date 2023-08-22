$PBExportHeader$uo_nrosemana.sru
forward
global type uo_nrosemana from nonvisualobject
end type
end forward

global type uo_nrosemana from nonvisualobject
end type
global uo_nrosemana uo_nrosemana

type variables
DataStore	ids_semana

Date		Lunes_Actual, Lunes, Fecha
Integer	Inicio_Temporada, Dia_Semana, NroSemana
end variables

forward prototypes
public function boolean semana (integer semana, integer temporada)
end prototypes

public function boolean semana (integer semana, integer temporada);ids_semana 		= Create DataStore

ids_semana.DataObject = 'dw_consulta_numerosemana'
ids_semana.SetTransObject(SQLCA)

If ids_semana.Retrieve(Semana, Temporada) = -1 Then
	MessageBox('Alerta', 'No se encuentra Informacion de semana requerida', Exclamation!, OK!)
	Return False
End If

If ids_semana.RowCount() > 0 Then
	Lunes_Actual		= Date(String(ids_semana.Object.lune_semana[1], 'dd/mm/yyyy'))
	Lunes					= Date(String(ids_semana.Object.fech_dlunes[1], 'dd/mm/yyyy'))
	Fecha					= Date(String(ids_semana.Object.fech_actual[1], 'dd/mm/yyyy'))
	Inicio_Temporada	= ids_semana.Object.iniciotemp[1]
	Dia_Semana		= ids_semana.Object.ndia_semana[1]
	NroSemana			= ids_semana.Object.nume_semana[1]
Else
	MessageBox('Alerta', 'No se encuentra Informacion de semana requerida', Exclamation!, OK!)
	Return False
End If

Destroy ids_Semana

Return True
end function

on uo_nrosemana.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_nrosemana.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

