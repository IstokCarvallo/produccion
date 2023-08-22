$PBExportHeader$uo_recepcionpdf.sru
$PBExportComments$Objecto Usuario que valida la Existencia de Embarques
forward
global type uo_recepcionpdf from nonvisualobject
end type
end forward

global type uo_recepcionpdf from nonvisualobject
end type
global uo_recepcionpdf uo_recepcionpdf

type variables
Integer	Cliente, Planta, Especie
Long		Productor
Date		Fecha
String		Rutas, Archivos
end variables

forward prototypes
public function boolean existe (integer ai_cliente, integer ai_planta, integer ai_especie, long al_productor, date ad_fecha, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_cliente, integer ai_planta, integer ai_especie, long al_productor, date ad_fecha, boolean ab_mensaje, transaction at_transaccion);
SELECT	clie_codigo, plde_codigo, espe_codigo,
		    prod_codigo, recp_fechap, recp_rutas, recp_archiv
	INTO	:Cliente, :Planta, :Especie, 
			:Productor, :Fecha, :Rutas, :Archivos
	FROM	dba.ctlcal_recepcionpdf
	WHERE	clie_codigo	=	:ai_Cliente
		And	plde_codigo	=	:ai_Planta
		And	espe_codigo	=	:ai_Especie
		And	prod_codigo	=	:al_Productor
		And	recp_fechap	=	:ad_Fecha
	Using	At_Transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de Recepciones PDF")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código "+ &
						", no ha sido~ringresado en tabla respectiva." + &
						"~r~rIngrese o seleccione otro Código.")
	END IF
	
	RETURN False
END IF

RETURN True
end function

on uo_recepcionpdf.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_recepcionpdf.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

