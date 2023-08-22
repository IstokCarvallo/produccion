$PBExportHeader$uo_clientesprod.sru
forward
global type uo_clientesprod from nonvisualobject
end type
end forward

global type uo_clientesprod from nonvisualobject
end type
global uo_clientesprod uo_clientesprod

type variables
integer	Codigo, Guia_Electronica, clie_ctlven, clie_conexi, cone_codigo, Genera_Pallet, Genera_Pucho,Controla_Rotulado
String		Nombre, Direccion, Comuna, Ciudad, Fono, FAX, Abrevi, Rut, ClienteRotulado, &
			NombreRotulado,FormatoGuia, clie_codbar, Giro
end variables

forward prototypes
public function boolean existe (integer as_cliente, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer as_cliente, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	clie_codigo, clie_nombre, clie_direcc, clie_comuna, clie_ciudad,
			clie_telefo, clie_nrofax, clie_abrevi, clie_nrorut, IsNull(clie_rotula,''),
			IsNull(clie_guiele, 0), IsNull(clie_rnombr, ''), IsNull(clie_ftodes, ''), 
			IsNull(clie_ctlven, 0), IsNull(clie_conexi, 0), IsNull(cone_codigo, 0), clie_codbar, IsNull(clie_giroem, ''),
			IsNull(clie_folioa, 0), IsNull(clie_puchoa, 0), IsNull(clie_ctlrot, 0)
	INTO	:Codigo, :Nombre, :Direccion, :Comuna, :Ciudad,
			:Fono, :FAX, :Abrevi, :Rut, :ClienteRotulado, 
			:Guia_Electronica, :NombreRotulado, :FormatoGuia,
			:clie_ctlven, :clie_conexi, :cone_codigo, :clie_codbar, :Giro,
			:Genera_Pallet, :Genera_Pucho, :Controla_Rotulado
	FROM	dbo.clientesprod
	WHERE	clie_codigo =	:as_cliente
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Plantas")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Cliente " + string(as_Cliente) + &
					", no ha sido ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_clientesprod.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_clientesprod.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

