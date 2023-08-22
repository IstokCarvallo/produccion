$PBExportHeader$uo_programasegregador.sru
$PBExportComments$Objeto de Validación de Programas de Segregacion.
forward
global type uo_programasegregador from nonvisualobject
end type
end forward

global type uo_programasegregador from nonvisualobject
end type
global uo_programasegregador uo_programasegregador

type variables
Integer	Especie
Date		Fecha
String		Observacion
end variables

forward prototypes
public function boolean existe (integer ai_especie, date ad_fecha, boolean ab_mensaje, transaction at_transaccion)
public function long maximo (integer ai_especie, date ad_fecha, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_especie, date ad_fecha, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	espe_codigo, pren_fechas, pren_observ
	INTO	:Especie, :Fecha, :Observacion
	FROM	dba.spro_programasegregadorenca
	WHERE	espe_codigo	=	:ai_especie
	AND   	Datediff(dd, pren_fechas, :ad_fecha) = 0
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Tipo de Pallet por Envase")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Programa de Segregacion para Especie: " + String(ai_Especie) + &
				", no ha sido registrado para Fecha: " + String(ad_Fecha, 'dd/mm/yyyy') + ".~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
END IF


RETURN lb_Retorno
end function

public function long maximo (integer ai_especie, date ad_fecha, transaction at_transaccion);Long	ll_Retorno	=	-1

SELECT	IsNull(Max(prde_numero), 0) + 1
	Into	:ll_Retorno
	FROM	dba.spro_programasegregadordeta
	WHERE	espe_codigo	=	:ai_especie
	AND   	Datediff(dd, pren_fechas, :ad_fecha) = 0
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Detalle Programa Segragador")
	ll_Retorno	=	-1
ELSEIF at_Transaccion.SQLCode = 100 THEN
	ll_Retorno	=	-1
END IF


RETURN ll_Retorno
end function

on uo_programasegregador.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_programasegregador.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

