$PBExportHeader$uo_correladhesivos.sru
$PBExportComments$Objeto de Validación de Correlativo de adhesivos según cliente y planta
forward
global type uo_correladhesivos from nonvisualobject
end type
end forward

global type uo_correladhesivos from nonvisualobject
end type
global uo_correladhesivos uo_correladhesivos

type variables
Long	Cliente, Planta, Correlativo
end variables

forward prototypes
public function boolean existe (integer ai_cliente, integer ai_planta, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe_folio (integer ai_cliente, integer ai_planta, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_cliente, integer ai_planta, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	plde_codigo, IsNull(cota_correl,0)
	INTO	:Planta, :Correlativo
	FROM	dba.spro_correltarjetones
	WHERE	plde_codigo	=	:ai_planta
	USING at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla spro_correltarjetones")
	
	lb_Retorno	=	False
	
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Numero de Correlativo para Planta " + String (ai_planta, '0000') + &
						", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
	
END IF

RETURN lb_Retorno
end function

public function boolean existe_folio (integer ai_cliente, integer ai_planta, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	plde_codigo, IsNull(cota_correl,0)
	INTO	:Planta, :Correlativo
	FROM	dba.spro_correltarjetones
	WHERE	plde_codigo	=	:ai_planta
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla spro_correltarjetones")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Numero de Correlativo para  Planta " + String (ai_planta, '0000') + &
						", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
	lb_Retorno	=	False
	
END IF

RETURN lb_Retorno
end function

on uo_correladhesivos.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_correladhesivos.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

