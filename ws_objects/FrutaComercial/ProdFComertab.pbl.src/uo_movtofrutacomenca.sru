$PBExportHeader$uo_movtofrutacomenca.sru
$PBExportComments$Objeto de Validación de Movimientos de Fruta Comercial.
forward
global type uo_movtofrutacomenca from nonvisualobject
end type
end forward

global type uo_movtofrutacomenca from nonvisualobject
end type
global uo_movtofrutacomenca uo_movtofrutacomenca

type variables
Integer	Planta, TipoMovto, PlantaCordina, Transporte
Long		Numero, Productor
Date		FechaMovto
String	Cliente
end variables

forward prototypes
public function boolean existe (integer ai_planta, integer ai_tipomovto, integer ai_numero, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_planta, integer ai_tipomovto, integer ai_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	plde_codigo, tpmv_codigo, mfco_numero, mfco_fecmov, prod_codigo, 
			plde_coorde, tran_codigo, clpr_rut
	INTO	:Planta, :TipoMovto, :Numero, :FechaMovto, :Productor, :PlantaCordina, 
			:Transporte, :Cliente
	FROM	dba.spro_movtofrutacomenca
	WHERE	plde_codigo =	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfco_numero =	:ai_Numero
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Movimiento de Fruta Comercial")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 AND ab_Mensaje THEN
	MessageBox("Atención", "Movimiento de Fruta Comercial" + String(ai_Numero, '00000000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Número.")

	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on uo_movtofrutacomenca.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_movtofrutacomenca.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

