$PBExportHeader$uo_spro_movtofrutagranenca.sru
$PBExportComments$Objeto de Validación de Parámetros de Madurez según Especie y Variedad para Control de Calidad
forward
global type uo_spro_movtofrutagranenca from nonvisualobject
end type
end forward

global type uo_spro_movtofrutagranenca from nonvisualobject
end type
global uo_spro_movtofrutagranenca uo_spro_movtofrutagranenca

type variables
Integer	ii_plde_codigo, ii_tpmv_codigo, ii_espe_codigo, ii_tran_codigo
Long		il_mfge_numero, il_prod_codigo
DateTime	idt_mfge_fecmov
end variables

forward prototypes
public function boolean existe (integer ai_planta, integer ai_tipomovto, integer ai_numero, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe (integer ai_planta, integer ai_cliente, integer ai_tipomovto, integer ai_numero, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_planta, integer ai_tipomovto, integer ai_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	plde_codigo, tpmv_codigo, mfge_numero, espe_codigo, prod_codigo,
			tran_codigo, mfge_fecmov
	INTO	:ii_plde_codigo, :ii_tpmv_codigo, :il_mfge_numero, :ii_espe_codigo, 
			:il_prod_codigo, :ii_tran_codigo, :idt_mfge_fecmov
	FROM	dba.spro_movtofrutagranenca
	WHERE	plde_codigo =	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero =	:ai_Numero
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Movimiento de Fruta")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Movimiento de Fruta " + String(ai_Numero, '00000000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Número.")
	END IF
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean existe (integer ai_planta, integer ai_cliente, integer ai_tipomovto, integer ai_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = False

SELECT	plde_codigo, tpmv_codigo, mfge_numero, espe_codigo, prod_codigo,
			tran_codigo, mfge_fecmov
	INTO	:ii_plde_codigo, :ii_tpmv_codigo, :il_mfge_numero, :ii_espe_codigo, 
			:il_prod_codigo, :ii_tran_codigo, :idt_mfge_fecmov
	FROM	dba.spro_movtofrutagranenca
	WHERE	plde_codigo =	:ai_Planta
	AND	clie_codigo =  :ai_cliente
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	defg_docrel =	:ai_Numero
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Movimiento de Fruta")

	lb_Retorno	=	True
ELSEIF at_Transaccion.SQLCode <> 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Movimiento de Fruta " + String(ai_Numero, '00000000') + &
					", ya ha sido registrado.~r~rIngrese o seleccione otro Número.")
	END IF
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

on uo_spro_movtofrutagranenca.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_spro_movtofrutagranenca.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

