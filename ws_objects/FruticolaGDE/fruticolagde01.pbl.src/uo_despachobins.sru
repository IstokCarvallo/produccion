$PBExportHeader$uo_despachobins.sru
forward
global type uo_despachobins from nonvisualobject
end type
end forward

global type uo_despachobins from nonvisualobject
end type
global uo_despachobins uo_despachobins

type variables
Integer	Cliente
Long		Planta, Numero
Datetime	Fecha
end variables

forward prototypes
public function long of_maximo (integer ai_cliente, long al_planta, transaction at_transaccion)
public function boolean of_existebins (integer ai_cliente, long al_planta, long al_bins, transaction at_transaccion)
public function boolean of_existe (integer ai_cliente, long al_planta, integer ai_tipo, long al_numero, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function long of_maximo (integer ai_cliente, long al_planta, transaction at_transaccion);Long	ll_Retorno = -1

SELECT	 IsNull(Max(desp_numero), 0) + 1
	INTO :ll_Retorno
	FROM	dbo.despachos
	WHERE	clie_codigo = :ai_cliente
	And		plde_codigo	= :al_planta
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Despachos")
	ll_Retorno	=	-1
ELSEIF at_Transaccion.SQLCode = 100 THEN
	ll_Retorno	=	-1
END IF

RETURN ll_Retorno
end function

public function boolean of_existebins (integer ai_cliente, long al_planta, long al_bins, transaction at_transaccion);Boolean	lb_Retorno = True
Long		ll_Cantidad
 
SELECT	Count(bins_numero)
	INTO :ll_Cantidad
	FROM	dbo.despachos
	WHERE	clie_codigo = :ai_Cliente
	And		plde_codigo	= :al_Planta
	And		bins_numero = :al_bins
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Despachos")
	lb_Retorno	=	False
ELSE
	If ll_Cantidad > 0 Then 
		lb_Retorno	=	False
		MessageBox("Atencion", "Numero de Bins se enuentra en otra recepcion.", StopSign!, Ok!)
	End If
END IF

RETURN lb_Retorno
end function

public function boolean of_existe (integer ai_cliente, long al_planta, integer ai_tipo, long al_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
 
SELECT	Distinct clie_codigo, plde_codigo, desp_numero, desp_fecdes
	INTO	:Cliente, :Planta, :Numero, :Fecha
	FROM	dbo.despachos
	WHERE	clie_codigo = :ai_Cliente
	And		plde_codigo	=	:al_Planta
	And		desp_numero = :al_Numero
	And		desp_tipode = :ai_Tipo
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Despachos")
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False
 	IF ab_Mensaje THEN
		MessageBox("Atención", "Numero de Recepcion " + String(al_Numero) + &
					", o ha sido ingresado.~r~rIngrese o seleccione otro Numero.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_despachobins.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_despachobins.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

