$PBExportHeader$uo_camarasbode.sru
forward
global type uo_camarasbode from nonvisualobject
end type
end forward

global type uo_camarasbode from nonvisualobject
end type
global uo_camarasbode uo_camarasbode

type variables
Integer	CodigoSag, TipoPlanta, Administra, Comuna, TipoCamara, cancal,canbas, canpos, Planta, Camara, &
			Grupo, SubGrupo, TipoEnvase, Envase, CodigoATS, Capacidad, TipoFrio
String		Atencion1, Atencion2, Atencion3, Atencion4, RazonSocial, &
			Representante, Direccion, Telefono, Rut, NombreCamara
			
			
end variables

forward prototypes
public function boolean existe (integer ai_planta, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
public function boolean existeats (integer ai_planta, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_planta, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	plde_codigo, cama_codigo, cama_nombre, cama_tipoca, cama_cancal,
			cama_canbas, cama_canpos, cama_codats, cama_capmax, IsNull(frio_tipofr, 6)
	INTO	:Planta, :Camara, :Nombrecamara, :TipoCamara, :cancal,
			:canbas, :canpos, :CodigoATS, :Capacidad, :TipoFrio
	FROM	dbo.camarasbode
	WHERE	plde_codigo =  :ai_planta
	AND   cama_codigo	=	:ai_Codigo
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Cámaras")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Cámara (" + String(ai_planta, '00') + " - " + &
						String(ai_Codigo, '0000') + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existeats (integer ai_planta, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	plde_codigo, cama_codigo, cama_nombre, cama_tipoca, cama_cancal,
			cama_canbas, cama_canpos, cama_codats, IsNull(frio_tipofr, 6)
	INTO	:Planta, :Camara, :Nombrecamara, :TipoCamara, :cancal,
			:canbas, :canpos, :CodigoATS, :TipoFrio
	FROM	dbo.camarasbode
	WHERE	plde_codigo =  :ai_planta
	AND   	cama_codats	=	:ai_Codigo
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Cámaras")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Cámara (" + String(ai_planta, '00') + " - " + &
						String(ai_Codigo, '0000') + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	END IF
END IF

RETURN lb_Retorno
end function

on uo_camarasbode.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_camarasbode.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

