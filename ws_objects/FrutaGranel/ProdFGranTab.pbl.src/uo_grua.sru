$PBExportHeader$uo_grua.sru
forward
global type uo_grua from nonvisualobject
end type
end forward

global type uo_grua from nonvisualobject
end type
global uo_grua uo_grua

type variables
Integer	plde_codigo, grua_codigo, grua_kiltar, turn_codigo
String	grua_descri
Date		tagr_fecha
Double	tagr_kiltar  
end variables

forward prototypes
public function boolean existe (integer ai_planta, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
public function boolean existefecha (integer ai_planta, integer ai_codigo, date ad_fecha, time at_hora, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_planta, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT  plde_codigo, grua_codigo, grua_descri, grua_kiltar
  INTO :plde_codigo,:grua_codigo,:grua_descri,:grua_kiltar  
  FROM dba.spro_gruas
 WHERE plde_codigo = :ai_planta
   AND grua_codigo = :ai_codigo
 USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	lb_Retorno	=	False
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de spro_gruas")

ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False
 	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Grua " + String(ai_Codigo) + ", " + &
					  "no ha sido ingresado para la planta requerida.~r~r" + &
					  "Ingrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existefecha (integer ai_planta, integer ai_codigo, date ad_fecha, time at_hora, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True


  SELECT Max(tagr_hordes)
    INTO :at_hora
	 FROM dba.spro_destaregruas
	WHERE plde_codigo	=	:ai_planta
	  AND grua_codigo	=	:ai_codigo
	  AND tagr_fecdes	=	:ad_fecha
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	lb_Retorno	=	False
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de spro_destaregruas")

ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False
 	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Grua " + String(ai_Codigo) + ", " + &
					  "no ha sido ingresado para la Planta y Fecha requerida.~r~r" + &
					  "Ingrese o seleccione otro Código.")	
	END IF
ELSE
  SELECT  plde_codigo, grua_codigo, tagr_fecdes, tagr_kiltar  
    INTO :plde_codigo,:grua_codigo,:tagr_fecha, :tagr_kiltar  
    FROM dba.spro_destaregruas
	WHERE plde_codigo	=	:ai_planta
	  AND grua_codigo	=	:ai_codigo
	  AND tagr_fecdes	=	:ad_fecha
	  AND tagr_hordes	=	:at_hora
	USING at_Transaccion;

	IF at_Transaccion.SQLCode = -1 THEN
		lb_Retorno	=	False
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de spro_destaregruas")
	
	ELSEIF at_Transaccion.SQLCode = 100 THEN
		lb_Retorno	=	False
		IF ab_Mensaje THEN
			MessageBox("Atención", "Código de Grua " + String(ai_Codigo) + ", " + &
						  "no ha sido ingresado para la Planta, Fecha y Hora requerida.~r~r" + &
						  "Ingrese o seleccione otro Código.")	
		END IF
	END IF
END IF

RETURN lb_Retorno
end function

on uo_grua.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_grua.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

