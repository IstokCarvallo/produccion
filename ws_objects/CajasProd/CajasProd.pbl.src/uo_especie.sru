$PBExportHeader$uo_especie.sru
$PBExportComments$Objeto de Validación de Especies
forward
global type uo_especie from nonvisualobject
end type
end forward

global type uo_especie from nonvisualobject
end type
global uo_especie uo_especie

type variables
Integer	Codigo,kildec,Quinto
String	Nombre, Nombre_ingles, CodAra, CodSAG
end variables

forward prototypes
public function boolean tiene_correlativo_planta (integer ai_planta)
public function boolean existecorrelativolote (integer ai_planta, integer ai_especie, boolean ab_mensaje, transaction at_transaccion)
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean tiene_correlativo_planta (integer ai_planta);RETURN True
end function

public function boolean existecorrelativolote (integer ai_planta, integer ai_especie, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
Integer	li_Ultimo
String		ls_nombrepc

ls_nombrepc	=	gstr_us.computador

SELECT	loco_ultcor
	INTO	:li_Ultimo
	FROM	dbo.spro_lotescorrelequipo
	WHERE	plde_codigo =  :ai_Planta
	AND	espe_codigo	=	:ai_Especie
	AND 	Upper(equi_nombre) 	=	Upper(:ls_nombrepc)
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Correlativos por Equipo y Especie")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 AND ab_Mensaje THEN
	MessageBox("Atención", "Para la Especie " + String(ai_Especie) + &
				", no se ha registrado el Correlativo Inicial de Lotes.~r~rIngrese o seleccione otro Código.")	
	
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	espe_codigo,espe_nombre,coar_codigo,espe_noming,espe_codsag,espe_kildec//,espe_quinto
	INTO	:Codigo,:Nombre,:CodAra,:Nombre_ingles,:CodSAG,:kildec//,:Quinto
	FROM	dbo.especies
	WHERE	espe_codigo	=	:ai_Codigo
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Especies")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Especie " + String(ai_Codigo) + &
					", no ha sido Ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_especie.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_especie.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

