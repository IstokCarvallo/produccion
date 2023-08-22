$PBExportHeader$uo_variedades.sru
$PBExportComments$Objeto de Validación de Variedades
forward
global type uo_variedades from nonvisualobject
end type
end forward

global type uo_variedades from nonvisualobject
end type
global uo_variedades uo_variedades

type variables
Integer	Especie, Variedad, Grupo, SubGrupo, TipoEnvase, Envase
String	NombreVariedad
end variables

forward prototypes
public function boolean existe (integer ai_especie, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_especie, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	espe_codigo,vari_codigo,vari_nombre,grva_codigo,grva_codsub,
			enva_tipoen,enva_codigo
	INTO	:Especie, :Variedad, :NombreVariedad, :Grupo, :SubGrupo, 
			:TipoEnvase, :Envase
	FROM	dbo.variedades
	WHERE	espe_codigo	=	:ai_Especie
	AND	vari_codigo	=	:ai_Codigo
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Variedades")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Especie y/o Variedad (" + String(ai_Especie, '00') + " - " + &
						String(ai_Codigo, '0000') + "), no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro(s) Código(s).")
	END IF
END IF

RETURN lb_Retorno
end function

on uo_variedades.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_variedades.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

