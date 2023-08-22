$PBExportHeader$uo_colordefondo.sru
$PBExportComments$Objeto de Validación de Colores de Fondo según Especie y Variedad
forward
global type uo_colordefondo from nonvisualobject
end type
end forward

global type uo_colordefondo from nonvisualobject
end type
global uo_colordefondo uo_colordefondo

type variables
Integer	Especie, Variedad, Codigo, Dano, Grupo, Subgrupo
String	Nombre
end variables

forward prototypes
public function boolean existe (integer ai_especie, integer ai_grupo, integer ai_subgrupo, integer ai_variedad, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_especie, integer ai_grupo, integer ai_subgrupo, integer ai_variedad, integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	espe_codigo, cofo_codigo, grva_codigo, grva_codsub, vari_codigo,
         cofo_nombre, cofo_codano
	INTO	:Especie, :Codigo, :Grupo, :SubGrupo, :Variedad, :Nombre, :Dano
	FROM	dba.spro_colordefondo
	WHERE	espe_codigo	=	:ai_Especie
	AND	cofo_codigo	=	:ai_Codigo
	AND   isnull(grva_codigo,-1) =  :ai_grupo
	AND   isnull(grva_codsub,-1) =  :ai_subgrupo
	AND	isnull(vari_codigo,-1) =  :ai_Variedad
	
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Color de Fondo")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código Color de Fondo " + String(ai_Codigo, '00') + &
		   			", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF					

	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on uo_colordefondo.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_colordefondo.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

