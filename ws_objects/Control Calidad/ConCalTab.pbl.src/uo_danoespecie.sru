$PBExportHeader$uo_danoespecie.sru
$PBExportComments$Objeto de Usuario de Validación de Daños por Especie.
forward
global type uo_danoespecie from nonvisualobject
end type
end forward

global type uo_danoespecie from nonvisualobject
end type
global uo_danoespecie uo_danoespecie

type variables
Integer	ii_Clie_codigo,ii_espe_Codigo,ii_ccda_secuen
String   ii_ccda_descri

end variables

forward prototypes
public function boolean existe (integer ai_espe_codigo, integer ai_codigo, boolean ab_conmensaje, transaction at_trans)
end prototypes

public function boolean existe (integer ai_espe_codigo, integer ai_codigo, boolean ab_conmensaje, transaction at_trans);

SELECT ccda_secuen, ccda_descri
INTO	:ii_ccda_secuen, : ii_ccda_descri
FROM	dba.ctlcaldanoespecie
WHERE	espe_codigo	=	:ai_espe_codigo
AND	ccda_secuen	=	:ai_codigo
Using	at_trans;

IF At_Trans.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Trans,"Lectura de Tabla de Daños y Defectos")
	RETURN False
ELSEIF at_Trans.SQLCode = 100	THEN
	IF ab_conmensaje	=	True THEN
		MessageBox("Atención", "Código de Daño y Defectos " + String(ai_codigo, '000') + ", no ha sido~r" + &
		"ingresado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	END IF
	RETURN False
END IF

RETURN True
end function

on uo_danoespecie.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_danoespecie.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

