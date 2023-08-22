$PBExportHeader$uo_ctlcaldanoespecie.sru
$PBExportComments$Objeto de Usuario de Validación de Daños por Especie.
forward
global type uo_ctlcaldanoespecie from nonvisualobject
end type
end forward

global type uo_ctlcaldanoespecie from nonvisualobject
end type
global uo_ctlcaldanoespecie uo_ctlcaldanoespecie

type variables
Integer	ii_Clie_codigo,ii_espe_Codigo,ii_ccda_secuen, Especie, Familia, Codigo
String   ii_ccda_descri, Nombre

end variables

forward prototypes
public function boolean existe (integer ai_espe_codigo, integer ai_codigo, boolean ab_conmensaje, transaction at_trans)
public function boolean existe (integer ai_especie, integer ai_codigo, integer ai_familia, boolean ab_mensaje, transaction at_transaction)
end prototypes

public function boolean existe (integer ai_espe_codigo, integer ai_codigo, boolean ab_conmensaje, transaction at_trans);long	ll_Existe


SELECT ccda_secuen
INTO	:ll_Existe
FROM	dbo.ctlcaldanoespecie
WHERE	espe_codigo	=	:ai_espe_codigo
AND	ccda_secuen	=	:ai_codigo
Using	at_trans;

IF At_Trans.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Trans,"Lectura de Tabla de Daños y Defectos")
	RETURN False
ELSEIF At_Trans.SQLCode = 100 THEN
	IF ab_conmensaje	=	True THEN
		MessageBox("Atención", "Código de Daño y Defectos " + String(ai_codigo, '000') + ", no ha sido~r" + &
		"ingresado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	END IF
	RETURN False
END IF

RETURN True
end function

public function boolean existe (integer ai_especie, integer ai_codigo, integer ai_familia, boolean ab_mensaje, transaction at_transaction);long	ll_Existe

SELECT espe_codigo, ccda_secuen, ccfa_codigo, ccda_descri
INTO	:Especie, :Codigo, :Familia, :Nombre
FROM	dbo.ctlcaldanoespecie
WHERE	espe_codigo	=	:ai_especie
AND	ccda_secuen	=	:ai_codigo
AND	ccfa_codigo		=	:ai_familia
Using	at_transaction;

IF at_transaction.SqlCode = -1 THEN
	F_ErrorBaseDatos(at_transaction,"Lectura de Tabla de Daños y Defectos")
	RETURN False
ELSEIF at_transaction.SQLCode = 100 THEN
	IF ab_mensaje THEN
		MessageBox("Atención", "Código de Daño y Defectos " + String(ai_codigo, '000') + ", no ha sido~r" + &
		"ingresado en tabla respectiva.~r~rIngrese o seleccione otro Código.")
	END IF
	RETURN False
END IF

RETURN True
end function

on uo_ctlcaldanoespecie.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_ctlcaldanoespecie.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

