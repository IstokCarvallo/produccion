$PBExportHeader$uo_paramprecos.sru
$PBExportComments$Objeto de Validación de Parámetros de Pre-Cosecha según Especie para Control de Calidad
forward
global type uo_paramprecos from nonvisualobject
end type
end forward

global type uo_paramprecos from nonvisualobject descriptor "PB_ObjectCodeAssistants" = "{BB0DD54A-B36E-11D1-BB47-000086095DDA}" 
end type
global uo_paramprecos uo_paramprecos

type variables
Integer	especie,pesfru,calibr,colvis,colfon,calabi,presio,pca100,colsem,&
			nrosem,almido,acidez,coracu,cormoh,solsol,sosofi,matsec,ph,gasto

				



end variables

forward prototypes
public function boolean existe (integer ai_especie, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_especie, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True

SELECT	espe_codigo,ccpr_pesfru,ccpr_calibr,ccpr_colvis,ccpr_colfon,ccpr_calabi,
			ccpr_presio,ccpr_pca100,ccpr_colsem,ccpr_nrosem,ccpr_almido,ccpr_acidez,
			ccpr_coracu,ccpr_cormoh,ccpr_solsol,ccpr_sosofi,ccpr_matsec,ccpr_ph,
			ccpr_gasto
	
	INTO	:especie, :pesfru, :calibr, :colvis, :colfon, :calabi, :presio,
			:pca100, :colsem, :nrosem, :almido, :acidez, :coracu, :cormoh,
			:solsol, :sosofi, :matsec, :ph, :gasto

	FROM	dbo.spro_concalparmadprecos
	
	WHERE	espe_codigo	=	:ai_especie
	
	USING at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Color de Fondo")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Parámetros de Precosecha de Especie " + String(Especie, '00') + &
					", no ha sido registrado.~r~rIngrese o seleccione otra Especie.")
	END IF
	lb_Retorno	=	False
END IF

RETURN lb_Retorno

end function

on uo_paramprecos.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_paramprecos.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

