$PBExportHeader$uo_categoriaguarda.sru
forward
global type uo_categoriaguarda from nonvisualobject
end type
end forward

global type uo_categoriaguarda from nonvisualobject
end type
global uo_categoriaguarda uo_categoriaguarda

type variables
Integer	Especie
String		Codigo, Nombre
Long		De_Minimo, De_Maximo, SS_Minimo, SS_Maximo, Fi_Minimo, Fi_Maximo
 
end variables

forward prototypes
public function boolean existe (integer ai_especie, string as_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existe (integer ai_especie, string as_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
  
	Select espe_codigo, ctcg_codigo, ctcg_mindef, ctcg_maxdef, ctcg_minsso, 
			 ctcg_maxsso, ctcg_minfir, ctcg_maxfir, ctcg_nombre
	Into	:Especie, :Codigo, :De_Minimo, :De_Maximo, :SS_Minimo, :SS_Maximo, :Fi_Minimo, :Fi_Maximo, :Nombre
	From dba.ctlcalcateguarda
	Where espe_codigo = :ai_Especie
	  And ctcg_codigo = :as_Codigo
	USING	at_Transaccion; 

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla ctlcalagronomos")

	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Categoria " + as_Codigo + ", no ha sido creado en tabla respectiva.~r~r" + &
						"Ingrese o seleccione otro.")
	END IF
END IF

RETURN lb_Retorno
end function

on uo_categoriaguarda.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_categoriaguarda.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

