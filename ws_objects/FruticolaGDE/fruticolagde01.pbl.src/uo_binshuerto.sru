$PBExportHeader$uo_binshuerto.sru
forward
global type uo_binshuerto from nonvisualobject
end type
end forward

global type uo_binshuerto from nonvisualobject
end type
global uo_binshuerto uo_binshuerto

type variables
Integer		Cliente, Especie, Variedad, Categoria, Color, Estado 
Long			Planta, Bins, Productor, Predio, Cuartel
Datetime		Fecha
Decimal{3}	PesoNeto
String			Grupo
end variables
forward prototypes
public function boolean of_existe (integer ai_cliente, long al_planta, long al_numero, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean of_existe (integer ai_cliente, long al_planta, long al_numero, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
 
SELECT	clie_codigo, plde_codigo, bins_numero, espe_codigo, vari_codigo, cate_codigo, 
			ccev_codigo, prod_codigo, prpr_codigo, prcc_codigo, bins_estado, bins_pesnet, 
			bins_fechac, bins_grucal
	INTO	:Cliente, :Planta, :Bins, :Especie, :Variedad, :Categoria, 
			:Color, :Productor, :Predio, :Cuartel, :Estado, :PesoNeto, 
			:Fecha, :Grupo
	FROM	dbo.BINS
	WHERE	clie_codigo = :ai_Cliente
	And		plde_codigo	=	:al_Planta
	And		bins_numero = :al_Numero
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de BINS")
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False
 	IF ab_Mensaje THEN
		MessageBox("Atención", "Numero de BINS " + String(al_Numero) + &
					", o ha sido ingresado.~r~rIngrese o seleccione otro Numero.")	
	END IF
END IF

RETURN lb_Retorno
end function

on uo_binshuerto.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_binshuerto.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

