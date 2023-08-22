$PBExportHeader$uo_productores.sru
$PBExportComments$Objeto de Validación de Productores
forward
global type uo_productores from nonvisualobject
end type
end forward

global type uo_productores from nonvisualobject
end type
global uo_productores uo_productores

type variables
Integer	Exportador, TipoProduc, Comuna, Zona
String	Nombre, Rut, Direcc, Telefono, NroFax, &
			EnvaseCosecha, Embalado, Refrigerado, LugarEntrega, &
			CalleEntrega, CiudadEntrega
Long		Codigo
end variables

forward prototypes
public function boolean existerutprod (string as_rut, ref integer ai_cantidad, boolean ab_mensaje, transaction at_transaccion)
public function boolean antecedentecontrato (transaction at_transaccion, boolean ab_mensaje)
public function boolean existe (long al_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public function boolean existerutprod (string as_rut, ref integer ai_cantidad, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	Count(prod_codigo)
	INTO	:ai_Cantidad
	FROM	dbo.productores
	WHERE	prod_rut	=	:as_Rut
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Productores")
	
	lb_Retorno	=	False
ELSEIF (at_Transaccion.SQLCode = 100 OR ai_cantidad = 0 ) THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "R.U.T. de Productor " + as_Rut + &
					", no ha sido registrado.~r~rIngrese o seleccione otro R.U.T.")
	END IF
ELSEIF ai_Cantidad = 1 THEN
	SELECT	prod_codigo, expo_codigo, tipr_codigo, comu_codigo, zona_codigo,
				prod_nombre, prod_rut, prod_direcc, prod_nrotel, prod_nrofax
		INTO	:Codigo, :Exportador, :TipoProduc, :Comuna, :Zona,
				:Nombre, :Rut, :Direcc, :Telefono, :NroFax
		FROM	dbo.productores
		WHERE	prod_rut	=	:as_Rut
		USING	at_Transaccion;

	IF at_Transaccion.SQLCode = -1 THEN
		F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Productores")
		
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean antecedentecontrato (transaction at_transaccion, boolean ab_mensaje);Boolean	lb_Retorno	=	True

IF Isnull(Codigo) OR Codigo = 0 THEN
	MessageBox("Atención", "Debe instanciar el Productor previamente para "+&
					"obtener los antecedentes de Contrato.")
	lb_Retorno =	False
ELSE
END IF

RETURN lb_Retorno

end function

public function boolean existe (long al_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno	=	True

SELECT	prod_codigo, expo_codigo, tipr_codigo, comu_codigo, zona_codigo,
			prod_nombre, prod_rut, prod_direcc, prod_nrotel, prod_nrofax
	INTO	:Codigo, :Exportador, :TipoProduc, :Comuna, :Zona,
			:Nombre, :Rut, :Direcc, :Telefono, :NroFax
	FROM	dbo.productores
	WHERE	prod_codigo	=	:al_Codigo
	USING	at_Transaccion;

IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Productores")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Productor " + String(al_Codigo, '00000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Código.")
	END IF
END IF

RETURN lb_Retorno
end function

on uo_productores.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_productores.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

