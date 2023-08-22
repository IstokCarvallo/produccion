$PBExportHeader$uo_ctrolusuario.sru
$PBExportComments$Objecto Controlar el acceso de usuario a ventana
forward
global type uo_ctrolusuario from nonvisualobject
end type
end forward

global type uo_ctrolusuario from nonvisualobject
end type
global uo_ctrolusuario uo_ctrolusuario

type variables
Integer	Mercado, Broker, Codigo
String	Nombre, Ejecutivo, Direccion, Localidad1, Localidad2, Localidad3, &
			Localidad4, Contacto, Telefono, NumeroFax, EMail, CuentaContab, &
			Comuna,Abreviacion,Rut,Ciudad
			
end variables

forward prototypes
public function boolean usuariovalido (string as_usuario, boolean ab_mensaje)
public function boolean existe (long al_cliente, boolean ab_mensaje, transaction at_transaccion)
public function boolean controla_usuario (string as_usuario, boolean ab_conmensaje, transaction at_trans)
end prototypes

public function boolean usuariovalido (string as_usuario, boolean ab_mensaje);IF Not IsNull(Ejecutivo) THEN
	IF Trim(Upper(as_Usuario)) <> Trim(Upper(Ejecutivo)) THEN
		IF ab_MeNsaje THEN
			MessageBox("Atención", "Usuario " + as_usuario + ", no es el ejecutivo~r" + &
			"Asociado al cliente.~r~rIngrese o seleccione otro Código.")
		END IF
		
		RETURN False
	END IF
ELSE
	RETURN False
END IF

RETURN True
end function

public function boolean existe (long al_cliente, boolean ab_mensaje, transaction at_transaccion);SELECT	clie_codigo, clie_nombre, clie_direcc, clie_comuna,
			clie_ciudad, clie_telefo, clie_nrofax, clie_abrevi, 
			clie_nrorut
	INTO	:Codigo, :Nombre, :Direccion, :Comuna,
			:Ciudad, :Telefono, :NumeroFax, :Abreviacion,
			:Rut
	FROM	dbo.clientesprod
	WHERE	clie_codigo	=	:al_cliente
	Using	at_Transaccion;

IF At_Transaccion.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Transaccion,"Lectura de Tabla de clientes")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de cliente " + &
						String(al_cliente, '00000') + ", no ha sido~r" + &
						"ingresado en tabla respectiva.~r~rIngrese o " + &
						"seleccione otro Código.")
	END IF
	
	RETURN False
END IF

RETURN True
end function

public function boolean controla_usuario (string as_usuario, boolean ab_conmensaje, transaction at_trans);Integer li_Contador

 SELECT	Count(*)
	INTO	:li_Contador
	FROM  dbo.admagrupousuario
	WHERE usua_codigo =:as_usuario
	AND   grpo_codigo = 10 
	AND   sist_codigo=19
	Using	at_trans;


IF At_Trans.SqlCode = -1 THEN
	F_ErrorBaseDatos(At_Trans,"Lectura de Tabla de Grupo Usuarios")
	RETURN False
ELSEIF at_Trans.SQLCode = 100 THEN
	IF ab_conmensaje	=	True THEN
		MessageBox("Atención", +as_usuario+ ", No tiene acceso a esta ventana.")
	END IF
	RETURN False
ELSEIF at_Trans.SQLCode = 0 AND li_Contador = 0 THEN
	IF ab_conmensaje	=	True THEN
		MessageBox("Atención", +as_usuario+ ", No tiene acceso a esta ventana.")
	END IF
	RETURN False
END IF

RETURN True
end function

on uo_ctrolusuario.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_ctrolusuario.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

