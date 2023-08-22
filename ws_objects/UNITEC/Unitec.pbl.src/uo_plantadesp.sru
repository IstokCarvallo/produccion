$PBExportHeader$uo_plantadesp.sru
forward
global type uo_plantadesp from nonvisualobject
end type
end forward

global type uo_plantadesp from nonvisualobject
end type
global uo_plantadesp uo_plantadesp

type variables
Integer	Codigo,  TipoPlanta, Administra, Comuna, CantCamaras, Zona, PackVirtual, &
			Conexion, ConexionProd, PacComercial, Capacidad, DiaProceso, CodigoMulti, PlantaSAG, Provincia, ComunaSAG, ProvinciaSAG
Long		CodigoSag
String		Nombre, Atencion1, Atencion2, Atencion3, Atencion4, RazonSocial, &
			Representante, Direccion, Telefono, Rut, Codigo_GLN, Mail, Region, &
			GiroEmpresa, CiudadOrigen, ComunaOrigen, DireccionOrigen, CodigoCSP
end variables

forward prototypes
public subroutine camaras (integer ai_codigo, transaction at_transaccion)
public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
public function boolean existefrigo (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
public function boolean existepacking (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion)
end prototypes

public subroutine camaras (integer ai_codigo, transaction at_transaccion);SELECT	Count(*)
	INTO	:CantCamaras
	FROM	dbo.camarasbode
	WHERE	plde_codigo	=	:ai_Codigo
	USING	at_Transaccion;
	
If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Cámaras de Plantas")
	CantCamaras	=	0
End If
end subroutine

public function boolean existe (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
 
SELECT	plde_codigo, plde_nombre, plde_atenc1, plde_atenc2, plde_atenc3,
			plde_atenc4, plde_razsoc, plde_repres, plde_direcc, plde_nrotel,
			plde_nrorut, plde_codsag, plde_tipopl, plde_admini, comu_codigo, 
			plde_glncod, zona_codigo, plde_pacvir, cone_codigo, plde_conpro,
			plde_paccom, plde_corplt, plde_capemb, plde_diapro, plde_codmul,
			plde_codpla, plde_region, plde_giroem, plde_ciuori, plde_comori,plde_dirori,
			plde_chasag, plde_provin, plde_comsag, plde_prvsag
	INTO	:Codigo, :Nombre, :Atencion1, :Atencion2, :Atencion3,
			:Atencion4, :RazonSocial, :Representante, :Direccion, :Telefono,
			:Rut, :CodigoSag, :TipoPlanta, :Administra, :Comuna, :Codigo_GLN,
			:zona, :PackVirtual, :conexion, :ConexionProd,	:paccomercial, :Mail,
			:Capacidad, :DiaProceso, :CodigoMulti, :PlantaSAG, :Region,
			:GiroEmpresa, :CiudadOrigen, :ComunaOrigen, :DireccionOrigen, :CodigoCSP, 
			:Provincia, :ComunaSAG, :ProvinciaSAG
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:ai_Codigo
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Plantas")
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False
 	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Planta " + String(ai_Codigo) + &
					", o ha sido ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existefrigo (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
 
SELECT	plde_codigo, plde_nombre, plde_atenc1, plde_atenc2, plde_atenc3,
			plde_atenc4, plde_razsoc, plde_repres, plde_direcc, plde_nrotel,
			plde_nrorut, plde_codsag, plde_tipopl, plde_admini, comu_codigo, plde_glncod,
			plde_giroem,plde_ciuori,plde_comori,plde_dirori
	INTO	:Codigo, :Nombre, :Atencion1, :Atencion2, :Atencion3,
			:Atencion4, :RazonSocial, :Representante, :Direccion, :Telefono,
			:Rut, :CodigoSag, :TipoPlanta, :Administra, :Comuna, :Codigo_GLN,
			:GiroEmpresa, :CiudadOrigen, :ComunaOrigen, :DireccionOrigen
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:ai_Codigo
	AND	plde_tipopl	=	1
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Plantas")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

 	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Planta " + String(ai_Codigo) + &
					", o ha sido ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existepacking (integer ai_codigo, boolean ab_mensaje, transaction at_transaccion);Boolean	lb_Retorno = True
 
SELECT	plde_codigo, plde_nombre, plde_atenc1, plde_atenc2, plde_atenc3,
			plde_atenc4, plde_razsoc, plde_repres, plde_direcc, plde_nrotel,
			plde_nrorut, plde_codsag, plde_tipopl, plde_admini, comu_codigo, plde_glncod,
			plde_giroem,plde_ciuori,plde_comori,plde_dirori
	INTO	:Codigo, :Nombre, :Atencion1, :Atencion2, :Atencion3,
			:Atencion4, :RazonSocial, :Representante, :Direccion, :Telefono,
			:Rut, :CodigoSag, :TipoPlanta, :Administra, :Comuna, :Codigo_GLN,
			:GiroEmpresa, :CiudadOrigen, :ComunaOrigen, :DireccionOrigen
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:ai_Codigo
	AND	plde_tipopl	=	2
	USING	at_Transaccion;
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla de Plantas")
	
	lb_Retorno	=	False
ELSEIF at_Transaccion.SQLCode = 100 THEN
	lb_Retorno	=	False

 	IF ab_Mensaje THEN
		MessageBox("Atención", "Código de Packing " + String(ai_Codigo) + &
					", o ha sido ingresado.~r~rIngrese o seleccione otro Código.")	
	END IF
END IF

RETURN lb_Retorno


end function

on uo_plantadesp.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_plantadesp.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

