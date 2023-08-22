$PBExportHeader$w_info_embarque_recibidor.srw
forward
global type w_info_embarque_recibidor from w_para_informes
end type
type st_1 from statictext within w_info_embarque_recibidor
end type
type st_2 from statictext within w_info_embarque_recibidor
end type
type em_desde from editmask within w_info_embarque_recibidor
end type
type dw_cliente from datawindow within w_info_embarque_recibidor
end type
type st_6 from statictext within w_info_embarque_recibidor
end type
type dw_planta from datawindow within w_info_embarque_recibidor
end type
type st_3 from statictext within w_info_embarque_recibidor
end type
type st_7 from statictext within w_info_embarque_recibidor
end type
type em_hasta from editmask within w_info_embarque_recibidor
end type
type st_8 from statictext within w_info_embarque_recibidor
end type
type gb_3 from groupbox within w_info_embarque_recibidor
end type
type st_5 from statictext within w_info_embarque_recibidor
end type
type cbx_peso from checkbox within w_info_embarque_recibidor
end type
type dw_pesoneto from datawindow within w_info_embarque_recibidor
end type
type tit_peso from statictext within w_info_embarque_recibidor
end type
type st_variedad from statictext within w_info_embarque_recibidor
end type
type st_embalaje from statictext within w_info_embarque_recibidor
end type
type cbx_embalaje from checkbox within w_info_embarque_recibidor
end type
type em_embalaje from editmask within w_info_embarque_recibidor
end type
type cb_buscaembalaje from commandbutton within w_info_embarque_recibidor
end type
type cbx_consembalaje from checkbox within w_info_embarque_recibidor
end type
type st_11 from statictext within w_info_embarque_recibidor
end type
type dw_etiqueta from datawindow within w_info_embarque_recibidor
end type
type cbx_etiqueta from checkbox within w_info_embarque_recibidor
end type
type cbx_consetiqueta from checkbox within w_info_embarque_recibidor
end type
type st_calidad from statictext within w_info_embarque_recibidor
end type
type em_calidad from editmask within w_info_embarque_recibidor
end type
type cbx_calidad from checkbox within w_info_embarque_recibidor
end type
type cbx_conscalidad from checkbox within w_info_embarque_recibidor
end type
type gb_13 from groupbox within w_info_embarque_recibidor
end type
type st_10 from statictext within w_info_embarque_recibidor
end type
type cbx_planta from checkbox within w_info_embarque_recibidor
end type
type cbx_plantascons from checkbox within w_info_embarque_recibidor
end type
type gb_23 from groupbox within w_info_embarque_recibidor
end type
type st_21 from statictext within w_info_embarque_recibidor
end type
type st_38 from statictext within w_info_embarque_recibidor
end type
type st_48 from statictext within w_info_embarque_recibidor
end type
type gb_53 from groupbox within w_info_embarque_recibidor
end type
type st_51 from statictext within w_info_embarque_recibidor
end type
type st_58 from statictext within w_info_embarque_recibidor
end type
type cbx_mercado from checkbox within w_info_embarque_recibidor
end type
type cbx_merccons from checkbox within w_info_embarque_recibidor
end type
type dw_mercado from datawindow within w_info_embarque_recibidor
end type
type cbx_recibidor from checkbox within w_info_embarque_recibidor
end type
type cbx_recibidorcons from checkbox within w_info_embarque_recibidor
end type
type dw_recibidor from datawindow within w_info_embarque_recibidor
end type
type cbx_destino from checkbox within w_info_embarque_recibidor
end type
type cbx_destcons from checkbox within w_info_embarque_recibidor
end type
type dw_destino from datawindow within w_info_embarque_recibidor
end type
type st_70 from statictext within w_info_embarque_recibidor
end type
type cbx_operacion from checkbox within w_info_embarque_recibidor
end type
type dw_operaciones from datawindow within w_info_embarque_recibidor
end type
type cbx_operacioncons from checkbox within w_info_embarque_recibidor
end type
type uo_selespecie from uo_seleccion_especie within w_info_embarque_recibidor
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_embarque_recibidor
end type
type cbx_varirotula from checkbox within w_info_embarque_recibidor
end type
type st_4 from statictext within w_info_embarque_recibidor
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_embarque_recibidor
end type
end forward

global type w_info_embarque_recibidor from w_para_informes
integer x = 14
integer y = 32
integer width = 3899
integer height = 2520
string title = "EMBARQUES HISTORICOS RESUMIDOS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
st_7 st_7
em_hasta em_hasta
st_8 st_8
gb_3 gb_3
st_5 st_5
cbx_peso cbx_peso
dw_pesoneto dw_pesoneto
tit_peso tit_peso
st_variedad st_variedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_consembalaje cbx_consembalaje
st_11 st_11
dw_etiqueta dw_etiqueta
cbx_etiqueta cbx_etiqueta
cbx_consetiqueta cbx_consetiqueta
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
gb_13 gb_13
st_10 st_10
cbx_planta cbx_planta
cbx_plantascons cbx_plantascons
gb_23 gb_23
st_21 st_21
st_38 st_38
st_48 st_48
gb_53 gb_53
st_51 st_51
st_58 st_58
cbx_mercado cbx_mercado
cbx_merccons cbx_merccons
dw_mercado dw_mercado
cbx_recibidor cbx_recibidor
cbx_recibidorcons cbx_recibidorcons
dw_recibidor dw_recibidor
cbx_destino cbx_destino
cbx_destcons cbx_destcons
dw_destino dw_destino
st_70 st_70
cbx_operacion cbx_operacion
dw_operaciones dw_operaciones
cbx_operacioncons cbx_operacioncons
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_4 st_4
uo_selproductor uo_selproductor
end type
global w_info_embarque_recibidor w_info_embarque_recibidor

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_operaciones, &
						idwc_productor, idwc_pesoneto, idwc_etiqueta, &
						idwc_recibidor, idwc_mercado, idwc_destino

String is_NomPlanta

uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad	
uo_seleccion_varios_productores	iuo_selproductor
uo_calibre								iuo_calibre
end variables

forward prototypes
public function boolean noexisteetiqueta (integer li_cliente, string ls_columna)
public function boolean existepacking (integer li_cliente, ref string ls_columna)
public function boolean existeespecie (integer cliente, integer especie)
public function boolean existeproductor (integer li_cliente, long ll_productor)
end prototypes

public function boolean noexisteetiqueta (integer li_cliente, string ls_columna);Integer	li_etiqueta
String	ls_nombre

li_etiqueta	=	Integer(ls_columna)

SELECT	etiq_nombre
INTO    :ls_nombre
FROM	dba.etiquetas
WHERE	etiq_codigo =  :li_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Etiquetas")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Etiqueta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	RETURN False 
END IF
end function

public function boolean existepacking (integer li_cliente, ref string ls_columna);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	plde_codigo =  :ls_Columna;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[7] = ls_columna	
	RETURN True 
END IF
end function

public function boolean existeespecie (integer cliente, integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dba.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean existeproductor (integer li_cliente, long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[4] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_embarque_recibidor.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.dw_pesoneto=create dw_pesoneto
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_consembalaje=create cbx_consembalaje
this.st_11=create st_11
this.dw_etiqueta=create dw_etiqueta
this.cbx_etiqueta=create cbx_etiqueta
this.cbx_consetiqueta=create cbx_consetiqueta
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.gb_13=create gb_13
this.st_10=create st_10
this.cbx_planta=create cbx_planta
this.cbx_plantascons=create cbx_plantascons
this.gb_23=create gb_23
this.st_21=create st_21
this.st_38=create st_38
this.st_48=create st_48
this.gb_53=create gb_53
this.st_51=create st_51
this.st_58=create st_58
this.cbx_mercado=create cbx_mercado
this.cbx_merccons=create cbx_merccons
this.dw_mercado=create dw_mercado
this.cbx_recibidor=create cbx_recibidor
this.cbx_recibidorcons=create cbx_recibidorcons
this.dw_recibidor=create dw_recibidor
this.cbx_destino=create cbx_destino
this.cbx_destcons=create cbx_destcons
this.dw_destino=create dw_destino
this.st_70=create st_70
this.cbx_operacion=create cbx_operacion
this.dw_operaciones=create dw_operaciones
this.cbx_operacioncons=create cbx_operacioncons
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_4=create st_4
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.dw_cliente
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_planta
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_hasta
this.Control[iCurrent+10]=this.st_8
this.Control[iCurrent+11]=this.gb_3
this.Control[iCurrent+12]=this.st_5
this.Control[iCurrent+13]=this.cbx_peso
this.Control[iCurrent+14]=this.dw_pesoneto
this.Control[iCurrent+15]=this.tit_peso
this.Control[iCurrent+16]=this.st_variedad
this.Control[iCurrent+17]=this.st_embalaje
this.Control[iCurrent+18]=this.cbx_embalaje
this.Control[iCurrent+19]=this.em_embalaje
this.Control[iCurrent+20]=this.cb_buscaembalaje
this.Control[iCurrent+21]=this.cbx_consembalaje
this.Control[iCurrent+22]=this.st_11
this.Control[iCurrent+23]=this.dw_etiqueta
this.Control[iCurrent+24]=this.cbx_etiqueta
this.Control[iCurrent+25]=this.cbx_consetiqueta
this.Control[iCurrent+26]=this.st_calidad
this.Control[iCurrent+27]=this.em_calidad
this.Control[iCurrent+28]=this.cbx_calidad
this.Control[iCurrent+29]=this.cbx_conscalidad
this.Control[iCurrent+30]=this.gb_13
this.Control[iCurrent+31]=this.st_10
this.Control[iCurrent+32]=this.cbx_planta
this.Control[iCurrent+33]=this.cbx_plantascons
this.Control[iCurrent+34]=this.gb_23
this.Control[iCurrent+35]=this.st_21
this.Control[iCurrent+36]=this.st_38
this.Control[iCurrent+37]=this.st_48
this.Control[iCurrent+38]=this.gb_53
this.Control[iCurrent+39]=this.st_51
this.Control[iCurrent+40]=this.st_58
this.Control[iCurrent+41]=this.cbx_mercado
this.Control[iCurrent+42]=this.cbx_merccons
this.Control[iCurrent+43]=this.dw_mercado
this.Control[iCurrent+44]=this.cbx_recibidor
this.Control[iCurrent+45]=this.cbx_recibidorcons
this.Control[iCurrent+46]=this.dw_recibidor
this.Control[iCurrent+47]=this.cbx_destino
this.Control[iCurrent+48]=this.cbx_destcons
this.Control[iCurrent+49]=this.dw_destino
this.Control[iCurrent+50]=this.st_70
this.Control[iCurrent+51]=this.cbx_operacion
this.Control[iCurrent+52]=this.dw_operaciones
this.Control[iCurrent+53]=this.cbx_operacioncons
this.Control[iCurrent+54]=this.uo_selespecie
this.Control[iCurrent+55]=this.uo_selvariedad
this.Control[iCurrent+56]=this.cbx_varirotula
this.Control[iCurrent+57]=this.st_4
this.Control[iCurrent+58]=this.uo_selproductor
end on

on w_info_embarque_recibidor.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.dw_pesoneto)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_consembalaje)
destroy(this.st_11)
destroy(this.dw_etiqueta)
destroy(this.cbx_etiqueta)
destroy(this.cbx_consetiqueta)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.gb_13)
destroy(this.st_10)
destroy(this.cbx_planta)
destroy(this.cbx_plantascons)
destroy(this.gb_23)
destroy(this.st_21)
destroy(this.st_38)
destroy(this.st_48)
destroy(this.gb_53)
destroy(this.st_51)
destroy(this.st_58)
destroy(this.cbx_mercado)
destroy(this.cbx_merccons)
destroy(this.dw_mercado)
destroy(this.cbx_recibidor)
destroy(this.cbx_recibidorcons)
destroy(this.dw_recibidor)
destroy(this.cbx_destino)
destroy(this.cbx_destcons)
destroy(this.dw_destino)
destroy(this.st_70)
destroy(this.cbx_operacion)
destroy(this.dw_operaciones)
destroy(this.cbx_operacioncons)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_4)
destroy(this.uo_selproductor)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

This.Icon	=	Gstr_apl.Icono

iuo_calibre   						=	Create uo_calibre

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

// uo_seleccion_productor
IF IsNull(uo_selproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	//Close(This)
	lb_Cerrar = False
ELSE
	uo_selproductor.Seleccion(True,True)
END IF

dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()
dw_etiqueta.InsertRow(0)

// iuo_selespecie = Create uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// iuo_selvariedad = Create uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
	uo_selvariedad.Enabled		=	False	
END IF

dw_recibidor.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
idwc_recibidor.Retrieve()
dw_recibidor.InsertRow(0)

dw_mercado.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(sqlca)
idwc_mercado.Retrieve()
dw_mercado.InsertRow(0)

dw_destino.GetChild("dest_codigo", idwc_destino)
idwc_destino.SetTransObject(sqlca)
idwc_destino.Retrieve(9)
dw_destino.InsertRow(0)

dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
idwc_operaciones.SetTransObject(SQLCA)
idwc_operaciones.Retrieve(gi_CodExport)
dw_operaciones.InsertRow(0)

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve()
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
//istr_mant.argumento[2]	=	"0"							//	especie
istr_mant.argumento[3]	= 	"0"							//	planta
//istr_mant.argumento[4]	= 	"0"							//	productor
//istr_mant.argumento[5]	= 	"0" 							// variedad
istr_mant.argumento[6]  =  "Z"							//	embalaje
istr_mant.argumento[7]  =  "0"							//	etiqueta
istr_mant.argumento[8]  =  "Z"							//	calidad
istr_mant.argumento[9]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[10]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[11] =  "1"							//	peso

istr_mant.argumento[30]	= 	"0"							//	Mercado
istr_mant.argumento[31]	= 	"0"							//	Destino
istr_mant.argumento[32]	= 	"0" 							// Recibidor
istr_mant.argumento[33]	= 	"0" 							// Operacion

istr_mant.argumento[13] =  "1"							//	Consolidado Planta
istr_mant.argumento[14] =  "1"							//	Consolidado Productor
//istr_mant.argumento[15] =  "1"							//	Consolidado Variedad
istr_mant.argumento[16] =  "1"							//	Consolidado Embalaje
istr_mant.argumento[17] =  "1"							//	Consolidado Etiqueta
istr_mant.argumento[18] =  "1"							//	Consolidado Calidad

istr_mant.argumento[20] =  "1"							//	Consolidado Mercado
istr_mant.argumento[21] =  "1"							//	Consolidado Destino
istr_mant.argumento[22] =  "1"							//	Consolidado Recibidor
istr_mant.argumento[23] =  "1"							//	Consolidado Operacion
//istr_mant.argumento[24] =  "1"							//	Consolidado Especie

cbx_planta.Enabled		=	False
cbx_mercado.Enabled		=	False
cbx_recibidor.Enabled	=	False
cbx_embalaje.Enabled		=	False
cbx_etiqueta.Enabled		=	False
cbx_calidad.Enabled		=	False
cbx_destino.Enabled		=	False
cbx_operacion.Enabled	=	False
end event

type st_computador from w_para_informes`st_computador within w_info_embarque_recibidor
end type

type st_usuario from w_para_informes`st_usuario within w_info_embarque_recibidor
end type

type st_temporada from w_para_informes`st_temporada within w_info_embarque_recibidor
end type

type p_logo from w_para_informes`p_logo within w_info_embarque_recibidor
end type

type st_titulo from w_para_informes`st_titulo within w_info_embarque_recibidor
integer width = 3195
integer height = 84
string text = "Informe de Embarques Recibidores"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_embarque_recibidor
string tag = "Imprimir Reporte"
integer x = 3561
integer y = 1716
integer taborder = 140
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_varirotula, li_etiqueta, &
			li_mercado, li_destino, li_operacion, li_consplanta, li_consproductor,&
			li_consvariedad, li_consembalaje, li_consetiqueta, li_conscalidad, &
			li_consmercado, li_consdestino, li_consrecibidor, li_consoperacion, &
			li_consespecie
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_cajas, ls_embalaje, ls_calidad, ls_lista
Long		ll_productor, ll_recibidor

istr_info.titulo	= 'INFORME DE EMBARQUES CONSIGNATARIOS'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_embarques_recibidor"

// Control de Argumentos de Clientes // 

	istr_mant.argumento[1]	=	string(dw_cliente.GetItemNumber(dw_cliente.Getrow(), "clie_codigo"))

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF	

// Control de Argumentos de Planta // 

IF cbx_planta.Checked THEN
	istr_mant.argumento[3]	=	"0"
	istr_mant.argumento[13]	=	"0"
END IF

IF cbx_plantascons.Checked THEN
	istr_mant.argumento[13]	=	"1"
ELSE
	istr_mant.argumento[13]	=	"0"
END IF

IF NOT cbx_planta.Checked AND NOT cbx_plantascons.Checked THEN
	istr_mant.argumento[3]	=	string(dw_planta.GetItemNumber(dw_planta.Getrow(), "plde_codigo"))
END IF	

// Control de Argumentos de Productor // 

/*
productor
*/
ls_lista = uo_selproductor.Lista

// Control de Argumentos de Embalaje //

IF cbx_embalaje.Checked THEN
	istr_mant.argumento[6]	=	"Z"
	istr_mant.argumento[16]	=	"0"
END IF

IF cbx_consembalaje.Checked THEN
	istr_mant.argumento[16]	=	"1"
ELSE
	istr_mant.argumento[16]	=	"0"
END IF

// Control de Argumentos de Etiqueta //

IF cbx_etiqueta.Checked THEN
	istr_mant.argumento[7]	=	"0"
	istr_mant.argumento[17]	=	"0"
ELSE	
	istr_mant.argumento[17]	=	"0"
END IF

IF cbx_consetiqueta.Checked THEN
	istr_mant.argumento[17]	=	"1"
ELSE	
	istr_mant.argumento[17]	=	"0"
END IF

IF NOT cbx_etiqueta.Checked AND NOT cbx_consetiqueta.Checked THEN
	istr_mant.argumento[7]	=	string(dw_etiqueta.GetItemNumber(dw_etiqueta.Getrow(), "etiq_codigo"))
END IF	

// Control de Argumentos de Calidad //

IF cbx_calidad.Checked THEN
	istr_mant.argumento[8]	=	"Z"
	istr_mant.argumento[18]	=	"0"
END IF

IF cbx_conscalidad.Checked THEN
	istr_mant.argumento[18]	=	"1"
ELSE	
	istr_mant.argumento[18]	=	"0"
END IF

// Control de Argumentos de Mercado //

IF cbx_mercado.Checked THEN
	istr_mant.argumento[30]	=	"0"
END IF

IF cbx_merccons.Checked THEN
	istr_mant.argumento[20]	=	"1"
ELSE	
	istr_mant.argumento[20]	=	"0"
END IF

// Control de Argumentos de Recibidor //

IF cbx_recibidor.Checked THEN
	istr_mant.argumento[32]	=	"0"
END IF

IF cbx_recibidorcons.Checked THEN
	istr_mant.argumento[22]	=	"1"
ELSE	
	istr_mant.argumento[22]	=	"0"
END IF

// Control de Argumentos de Recibidor //

IF cbx_destino.Checked THEN
	istr_mant.argumento[31]	=	"0"
END IF

IF cbx_destcons.Checked THEN
	istr_mant.argumento[21]	=	"1"
ELSE	
	istr_mant.argumento[21]	=	"0"
END IF

// Control de Argumentos de Operación //

IF cbx_operacion.Checked THEN
	istr_mant.argumento[33]	=	"0"
END IF

IF cbx_operacioncons.Checked THEN
	istr_mant.argumento[23]	=	"1"
ELSE	
	istr_mant.argumento[23]	=	"0"
END IF


// Asignación de Argumentos //

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[3])
ll_productor	=	Long(istr_mant.argumento[4])
ls_embalaje		=	istr_mant.argumento[6]
li_etiqueta		=	Integer(istr_mant.argumento[7])
ls_calidad		=	istr_mant.argumento[8]
ld_desde			=	Date(istr_mant.argumento[9])
ld_hasta			=	Date(istr_mant.argumento[10])
li_mercado		=	Integer(istr_mant.argumento[30])
li_destino		=	Integer(istr_mant.argumento[31])
ll_recibidor	=	Long(istr_mant.argumento[32])
li_operacion	=	Integer(istr_mant.argumento[33])

texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_peso.Checked=False THEN
	ls_cajas = "Bultos"
	istr_mant.argumento[11]	=	"1"
ELSE
	istr_mant.argumento[11]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[11] 
END IF

li_consplanta		=	Integer(istr_mant.argumento[13])
li_consembalaje	=	Integer(istr_mant.argumento[16])
li_consetiqueta	=	Integer(istr_mant.argumento[17])
li_conscalidad		=	Integer(istr_mant.argumento[18])
li_consmercado		=	Integer(istr_mant.argumento[20])
li_consdestino		=	Integer(istr_mant.argumento[21])
li_consrecibidor	=	Integer(istr_mant.argumento[22])
li_consoperacion	=	Integer(istr_mant.argumento[23])

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(li_cliente, uo_selEspecie.Codigo, li_planta,&
									 uo_selvariedad.Codigo, ls_embalaje, li_etiqueta, ls_calidad,&
									 ld_desde, ld_hasta, Dec(istr_mant.argumento[11]), li_consplanta,&
									 li_consembalaje, li_consetiqueta, li_conscalidad, &
									 li_mercado, li_destino, ll_recibidor, li_consmercado, li_consdestino, &
									 li_consrecibidor, li_operacion, li_consoperacion, li_varirotula, ls_lista)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	   F_Membrete(vinf.dw_1)
//		vinf.dw_1.Object.titulo_informe.text = 'Producción Historica'
		vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		vinf.dw_1.Modify("Cajas.text = '" + ls_cajas + "'")
		vinf.dw_1.Modify("cliente.text = '" + gstr_apl.razon_social+ "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_embarque_recibidor
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3566
integer y = 2008
integer taborder = 150
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_embarque_recibidor
integer x = 302
integer y = 892
integer width = 238
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_embarque_recibidor
integer x = 302
integer y = 1428
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Inicio Desp."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_embarque_recibidor
integer x = 681
integer y = 1412
integer width = 375
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[9]	=	This.Text
end event

type dw_cliente from datawindow within w_info_embarque_recibidor
integer x = 645
integer y = 472
integer width = 1143
integer height = 104
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null

SetNull(ls_null)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	idwc_planta.Retrieve(1)
	idwc_productor.Retrieve()	
	idwc_etiqueta.Retrieve()
	
	dw_recibidor.GetChild("reci_codigo", idwc_recibidor)
	idwc_recibidor.SetTransObject(sqlca)
	idwc_recibidor.Retrieve()
	dw_recibidor.InsertRow(0)		
	
	dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
	idwc_operaciones.SetTransObject(SQLCA)
	idwc_operaciones.Retrieve()
	dw_operaciones.InsertRow(0)
	
	uo_selproductor.Filtra(-1,-1,Integer(data))
	
ELSE
	This.SetItem(1, "clie_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_embarque_recibidor
integer x = 302
integer y = 472
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_embarque_recibidor
integer x = 645
integer y = 892
integer width = 969
integer height = 92
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_null
Integer	li_Cliente

SetNull(ls_null)

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_embarque_recibidor
integer x = 302
integer y = 704
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_embarque_recibidor
integer x = 1065
integer y = 1428
integer width = 439
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Final Desp."
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_embarque_recibidor
integer x = 1408
integer y = 1412
integer width = 375
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[10]	=	This.Text
end event

type st_8 from statictext within w_info_embarque_recibidor
integer x = 302
integer y = 1140
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_embarque_recibidor
integer x = 279
integer y = 1348
integer width = 1527
integer height = 212
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_5 from statictext within w_info_embarque_recibidor
integer x = 1879
integer y = 440
integer width = 1563
integer height = 896
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_peso from checkbox within w_info_embarque_recibidor
integer x = 1870
integer y = 1424
integer width = 631
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))
END IF

end event

type dw_pesoneto from datawindow within w_info_embarque_recibidor
integer x = 2702
integer y = 1420
integer width = 686
integer height = 100
integer taborder = 130
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type tit_peso from statictext within w_info_embarque_recibidor
integer x = 2528
integer y = 1432
integer width = 155
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_embarque_recibidor
integer x = 1934
integer y = 540
integer width = 302
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_embalaje from statictext within w_info_embarque_recibidor
integer x = 1934
integer y = 776
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_embarque_recibidor
integer x = 2263
integer y = 672
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consembalaje.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	//istr_mant.argumento[6]		=	'Z'
	//istr_mant.argumento[16]		=	'0'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type em_embalaje from editmask within w_info_embarque_recibidor
integer x = 2263
integer y = 764
integer width = 297
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dba.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_embarque_recibidor
integer x = 2574
integer y = 772
integer width = 96
integer height = 84
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;
Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[6]	=	lstr_busq.argum[2]
END IF
end event

type cbx_consembalaje from checkbox within w_info_embarque_recibidor
integer x = 2779
integer y = 676
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	//istr_mant.argumento[16]	=	'1'
	cbx_embalaje.Enabled	=	False
	cbx_embalaje.Checked	=	True
ELSE
	//istr_mant.argumento[16]	=	'0'
	cbx_embalaje.Enabled	=	True

END IF

end event

type st_11 from statictext within w_info_embarque_recibidor
integer x = 1934
integer y = 980
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Etiqueta"
boolean focusrectangle = false
end type

type dw_etiqueta from datawindow within w_info_embarque_recibidor
integer x = 2263
integer y = 988
integer width = 891
integer height = 96
integer taborder = 110
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_etiquetas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_cliente

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente

IF NoExisteEtiqueta(li_cliente, data) THEN
	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
	dw_etiqueta.SetFocus()
	RETURN 1
ELSE
	istr_mant.argumento[7]	=	data
	
END IF


end event

event itemerror;RETURN 1
end event

type cbx_etiqueta from checkbox within w_info_embarque_recibidor
integer x = 2263
integer y = 904
integer width = 293
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consetiqueta.Enabled									=	True
	dw_etiqueta.Enabled										=	False
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(166,180,210)
	//istr_mant.argumento[7]									=	'0'
	//istr_mant.argumento[17]									=	'0'
ELSE
	cbx_consetiqueta.Enabled									=	False
	cbx_consetiqueta.Checked									=	False
	dw_etiqueta.Enabled											=	True
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_etiqueta.SetFocus()
	//istr_mant.argumento[17]	=	'0'
END IF
end event

type cbx_consetiqueta from checkbox within w_info_embarque_recibidor
integer x = 2779
integer y = 904
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	//istr_mant.argumento[17]	=	'1'
	cbx_etiqueta.Enabled	=	False
	cbx_etiqueta.Checked	=	True

ELSE
	//istr_mant.argumento[17]	=	'0'
	cbx_etiqueta.Enabled	=	True
END IF

end event

type st_calidad from statictext within w_info_embarque_recibidor
integer x = 1934
integer y = 1192
integer width = 256
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_embarque_recibidor
integer x = 2263
integer y = 1180
integer width = 297
integer height = 96
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[8]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	

end event

type cbx_calidad from checkbox within w_info_embarque_recibidor
integer x = 2263
integer y = 1096
integer width = 297
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	//istr_mant.argumento[8]		=	'Z'
	//istr_mant.argumento[18]		=	'0'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type cbx_conscalidad from checkbox within w_info_embarque_recibidor
integer x = 2779
integer y = 1096
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	//istr_mant.argumento[18]	=	'1'
	cbx_calidad.Enabled	=	False
	cbx_calidad.Checked	=	True

ELSE
	//istr_mant.argumento[18]	=	'0'
	cbx_calidad.Enabled	=	True
END IF

end event

type gb_13 from groupbox within w_info_embarque_recibidor
integer x = 1829
integer y = 1348
integer width = 1586
integer height = 212
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_10 from statictext within w_info_embarque_recibidor
integer x = 247
integer y = 1336
integer width = 3195
integer height = 380
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_embarque_recibidor
integer x = 649
integer y = 812
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_plantascons.Enabled									=	True
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
	//istr_mant.argumento[3]									=	'0'
	//istr_mant.argumento[13]									=	'0'
ELSE
	cbx_plantascons.Enabled									=	False
	cbx_plantascons.Checked									=	False
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)

	dw_planta.SetFocus()
END IF
end event

type cbx_plantascons from checkbox within w_info_embarque_recibidor
integer x = 1125
integer y = 812
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	//istr_mant.argumento[13]	=	'1'
	cbx_planta.Enabled	=	False
	cbx_planta.Checked	=	True
ELSE
	//istr_mant.argumento[13]	=	'0'
	cbx_planta.Enabled	=	True
END IF
	
end event

type gb_23 from groupbox within w_info_embarque_recibidor
integer x = 279
integer y = 1740
integer width = 1568
integer height = 460
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_21 from statictext within w_info_embarque_recibidor
integer x = 247
integer y = 1716
integer width = 1627
integer height = 528
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_38 from statictext within w_info_embarque_recibidor
integer x = 302
integer y = 1856
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Mercado"
boolean focusrectangle = false
end type

type st_48 from statictext within w_info_embarque_recibidor
integer x = 1957
integer y = 1856
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Destino"
boolean focusrectangle = false
end type

type gb_53 from groupbox within w_info_embarque_recibidor
integer x = 1902
integer y = 1740
integer width = 1518
integer height = 460
integer taborder = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_51 from statictext within w_info_embarque_recibidor
integer x = 1879
integer y = 1716
integer width = 1563
integer height = 528
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_58 from statictext within w_info_embarque_recibidor
integer x = 302
integer y = 2076
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Recibidor"
boolean focusrectangle = false
end type

type cbx_mercado from checkbox within w_info_embarque_recibidor
integer x = 645
integer y = 1796
integer width = 366
integer height = 68
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_mercado.Enabled										=	False
	dw_mercado.Object.merc_codigo.BackGround.Color	=	RGB(166,180,210)
	//istr_mant.argumento[30]									=	'0'
ELSE
	dw_mercado.Enabled										=	True
	dw_mercado.Object.merc_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_mercado.SetFocus()
END IF

	
end event

type cbx_merccons from checkbox within w_info_embarque_recibidor
integer x = 1047
integer y = 1796
integer width = 471
integer height = 68
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	//istr_mant.argumento[20]	=	'1'
	cbx_mercado.Enabled	=	False
	cbx_mercado.Checked	=	True
ELSE
//	istr_mant.argumento[20]	=	'0'
	cbx_mercado.Enabled	=	True

END IF
end event

type dw_mercado from datawindow within w_info_embarque_recibidor
integer x = 645
integer y = 1864
integer width = 887
integer height = 92
integer taborder = 220
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_mercado"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[30]	=	data
idwc_destino.Reset()
idwc_destino.Retrieve(Integer(istr_mant.argumento[30]))

end event

event itemerror;RETURN 1
end event

type cbx_recibidor from checkbox within w_info_embarque_recibidor
integer x = 645
integer y = 1988
integer width = 402
integer height = 72
integer taborder = 220
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_recibidorcons.Enabled									=	True
	dw_recibidor.Enabled											=	False
	dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(166,180,210)
	//istr_mant.argumento[32]										=	'0'
ELSE
	cbx_recibidorcons.Enabled									=	False
	cbx_recibidorcons.Checked									=	False
	dw_recibidor.Enabled											=	True
	dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_recibidor.SetFocus()
END IF

	
end event

type cbx_recibidorcons from checkbox within w_info_embarque_recibidor
integer x = 1047
integer y = 1988
integer width = 471
integer height = 72
integer taborder = 230
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	//istr_mant.argumento[22]	=	'1'
	cbx_recibidor.Enabled	=	False
	cbx_recibidor.Checked	=	True
	
ELSE
	//istr_mant.argumento[22]	=	'0'
	cbx_recibidor.Enabled	=	True

END IF
	
end event

type dw_recibidor from datawindow within w_info_embarque_recibidor
integer x = 645
integer y = 2064
integer width = 1125
integer height = 92
integer taborder = 260
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_consignatarios"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[32]	=	data

end event

event itemerror;RETURN 1
end event

type cbx_destino from checkbox within w_info_embarque_recibidor
integer x = 2286
integer y = 1796
integer width = 366
integer height = 68
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_destino.Enabled										=	False
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(166,180,210)
	//istr_mant.argumento[31]									=	'0'
ELSE
	dw_destino.Enabled										=	True
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_destino.SetFocus()
END IF

	
end event

type cbx_destcons from checkbox within w_info_embarque_recibidor
integer x = 2688
integer y = 1796
integer width = 471
integer height = 68
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	//istr_mant.argumento[21]	=	'1'
	cbx_destino.Enabled	=	False
	cbx_destino.Checked	=	True
	
ELSE
	//istr_mant.argumento[21]	=	'0'
	cbx_destino.Enabled	=	True

END IF
end event

type dw_destino from datawindow within w_info_embarque_recibidor
integer x = 2286
integer y = 1864
integer width = 887
integer height = 92
integer taborder = 230
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[31]	=	data

end event

event itemerror;RETURN 1
end event

type st_70 from statictext within w_info_embarque_recibidor
integer x = 1957
integer y = 2076
integer width = 315
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Operacion"
boolean focusrectangle = false
end type

type cbx_operacion from checkbox within w_info_embarque_recibidor
integer x = 2286
integer y = 1988
integer width = 379
integer height = 72
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_operacioncons.Enabled									=	True
	dw_operaciones.Enabled											=	False
	dw_operaciones.Object.oper_codigo.BackGround.Color	=	RGB(166,180,210)
	//istr_mant.argumento[33]										=	'0'
ELSE
	cbx_operacioncons.Enabled									=	False
	cbx_operacioncons.Checked									=	False
	dw_operaciones.Enabled											=	True
	dw_operaciones.Object.oper_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_operaciones.SetFocus()
END IF

	
end event

type dw_operaciones from datawindow within w_info_embarque_recibidor
integer x = 2286
integer y = 2064
integer width = 974
integer height = 92
integer taborder = 100
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_operaciones"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[33]	=	data

end event

event itemerror;RETURN 1
end event

type cbx_operacioncons from checkbox within w_info_embarque_recibidor
integer x = 2688
integer y = 1988
integer width = 471
integer height = 72
integer taborder = 230
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	//istr_mant.argumento[23]	=	'1'
	cbx_operacion.Enabled	=	False
	cbx_operacion.Checked	=	True

ELSE
	//istr_mant.argumento[23]	=	'0'
	cbx_operacion.Enabled	=	True

END IF
	
end event

type uo_selespecie from uo_seleccion_especie within w_info_embarque_recibidor
event destroy ( )
integer x = 645
integer y = 604
integer height = 180
integer taborder = 260
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled	=	True
		uo_selvariedad.Enabled						=	False		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.Enabled						=	True	
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_embarque_recibidor
event destroy ( )
integer x = 2263
integer y = 468
integer taborder = 280
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_embarque_recibidor
integer x = 1591
integer y = 1596
integer width = 626
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Variedad Rotulada"
end type

type st_4 from statictext within w_info_embarque_recibidor
integer x = 247
integer y = 440
integer width = 1627
integer height = 896
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_embarque_recibidor
integer x = 649
integer y = 1004
integer taborder = 110
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

