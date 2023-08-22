$PBExportHeader$w_info_resumen_despachos_porpallet.srw
forward
global type w_info_resumen_despachos_porpallet from w_para_informes
end type
type st_4 from statictext within w_info_resumen_despachos_porpallet
end type
type st_1 from statictext within w_info_resumen_despachos_porpallet
end type
type st_2 from statictext within w_info_resumen_despachos_porpallet
end type
type em_desde from editmask within w_info_resumen_despachos_porpallet
end type
type dw_cliente from datawindow within w_info_resumen_despachos_porpallet
end type
type st_6 from statictext within w_info_resumen_despachos_porpallet
end type
type dw_planta from datawindow within w_info_resumen_despachos_porpallet
end type
type st_3 from statictext within w_info_resumen_despachos_porpallet
end type
type st_7 from statictext within w_info_resumen_despachos_porpallet
end type
type em_hasta from editmask within w_info_resumen_despachos_porpallet
end type
type st_variedad from statictext within w_info_resumen_despachos_porpallet
end type
type st_embalaje from statictext within w_info_resumen_despachos_porpallet
end type
type cbx_embalaje from checkbox within w_info_resumen_despachos_porpallet
end type
type em_embalaje from editmask within w_info_resumen_despachos_porpallet
end type
type cb_buscaembalaje from commandbutton within w_info_resumen_despachos_porpallet
end type
type cbx_consembalaje from checkbox within w_info_resumen_despachos_porpallet
end type
type st_11 from statictext within w_info_resumen_despachos_porpallet
end type
type dw_etiqueta from datawindow within w_info_resumen_despachos_porpallet
end type
type cbx_etiqueta from checkbox within w_info_resumen_despachos_porpallet
end type
type cbx_consetiqueta from checkbox within w_info_resumen_despachos_porpallet
end type
type cbx_planta from checkbox within w_info_resumen_despachos_porpallet
end type
type st_21 from statictext within w_info_resumen_despachos_porpallet
end type
type st_38 from statictext within w_info_resumen_despachos_porpallet
end type
type st_48 from statictext within w_info_resumen_despachos_porpallet
end type
type st_51 from statictext within w_info_resumen_despachos_porpallet
end type
type st_58 from statictext within w_info_resumen_despachos_porpallet
end type
type cbx_mercado from checkbox within w_info_resumen_despachos_porpallet
end type
type cbx_merccons from checkbox within w_info_resumen_despachos_porpallet
end type
type dw_mercado from datawindow within w_info_resumen_despachos_porpallet
end type
type cbx_recibidor from checkbox within w_info_resumen_despachos_porpallet
end type
type cbx_recibidorcons from checkbox within w_info_resumen_despachos_porpallet
end type
type dw_recibidor from datawindow within w_info_resumen_despachos_porpallet
end type
type cbx_destino from checkbox within w_info_resumen_despachos_porpallet
end type
type cbx_destcons from checkbox within w_info_resumen_despachos_porpallet
end type
type dw_destino from datawindow within w_info_resumen_despachos_porpallet
end type
type st_70 from statictext within w_info_resumen_despachos_porpallet
end type
type cbx_operacion from checkbox within w_info_resumen_despachos_porpallet
end type
type dw_operaciones from datawindow within w_info_resumen_despachos_porpallet
end type
type cbx_operacioncons from checkbox within w_info_resumen_despachos_porpallet
end type
type st_12 from statictext within w_info_resumen_despachos_porpallet
end type
type cbx_puertos from checkbox within w_info_resumen_despachos_porpallet
end type
type cbx_puertoscons from checkbox within w_info_resumen_despachos_porpallet
end type
type st_10 from statictext within w_info_resumen_despachos_porpallet
end type
type st_5 from statictext within w_info_resumen_despachos_porpallet
end type
type dw_puertos from datawindow within w_info_resumen_despachos_porpallet
end type
type st_8 from statictext within w_info_resumen_despachos_porpallet
end type
type cbx_porpallet from checkbox within w_info_resumen_despachos_porpallet
end type
type uo_selespecie from uo_seleccion_especie within w_info_resumen_despachos_porpallet
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_resumen_despachos_porpallet
end type
type cbx_varirotula from checkbox within w_info_resumen_despachos_porpallet
end type
type st_14 from statictext within w_info_resumen_despachos_porpallet
end type
type cbx_embarque from checkbox within w_info_resumen_despachos_porpallet
end type
type dw_embarques from datawindow within w_info_resumen_despachos_porpallet
end type
type st_9 from statictext within w_info_resumen_despachos_porpallet
end type
type em_calidad from editmask within w_info_resumen_despachos_porpallet
end type
type cbx_calidad from checkbox within w_info_resumen_despachos_porpallet
end type
type cbx_calidadcons from checkbox within w_info_resumen_despachos_porpallet
end type
type cbx_1 from checkbox within w_info_resumen_despachos_porpallet
end type
type st_17 from statictext within w_info_resumen_despachos_porpallet
end type
type uo_selcondicion from uo_seleccion_condicion within w_info_resumen_despachos_porpallet
end type
end forward

global type w_info_resumen_despachos_porpallet from w_para_informes
integer x = 14
integer y = 32
integer width = 3863
integer height = 2268
string title = "Resumen Despachos por Pallet"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_3 st_3
st_7 st_7
em_hasta em_hasta
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
cbx_planta cbx_planta
st_21 st_21
st_38 st_38
st_48 st_48
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
st_12 st_12
cbx_puertos cbx_puertos
cbx_puertoscons cbx_puertoscons
st_10 st_10
st_5 st_5
dw_puertos dw_puertos
st_8 st_8
cbx_porpallet cbx_porpallet
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_14 st_14
cbx_embarque cbx_embarque
dw_embarques dw_embarques
st_9 st_9
em_calidad em_calidad
cbx_calidad cbx_calidad
cbx_calidadcons cbx_calidadcons
cbx_1 cbx_1
st_17 st_17
uo_selcondicion uo_selcondicion
end type
global w_info_resumen_despachos_porpallet w_info_resumen_despachos_porpallet

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta, idwc_especie, idwc_etiqueta, idwc_recibidor, & 
						idwc_mercado, idwc_destino, idwc_operaciones, idwc_puertos, idwc_embarque
						
String is_NomPlanta

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
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
FROM	dbo.etiquetas
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
FROM	dbo.plantadesp
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
	FROM	dbo.especies
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
	FROM	dbo.productores
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

on w_info_resumen_despachos_porpallet.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
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
this.cbx_planta=create cbx_planta
this.st_21=create st_21
this.st_38=create st_38
this.st_48=create st_48
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
this.st_12=create st_12
this.cbx_puertos=create cbx_puertos
this.cbx_puertoscons=create cbx_puertoscons
this.st_10=create st_10
this.st_5=create st_5
this.dw_puertos=create dw_puertos
this.st_8=create st_8
this.cbx_porpallet=create cbx_porpallet
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_14=create st_14
this.cbx_embarque=create cbx_embarque
this.dw_embarques=create dw_embarques
this.st_9=create st_9
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_calidadcons=create cbx_calidadcons
this.cbx_1=create cbx_1
this.st_17=create st_17
this.uo_selcondicion=create uo_selcondicion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_7
this.Control[iCurrent+10]=this.em_hasta
this.Control[iCurrent+11]=this.st_variedad
this.Control[iCurrent+12]=this.st_embalaje
this.Control[iCurrent+13]=this.cbx_embalaje
this.Control[iCurrent+14]=this.em_embalaje
this.Control[iCurrent+15]=this.cb_buscaembalaje
this.Control[iCurrent+16]=this.cbx_consembalaje
this.Control[iCurrent+17]=this.st_11
this.Control[iCurrent+18]=this.dw_etiqueta
this.Control[iCurrent+19]=this.cbx_etiqueta
this.Control[iCurrent+20]=this.cbx_consetiqueta
this.Control[iCurrent+21]=this.cbx_planta
this.Control[iCurrent+22]=this.st_21
this.Control[iCurrent+23]=this.st_38
this.Control[iCurrent+24]=this.st_48
this.Control[iCurrent+25]=this.st_51
this.Control[iCurrent+26]=this.st_58
this.Control[iCurrent+27]=this.cbx_mercado
this.Control[iCurrent+28]=this.cbx_merccons
this.Control[iCurrent+29]=this.dw_mercado
this.Control[iCurrent+30]=this.cbx_recibidor
this.Control[iCurrent+31]=this.cbx_recibidorcons
this.Control[iCurrent+32]=this.dw_recibidor
this.Control[iCurrent+33]=this.cbx_destino
this.Control[iCurrent+34]=this.cbx_destcons
this.Control[iCurrent+35]=this.dw_destino
this.Control[iCurrent+36]=this.st_70
this.Control[iCurrent+37]=this.cbx_operacion
this.Control[iCurrent+38]=this.dw_operaciones
this.Control[iCurrent+39]=this.cbx_operacioncons
this.Control[iCurrent+40]=this.st_12
this.Control[iCurrent+41]=this.cbx_puertos
this.Control[iCurrent+42]=this.cbx_puertoscons
this.Control[iCurrent+43]=this.st_10
this.Control[iCurrent+44]=this.st_5
this.Control[iCurrent+45]=this.dw_puertos
this.Control[iCurrent+46]=this.st_8
this.Control[iCurrent+47]=this.cbx_porpallet
this.Control[iCurrent+48]=this.uo_selespecie
this.Control[iCurrent+49]=this.uo_selvariedad
this.Control[iCurrent+50]=this.cbx_varirotula
this.Control[iCurrent+51]=this.st_14
this.Control[iCurrent+52]=this.cbx_embarque
this.Control[iCurrent+53]=this.dw_embarques
this.Control[iCurrent+54]=this.st_9
this.Control[iCurrent+55]=this.em_calidad
this.Control[iCurrent+56]=this.cbx_calidad
this.Control[iCurrent+57]=this.cbx_calidadcons
this.Control[iCurrent+58]=this.cbx_1
this.Control[iCurrent+59]=this.st_17
this.Control[iCurrent+60]=this.uo_selcondicion
end on

on w_info_resumen_despachos_porpallet.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
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
destroy(this.cbx_planta)
destroy(this.st_21)
destroy(this.st_38)
destroy(this.st_48)
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
destroy(this.st_12)
destroy(this.cbx_puertos)
destroy(this.cbx_puertoscons)
destroy(this.st_10)
destroy(this.st_5)
destroy(this.dw_puertos)
destroy(this.st_8)
destroy(this.cbx_porpallet)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_14)
destroy(this.cbx_embarque)
destroy(this.dw_embarques)
destroy(this.st_9)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_calidadcons)
destroy(this.cbx_1)
destroy(this.st_17)
destroy(this.uo_selcondicion)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

IF IsNull(uo_selcondicion.Codigo) THEN lb_Cerrar	=	True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selcondicion.Seleccion(True, True)
END IF

iuo_calibre   						=	Create uo_calibre

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_planta.Enabled											=	False
dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()
dw_etiqueta.InsertRow(0)

dw_etiqueta.Enabled											=	False
dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(192, 192, 192)

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
END IF

dw_embarques.GetChild("embq_codigo", idwc_embarque)
idwc_embarque.SetTransObject(SQLCA)
idwc_embarque.Retrieve(gi_CodExport, 0)
dw_embarques.InsertRow(0)

dw_recibidor.GetChild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
idwc_recibidor.Retrieve()
dw_recibidor.InsertRow(0)

dw_recibidor.Enabled											=	False
dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_puertos.GetChild("puer_codigo", idwc_puertos)
idwc_puertos.SetTransObject(sqlca)
idwc_puertos.Retrieve(900)
dw_puertos.InsertRow(0)

dw_puertos.Enabled											=	False
dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_mercado.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(sqlca)
idwc_mercado.Retrieve()
dw_mercado.InsertRow(0)

dw_mercado.Enabled										=	False
dw_mercado.Object.merc_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_destino.GetChild("dest_codigo", idwc_destino)
idwc_destino.SetTransObject(sqlca)
idwc_destino.Retrieve(0)
dw_destino.InsertRow(0)

dw_destino.Enabled										=	False
dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
idwc_operaciones.SetTransObject(SQLCA)
idwc_operaciones.Retrieve(gi_CodExport)
dw_operaciones.InsertRow(0)

dw_operaciones.Enabled										=	False
dw_operaciones.Object.oper_codigo.BackGround.Color	=	RGB(192, 192, 192)

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())

istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
//istr_mant.argumento[2]	=	String(gi_CodEspecie)	//	especie
istr_mant.argumento[2]	=	"-1"
istr_mant.argumento[3]	= 	"-1"							//	planta
//istr_mant.argumento[4]	= 	"-9" 							// variedad
istr_mant.argumento[5]  =  "-9"							//	embalaje
istr_mant.argumento[6]  =  "-9"							//	etiqueta
istr_mant.argumento[7]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[8]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[9]	= 	"1"							//	por pallets
istr_mant.argumento[10]	= 	"-9" 							// Operacion
istr_mant.argumento[11]	= 	"-9" 							// Puertos
istr_mant.argumento[12]	= 	"-9"							//	Mercado
istr_mant.argumento[13]	= 	"-9"							//	Destino
istr_mant.argumento[14]	= 	"-9" 							// Recibidor
istr_mant.argumento[15] =  "-1"							//	embarque
istr_mant.argumento[20] =  "-9"							//	calidad
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_despachos_porpallet
integer x = 3483
integer y = 968
end type

type st_computador from w_para_informes`st_computador within w_info_resumen_despachos_porpallet
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_despachos_porpallet
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_despachos_porpallet
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_despachos_porpallet
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_despachos_porpallet
integer width = 3086
string text = "Resumen Despachos por Pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_despachos_porpallet
string tag = "Imprimir Reporte"
integer x = 3424
integer y = 1488
integer taborder = 320
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	li_Cliente,  li_Planta, li_Etiqueta, li_PorPallet, li_Operacion, &
			li_Puerto, li_Mercado, li_Destino,li_varirotula, li_condicion
String	ls_Embalaje, texto_desde, texto_hasta, texto_fecha, ls_Embarque, ls_calidad
Date		ld_Desde, ld_Hasta
Long		Fila, ll_Recibidor

istr_info.titulo	= 'RESUMEN DESPACHO POR PALLET'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_resumen_despachos_porpallet_2"
vinf.dw_1.SetTransObject(sqlca)

IF cbx_operacion.Checked=False THEN
	istr_mant.argumento[10]	=	String(dw_operaciones.Object.oper_codigo[1])
END IF

/*
Condicion
*/
li_condicion	=  uo_selcondicion.Codigo

IF IsNull(li_condicion)THEN
	MessageBox("Atención","Debe Seleccionar una Condición Previamente",Exclamation!)
	RETURN
END IF

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

li_Cliente 		=	Integer(istr_mant.argumento[1])
li_Planta			=	Integer(istr_mant.argumento[3])
ls_Embalaje		=	istr_mant.argumento[5]
li_Etiqueta		=	Integer(istr_mant.argumento[6])
ld_Desde			=	Date(istr_mant.argumento[7])
ld_Hasta			=	Date(istr_mant.argumento[8])
li_Operacion		=	Integer(istr_mant.argumento[10])
li_Puerto   		=  Integer(istr_mant.argumento[11])
li_Mercado		=	Integer(istr_mant.argumento[12])
li_Destino		=	Integer(istr_mant.argumento[13])
ll_Recibidor		=	Long(istr_mant.argumento[14])
ls_Embarque	=	istr_mant.argumento[15]
ls_calidad		=	istr_mant.argumento[20]

IF cbx_porpallet.checked THEN
	li_PorPallet	= 0
ELSE
	li_PorPallet	= 1
END IF	

texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

fila = vinf.dw_1.Retrieve(li_Cliente,uo_selespecie.Codigo,li_Planta,uo_selvariedad.Codigo,ls_Embalaje, &
								  li_Etiqueta,ld_Desde,ld_Hasta,li_PorPallet,li_Operacion,li_Puerto, &
								  li_Mercado,li_Destino,ll_Recibidor,li_varirotula,ls_Embarque, ls_Calidad, li_condicion)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_despachos_porpallet
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3424
integer y = 1768
integer taborder = 330
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_resumen_despachos_porpallet
integer x = 247
integer y = 444
integer width = 1550
integer height = 396
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_resumen_despachos_porpallet
integer x = 302
integer y = 672
integer width = 238
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumen_despachos_porpallet
integer x = 302
integer y = 1124
integer width = 315
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Inicio Desp."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_resumen_despachos_porpallet
integer x = 585
integer y = 1100
integer width = 411
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[7]	=	This.Text
end event

type dw_cliente from datawindow within w_info_resumen_despachos_porpallet
integer x = 626
integer y = 496
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

	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1)

	dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
	idwc_etiqueta.SetTransObject(sqlca)
	idwc_etiqueta.Retrieve()

	dw_recibidor.GetChild("reci_codigo", idwc_recibidor)
	idwc_recibidor.SetTransObject(sqlca)
	idwc_recibidor.Retrieve()
	dw_recibidor.InsertRow(0)	

	dw_operaciones.GetChild("oper_codigo", idwc_operaciones)
	idwc_operaciones.SetTransObject(SQLCA)
	idwc_operaciones.Retrieve(Integer(Data))
	dw_operaciones.InsertRow(0)

ELSE
	This.SetItem(1, "clie_codigo", Integer(ls_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type st_6 from statictext within w_info_resumen_despachos_porpallet
integer x = 306
integer y = 496
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_resumen_despachos_porpallet
integer x = 626
integer y = 672
integer width = 969
integer height = 92
integer taborder = 20
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

event losefocus;AcceptText()
end event

type st_3 from statictext within w_info_resumen_despachos_porpallet
integer x = 1851
integer y = 604
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_resumen_despachos_porpallet
integer x = 1042
integer y = 1124
integer width = 302
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Final Desp."
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_resumen_despachos_porpallet
integer x = 1367
integer y = 1100
integer width = 411
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[8]	=	This.Text
end event

type st_variedad from statictext within w_info_resumen_despachos_porpallet
integer x = 1851
integer y = 808
integer width = 302
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_embalaje from statictext within w_info_resumen_despachos_porpallet
integer x = 1851
integer y = 984
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_resumen_despachos_porpallet
integer x = 2135
integer y = 916
integer width = 402
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consembalaje.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[5]		=	'-1'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type em_embalaje from editmask within w_info_resumen_despachos_porpallet
integer x = 2135
integer y = 972
integer width = 261
integer height = 96
integer taborder = 80
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
	FROM	dbo.embalajesprod
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
	istr_mant.argumento[5]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_resumen_despachos_porpallet
integer x = 2405
integer y = 980
integer width = 96
integer height = 84
integer taborder = 90
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

type cbx_consembalaje from checkbox within w_info_resumen_despachos_porpallet
integer x = 2629
integer y = 916
integer width = 471
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[5]	=	'-9'
	cbx_embalaje.Enabled	=	False
	cbx_embalaje.Checked	=	True
ELSE
	istr_mant.argumento[5]	=	'-1'
	cbx_embalaje.Enabled	=	True
END IF

end event

type st_11 from statictext within w_info_resumen_despachos_porpallet
integer x = 1851
integer y = 1156
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Etiqueta"
boolean focusrectangle = false
end type

type dw_etiqueta from datawindow within w_info_resumen_despachos_porpallet
integer x = 2130
integer y = 1152
integer width = 891
integer height = 96
integer taborder = 100
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
	istr_mant.argumento[6]	=	data
	
END IF


end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type cbx_etiqueta from checkbox within w_info_resumen_despachos_porpallet
integer x = 2135
integer y = 1088
integer width = 293
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consetiqueta.Enabled								=	True
	dw_etiqueta.Enabled										=	False
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[6]									=	'-1'
ELSE
	cbx_consetiqueta.Enabled								=	False
	cbx_consetiqueta.Checked								=	False
	dw_etiqueta.Enabled										=	True
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_etiqueta.SetFocus()
END IF
end event

type cbx_consetiqueta from checkbox within w_info_resumen_despachos_porpallet
integer x = 2629
integer y = 1088
integer width = 471
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[6]	=	'-9'
	cbx_etiqueta.Enabled		=	False
	cbx_etiqueta.Checked		=	True
ELSE
	istr_mant.argumento[6]	=	'-1'
	cbx_etiqueta.Enabled		=	True
END IF

end event

type cbx_planta from checkbox within w_info_resumen_despachos_porpallet
integer x = 631
integer y = 608
integer width = 402
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[3]									=	'-1'
ELSE
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_planta.SetFocus()
END IF
end event

type st_21 from statictext within w_info_resumen_despachos_porpallet
integer x = 247
integer y = 1248
integer width = 1550
integer height = 832
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_38 from statictext within w_info_resumen_despachos_porpallet
integer x = 1851
integer y = 1616
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Mercado"
boolean focusrectangle = false
end type

type st_48 from statictext within w_info_resumen_despachos_porpallet
integer x = 1851
integer y = 1792
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Destino"
boolean focusrectangle = false
end type

type st_51 from statictext within w_info_resumen_despachos_porpallet
integer x = 1797
integer y = 1524
integer width = 1550
integer height = 556
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_58 from statictext within w_info_resumen_despachos_porpallet
integer x = 265
integer y = 1956
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consignatario"
boolean focusrectangle = false
end type

type cbx_mercado from checkbox within w_info_resumen_despachos_porpallet
integer x = 2130
integer y = 1544
integer width = 366
integer height = 56
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_merccons.Enabled										=	True
	dw_mercado.Enabled										=	False
	dw_mercado.Object.merc_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[12]									=	'-1'
ELSE
	cbx_merccons.Enabled										=	False
	cbx_merccons.Checked										=	False
	dw_mercado.Enabled										=	True
	dw_mercado.Object.merc_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_mercado.SetFocus()
END IF

	
end event

type cbx_merccons from checkbox within w_info_resumen_despachos_porpallet
integer x = 2629
integer y = 1544
integer width = 471
integer height = 56
integer taborder = 230
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[12]	=	'-9'
	cbx_mercado.Enabled	=	False
	cbx_mercado.Checked	=	True
ELSE
	istr_mant.argumento[12]	=	'-1'
	cbx_mercado.Enabled	=	True
END IF
end event

type dw_mercado from datawindow within w_info_resumen_despachos_porpallet
integer x = 2130
integer y = 1596
integer width = 887
integer height = 92
integer taborder = 250
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_mercado"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[12]	=	data


dw_destino.GetChild("dest_codigo", idwc_destino)
idwc_destino.SetTransObject(sqlca)
idwc_destino.Retrieve(Integer(istr_mant.argumento[12]))
dw_destino.InsertRow(0)	


end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type cbx_recibidor from checkbox within w_info_resumen_despachos_porpallet
integer x = 645
integer y = 1872
integer width = 402
integer height = 56
integer taborder = 300
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_recibidorcons.Enabled									=	True
	dw_recibidor.Enabled											=	False
	dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[14]										=	'-1'
ELSE
	cbx_recibidorcons.Enabled									=	False
	cbx_recibidorcons.Checked									=	False
	dw_recibidor.Enabled											=	True
	dw_recibidor.Object.reci_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_recibidor.SetFocus()
END IF

	
end event

type cbx_recibidorcons from checkbox within w_info_resumen_despachos_porpallet
integer x = 1166
integer y = 1872
integer width = 471
integer height = 56
integer taborder = 290
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[14]	=	'-9'
	cbx_recibidor.Enabled	=	False
	cbx_recibidor.Checked	=	True	
ELSE
	istr_mant.argumento[14]	=	'-1'
	cbx_recibidor.Enabled	=	True
END IF
	
end event

type dw_recibidor from datawindow within w_info_resumen_despachos_porpallet
integer x = 645
integer y = 1928
integer width = 1125
integer height = 92
integer taborder = 310
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_consignatarios"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[14]	=	data

end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type cbx_destino from checkbox within w_info_resumen_despachos_porpallet
integer x = 2130
integer y = 1708
integer width = 366
integer height = 56
integer taborder = 270
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_destcons.Enabled										=	True
	dw_destino.Enabled										=	False
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[13]									=	'-1'
ELSE
	cbx_destcons.Enabled										=	False
	cbx_destcons.Checked										=	False
	dw_destino.Enabled										=	True
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_destino.SetFocus()
END IF


end event

type cbx_destcons from checkbox within w_info_resumen_despachos_porpallet
integer x = 2629
integer y = 1708
integer width = 471
integer height = 56
integer taborder = 260
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[13]	=	'-9'
	cbx_destino.Enabled		=	False
	cbx_destino.Checked		=	True	
ELSE
	istr_mant.argumento[13]	=	'-1'
	cbx_destino.Enabled		=	True
END IF
end event

type dw_destino from datawindow within w_info_resumen_despachos_porpallet
integer x = 2130
integer y = 1760
integer width = 887
integer height = 92
integer taborder = 280
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[13]	=	data

end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type st_70 from statictext within w_info_resumen_despachos_porpallet
integer x = 265
integer y = 1360
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Operacion"
boolean focusrectangle = false
end type

type cbx_operacion from checkbox within w_info_resumen_despachos_porpallet
integer x = 645
integer y = 1280
integer width = 379
integer height = 56
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_operacioncons.Enabled									=	True
	dw_operaciones.Enabled											=	False
	dw_operaciones.Object.oper_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[10]										=	'-1'
ELSE
	cbx_operacioncons.Enabled									=	False
	cbx_operacioncons.Checked									=	False
	dw_operaciones.Enabled											=	True
	dw_operaciones.Object.oper_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_operaciones.SetFocus()
END IF

	
end event

type dw_operaciones from datawindow within w_info_resumen_despachos_porpallet
integer x = 645
integer y = 1340
integer width = 974
integer height = 92
integer taborder = 160
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_operaciones"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[10]	=	data

end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type cbx_operacioncons from checkbox within w_info_resumen_despachos_porpallet
integer x = 1166
integer y = 1280
integer width = 471
integer height = 56
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[10]	=	'-9'
	cbx_operacion.Enabled	=	False
	cbx_operacion.Checked	=	True
ELSE
	istr_mant.argumento[10]	=	'-1'
	cbx_operacion.Enabled	=	True
END IF
	
end event

type st_12 from statictext within w_info_resumen_despachos_porpallet
integer x = 265
integer y = 1544
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Puerto Orig."
boolean focusrectangle = false
end type

type cbx_puertos from checkbox within w_info_resumen_despachos_porpallet
integer x = 645
integer y = 1476
integer width = 366
integer height = 56
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_puertoscons.Enabled									=	True
	dw_puertos.Enabled										=	False
	dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[11]									=	'-1'
ELSE
	cbx_puertoscons.Enabled									=	False
	cbx_puertoscons.Checked									=	False
	dw_puertos.Enabled										=	True
	dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_puertos.SetFocus()
END IF


end event

type cbx_puertoscons from checkbox within w_info_resumen_despachos_porpallet
integer x = 1166
integer y = 1476
integer width = 471
integer height = 56
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[11]	=	'-9'
	cbx_puertos.Enabled	=	False
	cbx_puertos.Checked	=	True	
ELSE
	cbx_puertos.Enabled	=	True
   istr_mant.argumento[11] =	'-1'
END IF
end event

type st_10 from statictext within w_info_resumen_despachos_porpallet
integer x = 247
integer y = 1044
integer width = 1550
integer height = 204
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_resumen_despachos_porpallet
integer x = 1797
integer y = 444
integer width = 1550
integer height = 1080
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_puertos from datawindow within w_info_resumen_despachos_porpallet
integer x = 645
integer y = 1532
integer width = 997
integer height = 92
integer taborder = 190
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_puertos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[11]	=	data

end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type st_8 from statictext within w_info_resumen_despachos_porpallet
integer x = 247
integer y = 840
integer width = 1550
integer height = 204
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_porpallet from checkbox within w_info_resumen_despachos_porpallet
integer x = 320
integer y = 908
integer width = 425
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Por Pallet   "
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[9]	=	'1'
ELSE
	istr_mant.argumento[9]	=	'0'
END IF
	
end event

type uo_selespecie from uo_seleccion_especie within w_info_resumen_despachos_porpallet
event destroy ( )
integer x = 2135
integer y = 512
integer height = 180
integer taborder = 60
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
		uo_selvariedad.dw_Seleccion.Enabled		=	False
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.dw_Seleccion.Enabled		=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_resumen_despachos_porpallet
event destroy ( )
integer x = 2135
integer y = 712
integer taborder = 70
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_resumen_despachos_porpallet
integer x = 905
integer y = 908
integer width = 745
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rotulada"
end type

type st_14 from statictext within w_info_resumen_despachos_porpallet
integer x = 265
integer y = 1732
integer width = 311
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Embarque"
boolean focusrectangle = false
end type

type cbx_embarque from checkbox within w_info_resumen_despachos_porpallet
integer x = 645
integer y = 1660
integer width = 315
integer height = 68
integer taborder = 200
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[15] =  "-1"
	dw_embarques.Enabled		=	False
	dw_embarques.Reset()
	dw_embarques.InsertRow(0)
ELSE
	dw_embarques.Enabled		=	True
	dw_embarques.SetFocus()
END IF
end event

type dw_embarques from datawindow within w_info_resumen_despachos_porpallet
integer x = 645
integer y = 1728
integer width = 960
integer height = 92
integer taborder = 220
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_embarques_operacion"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_Nula

SetNull(ls_Nula)

IF ExistEmbarque(Integer(istr_mant.argumento[1]), data, ls_Columna[]) THEN
	istr_mant.argumento[15] =	ls_Columna[3]
	IF Integer(ls_Columna[5]) <> Integer(istr_mant.argumento[10]) THEN
		MessageBox("Atención", "Embarque corresponde a otra Operación (" + &
						ls_Columna[3] + ")")
		This.SetItem(1, "embq_codigo", ls_Nula)
						
		RETURN 1
	END IF
	
END IF
end event

event itemerror;RETURN 1
end event

type st_9 from statictext within w_info_resumen_despachos_porpallet
integer x = 1851
integer y = 1356
integer width = 256
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_resumen_despachos_porpallet
integer x = 2135
integer y = 1356
integer width = 261
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
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

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	
	li_especie	=	Integer(uo_selespecie.Codigo) // Especie
	li_variedad	=	Integer(uo_selvariedad.Codigo) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[20]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	

end event

type cbx_calidad from checkbox within w_info_resumen_despachos_porpallet
integer x = 2135
integer y = 1272
integer width = 261
integer height = 80
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[20]	=	'-1'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type cbx_calidadcons from checkbox within w_info_resumen_despachos_porpallet
integer x = 2629
integer y = 1272
integer width = 471
integer height = 56
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[20]	=	'-9'
	cbx_calidad.Enabled		=	False
	cbx_calidad.Checked		=	True
ELSE
	istr_mant.argumento[20]	=	'-1'
	cbx_calidad.Enabled		=	True
END IF

end event

type cbx_1 from checkbox within w_info_resumen_despachos_porpallet
integer x = 1166
integer y = 1660
integer width = 471
integer height = 68
integer taborder = 210
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[15]	=	'-9'
	cbx_embarque.Enabled	=	False
	cbx_embarque.Checked	=	True	
ELSE
	cbx_embarque.Enabled	=	True
   istr_mant.argumento[15] =	'-1'
END IF
end event

type st_17 from statictext within w_info_resumen_despachos_porpallet
integer x = 1851
integer y = 1956
integer width = 293
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Condición"
boolean focusrectangle = false
end type

type uo_selcondicion from uo_seleccion_condicion within w_info_resumen_despachos_porpallet
integer x = 2130
integer y = 1872
integer taborder = 290
boolean bringtotop = true
end type

on uo_selcondicion.destroy
call uo_seleccion_condicion::destroy
end on

