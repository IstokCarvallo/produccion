$PBExportHeader$w_info_resumen_facturacion.srw
forward
global type w_info_resumen_facturacion from w_para_informes
end type
type st_4 from statictext within w_info_resumen_facturacion
end type
type st_1 from statictext within w_info_resumen_facturacion
end type
type st_2 from statictext within w_info_resumen_facturacion
end type
type em_desde from editmask within w_info_resumen_facturacion
end type
type dw_cliente from datawindow within w_info_resumen_facturacion
end type
type st_6 from statictext within w_info_resumen_facturacion
end type
type dw_planta from datawindow within w_info_resumen_facturacion
end type
type st_7 from statictext within w_info_resumen_facturacion
end type
type em_hasta from editmask within w_info_resumen_facturacion
end type
type cbx_planta from checkbox within w_info_resumen_facturacion
end type
type st_21 from statictext within w_info_resumen_facturacion
end type
type st_51 from statictext within w_info_resumen_facturacion
end type
type st_12 from statictext within w_info_resumen_facturacion
end type
type cbx_puertos from checkbox within w_info_resumen_facturacion
end type
type st_10 from statictext within w_info_resumen_facturacion
end type
type st_5 from statictext within w_info_resumen_facturacion
end type
type dw_puertos from datawindow within w_info_resumen_facturacion
end type
type st_9 from statictext within w_info_resumen_facturacion
end type
type cbx_consplanta from checkbox within w_info_resumen_facturacion
end type
type st_3 from statictext within w_info_resumen_facturacion
end type
type em_prod_codigo from editmask within w_info_resumen_facturacion
end type
type cb_buscaproductor from commandbutton within w_info_resumen_facturacion
end type
type sle_nombre_prod from singlelineedit within w_info_resumen_facturacion
end type
type cbx_productor from checkbox within w_info_resumen_facturacion
end type
type cbx_flete from checkbox within w_info_resumen_facturacion
end type
type st_11 from statictext within w_info_resumen_facturacion
end type
type st_13 from statictext within w_info_resumen_facturacion
end type
type dw_tica from datawindow within w_info_resumen_facturacion
end type
type cbx_tica from checkbox within w_info_resumen_facturacion
end type
type em_despadesde from editmask within w_info_resumen_facturacion
end type
type em_despahasta from editmask within w_info_resumen_facturacion
end type
type st_14 from statictext within w_info_resumen_facturacion
end type
type cbx_recep from checkbox within w_info_resumen_facturacion
end type
type st_15 from statictext within w_info_resumen_facturacion
end type
type st_16 from statictext within w_info_resumen_facturacion
end type
type dw_fruta from datawindow within w_info_resumen_facturacion
end type
type cbx_caracte from checkbox within w_info_resumen_facturacion
end type
type ddlb_tipoent from dropdownlistbox within w_info_resumen_facturacion
end type
type cbx_tipoent from checkbox within w_info_resumen_facturacion
end type
type cbx_inspe from checkbox within w_info_resumen_facturacion
end type
type st_17 from statictext within w_info_resumen_facturacion
end type
type cbx_destino from checkbox within w_info_resumen_facturacion
end type
type dw_destino from datawindow within w_info_resumen_facturacion
end type
type cbx_condicion from checkbox within w_info_resumen_facturacion
end type
type cbx_numeral from checkbox within w_info_resumen_facturacion
end type
type st_18 from statictext within w_info_resumen_facturacion
end type
type cbx_repa from checkbox within w_info_resumen_facturacion
end type
type st_19 from statictext within w_info_resumen_facturacion
end type
type st_20 from statictext within w_info_resumen_facturacion
end type
type cbx_condi from checkbox within w_info_resumen_facturacion
end type
type dw_condicion from datawindow within w_info_resumen_facturacion
end type
type st_24 from statictext within w_info_resumen_facturacion
end type
type cbx_repal from checkbox within w_info_resumen_facturacion
end type
type ddlb_repal from dropdownlistbox within w_info_resumen_facturacion
end type
type st_8 from statictext within w_info_resumen_facturacion
end type
type st_imp from statictext within w_info_resumen_facturacion
end type
type st_informe from statictext within w_info_resumen_facturacion
end type
type dw_1 from datawindow within w_info_resumen_facturacion
end type
type dw_periodo from datawindow within w_info_resumen_facturacion
end type
type st_fechasperiodo from statictext within w_info_resumen_facturacion
end type
type cbx_varirotula from checkbox within w_info_resumen_facturacion
end type
end forward

global type w_info_resumen_facturacion from w_para_informes
integer x = 14
integer y = 32
integer width = 3831
integer height = 2428
string title = "Resumen de Facturación"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_desde em_desde
dw_cliente dw_cliente
st_6 st_6
dw_planta dw_planta
st_7 st_7
em_hasta em_hasta
cbx_planta cbx_planta
st_21 st_21
st_51 st_51
st_12 st_12
cbx_puertos cbx_puertos
st_10 st_10
st_5 st_5
dw_puertos dw_puertos
st_9 st_9
cbx_consplanta cbx_consplanta
st_3 st_3
em_prod_codigo em_prod_codigo
cb_buscaproductor cb_buscaproductor
sle_nombre_prod sle_nombre_prod
cbx_productor cbx_productor
cbx_flete cbx_flete
st_11 st_11
st_13 st_13
dw_tica dw_tica
cbx_tica cbx_tica
em_despadesde em_despadesde
em_despahasta em_despahasta
st_14 st_14
cbx_recep cbx_recep
st_15 st_15
st_16 st_16
dw_fruta dw_fruta
cbx_caracte cbx_caracte
ddlb_tipoent ddlb_tipoent
cbx_tipoent cbx_tipoent
cbx_inspe cbx_inspe
st_17 st_17
cbx_destino cbx_destino
dw_destino dw_destino
cbx_condicion cbx_condicion
cbx_numeral cbx_numeral
st_18 st_18
cbx_repa cbx_repa
st_19 st_19
st_20 st_20
cbx_condi cbx_condi
dw_condicion dw_condicion
st_24 st_24
cbx_repal cbx_repal
ddlb_repal ddlb_repal
st_8 st_8
st_imp st_imp
st_informe st_informe
dw_1 dw_1
dw_periodo dw_periodo
st_fechasperiodo st_fechasperiodo
cbx_varirotula cbx_varirotula
end type
global w_info_resumen_facturacion w_info_resumen_facturacion

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente, idwc_planta, idwc_puertos, idwc_tica, idwc_fruta, &
						idwc_mercado, idwc_condicion, idwc_periodo
						
String 	is_NomPlanta, is_NomCliente, is_Periodo
Integer	ii_Productor

uo_tipocamion			iuo_tipocamion	
uo_frutarecepcion		iuo_frutarecepcion
uo_condicion   		iuo_condicion 

end variables

forward prototypes
public function boolean noexisteetiqueta (integer li_cliente, string ls_columna)
public function boolean existepacking (integer li_cliente, ref string ls_columna)
public function boolean existeespecie (integer cliente, integer especie)
public function boolean existeproductor (integer li_cliente, long ll_productor)
public function boolean existeperiodo (integer ai_periodo)
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

public function boolean existeperiodo (integer ai_periodo);Integer	li_Cliente, li_Estado
String	ls_Periodo
Date		ld_Desde,ld_Hasta

li_Cliente	=	Integer(istr_mant.argumento[1])
li_estado 	=	1

SELECT	fape_estado,fape_observ,fape_fdesde,fape_fhasta
	INTO	:li_estado, :ls_Periodo, :ld_Desde, :ld_Hasta
	FROM	dba.FacturPeriodos
	WHERE	clie_codigo	=	:li_Cliente
	and 	fape_numero =  :ai_periodo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla FaturPeriodos")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Periodo Seleccionado no ha sido ingresado para este Cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
//ELSEIF li_estado = 1 THEN
//	MessageBox("Atención", "Periodo Seleccionado ha sido Cerrado.~r~r" + &
//					"Ingrese o seleccione otro Código.")
//	RETURN False
ELSE
	st_fechasperiodo.Text	=	'Desde el '+ string(ld_Desde) +' Al '+String(ld_Hasta)
	is_Periodo	=	ls_Periodo
	RETURN True
END IF
end function

on w_info_resumen_facturacion.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_7=create st_7
this.em_hasta=create em_hasta
this.cbx_planta=create cbx_planta
this.st_21=create st_21
this.st_51=create st_51
this.st_12=create st_12
this.cbx_puertos=create cbx_puertos
this.st_10=create st_10
this.st_5=create st_5
this.dw_puertos=create dw_puertos
this.st_9=create st_9
this.cbx_consplanta=create cbx_consplanta
this.st_3=create st_3
this.em_prod_codigo=create em_prod_codigo
this.cb_buscaproductor=create cb_buscaproductor
this.sle_nombre_prod=create sle_nombre_prod
this.cbx_productor=create cbx_productor
this.cbx_flete=create cbx_flete
this.st_11=create st_11
this.st_13=create st_13
this.dw_tica=create dw_tica
this.cbx_tica=create cbx_tica
this.em_despadesde=create em_despadesde
this.em_despahasta=create em_despahasta
this.st_14=create st_14
this.cbx_recep=create cbx_recep
this.st_15=create st_15
this.st_16=create st_16
this.dw_fruta=create dw_fruta
this.cbx_caracte=create cbx_caracte
this.ddlb_tipoent=create ddlb_tipoent
this.cbx_tipoent=create cbx_tipoent
this.cbx_inspe=create cbx_inspe
this.st_17=create st_17
this.cbx_destino=create cbx_destino
this.dw_destino=create dw_destino
this.cbx_condicion=create cbx_condicion
this.cbx_numeral=create cbx_numeral
this.st_18=create st_18
this.cbx_repa=create cbx_repa
this.st_19=create st_19
this.st_20=create st_20
this.cbx_condi=create cbx_condi
this.dw_condicion=create dw_condicion
this.st_24=create st_24
this.cbx_repal=create cbx_repal
this.ddlb_repal=create ddlb_repal
this.st_8=create st_8
this.st_imp=create st_imp
this.st_informe=create st_informe
this.dw_1=create dw_1
this.dw_periodo=create dw_periodo
this.st_fechasperiodo=create st_fechasperiodo
this.cbx_varirotula=create cbx_varirotula
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_hasta
this.Control[iCurrent+10]=this.cbx_planta
this.Control[iCurrent+11]=this.st_21
this.Control[iCurrent+12]=this.st_51
this.Control[iCurrent+13]=this.st_12
this.Control[iCurrent+14]=this.cbx_puertos
this.Control[iCurrent+15]=this.st_10
this.Control[iCurrent+16]=this.st_5
this.Control[iCurrent+17]=this.dw_puertos
this.Control[iCurrent+18]=this.st_9
this.Control[iCurrent+19]=this.cbx_consplanta
this.Control[iCurrent+20]=this.st_3
this.Control[iCurrent+21]=this.em_prod_codigo
this.Control[iCurrent+22]=this.cb_buscaproductor
this.Control[iCurrent+23]=this.sle_nombre_prod
this.Control[iCurrent+24]=this.cbx_productor
this.Control[iCurrent+25]=this.cbx_flete
this.Control[iCurrent+26]=this.st_11
this.Control[iCurrent+27]=this.st_13
this.Control[iCurrent+28]=this.dw_tica
this.Control[iCurrent+29]=this.cbx_tica
this.Control[iCurrent+30]=this.em_despadesde
this.Control[iCurrent+31]=this.em_despahasta
this.Control[iCurrent+32]=this.st_14
this.Control[iCurrent+33]=this.cbx_recep
this.Control[iCurrent+34]=this.st_15
this.Control[iCurrent+35]=this.st_16
this.Control[iCurrent+36]=this.dw_fruta
this.Control[iCurrent+37]=this.cbx_caracte
this.Control[iCurrent+38]=this.ddlb_tipoent
this.Control[iCurrent+39]=this.cbx_tipoent
this.Control[iCurrent+40]=this.cbx_inspe
this.Control[iCurrent+41]=this.st_17
this.Control[iCurrent+42]=this.cbx_destino
this.Control[iCurrent+43]=this.dw_destino
this.Control[iCurrent+44]=this.cbx_condicion
this.Control[iCurrent+45]=this.cbx_numeral
this.Control[iCurrent+46]=this.st_18
this.Control[iCurrent+47]=this.cbx_repa
this.Control[iCurrent+48]=this.st_19
this.Control[iCurrent+49]=this.st_20
this.Control[iCurrent+50]=this.cbx_condi
this.Control[iCurrent+51]=this.dw_condicion
this.Control[iCurrent+52]=this.st_24
this.Control[iCurrent+53]=this.cbx_repal
this.Control[iCurrent+54]=this.ddlb_repal
this.Control[iCurrent+55]=this.st_8
this.Control[iCurrent+56]=this.st_imp
this.Control[iCurrent+57]=this.st_informe
this.Control[iCurrent+58]=this.dw_1
this.Control[iCurrent+59]=this.dw_periodo
this.Control[iCurrent+60]=this.st_fechasperiodo
this.Control[iCurrent+61]=this.cbx_varirotula
end on

on w_info_resumen_facturacion.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.cbx_planta)
destroy(this.st_21)
destroy(this.st_51)
destroy(this.st_12)
destroy(this.cbx_puertos)
destroy(this.st_10)
destroy(this.st_5)
destroy(this.dw_puertos)
destroy(this.st_9)
destroy(this.cbx_consplanta)
destroy(this.st_3)
destroy(this.em_prod_codigo)
destroy(this.cb_buscaproductor)
destroy(this.sle_nombre_prod)
destroy(this.cbx_productor)
destroy(this.cbx_flete)
destroy(this.st_11)
destroy(this.st_13)
destroy(this.dw_tica)
destroy(this.cbx_tica)
destroy(this.em_despadesde)
destroy(this.em_despahasta)
destroy(this.st_14)
destroy(this.cbx_recep)
destroy(this.st_15)
destroy(this.st_16)
destroy(this.dw_fruta)
destroy(this.cbx_caracte)
destroy(this.ddlb_tipoent)
destroy(this.cbx_tipoent)
destroy(this.cbx_inspe)
destroy(this.st_17)
destroy(this.cbx_destino)
destroy(this.dw_destino)
destroy(this.cbx_condicion)
destroy(this.cbx_numeral)
destroy(this.st_18)
destroy(this.cbx_repa)
destroy(this.st_19)
destroy(this.st_20)
destroy(this.cbx_condi)
destroy(this.dw_condicion)
destroy(this.st_24)
destroy(this.cbx_repal)
destroy(this.ddlb_repal)
destroy(this.st_8)
destroy(this.st_imp)
destroy(this.st_informe)
destroy(this.dw_1)
destroy(this.dw_periodo)
destroy(this.st_fechasperiodo)
destroy(this.cbx_varirotula)
end on

event open;Integer li_Busca

x	=	0
y	=	0

iuo_tipocamion			=	CREATE   uo_tipocamion
iuo_frutarecepcion	=	CREATE	uo_frutarecepcion	
iuo_condicion  		=	CREATE	uo_condicion

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

li_Busca 	  = idwc_cliente.Find("clie_codigo = " + String(gi_CodExport), 1, idwc_cliente.RowCount())
is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.Enabled											=	False
dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_tica.GetChild("tica_codigo", idwc_tica)
idwc_tica.SetTransObject(SQLCA)
idwc_tica.Retrieve()
dw_tica.InsertRow(0)
dw_tica.Enabled											=	False
dw_tica.Object.tica_codigo.BackGround.Color		=	RGB(192, 192, 192)

dw_puertos.GetChild("puer_codigo", idwc_puertos)
idwc_puertos.SetTransObject(sqlca)
idwc_puertos.Retrieve(900)
dw_puertos.InsertRow(0)
dw_puertos.Enabled										=	False
dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(192, 192, 192)

em_despadesde.Enabled									=	False
em_despahasta.Enabled									=	False

/////////// Recepciones
dw_fruta.GetChild("frre_codigo", idwc_fruta)
idwc_fruta.SetTransObject(sqlca)
idwc_fruta.Retrieve()
dw_fruta.Object.frre_codigo[1] = 1
dw_fruta.Enabled											=	False
dw_fruta.Object.frre_codigo.BackGround.Color		=	RGB(192, 192, 192)

ddlb_tipoent.Enabled		=	False

////  Inspeccion
dw_destino.GetChild("dest_codigo", idwc_mercado)
idwc_mercado.SetTransObject(Sqlca)
idwc_mercado.Retrieve(0)
dw_destino.SetTransObject(Sqlca)
dw_destino.InsertRow(0)
dw_destino.Enabled										=	False
dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(192, 192, 192)

/// Repalletizajes
ddlb_repal.Enabled	=	 False

/// Condicion
dw_condicion.GetChild("cond_codigo", idwc_condicion)
idwc_condicion.SetTransObject(sqlca)
idwc_condicion.Retrieve()
dw_condicion.InsertRow(0)
dw_condicion.SetItem(1, "cond_codigo", 1)
dw_condicion.Enabled										=	False
dw_condicion.Object.cond_codigo.BackGround.Color=	RGB(192, 192, 192)

// Numerales
dw_periodo.GetChild("fape_numero", idwc_periodo)
idwc_periodo.SetTransObject(sqlca)
dw_periodo.SetTransObject(sqlca)
idwc_periodo.Retrieve(gi_CodExport)
dw_periodo.InsertRow(0)
dw_periodo.Enabled	=	False
dw_periodo.Object.fape_numero.BackGround.Color	=	RGB(192, 192, 192)

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())

istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[2]	=	"-9"							//	planta
istr_mant.argumento[3]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[4]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[5]	=	"-1"							//	Productor

istr_mant.argumento[6] 	=	"0"							// Despacho Nro. Desde
istr_mant.argumento[7] 	=	"0"							// Despacho Nro. Hasta
istr_mant.argumento[8] 	=	"-1"							// Despacho Tipo camion
istr_mant.argumento[9] 	=	"-1"							// Despacho Puerto Zarpe

istr_mant.argumento[10]	= 	"-1" 							// Recepción Caracteristica
istr_mant.argumento[11]	= 	"-1" 							// Recepción Tipo de Entrada

istr_mant.argumento[12]	= 	"-1" 							// Inspección Destino

istr_mant.argumento[13]	=	"0"							// Reetiquetado Nro. Desde
istr_mant.argumento[14]	=	"0"							// Reetiquetado Nro. Hasta

istr_mant.argumento[15]	= 	"-1" 							// Condición Tipo

istr_mant.argumento[16]	= 	"0" 							// Numeral Periodo
istr_mant.argumento[17]	= 	"" 							// Numeral Tipo Productor
istr_mant.argumento[18]	= 	"" 							// Numeral Productor
istr_mant.argumento[19]	= 	"" 							// Numeral Especie
istr_mant.argumento[20]	= 	"" 							// Numeral Variedad
istr_mant.argumento[21]	= 	"" 							// Numeral Embalaje

st_24.Enabled	=	False

//cbx_operacion.Enabled	=	False
//cbx_especie.Enabled		=	False
//cbx_puertos.Enabled		=	False
end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_info_resumen_facturacion
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_facturacion
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_facturacion
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_facturacion
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_facturacion
boolean visible = false
integer width = 3095
string text = "Resumen de Facturación"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_facturacion
string tag = "Imprimir Reporte"
integer x = 3433
integer y = 1664
integer taborder = 200
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer 	li_Cliente, li_Planta, li_Tica, li_Puerto, li_Caracte, li_TipoEnt, li_Destino, &
			li_Condic, li_Periodo, li_varirotula
String	texto_desde, texto_hasta, texto_fecha, ls_Rango
Date		ld_Desde, ld_Hasta
Long		ll_Productor, ll_NroDesde, ll_NroHasta

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

li_Cliente		=	Integer(istr_mant.argumento[1])
li_Planta		=	Integer(istr_mant.argumento[2])
ll_Productor	=	Long(istr_mant.argumento[5])
ld_Desde			=	Date(istr_mant.argumento[3])
ld_Hasta			=	Date(istr_mant.argumento[4])

texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_flete.Checked THEN
	ll_NroDesde		=	Long(istr_mant.argumento[6])
	ll_NroHasta		=	Long(istr_mant.argumento[7])
	li_Tica			=	Integer(istr_mant.argumento[8])
	li_Puerto		= 	Integer(istr_mant.argumento[9])
	
	ls_Rango			=	"Nro.Despacho entre:   " + String(ll_NroDesde) + "   Al   " + String(ll_NroHasta)
	
	st_informe.text	=	"Informe Fletes"
	dw_1.Reset()
	dw_1.DataObject	=	'dw_info_factur_flete'
	dw_1.SetTransObject(sqlca)
	
	IF dw_1.Retrieve(li_Cliente,li_Planta,ld_Desde,ld_Hasta, &
						  ll_Productor,ll_NroDesde,ll_NroHasta, &
						  li_Tica, li_Puerto, li_varirotula) > 0 THEN
		F_Membrete(dw_1)
		dw_1.Modify("cliente.text = '" + is_NomCliente + "'")
		dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		dw_1.Modify("rango.text = '" + ls_Rango + "'")
		dw_1.Print()		
	END IF		
END IF

IF cbx_recep.Checked THEN
	li_Caracte	=	Integer(istr_mant.argumento[10])
	li_TipoEnt	=	Integer(istr_mant.argumento[11])
	
	st_informe.text	=	"Informe Recepciones"
	dw_1.Reset()
	dw_1.DataObject	=	'dw_info_factur_recepciones'
	dw_1.SetTransObject(sqlca)
	IF dw_1.Retrieve(li_Cliente,li_Planta,ld_Desde,ld_Hasta, &
						  ll_Productor,li_Caracte,li_TipoEnt,li_varirotula) > 0 THEN
		F_Membrete(dw_1)
		dw_1.Modify("cliente.text = '" + is_NomCliente + "'")
		dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		dw_1.Print()
	END IF
	
END IF

IF cbx_inspe.Checked THEN
	li_Destino	=	Integer(istr_mant.argumento[12])
	
	st_informe.text	=	"Informe Inspeccionados"
	dw_1.Reset()
	dw_1.DataObject	=	'dw_info_factur_inspeccion'
	dw_1.SetTransObject(sqlca)
	IF dw_1.Retrieve(li_Cliente,li_Planta,ld_Desde,ld_Hasta, &
						  ll_Productor,li_Destino,li_varirotula) > 0 THEN
		F_Membrete(dw_1)
		dw_1.Modify("cliente.text = '" + is_NomCliente + "'")
		dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		dw_1.Print()
	END IF

END IF

IF cbx_repa.Checked THEN
END IF

IF cbx_condicion.Checked THEN
	li_Condic	= Integer(istr_mant.argumento[15])
	
	st_informe.text	=	"Informe Condiciones"
	dw_1.Reset()
	dw_1.DataObject	=	'dw_info_factur_condicion'
	dw_1.SetTransObject(sqlca)
	IF dw_1.Retrieve(li_Cliente,li_Planta,ld_Desde,ld_Hasta, &
						  ll_Productor,li_Condic,li_varirotula) > 0 THEN
		F_Membrete(dw_1)
		dw_1.Modify("cliente.text = '" + is_NomCliente + "'")
		dw_1.Modify("fechas.text = '" + texto_fecha + "'")
		dw_1.Print()
	END IF
	
END IF

IF cbx_numeral.Checked AND Integer(istr_mant.argumento[16]) > 0 THEN
	OpenWithParm(w_info_numerales_facturacion, istr_mant)
	istr_mant = Message.PowerObjectParm
	
	li_Periodo	=	Integer(istr_mant.argumento[16])
	
	st_informe.text	=	"Informe Condiciones"
	dw_1.Reset()
	dw_1.DataObject	=	'dw_info_factur_numerales'
	dw_1.SetTransObject(sqlca)
	IF dw_1.Retrieve(li_Cliente,li_Planta, &
						  li_Periodo, &
						  Integer(istr_mant.argumento[18]), &
						  Integer(istr_mant.argumento[17]), &
						  Integer(istr_mant.argumento[19]), &
						  Integer(istr_mant.argumento[20]), &
						  istr_mant.argumento[21] ,li_varirotula) > 0 THEN
		F_Membrete(dw_1)
		dw_1.Modify("cliente.text = '" + is_NomCliente + "'")
		dw_1.Modify("fechas.text = '" + is_Periodo + "'")
		dw_1.Modify("rango.text = '" + st_fechasperiodo.Text+ "'")
		dw_1.Print()
	END IF
	
	
END IF

st_informe.text	= "Proceso Terminado"

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_facturacion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3433
integer y = 1944
integer taborder = 210
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_resumen_facturacion
integer x = 242
integer y = 1076
integer width = 1550
integer height = 468
boolean bringtotop = true
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

type st_1 from statictext within w_info_resumen_facturacion
integer x = 521
integer y = 668
integer width = 238
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

type st_2 from statictext within w_info_resumen_facturacion
integer x = 521
integer y = 788
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Fecha Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_resumen_facturacion
integer x = 887
integer y = 756
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

event modified;istr_mant.argumento[3]	=	This.Text
end event

type dw_cliente from datawindow within w_info_resumen_facturacion
integer x = 887
integer y = 484
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
   is_NomCliente				=	ls_Columna[1]
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1)
	
	idwc_periodo.Retrieve(Integer(istr_mant.argumento[1]))
	
ELSE
	This.SetItem(1, "clie_codigo", Integer(ls_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type st_6 from statictext within w_info_resumen_facturacion
integer x = 521
integer y = 492
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_resumen_facturacion
integer x = 887
integer y = 648
integer width = 969
integer height = 92
integer taborder = 40
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
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF 
end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type st_7 from statictext within w_info_resumen_facturacion
integer x = 1701
integer y = 788
integer width = 178
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_resumen_facturacion
integer x = 1975
integer y = 756
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

event modified;istr_mant.argumento[4]	=	This.Text
end event

type cbx_planta from checkbox within w_info_resumen_facturacion
integer x = 891
integer y = 588
integer width = 402
integer height = 60
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
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
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[2]									=	'-1'
ELSE
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_planta.SetFocus()
END IF
end event

type st_21 from statictext within w_info_resumen_facturacion
integer x = 242
integer y = 1768
integer width = 1550
integer height = 224
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

type st_51 from statictext within w_info_resumen_facturacion
integer x = 1792
integer y = 1768
integer width = 1550
integer height = 224
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

type st_12 from statictext within w_info_resumen_facturacion
integer x = 338
integer y = 1424
integer width = 343
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Puerto Orig."
boolean focusrectangle = false
end type

type cbx_puertos from checkbox within w_info_resumen_facturacion
integer x = 727
integer y = 1372
integer width = 366
integer height = 56
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_puertos.Enabled										=	False
	dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[9]									=	'-1'
ELSE
	dw_puertos.Enabled										=	True
	dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_puertos.SetFocus()
END IF


end event

type st_10 from statictext within w_info_resumen_facturacion
integer x = 242
integer y = 1544
integer width = 1550
integer height = 224
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

type st_5 from statictext within w_info_resumen_facturacion
integer x = 1792
integer y = 1076
integer width = 1550
integer height = 468
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

type dw_puertos from datawindow within w_info_resumen_facturacion
integer x = 722
integer y = 1424
integer width = 997
integer height = 92
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_puertos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[9]	=	data

end event

event itemerror;RETURN 1
end event

event losefocus;AcceptText()
end event

type st_9 from statictext within w_info_resumen_facturacion
integer x = 242
integer y = 448
integer width = 3095
integer height = 624
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

type cbx_consplanta from checkbox within w_info_resumen_facturacion
integer x = 1321
integer y = 588
integer width = 503
integer height = 60
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	cbx_consplanta.Enabled	=	True
	istr_mant.argumento[2]	=	'-9'
	cbx_planta.Enabled		=	False
	cbx_planta.Checked		=	True	
ELSE
	cbx_consplanta.Enabled	=	False
	cbx_consplanta.Checked	=	False
	cbx_planta.Enabled		=	True
   istr_mant.argumento[2]	=	'-1'
END IF
end event

type st_3 from statictext within w_info_resumen_facturacion
integer x = 521
integer y = 972
integer width = 288
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Productor"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_prod_codigo from editmask within w_info_resumen_facturacion
integer x = 887
integer y = 944
integer width = 261
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
string displaydata = "Ä~t/"
end type

event modified;String	ls_Nombre
Long		ll_productor

ll_Productor	=	Long(This.Text)

IF Long(ll_Productor) <> 0 THEN
	
	SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
		This.Text	=	''
		This.SetFocus()	
		RETURN 1
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
						"Ingrese o seleccione otro Código.")
		This.Text	=	''
		This.SetFocus()					
		RETURN 1
	ELSE
		istr_mant.argumento[5] = String(ll_Productor)	
		ii_productor = ll_productor
		sle_nombre_prod.Text	=	ls_Nombre
	END IF
END IF
	

end event

type cb_buscaproductor from commandbutton within w_info_resumen_facturacion
integer x = 1170
integer y = 948
integer width = 96
integer height = 92
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

event clicked;Str_Busqueda	lstr_busq

OpenWithParm(w_busc_productores, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

//IF lstr_busq.Argum[1] <> "" THEN
IF lstr_busq.Argum[3] <> "" THEN	
	em_prod_codigo.Text	=	lstr_busq.Argum[3]
	sle_nombre_prod.Text	=	lstr_busq.Argum[4]
	ii_productor			=	Long(lstr_busq.Argum[3])
ELSE
	This.SetFocus()
END IF
end event

type sle_nombre_prod from singlelineedit within w_info_resumen_facturacion
integer x = 1289
integer y = 948
integer width = 1783
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type cbx_productor from checkbox within w_info_resumen_facturacion
integer x = 891
integer y = 864
integer width = 279
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_prod_codigo.Text			=	""
	sle_nombre_prod.Text			=	""
	cb_buscaproductor.Enabled	=	False
	em_prod_codigo.Enabled		=	False
	sle_nombre_prod.Enabled		=	False
   ii_productor 					=  0	
ELSE
	cb_buscaproductor.Enabled	=	True
	em_prod_codigo.Enabled		=	True
//	sle_nombre_prod.Enabled		=	True
	
	em_prod_codigo.SetFocus()
END IF

RETURN 0
end event

type cbx_flete from checkbox within w_info_resumen_facturacion
integer x = 306
integer y = 1096
integer width = 242
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Flete"
end type

event clicked;IF This.Checked THEN
	em_despadesde.Enabled	=	True
	em_despahasta.Enabled	=	True	
	cbx_tica.Enabled			=	True
	cbx_puertos.Enabled		=	True
	em_despadesde.SetFocus()
ELSE
	em_despadesde.Enabled	=	False
	em_despahasta.Enabled	=	False
	cbx_tica.Enabled			=	False
	cbx_puertos.Enabled		=	False	
	dw_tica.Enabled			=	False
	dw_puertos.Enabled		=	False
	dw_tica.Object.tica_codigo.BackGround.Color		=	RGB(192, 192, 192)
	dw_puertos.Object.puer_codigo.BackGround.Color	=	RGB(192, 192, 192)
END IF
end event

type st_11 from statictext within w_info_resumen_facturacion
integer x = 590
integer y = 1096
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Nro.  Del"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_resumen_facturacion
integer x = 338
integer y = 1264
integer width = 343
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Tipo Camión"
boolean focusrectangle = false
end type

type dw_tica from datawindow within w_info_resumen_facturacion
integer x = 722
integer y = 1264
integer width = 997
integer height = 92
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_tipocamion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_nula
SetNull(li_nula)

IF iuo_tipocamion.existe(Integer(data),True,sqlca) THEN
	istr_mant.argumento[8] = data
ELSE
	This.SetItem(1, "tica_codigo", li_nula)
	RETURN 1
END IF
end event

type cbx_tica from checkbox within w_info_resumen_facturacion
integer x = 727
integer y = 1196
integer width = 366
integer height = 64
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[8]	=	'-1'
	dw_tica.Enabled			=	False
	dw_tica.Enabled			=	False
	dw_tica.Object.tica_codigo.BackGround.Color	=	RGB(192, 192, 192)
	dw_tica.Reset()
	dw_tica.InsertRow(0)
ELSE
	dw_tica.Enabled			=	True
	dw_tica.Object.tica_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_tica.SetFocus()
END IF
end event

type em_despadesde from editmask within w_info_resumen_facturacion
integer x = 855
integer y = 1096
integer width = 375
integer height = 96
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[6]	=	this.text
end event

type em_despahasta from editmask within w_info_resumen_facturacion
integer x = 1381
integer y = 1096
integer width = 375
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
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[7]	=	this.text
end event

type st_14 from statictext within w_info_resumen_facturacion
integer x = 1262
integer y = 1096
integer width = 78
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "al"
alignment alignment = center!
boolean focusrectangle = false
end type

type cbx_recep from checkbox within w_info_resumen_facturacion
integer x = 1842
integer y = 1096
integer width = 402
integer height = 80
integer taborder = 180
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Recepción"
end type

event clicked;IF This.Checked THEN
	cbx_caracte.Enabled		=	True
	cbx_tipoent.Enabled		=	True
	cbx_caracte.SetFocus()
ELSE
	cbx_caracte.Enabled		=	False
	cbx_tipoent.Enabled		=	False	
	dw_fruta.Enabled			=	False
	ddlb_tipoent.Enabled		= 	False
	dw_fruta.Object.frre_codigo.BackGround.Color	=	RGB(192, 192, 192)
END IF
end event

type st_15 from statictext within w_info_resumen_facturacion
integer x = 1893
integer y = 1212
integer width = 398
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Característica"
boolean focusrectangle = false
end type

type st_16 from statictext within w_info_resumen_facturacion
integer x = 1893
integer y = 1344
integer width = 398
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Tipo de Entrada"
boolean focusrectangle = false
end type

type dw_fruta from datawindow within w_info_resumen_facturacion
integer x = 2299
integer y = 1180
integer width = 599
integer height = 100
integer taborder = 190
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_frutarecep"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_nula
SetNull(li_nula)

IF iuo_frutarecepcion.existe(Integer(data),True,sqlca) THEN
	istr_mant.argumento[10] = data
ELSE
	This.SetItem(1, "frre_codigo", li_nula)
	RETURN 1
END IF
end event

type cbx_caracte from checkbox within w_info_resumen_facturacion
integer x = 2304
integer y = 1120
integer width = 270
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[10]	=	'-1'
	dw_fruta.Enabled			=	False
	dw_fruta.Enabled										=	False
	dw_fruta.Object.frre_codigo.BackGround.Color	=	RGB(192, 192, 192)
	dw_fruta.Reset()
	dw_fruta.InsertRow(0)
ELSE
	dw_fruta.Enabled			=	True
	dw_fruta.Object.frre_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_fruta.SetFocus()
END IF
end event

type ddlb_tipoent from dropdownlistbox within w_info_resumen_facturacion
integer x = 2299
integer y = 1348
integer width = 1006
integer height = 508
integer taborder = 160
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string item[] = {"Ingreso desde Packing","Ingreso Interplanta","Devolución de Embarque","Recepción Servicios Terceros","Compras a Terceros","InterPlanta Zonal"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	istr_mant.argumento[11]	= "5"
ELSEIF Integer(index) = 2 THEN
	istr_mant.argumento[11]	= "3"	
ELSEIF Integer(index) = 3 THEN
	istr_mant.argumento[11]	= "1"	
ELSEIF Integer(index) = 4 THEN
	istr_mant.argumento[11]	= "2"	
ELSEIF Integer(index) = 5 THEN
	istr_mant.argumento[11]	= "6"
ELSEIF Integer(index) = 6 THEN
	istr_mant.argumento[11]	= "4"
END IF

ddlb_tipoent.PostEvent(Clicked!)
end event

type cbx_tipoent from checkbox within w_info_resumen_facturacion
integer x = 2304
integer y = 1280
integer width = 270
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[11]	=	'-1'
	ddlb_tipoent.Enabled		=	False
ELSE
	ddlb_tipoent.Enabled		=	True
	ddlb_tipoent.SetFocus()
END IF


end event

type cbx_inspe from checkbox within w_info_resumen_facturacion
integer x = 306
integer y = 1556
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Inspección"
end type

event clicked;IF This.Checked THEN
	cbx_destino.Enabled		=	True	
	cbx_destino.SetFocus()
ELSE
	cbx_destino.Enabled		=	False
	dw_destino.Enabled		=	False
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(192, 192, 192)
END IF
end event

type st_17 from statictext within w_info_resumen_facturacion
integer x = 338
integer y = 1660
integer width = 343
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Destino"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_destino from checkbox within w_info_resumen_facturacion
integer x = 727
integer y = 1576
integer width = 279
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[12]	=	'-1'
	dw_destino.Enabled			=	False
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(192, 192, 192)
	dw_destino.Reset()
	dw_destino.InsertRow(0)
ELSE
	dw_destino.Enabled			=	True
	dw_destino.Object.dest_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_destino.SetFocus()
END IF
end event

type dw_destino from datawindow within w_info_resumen_facturacion
integer x = 718
integer y = 1644
integer width = 887
integer height = 104
integer taborder = 220
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[12]	=	data
end event

type cbx_condicion from checkbox within w_info_resumen_facturacion
integer x = 306
integer y = 1792
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Condición"
end type

event clicked;IF This.Checked THEN
	cbx_condi.Enabled		=	True	
	cbx_condi.SetFocus()
ELSE
	cbx_condi.Enabled		=	False
	dw_condicion.Enabled	=	False
	//dw_condicion.Object.dest_codigo.BackGround.Color	=	RGB(192, 192, 192)
END IF
end event

type cbx_numeral from checkbox within w_info_resumen_facturacion
integer x = 1842
integer y = 1780
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Numerales"
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[16]	=	""
	dw_periodo.Enabled		=	True
	dw_periodo.Object.fape_numero.BackGround.Color	=	RGB(255, 255, 255)
	dw_periodo.SetFocus()
ELSE
	istr_mant.argumento[16]	=	""
	dw_periodo.Enabled		=	False
	dw_periodo.Object.fape_numero.BackGround.Color	=	RGB(192 , 192, 192)
	dw_periodo.Reset()
	dw_periodo.InsertRow(0)
END IF

end event

type st_18 from statictext within w_info_resumen_facturacion
integer x = 1792
integer y = 1544
integer width = 1550
integer height = 224
boolean bringtotop = true
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

type cbx_repa from checkbox within w_info_resumen_facturacion
integer x = 1842
integer y = 1552
integer width = 494
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Repalletizados"
end type

event clicked;IF This.Checked THEN
	cbx_repal.Enabled		=	True	
	cbx_repal.SetFocus()
ELSE
	cbx_repal.Enabled		=	False
	ddlb_repal.Enabled	=	False
END IF
end event

type st_19 from statictext within w_info_resumen_facturacion
integer x = 1893
integer y = 1852
integer width = 398
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Periodo"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_20 from statictext within w_info_resumen_facturacion
integer x = 338
integer y = 1896
integer width = 343
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Tipo"
boolean focusrectangle = false
end type

type cbx_condi from checkbox within w_info_resumen_facturacion
integer x = 727
integer y = 1824
integer width = 274
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[15]		=	'-1'
	dw_condicion.Enabled			=	False
	dw_condicion.Object.cond_codigo.BackGround.Color	=	RGB(192, 192, 192)
	dw_condicion.Reset()
	dw_condicion.InsertRow(0)
ELSE
	dw_condicion.Enabled			=	True
	dw_condicion.Object.cond_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_condicion.SetFocus()
END IF

end event

type dw_condicion from datawindow within w_info_resumen_facturacion
integer x = 718
integer y = 1888
integer width = 891
integer height = 96
integer taborder = 230
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_condicion"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF iuo_condicion.Existe(Integer(Data),True,SqlCa) THEN
   istr_mant.argumento[15] = Data
ELSE
	This.SetItem(1,"cond_codigo",li_Null)
	RETURN 1
END IF


end event

type st_24 from statictext within w_info_resumen_facturacion
integer x = 1893
integer y = 1660
integer width = 398
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Tipo"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
boolean disabledlook = true
end type

type cbx_repal from checkbox within w_info_resumen_facturacion
integer x = 2304
integer y = 1576
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[18]		=	'-1'
	ddlb_repal.Enabled			=	 False
ELSE
	ddlb_repal.Enabled			=	 True
	ddlb_repal.SetFocus()
END IF

end event

type ddlb_repal from dropdownlistbox within w_info_resumen_facturacion
integer x = 2299
integer y = 1644
integer width = 946
integer height = 400
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string item[] = {"1. Rebaja de Altura","2. Levantamiento de Altura","3. Completar Pallets","4. Cambio Etiqueta Variedad","5. Cambio Etiqueta Embalaje","6. Cambio de Folio","7. Reembalaje"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[19]	= String(index)

ddlb_tipoent.PostEvent(Clicked!)
end event

type st_8 from statictext within w_info_resumen_facturacion
integer x = 247
integer y = 1992
integer width = 3095
integer height = 148
boolean bringtotop = true
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

type st_imp from statictext within w_info_resumen_facturacion
integer x = 357
integer y = 2032
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Imprimiendo"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_informe from statictext within w_info_resumen_facturacion
integer x = 864
integer y = 2028
integer width = 1833
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_resumen_facturacion
boolean visible = false
integer x = 3744
integer y = 768
integer width = 686
integer height = 400
boolean bringtotop = true
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_periodo from datawindow within w_info_resumen_facturacion
integer x = 2299
integer y = 1820
integer width = 827
integer height = 92
integer taborder = 210
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_facperiodos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	 li_null
SetNull(li_null)

IF existeperiodo(integer(data)) THEN
	istr_mant.argumento[16]	=	data
	RETURN 0
ELSE
	This.SetItem(1, "fape_numero", li_null)
	istr_mant.argumento[16]	=	""
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_fechasperiodo from statictext within w_info_resumen_facturacion
integer x = 2309
integer y = 1908
integer width = 1001
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean focusrectangle = false
end type

type cbx_varirotula from checkbox within w_info_resumen_facturacion
integer x = 2437
integer y = 772
integer width = 626
integer height = 72
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

