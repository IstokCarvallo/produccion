$PBExportHeader$w_info_produccion_resumido_bkp.srw
forward
global type w_info_produccion_resumido_bkp from w_para_informes
end type
type st_4 from statictext within w_info_produccion_resumido_bkp
end type
type st_1 from statictext within w_info_produccion_resumido_bkp
end type
type st_2 from statictext within w_info_produccion_resumido_bkp
end type
type em_desde from editmask within w_info_produccion_resumido_bkp
end type
type dw_cliente from datawindow within w_info_produccion_resumido_bkp
end type
type st_6 from statictext within w_info_produccion_resumido_bkp
end type
type dw_planta from datawindow within w_info_produccion_resumido_bkp
end type
type st_3 from statictext within w_info_produccion_resumido_bkp
end type
type dw_especie from datawindow within w_info_produccion_resumido_bkp
end type
type st_7 from statictext within w_info_produccion_resumido_bkp
end type
type em_hasta from editmask within w_info_produccion_resumido_bkp
end type
type st_8 from statictext within w_info_produccion_resumido_bkp
end type
type dw_productor from datawindow within w_info_produccion_resumido_bkp
end type
type gb_3 from groupbox within w_info_produccion_resumido_bkp
end type
type st_5 from statictext within w_info_produccion_resumido_bkp
end type
type cbx_peso from checkbox within w_info_produccion_resumido_bkp
end type
type tit_peso from statictext within w_info_produccion_resumido_bkp
end type
type st_variedad from statictext within w_info_produccion_resumido_bkp
end type
type cbx_variedad from checkbox within w_info_produccion_resumido_bkp
end type
type em_variedad from editmask within w_info_produccion_resumido_bkp
end type
type cb_buscavariedad from commandbutton within w_info_produccion_resumido_bkp
end type
type cbx_consvariedad from checkbox within w_info_produccion_resumido_bkp
end type
type st_embalaje from statictext within w_info_produccion_resumido_bkp
end type
type cbx_embalaje from checkbox within w_info_produccion_resumido_bkp
end type
type em_embalaje from editmask within w_info_produccion_resumido_bkp
end type
type cb_buscaembalaje from commandbutton within w_info_produccion_resumido_bkp
end type
type cbx_consembalaje from checkbox within w_info_produccion_resumido_bkp
end type
type st_11 from statictext within w_info_produccion_resumido_bkp
end type
type cbx_etiqueta from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_consetiqueta from checkbox within w_info_produccion_resumido_bkp
end type
type st_calidad from statictext within w_info_produccion_resumido_bkp
end type
type cbx_calidad from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_conscalidad from checkbox within w_info_produccion_resumido_bkp
end type
type gb_13 from groupbox within w_info_produccion_resumido_bkp
end type
type st_10 from statictext within w_info_produccion_resumido_bkp
end type
type cbx_productor from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_productorcons from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_planta from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_plantascons from checkbox within w_info_produccion_resumido_bkp
end type
type st_13 from statictext within w_info_produccion_resumido_bkp
end type
type cbx_fecemb from checkbox within w_info_produccion_resumido_bkp
end type
type gb_4 from groupbox within w_info_produccion_resumido_bkp
end type
type cbx_infopacking from checkbox within w_info_produccion_resumido_bkp
end type
type st_14 from statictext within w_info_produccion_resumido_bkp
end type
type dw_pesoneto from datawindow within w_info_produccion_resumido_bkp
end type
type sle_variedad from singlelineedit within w_info_produccion_resumido_bkp
end type
type dw_etiqueta from datawindow within w_info_produccion_resumido_bkp
end type
type em_calidad from editmask within w_info_produccion_resumido_bkp
end type
type cbx_especie from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_especiecons from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_cliente from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_predio from checkbox within w_info_produccion_resumido_bkp
end type
type st_16 from statictext within w_info_produccion_resumido_bkp
end type
type em_orden from editmask within w_info_produccion_resumido_bkp
end type
type cbx_ordentodo from checkbox within w_info_produccion_resumido_bkp
end type
type cbx_ordencon from checkbox within w_info_produccion_resumido_bkp
end type
type st_17 from statictext within w_info_produccion_resumido_bkp
end type
end forward

global type w_info_produccion_resumido_bkp from w_para_informes
integer x = 14
integer y = 32
integer width = 3616
integer height = 1492
string title = "PRODUCCION HISTORICA RESUMIDA"
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
dw_especie dw_especie
st_7 st_7
em_hasta em_hasta
st_8 st_8
dw_productor dw_productor
gb_3 gb_3
st_5 st_5
cbx_peso cbx_peso
tit_peso tit_peso
st_variedad st_variedad
cbx_variedad cbx_variedad
em_variedad em_variedad
cb_buscavariedad cb_buscavariedad
cbx_consvariedad cbx_consvariedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_consembalaje cbx_consembalaje
st_11 st_11
cbx_etiqueta cbx_etiqueta
cbx_consetiqueta cbx_consetiqueta
st_calidad st_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
gb_13 gb_13
st_10 st_10
cbx_productor cbx_productor
cbx_productorcons cbx_productorcons
cbx_planta cbx_planta
cbx_plantascons cbx_plantascons
st_13 st_13
cbx_fecemb cbx_fecemb
gb_4 gb_4
cbx_infopacking cbx_infopacking
st_14 st_14
dw_pesoneto dw_pesoneto
sle_variedad sle_variedad
dw_etiqueta dw_etiqueta
em_calidad em_calidad
cbx_especie cbx_especie
cbx_especiecons cbx_especiecons
cbx_cliente cbx_cliente
cbx_predio cbx_predio
st_16 st_16
em_orden em_orden
cbx_ordentodo cbx_ordentodo
cbx_ordencon cbx_ordencon
st_17 st_17
end type
global w_info_produccion_resumido_bkp w_info_produccion_resumido_bkp

type variables
Long	ll_nroorden
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_especie, idwc_productor, idwc_pesoneto, &
						idwc_etiqueta, idwc_packing, idwc_zonas, idwc_fruta

String is_NomPlanta
end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existeproductor (long ll_productor)
public function boolean existepacking (integer li_planta)
public function boolean noexisteetiqueta (integer li_etiqueta)
public function string buscdescfruta (integer codigo)
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

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

public function boolean existeproductor (long ll_productor);String	ls_Nombre

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

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dba.plantadesp
WHERE	plde_codigo =  :li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	//istr_mant.argumento[7] = String(li_planta)
	RETURN True 
END IF
end function

public function boolean noexisteetiqueta (integer li_etiqueta);String	ls_nombre


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

public function string buscdescfruta (integer codigo);String	ls_descri


    SELECT frre_descri  
    INTO :ls_descri  
    FROM dba.recfruprocee as re, dba.frutarecibida as fr 
   WHERE re.frre_codigo = fr.frre_codigo and  
         re.frre_codigo = :codigo ;

		
RETURN ls_descri

end function

on w_info_produccion_resumido_bkp.create
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
this.dw_especie=create dw_especie
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_8=create st_8
this.dw_productor=create dw_productor
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.cbx_variedad=create cbx_variedad
this.em_variedad=create em_variedad
this.cb_buscavariedad=create cb_buscavariedad
this.cbx_consvariedad=create cbx_consvariedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_consembalaje=create cbx_consembalaje
this.st_11=create st_11
this.cbx_etiqueta=create cbx_etiqueta
this.cbx_consetiqueta=create cbx_consetiqueta
this.st_calidad=create st_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
this.gb_13=create gb_13
this.st_10=create st_10
this.cbx_productor=create cbx_productor
this.cbx_productorcons=create cbx_productorcons
this.cbx_planta=create cbx_planta
this.cbx_plantascons=create cbx_plantascons
this.st_13=create st_13
this.cbx_fecemb=create cbx_fecemb
this.gb_4=create gb_4
this.cbx_infopacking=create cbx_infopacking
this.st_14=create st_14
this.dw_pesoneto=create dw_pesoneto
this.sle_variedad=create sle_variedad
this.dw_etiqueta=create dw_etiqueta
this.em_calidad=create em_calidad
this.cbx_especie=create cbx_especie
this.cbx_especiecons=create cbx_especiecons
this.cbx_cliente=create cbx_cliente
this.cbx_predio=create cbx_predio
this.st_16=create st_16
this.em_orden=create em_orden
this.cbx_ordentodo=create cbx_ordentodo
this.cbx_ordencon=create cbx_ordencon
this.st_17=create st_17
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.dw_cliente
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.dw_especie
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.em_hasta
this.Control[iCurrent+12]=this.st_8
this.Control[iCurrent+13]=this.dw_productor
this.Control[iCurrent+14]=this.gb_3
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.cbx_peso
this.Control[iCurrent+17]=this.tit_peso
this.Control[iCurrent+18]=this.st_variedad
this.Control[iCurrent+19]=this.cbx_variedad
this.Control[iCurrent+20]=this.em_variedad
this.Control[iCurrent+21]=this.cb_buscavariedad
this.Control[iCurrent+22]=this.cbx_consvariedad
this.Control[iCurrent+23]=this.st_embalaje
this.Control[iCurrent+24]=this.cbx_embalaje
this.Control[iCurrent+25]=this.em_embalaje
this.Control[iCurrent+26]=this.cb_buscaembalaje
this.Control[iCurrent+27]=this.cbx_consembalaje
this.Control[iCurrent+28]=this.st_11
this.Control[iCurrent+29]=this.cbx_etiqueta
this.Control[iCurrent+30]=this.cbx_consetiqueta
this.Control[iCurrent+31]=this.st_calidad
this.Control[iCurrent+32]=this.cbx_calidad
this.Control[iCurrent+33]=this.cbx_conscalidad
this.Control[iCurrent+34]=this.gb_13
this.Control[iCurrent+35]=this.st_10
this.Control[iCurrent+36]=this.cbx_productor
this.Control[iCurrent+37]=this.cbx_productorcons
this.Control[iCurrent+38]=this.cbx_planta
this.Control[iCurrent+39]=this.cbx_plantascons
this.Control[iCurrent+40]=this.st_13
this.Control[iCurrent+41]=this.cbx_fecemb
this.Control[iCurrent+42]=this.gb_4
this.Control[iCurrent+43]=this.cbx_infopacking
this.Control[iCurrent+44]=this.st_14
this.Control[iCurrent+45]=this.dw_pesoneto
this.Control[iCurrent+46]=this.sle_variedad
this.Control[iCurrent+47]=this.dw_etiqueta
this.Control[iCurrent+48]=this.em_calidad
this.Control[iCurrent+49]=this.cbx_especie
this.Control[iCurrent+50]=this.cbx_especiecons
this.Control[iCurrent+51]=this.cbx_cliente
this.Control[iCurrent+52]=this.cbx_predio
this.Control[iCurrent+53]=this.st_16
this.Control[iCurrent+54]=this.em_orden
this.Control[iCurrent+55]=this.cbx_ordentodo
this.Control[iCurrent+56]=this.cbx_ordencon
this.Control[iCurrent+57]=this.st_17
end on

on w_info_produccion_resumido_bkp.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.dw_especie)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_8)
destroy(this.dw_productor)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.cbx_variedad)
destroy(this.em_variedad)
destroy(this.cb_buscavariedad)
destroy(this.cbx_consvariedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_consembalaje)
destroy(this.st_11)
destroy(this.cbx_etiqueta)
destroy(this.cbx_consetiqueta)
destroy(this.st_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
destroy(this.gb_13)
destroy(this.st_10)
destroy(this.cbx_productor)
destroy(this.cbx_productorcons)
destroy(this.cbx_planta)
destroy(this.cbx_plantascons)
destroy(this.st_13)
destroy(this.cbx_fecemb)
destroy(this.gb_4)
destroy(this.cbx_infopacking)
destroy(this.st_14)
destroy(this.dw_pesoneto)
destroy(this.sle_variedad)
destroy(this.dw_etiqueta)
destroy(this.em_calidad)
destroy(this.cbx_especie)
destroy(this.cbx_especiecons)
destroy(this.cbx_cliente)
destroy(this.cbx_predio)
destroy(this.st_16)
destroy(this.em_orden)
destroy(this.cbx_ordentodo)
destroy(this.cbx_ordencon)
destroy(this.st_17)
end on

event open;x	=	0
y	=	0

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

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve()
dw_productor.InsertRow(0)

dw_productor.Enabled											=	False
dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()
dw_etiqueta.InsertRow(0)

dw_etiqueta.Enabled											=	False
dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve()
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(192,192,192)))


em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text					=	String(Today())
istr_mant.argumento[1]		= 	String(gi_CodExport) 		// 	cliente
istr_mant.argumento[2]		=	String(gi_CodEspecie)	//	especie
istr_mant.argumento[3]		= 	"0"								//	planta
istr_mant.argumento[4]		= 	"-9"							//	productor
istr_mant.argumento[5]		=	"-9" 							// variedad
istr_mant.argumento[6]  	= 	"-9"							//	embalaje
istr_mant.argumento[7]  	= 	"-9"							//	etiqueta
istr_mant.argumento[8]  	= 	"-9"							//	calidad
istr_mant.argumento[9]		= 	em_desde.Text			//	fecha inicio
istr_mant.argumento[10]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[11] 	=  "1"								//	peso
istr_mant.argumento[23]	= 	"0"								//	packing

istr_mant.argumento[13] 	=  "1"								//	Consolidado Planta
istr_mant.argumento[14] 	=  "1"								//	Consolidado Productor
istr_mant.argumento[15] 	=  "1"								//	Consolidado Variedad
istr_mant.argumento[16] 	=  "1"								//	Consolidado Embalaje
istr_mant.argumento[17] 	=  "1"								//	Consolidado Etiqueta
istr_mant.argumento[18] 	=  "1"								//	Consolidado Calidad
istr_mant.argumento[33] 	=  "0"								//	Todos Packing

istr_mant.argumento[40]	= 	"-9"							//	Zonas
istr_mant.argumento[41]	=	Message.StringParm		//	Tipo Orden
ll_nroorden						=	-9								// Orden de Proceso
end event

event resize;//
end event

type st_titulo from w_para_informes`st_titulo within w_info_produccion_resumido_bkp
integer x = 27
integer y = 64
integer width = 3259
string text = "Informe de Cajas Embaladas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_resumido_bkp
integer x = 3365
integer y = 860
integer taborder = 160
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta, li_especie, li_variedad, li_etiqueta, li_consplanta,&
			li_consproductor, li_consvariedad, li_consembalaje, li_consetiqueta,li_conscalidad,&
			li_conspacking, li_packing, li_zona, li_fruta, li_Agru, li_tipoorden, li_consfecha
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_cajas, ls_embalaje, ls_calidad,&
         ls_null, ls_cliente, ls_especie, ls_descri
Long		ll_productor

SetNull(ls_null)

istr_info.titulo	= 'INFORME DE CAJAS EMBALADAS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_emba_cajas_encab"

li_cliente 			=	Integer(istr_mant.argumento[1])
li_especie		=	Integer(istr_mant.argumento[2])
li_planta			=	Integer(istr_mant.argumento[3])
ll_productor		=	Long(istr_mant.argumento[4])
li_variedad		=	Integer(istr_mant.argumento[5])
ls_embalaje		=	istr_mant.argumento[6]
li_etiqueta		=	Integer(istr_mant.argumento[7])
ls_calidad		=	istr_mant.argumento[8]
ld_desde			=	Date(istr_mant.argumento[9])
ld_hasta			=	Date(istr_mant.argumento[10])
li_packing		=	Integer(istr_mant.argumento[23])
li_tipoorden		=	Integer(istr_mant.argumento[41])

texto_desde	=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF cbx_fecemb.Checked THEN
	li_ConsFecha = 1
ELSE
	li_ConsFecha = 0
END IF

//Obtiene descripciones de cliente y especie
  Select clie_nombre  
    into :ls_cliente  
    from dba.clientesprod  
   where clie_codigo = :li_cliente ;
	
	Select espe_nombre  
    into :ls_especie  
    from dba.especies  
   where espe_codigo = :li_especie ;

IF cbx_peso.Checked=False THEN
	ls_cajas = "Bultos"
	istr_mant.argumento[11]	=	"1"
ELSE
	istr_mant.argumento[11]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[11] 
END IF

li_consplanta		=	Integer(istr_mant.argumento[13])
li_consproductor	=	Integer(istr_mant.argumento[14])
li_consvariedad		=	Integer(istr_mant.argumento[15])
li_consembalaje	=	Integer(istr_mant.argumento[16])
li_consetiqueta		=	Integer(istr_mant.argumento[17])
li_conscalidad		=	Integer(istr_mant.argumento[18])
li_conspacking		=	Integer(istr_mant.argumento[33])
li_zona  				=	Integer(istr_mant.argumento[40])

IF cbx_fecemb.checked THEN
	istr_mant.argumento[41] = "0"
ELSE
	istr_mant.argumento[41] = "1"
END IF

IF cbx_infopacking.Checked <> True THEN
	IF (ll_nroorden < 1 AND ll_nroorden > -1)  OR isnull(ll_nroorden) THEN
		MessageBox("Atención","Debe Seleccionar Nº Orden de Proceso Previamente",Exclamation!)
		em_orden.SetFocus()
		RETURN
	END IF		
END IF	

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Dec(li_cliente),Dec(li_planta), Dec(li_tipoorden),Dec(ll_nroorden),Dec(ll_productor),Dec(li_especie),Dec(li_variedad),&
								 ls_embalaje,ls_calidad,Dec(li_etiqueta),ld_desde, ld_hasta,Dec(li_ConsFecha))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)
ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify("Cliente.text = '" + ls_cliente + "'")
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
		vinf.Visible	= True
		vinf.Enabled	= True
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_resumido_bkp
integer x = 3365
integer y = 1144
integer taborder = 170
end type

type st_4 from statictext within w_info_produccion_resumido_bkp
integer x = 27
integer y = 176
integer width = 1545
integer height = 804
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_produccion_resumido_bkp
integer x = 82
integer y = 460
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
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_produccion_resumido_bkp
integer x = 82
integer y = 1216
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_produccion_resumido_bkp
integer x = 462
integer y = 1204
integer width = 375
integer height = 96
integer taborder = 140
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

type dw_cliente from datawindow within w_info_produccion_resumido_bkp
integer x = 393
integer y = 260
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
	idwc_especie.Retrieve()
	idwc_productor.Retrieve()	
	idwc_etiqueta.Retrieve()
	dw_especie.SetItem(1, "espe_codigo", dw_especie.Object.espe_codigo[1])
ELSE
	This.SetItem(1, "clie_codigo", Integer(ls_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_produccion_resumido_bkp
integer x = 82
integer y = 252
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_produccion_resumido_bkp
integer x = 393
integer y = 460
integer width = 1161
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

type st_3 from statictext within w_info_produccion_resumido_bkp
integer x = 82
integer y = 848
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_especie from datawindow within w_info_produccion_resumido_bkp
integer x = 393
integer y = 856
integer width = 878
integer height = 100
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
IF ExisteEspecie(Integer(data)) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "espe_codigo", gi_CodEspecie)
	istr_mant.argumento[2]	=	String(gi_CodEspecie)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_7 from statictext within w_info_produccion_resumido_bkp
integer x = 905
integer y = 1224
integer width = 279
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_produccion_resumido_bkp
integer x = 1225
integer y = 1204
integer width = 375
integer height = 96
integer taborder = 150
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

type st_8 from statictext within w_info_produccion_resumido_bkp
integer x = 82
integer y = 680
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
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_produccion_resumido_bkp
integer x = 393
integer y = 660
integer width = 974
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;RETURN 1
end event

event itemchanged;String	 ls_null
SetNull(ls_null)

IF ExisteProductor(Long(data)) THEN
	istr_mant.argumento[4]	=	data	
	RETURN 0
ELSE
	This.SetItem(1, "prod_codigo", ls_null)
	RETURN 1
END IF
end event

type gb_3 from groupbox within w_info_produccion_resumido_bkp
integer x = 55
integer y = 1144
integer width = 3209
integer height = 188
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_5 from statictext within w_info_produccion_resumido_bkp
integer x = 1573
integer y = 176
integer width = 1714
integer height = 804
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_peso from checkbox within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 64
integer y = 2260
integer width = 631
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cajas Equivalentes"
end type

event clicked;IF This.Checked THEN
	tit_peso.Enabled		=	True
	dw_pesoneto.Enabled	=	True
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(255,255,255)))

ELSE
	tit_peso.Enabled		=	False
	dw_pesoneto.Enabled	=	False
	dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(rgb(192,192,192)))
END IF

end event

type tit_peso from statictext within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 713
integer y = 2268
integer width = 155
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_produccion_resumido_bkp
integer x = 1650
integer y = 260
integer width = 302
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type cbx_variedad from checkbox within w_info_produccion_resumido_bkp
integer x = 2176
integer y = 196
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consvariedad.Enabled	=	True
	em_variedad.Enabled			=	False
	cb_buscavariedad.Enabled	=	False
	em_variedad.Text				=	''
	sle_variedad.Text				=	''
	istr_mant.argumento[5]		=	'0'
	istr_mant.argumento[15]		=	'0'
ELSE
	cbx_consvariedad.Enabled	=	False
	cbx_consvariedad.Checked	=	False
	em_variedad.Enabled			=	True
	cb_buscavariedad.Enabled	=	True
END IF

end event

type em_variedad from editmask within w_info_produccion_resumido_bkp
integer x = 2176
integer y = 260
integer width = 297
integer height = 92
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
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "####"
end type

event modified;Integer		li_cliente,	li_especie, li_variedad
String		ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
li_especie	=	Integer(istr_mant.argumento[2]) // Especie
li_variedad	=	Integer(This.Text)

SELECT	vari_nombre
	INTO	:ls_Nombre
	FROM	dba.variedades
	WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variedades")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Variedad no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	sle_variedad.Text			=	ls_nombre
	istr_mant.argumento[5]	=	String(li_variedad)	
END IF
end event

type cb_buscavariedad from commandbutton within w_info_produccion_resumido_bkp
integer x = 2487
integer y = 260
integer width = 96
integer height = 84
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

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente
lstr_busq.argum[2]	=	istr_mant.argumento[2] // Especie

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[4] = "" THEN
	em_variedad.SetFocus()
ELSE
	em_variedad.Text			=	lstr_busq.argum[4]
	sle_variedad.Text			=	lstr_busq.argum[5]
	istr_mant.argumento[5]	=	lstr_busq.argum[4]
END IF

end event

type cbx_consvariedad from checkbox within w_info_produccion_resumido_bkp
integer x = 2693
integer y = 196
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[5]	=	'-9'
ELSE
	istr_mant.argumento[5]	=	'0'
END IF

end event

type st_embalaje from statictext within w_info_produccion_resumido_bkp
integer x = 1650
integer y = 676
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
long backcolor = 12632256
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_produccion_resumido_bkp
integer x = 2176
integer y = 592
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consembalaje.Enabled	=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[6]		=	'Z'
	istr_mant.argumento[16]		=	'0'
ELSE
	cbx_consembalaje.Enabled	=	False
	cbx_consembalaje.Checked	=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type em_embalaje from editmask within w_info_produccion_resumido_bkp
integer x = 2176
integer y = 660
integer width = 297
integer height = 92
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
	MessageBox("Atención", "Código de Embalaje no ha sido Definido para el cliente " + String(li_cliente) + ".~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 2487
integer y = 664
integer width = 96
integer height = 84
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

event clicked;//
//Str_busqueda	lstr_busq
//
//lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente
//
//OpenWithParm(w_busc_embalajes, lstr_busq)
//
//lstr_busq	       = Message.PowerObjectParm
//
//IF lstr_busq.argum[2] = "" THEN
//	em_embalaje.SetFocus()
//ELSE
//	em_embalaje.Text			=	lstr_busq.argum[2]
//	istr_mant.argumento[6]	=	lstr_busq.argum[2]
//END IF
end event

type cbx_consembalaje from checkbox within w_info_produccion_resumido_bkp
integer x = 2693
integer y = 592
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'.9'
ELSE
	istr_mant.argumento[16]	=	'0'
END IF

end event

type st_11 from statictext within w_info_produccion_resumido_bkp
integer x = 1650
integer y = 468
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
long backcolor = 12632256
boolean enabled = false
string text = "Etiqueta"
boolean focusrectangle = false
end type

type cbx_etiqueta from checkbox within w_info_produccion_resumido_bkp
integer x = 2176
integer y = 392
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_consetiqueta.Enabled									=	True
	dw_etiqueta.Enabled										=	False
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[7]									=	'0'
	istr_mant.argumento[17]									=	'0'
ELSE
	cbx_consetiqueta.Enabled									=	False
	cbx_consetiqueta.Checked									=	False
	dw_etiqueta.Enabled											=	True
	dw_etiqueta.Object.etiq_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_etiqueta.SetFocus()
	istr_mant.argumento[17]	=	'0'
END IF
end event

type cbx_consetiqueta from checkbox within w_info_produccion_resumido_bkp
integer x = 2693
integer y = 392
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[17]	=	'1'
ELSE
	istr_mant.argumento[17]	=	'0'
END IF

end event

type st_calidad from statictext within w_info_produccion_resumido_bkp
integer x = 1655
integer y = 868
integer width = 256
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Calibre"
boolean focusrectangle = false
end type

type cbx_calidad from checkbox within w_info_produccion_resumido_bkp
integer x = 2176
integer y = 784
integer width = 297
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_conscalidad.Enabled	=	True
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[8]		=	'Z'
	istr_mant.argumento[18]		=	'0'
ELSE
	cbx_conscalidad.Enabled	=	False
	cbx_conscalidad.Checked	=	False
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type cbx_conscalidad from checkbox within w_info_produccion_resumido_bkp
integer x = 2693
integer y = 784
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'-9'
ELSE
	istr_mant.argumento[18]	=	'0'
END IF

end event

type gb_13 from groupbox within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 37
integer y = 2200
integer width = 1563
integer height = 168
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type st_10 from statictext within w_info_produccion_resumido_bkp
integer x = 27
integer y = 1140
integer width = 3259
integer height = 232
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_productor from checkbox within w_info_produccion_resumido_bkp
integer x = 398
integer y = 588
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_productorcons.Enabled									=	True
	dw_productor.Enabled										=	False
	dw_productor.Object.prod_codigo.BackGround.Color=	RGB(192, 192, 192)
	istr_mant.argumento[4]										=	'0'
	istr_mant.argumento[14]									=	'0'
ELSE
	cbx_productorcons.Enabled									=	False
	cbx_productorcons.Checked								=	False
	dw_productor.Enabled										=	True
	dw_productor.Object.prod_codigo.BackGround.Color=	RGB(255, 255, 255)
	dw_productor.SetFocus()
	istr_mant.argumento[14]	=	'0'
END IF
end event

type cbx_productorcons from checkbox within w_info_produccion_resumido_bkp
integer x = 873
integer y = 592
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[4]			=	'-9'
ELSE
	istr_mant.argumento[4]			=	'0'
END IF
	
end event

type cbx_planta from checkbox within w_info_produccion_resumido_bkp
integer x = 398
integer y = 392
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_plantascons.Enabled									=	True
	dw_planta.Enabled											=	False
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[3]									=	'0'
	istr_mant.argumento[13]									=	'0'
ELSE
	cbx_plantascons.Enabled									=	False
	cbx_plantascons.Checked									=	False
	dw_planta.Enabled											=	True
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)

	dw_planta.SetFocus()
END IF
end event

type cbx_plantascons from checkbox within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 873
integer y = 392
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[13]	=	'1'
ELSE
	istr_mant.argumento[13]	=	'0'
END IF
	
end event

type st_13 from statictext within w_info_produccion_resumido_bkp
integer x = 1934
integer y = 1224
integer width = 507
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Embalaje "
boolean focusrectangle = false
end type

type cbx_fecemb from checkbox within w_info_produccion_resumido_bkp
integer x = 2514
integer y = 1220
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidada"
boolean checked = true
end type

type gb_4 from groupbox within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 1678
integer y = 2200
integer width = 1563
integer height = 168
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type cbx_infopacking from checkbox within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 1696
integer y = 2264
integer width = 974
integer height = 80
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Informe por Packing (Crosstab)"
end type

event clicked;IF This.Checked THEN
	cbx_predio.Checked = False
	em_orden.Enabled	 = False	
	cbx_ordentodo.Enabled = False
	cbx_ordencon.Enabled = False
	em_orden.Text = ''
ELSE
	cbx_ordentodo.Enabled = True
	cbx_ordencon.Enabled = True
END IF	
end event

type st_14 from statictext within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 9
integer y = 2212
integer width = 3259
integer height = 176
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_pesoneto from datawindow within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 859
integer y = 2252
integer width = 695
integer height = 100
integer taborder = 120
boolean bringtotop = true
string dataobject = "dddw_pesoneto"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

type sle_variedad from singlelineedit within w_info_produccion_resumido_bkp
integer x = 2592
integer y = 260
integer width = 645
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
borderstyle borderstyle = stylelowered!
end type

type dw_etiqueta from datawindow within w_info_produccion_resumido_bkp
integer x = 2162
integer y = 456
integer width = 905
integer height = 96
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_etiquetas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
IF NoExisteEtiqueta(Integer(data)) THEN
	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
	dw_etiqueta.SetFocus()
	RETURN 1
ELSE
	istr_mant.argumento[7]	=	data
	
END IF


end event

event itemerror;RETURN 1
end event

type em_calidad from editmask within w_info_produccion_resumido_bkp
integer x = 2176
integer y = 856
integer width = 297
integer height = 92
integer taborder = 100
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

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
li_especie	=	Integer(istr_mant.argumento[2]) // Especie
li_variedad	=	Integer(istr_mant.argumento[5]) // Variedad
ls_calibre	=	This.Text

ls_calibre	=	Trim(ls_calibre) + Fill(" ",3 - Len(ls_calibre))

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dba.variecalibre
	WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad
	AND	vaca_calibr	=	:ls_calibre;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variecalibre")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Calibre no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[8]	=	ls_calibre
	em_calidad.Text			=	ls_calibre
END IF
end event

type cbx_especie from checkbox within w_info_produccion_resumido_bkp
integer x = 398
integer y = 784
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	cbx_especiecons.Enabled									=	True
	dw_especie.Enabled											=	False
	dw_especie.Object.espe_codigo.BackGround.Color	=	RGB(192, 192, 192)
   	istr_mant.argumento[2]                          					= '0'
	dw_especie.SetItem(1,"espe_codigo",li_null)
ELSE
	cbx_especiecons.Enabled									=	False
	cbx_especiecons.Checked									=	False
	dw_especie.Enabled											=	True
	dw_especie.Object.espe_codigo.BackGround.Color	=	RGB(255, 255, 255)

	dw_especie.SetFocus()
END IF
end event

type cbx_especiecons from checkbox within w_info_produccion_resumido_bkp
integer x = 878
integer y = 784
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Consolidadas"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[2]	=	'-9'
END IF
	
end event

type cbx_cliente from checkbox within w_info_produccion_resumido_bkp
integer x = 398
integer y = 196
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	dw_cliente.Enabled										=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(192, 192, 192)
  	 istr_mant.argumento[1]                          = '-1'
	dw_cliente.SetItem(1,"clie_codigo",li_null)
ELSE
	dw_cliente.Enabled										=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_cliente.SetFocus()
END IF
end event

type cbx_predio from checkbox within w_info_produccion_resumido_bkp
boolean visible = false
integer x = 2761
integer y = 2264
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Por Predio"
end type

event clicked;IF This.Checked THEN
	cbx_infopacking.Checked = False
END IF	
end event

type st_16 from statictext within w_info_produccion_resumido_bkp
integer x = 27
integer y = 980
integer width = 3259
integer height = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_orden from editmask within w_info_produccion_resumido_bkp
integer x = 562
integer y = 1004
integer width = 375
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
string minmax = "~~8"
end type

event modified;ll_nroorden	=	Long(This.Text)
end event

type cbx_ordentodo from checkbox within w_info_produccion_resumido_bkp
integer x = 1006
integer y = 1020
integer width = 293
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_ordencon.Enabled									=	True
	em_orden.Enabled										=	False
	em_orden.Text 											=	''
	ll_nroorden													=	-1
ELSE
	cbx_ordencon.Enabled									=	True
	em_orden.Enabled										=	True
	cbx_ordencon.Checked									=	False
	ll_nroorden													=	Long(em_orden.Text)
	em_orden.SetFocus()
END IF
end event

type cbx_ordencon from checkbox within w_info_produccion_resumido_bkp
integer x = 1371
integer y = 1020
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_ordentodo.Checked								=	True
	em_orden.Enabled										=	False
	em_orden.Text 											=	''
	ll_nroorden												=	-9
ELSE
	cbx_ordentodo.Enabled								=	True
	em_orden.Enabled										=	True
	cbx_ordentodo.Checked								=	False
	ll_nroorden												=	Long(em_orden.Text)
	em_orden.SetFocus()
END IF
	
end event

type st_17 from statictext within w_info_produccion_resumido_bkp
integer x = 82
integer y = 1016
integer width = 443
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Orden Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

