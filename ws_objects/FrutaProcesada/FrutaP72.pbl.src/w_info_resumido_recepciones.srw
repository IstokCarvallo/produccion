$PBExportHeader$w_info_resumido_recepciones.srw
forward
global type w_info_resumido_recepciones from w_para_informes
end type
type st_4 from statictext within w_info_resumido_recepciones
end type
type st_1 from statictext within w_info_resumido_recepciones
end type
type st_2 from statictext within w_info_resumido_recepciones
end type
type em_desde from editmask within w_info_resumido_recepciones
end type
type dw_cliente from datawindow within w_info_resumido_recepciones
end type
type st_6 from statictext within w_info_resumido_recepciones
end type
type dw_planta from datawindow within w_info_resumido_recepciones
end type
type st_3 from statictext within w_info_resumido_recepciones
end type
type st_7 from statictext within w_info_resumido_recepciones
end type
type em_hasta from editmask within w_info_resumido_recepciones
end type
type st_8 from statictext within w_info_resumido_recepciones
end type
type dw_productor from datawindow within w_info_resumido_recepciones
end type
type gb_3 from groupbox within w_info_resumido_recepciones
end type
type st_5 from statictext within w_info_resumido_recepciones
end type
type cbx_peso from checkbox within w_info_resumido_recepciones
end type
type tit_peso from statictext within w_info_resumido_recepciones
end type
type st_variedad from statictext within w_info_resumido_recepciones
end type
type st_embalaje from statictext within w_info_resumido_recepciones
end type
type cbx_embalaje from checkbox within w_info_resumido_recepciones
end type
type em_embalaje from editmask within w_info_resumido_recepciones
end type
type cb_buscaembalaje from commandbutton within w_info_resumido_recepciones
end type
type cbx_consembalaje from checkbox within w_info_resumido_recepciones
end type
type st_11 from statictext within w_info_resumido_recepciones
end type
type cbx_etiqueta from checkbox within w_info_resumido_recepciones
end type
type cbx_consetiqueta from checkbox within w_info_resumido_recepciones
end type
type st_calidad from statictext within w_info_resumido_recepciones
end type
type cbx_calidad from checkbox within w_info_resumido_recepciones
end type
type cbx_conscalidad from checkbox within w_info_resumido_recepciones
end type
type gb_13 from groupbox within w_info_resumido_recepciones
end type
type cbx_productor from checkbox within w_info_resumido_recepciones
end type
type cbx_productorcons from checkbox within w_info_resumido_recepciones
end type
type cbx_planta from checkbox within w_info_resumido_recepciones
end type
type cbx_plantascons from checkbox within w_info_resumido_recepciones
end type
type st_18 from statictext within w_info_resumido_recepciones
end type
type cbx_packing from checkbox within w_info_resumido_recepciones
end type
type cbx_packingcons from checkbox within w_info_resumido_recepciones
end type
type dw_packing from datawindow within w_info_resumido_recepciones
end type
type st_9 from statictext within w_info_resumido_recepciones
end type
type cbx_zonas from checkbox within w_info_resumido_recepciones
end type
type dw_zonas from datawindow within w_info_resumido_recepciones
end type
type cbx_zonascons from checkbox within w_info_resumido_recepciones
end type
type cbx_fecemb from checkbox within w_info_resumido_recepciones
end type
type gb_4 from groupbox within w_info_resumido_recepciones
end type
type st_12 from statictext within w_info_resumido_recepciones
end type
type st_14 from statictext within w_info_resumido_recepciones
end type
type dw_pesoneto from datawindow within w_info_resumido_recepciones
end type
type st_15 from statictext within w_info_resumido_recepciones
end type
type dw_etiqueta from datawindow within w_info_resumido_recepciones
end type
type cbx_todosfru from checkbox within w_info_resumido_recepciones
end type
type cbx_consfru from checkbox within w_info_resumido_recepciones
end type
type dw_frurecep from datawindow within w_info_resumido_recepciones
end type
type em_calidad from editmask within w_info_resumido_recepciones
end type
type cbx_cliente from checkbox within w_info_resumido_recepciones
end type
type uo_selespecie from uo_seleccion_especie within w_info_resumido_recepciones
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_resumido_recepciones
end type
type cbx_varirotula from checkbox within w_info_resumido_recepciones
end type
type cbx_1 from checkbox within w_info_resumido_recepciones
end type
type em_norden from editmask within w_info_resumido_recepciones
end type
type st_21 from statictext within w_info_resumido_recepciones
end type
type cbx_2 from checkbox within w_info_resumido_recepciones
end type
type gb_5 from groupbox within w_info_resumido_recepciones
end type
type st_10 from statictext within w_info_resumido_recepciones
end type
type ddlb_tipoen from dropdownlistbox within w_info_resumido_recepciones
end type
type st_16 from statictext within w_info_resumido_recepciones
end type
type st_17 from statictext within w_info_resumido_recepciones
end type
type dw_categorias from datawindow within w_info_resumido_recepciones
end type
type cbx_categorias from checkbox within w_info_resumido_recepciones
end type
type uo_selcategoria from uo_seleccion_categoria within w_info_resumido_recepciones
end type
type gb_7 from groupbox within w_info_resumido_recepciones
end type
type st_13 from statictext within w_info_resumido_recepciones
end type
type st_19 from statictext within w_info_resumido_recepciones
end type
type em_guia from editmask within w_info_resumido_recepciones
end type
type cbx_4 from checkbox within w_info_resumido_recepciones
end type
type cbx_3 from checkbox within w_info_resumido_recepciones
end type
end forward

global type w_info_resumido_recepciones from w_para_informes
integer x = 14
integer y = 32
integer width = 3968
integer height = 2460
string title = "DETALLE RECEPCIONES ESPECIALES"
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
st_8 st_8
dw_productor dw_productor
gb_3 gb_3
st_5 st_5
cbx_peso cbx_peso
tit_peso tit_peso
st_variedad st_variedad
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
cbx_productor cbx_productor
cbx_productorcons cbx_productorcons
cbx_planta cbx_planta
cbx_plantascons cbx_plantascons
st_18 st_18
cbx_packing cbx_packing
cbx_packingcons cbx_packingcons
dw_packing dw_packing
st_9 st_9
cbx_zonas cbx_zonas
dw_zonas dw_zonas
cbx_zonascons cbx_zonascons
cbx_fecemb cbx_fecemb
gb_4 gb_4
st_12 st_12
st_14 st_14
dw_pesoneto dw_pesoneto
st_15 st_15
dw_etiqueta dw_etiqueta
cbx_todosfru cbx_todosfru
cbx_consfru cbx_consfru
dw_frurecep dw_frurecep
em_calidad em_calidad
cbx_cliente cbx_cliente
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
cbx_1 cbx_1
em_norden em_norden
st_21 st_21
cbx_2 cbx_2
gb_5 gb_5
st_10 st_10
ddlb_tipoen ddlb_tipoen
st_16 st_16
st_17 st_17
dw_categorias dw_categorias
cbx_categorias cbx_categorias
uo_selcategoria uo_selcategoria
gb_7 gb_7
st_13 st_13
st_19 st_19
em_guia em_guia
cbx_4 cbx_4
cbx_3 cbx_3
end type
global w_info_resumido_recepciones w_info_resumido_recepciones

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_pesoneto, &
						idwc_etiqueta, idwc_packing, idwc_zonas, idwc_fruta, idwc_categorias

String 	is_NomPlanta
Long		ll_norden, il_guia
Integer	ii_tipoi

uo_frutarecepcion			iuo_frutarecepcion
uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
uo_seleccion_categoria	iuo_selcategoria
uo_calibre								iuo_calibre
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
Integer	li_cliente

li_cliente	= dw_cliente.Object.clie_Codigo[1]

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores as pro,dba.productoresclientes as cli
	WHERE	pro.prod_codigo =	:ll_productor
	AND	pro.prod_codigo = cli.prod_codigo
	AND	:li_cliente in (-1,cli.clie_codigo);
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de productor no ha sido definido o pertenece a otro cliente.~r~r" + &
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

on w_info_resumido_recepciones.create
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
this.st_8=create st_8
this.dw_productor=create dw_productor
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
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
this.cbx_productor=create cbx_productor
this.cbx_productorcons=create cbx_productorcons
this.cbx_planta=create cbx_planta
this.cbx_plantascons=create cbx_plantascons
this.st_18=create st_18
this.cbx_packing=create cbx_packing
this.cbx_packingcons=create cbx_packingcons
this.dw_packing=create dw_packing
this.st_9=create st_9
this.cbx_zonas=create cbx_zonas
this.dw_zonas=create dw_zonas
this.cbx_zonascons=create cbx_zonascons
this.cbx_fecemb=create cbx_fecemb
this.gb_4=create gb_4
this.st_12=create st_12
this.st_14=create st_14
this.dw_pesoneto=create dw_pesoneto
this.st_15=create st_15
this.dw_etiqueta=create dw_etiqueta
this.cbx_todosfru=create cbx_todosfru
this.cbx_consfru=create cbx_consfru
this.dw_frurecep=create dw_frurecep
this.em_calidad=create em_calidad
this.cbx_cliente=create cbx_cliente
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.cbx_1=create cbx_1
this.em_norden=create em_norden
this.st_21=create st_21
this.cbx_2=create cbx_2
this.gb_5=create gb_5
this.st_10=create st_10
this.ddlb_tipoen=create ddlb_tipoen
this.st_16=create st_16
this.st_17=create st_17
this.dw_categorias=create dw_categorias
this.cbx_categorias=create cbx_categorias
this.uo_selcategoria=create uo_selcategoria
this.gb_7=create gb_7
this.st_13=create st_13
this.st_19=create st_19
this.em_guia=create em_guia
this.cbx_4=create cbx_4
this.cbx_3=create cbx_3
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
this.Control[iCurrent+11]=this.st_8
this.Control[iCurrent+12]=this.dw_productor
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.st_5
this.Control[iCurrent+15]=this.cbx_peso
this.Control[iCurrent+16]=this.tit_peso
this.Control[iCurrent+17]=this.st_variedad
this.Control[iCurrent+18]=this.st_embalaje
this.Control[iCurrent+19]=this.cbx_embalaje
this.Control[iCurrent+20]=this.em_embalaje
this.Control[iCurrent+21]=this.cb_buscaembalaje
this.Control[iCurrent+22]=this.cbx_consembalaje
this.Control[iCurrent+23]=this.st_11
this.Control[iCurrent+24]=this.cbx_etiqueta
this.Control[iCurrent+25]=this.cbx_consetiqueta
this.Control[iCurrent+26]=this.st_calidad
this.Control[iCurrent+27]=this.cbx_calidad
this.Control[iCurrent+28]=this.cbx_conscalidad
this.Control[iCurrent+29]=this.gb_13
this.Control[iCurrent+30]=this.cbx_productor
this.Control[iCurrent+31]=this.cbx_productorcons
this.Control[iCurrent+32]=this.cbx_planta
this.Control[iCurrent+33]=this.cbx_plantascons
this.Control[iCurrent+34]=this.st_18
this.Control[iCurrent+35]=this.cbx_packing
this.Control[iCurrent+36]=this.cbx_packingcons
this.Control[iCurrent+37]=this.dw_packing
this.Control[iCurrent+38]=this.st_9
this.Control[iCurrent+39]=this.cbx_zonas
this.Control[iCurrent+40]=this.dw_zonas
this.Control[iCurrent+41]=this.cbx_zonascons
this.Control[iCurrent+42]=this.cbx_fecemb
this.Control[iCurrent+43]=this.gb_4
this.Control[iCurrent+44]=this.st_12
this.Control[iCurrent+45]=this.st_14
this.Control[iCurrent+46]=this.dw_pesoneto
this.Control[iCurrent+47]=this.st_15
this.Control[iCurrent+48]=this.dw_etiqueta
this.Control[iCurrent+49]=this.cbx_todosfru
this.Control[iCurrent+50]=this.cbx_consfru
this.Control[iCurrent+51]=this.dw_frurecep
this.Control[iCurrent+52]=this.em_calidad
this.Control[iCurrent+53]=this.cbx_cliente
this.Control[iCurrent+54]=this.uo_selespecie
this.Control[iCurrent+55]=this.uo_selvariedad
this.Control[iCurrent+56]=this.cbx_varirotula
this.Control[iCurrent+57]=this.cbx_1
this.Control[iCurrent+58]=this.em_norden
this.Control[iCurrent+59]=this.st_21
this.Control[iCurrent+60]=this.cbx_2
this.Control[iCurrent+61]=this.gb_5
this.Control[iCurrent+62]=this.st_10
this.Control[iCurrent+63]=this.ddlb_tipoen
this.Control[iCurrent+64]=this.st_16
this.Control[iCurrent+65]=this.st_17
this.Control[iCurrent+66]=this.dw_categorias
this.Control[iCurrent+67]=this.cbx_categorias
this.Control[iCurrent+68]=this.uo_selcategoria
this.Control[iCurrent+69]=this.gb_7
this.Control[iCurrent+70]=this.st_13
this.Control[iCurrent+71]=this.st_19
this.Control[iCurrent+72]=this.em_guia
this.Control[iCurrent+73]=this.cbx_4
this.Control[iCurrent+74]=this.cbx_3
end on

on w_info_resumido_recepciones.destroy
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
destroy(this.st_8)
destroy(this.dw_productor)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.tit_peso)
destroy(this.st_variedad)
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
destroy(this.cbx_productor)
destroy(this.cbx_productorcons)
destroy(this.cbx_planta)
destroy(this.cbx_plantascons)
destroy(this.st_18)
destroy(this.cbx_packing)
destroy(this.cbx_packingcons)
destroy(this.dw_packing)
destroy(this.st_9)
destroy(this.cbx_zonas)
destroy(this.dw_zonas)
destroy(this.cbx_zonascons)
destroy(this.cbx_fecemb)
destroy(this.gb_4)
destroy(this.st_12)
destroy(this.st_14)
destroy(this.dw_pesoneto)
destroy(this.st_15)
destroy(this.dw_etiqueta)
destroy(this.cbx_todosfru)
destroy(this.cbx_consfru)
destroy(this.dw_frurecep)
destroy(this.em_calidad)
destroy(this.cbx_cliente)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.cbx_1)
destroy(this.em_norden)
destroy(this.st_21)
destroy(this.cbx_2)
destroy(this.gb_5)
destroy(this.st_10)
destroy(this.ddlb_tipoen)
destroy(this.st_16)
destroy(this.st_17)
destroy(this.dw_categorias)
destroy(this.cbx_categorias)
destroy(this.uo_selcategoria)
destroy(this.gb_7)
destroy(this.st_13)
destroy(this.st_19)
destroy(this.em_guia)
destroy(this.cbx_4)
destroy(this.cbx_3)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

iuo_frutarecepcion		=	CREATE	uo_frutarecepcion		
iuo_calibre   				=	Create uo_calibre

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_zonas.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
idwc_zonas.Retrieve()
dw_zonas.InsertRow(0)

dw_zonas.Enabled											=	False
dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(192, 192, 192)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

dw_planta.Enabled											=	False
dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(2)
dw_packing.InsertRow(0)

dw_packing.Enabled											=	False
dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_CodExport)
dw_productor.InsertRow(0)

dw_productor.Enabled											=	False
dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(192, 192, 192)

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

//categoria
IF IsNull(uo_selcategoria.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selcategoria.Seleccion(True,True)
END IF

// uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
END IF

dw_pesoneto.GetChild("enva_pesone", idwc_pesoneto)
idwc_pesoneto.SetTransObject(SQLCA)
idwc_pesoneto.Retrieve()
dw_pesoneto.InsertRow(0)
dw_pesoneto.SetItem(1, "enva_pesone", 8.20)
tit_peso.Enabled		=	False
dw_pesoneto.Enabled	=	False
dw_pesoneto.Modify("enva_pesone.BackGround.Color = " + String(RGB(166,180,210)))

dw_frurecep.GetChild("frre_codigo", idwc_fruta)
idwc_fruta.SetTransObject(SQLCA)
idwc_fruta.Retrieve()
dw_frurecep.InsertRow(0)

dw_frurecep.enabled	=	false

//dw_categorias.GetChild("cate_codigo", idwc_categorias)
//idwc_categorias.SetTransObject(SQLCA)
//idwc_categorias.Retrieve(0)
////idwc_categorias.SetFilter("cate_embala = -1")
////idwc_categorias.Filter()
//dw_categorias.InsertRow(0)

em_desde.Text				=	String(RelativeDate(Today(), -365))
em_hasta.Text				=	String(Today())
istr_mant.argumento[1]	= 	String(gi_CodExport) 	// cliente
istr_mant.argumento[3]	= 	"0"							//	planta
istr_mant.argumento[4]	= 	"0"							//	productor
istr_mant.argumento[6]  =  "Z"							//	embalaje
istr_mant.argumento[7]  =  "0"							//	etiqueta
istr_mant.argumento[8]  =  "Z"							//	calidad
istr_mant.argumento[9]	= 	em_desde.Text				//	fecha inicio
istr_mant.argumento[10]	=	em_hasta.Text				//	fecha final
istr_mant.argumento[11] =  "1"							//	peso
istr_mant.argumento[23]	= 	"0"							//	packing
istr_mant.argumento[13] =  "1"							//	Consolidado Planta
istr_mant.argumento[14] =  "1"							//	Consolidado Productor
istr_mant.argumento[16] =  "1"							//	Consolidado Embalaje
istr_mant.argumento[17] =  "1"							//	Consolidado Etiqueta
istr_mant.argumento[18] =  "1"							//	Consolidado Calidad
istr_mant.argumento[33] =  "0"							//	Todos Packing
istr_mant.argumento[40]	= 	"-9"							//	Zonas
ll_norden					=	-9								// Orden de proceso
il_guia						= 	-9								//	Guia
end event

type st_computador from w_para_informes`st_computador within w_info_resumido_recepciones
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumido_recepciones
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumido_recepciones
end type

type p_logo from w_para_informes`p_logo within w_info_resumido_recepciones
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumido_recepciones
integer width = 3259
string text = "Informe Detalle Recepciones Especiales"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumido_recepciones
integer x = 3625
integer y = 1680
integer taborder = 160
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta,  li_etiqueta, li_consplanta,&
			li_consproductor, li_consembalaje, li_consetiqueta,li_conscalidad,&
			li_conspacking, li_packing, li_zona, li_fruta, li_Agru, li_varirotula, li_cate
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_cajas, ls_embalaje, ls_calidad,&
         ls_null, ls_cliente, ls_descri, ls_entrada
Long		ll_productor

SetNull(ls_null)


istr_info.titulo	= 'INFORME DETALLE RECEPCIONES'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_resumido_recepcion"
li_Agru	=	1

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
/*categoria*/
IF IsNull(uo_selcategoria.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Categoria Previamente",Exclamation!)
	uo_selcategoria.dw_Seleccion.SetFocus()
	RETURN
END IF

//IF cbx_categorias.Checked THEN
//	li_cate = -1
//ELSE
//	li_cate = dw_categorias.object.cate_codigo[1]
//END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[3])
ll_productor	=	Long(istr_mant.argumento[4])
ls_embalaje		=	istr_mant.argumento[6]
li_etiqueta		=	Integer(istr_mant.argumento[7])
ls_calidad		=	istr_mant.argumento[8]
ld_desde			=	Date(istr_mant.argumento[9])
ld_hasta			=	Date(istr_mant.argumento[10])
li_packing		=	Integer(istr_mant.argumento[23])
texto_desde		=  f_fecha_texto(String(ld_desde), 1)
texto_hasta		=	f_fecha_texto(String(ld_hasta), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

//Obtiene descripciones de cliente y especie
  Select clie_nombre  
    into :ls_cliente  
    from dba.clientesprod  
   where clie_codigo = :li_cliente ;

IF cbx_peso.Checked=False THEN
	ls_cajas = "Bultos"
	istr_mant.argumento[11]	=	"1"
ELSE
	istr_mant.argumento[11]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[11] 
END IF

li_consplanta		=	Integer(istr_mant.argumento[13])
li_consproductor	=	Integer(istr_mant.argumento[14])
li_consembalaje	=	Integer(istr_mant.argumento[16])
li_consetiqueta	=	Integer(istr_mant.argumento[17])
li_conscalidad		=	Integer(istr_mant.argumento[18])
li_conspacking		=	Integer(istr_mant.argumento[33])
li_zona  			=	Integer(istr_mant.argumento[40])

IF cbx_fecemb.checked THEN
	istr_mant.argumento[41] = "0"
ELSE
	istr_mant.argumento[41] = "1"
END IF

//Caracteristica de Recepción
IF cbx_consfru.Checked THEN
	li_fruta 	= -9
	ls_descri	= 'CONSOLIDADA'
ELSEIF cbx_todosfru.checked  THEN
	li_fruta  =   -1
	ls_descri	=	'TODAS'
ELSE
	li_fruta	=	dw_frurecep.Object.frre_codigo[1]
	IF IsNull(li_fruta) OR li_fruta = 0 THEN
	   MessageBox("Atención","Debe Seleccionar una Carácteristica Previamente",Exclamation!)
		dw_frurecep.setfocus()
		RETURN
	ELSE
		ls_descri	=	buscdescfruta(li_fruta)
	END IF 
END IF

vinf.dw_1.SetTransObject(sqlca)

//ll_norden

fila	=	vinf.dw_1.Retrieve(li_cliente, uo_selespecie.Codigo, li_planta, ll_productor,&
										 uo_selvariedad.Codigo, ls_embalaje, li_etiqueta, ls_calidad,&
										 ld_desde, ld_hasta, Dec(istr_mant.argumento[11]), li_consplanta, &
										 li_consproductor, li_consembalaje, li_consetiqueta, li_conscalidad,&
										 li_packing,li_conspacking, li_zona,Integer(istr_mant.argumento[41]),&
										 li_fruta,li_Agru, li_varirotula,ll_norden,ii_tipoi,uo_selcategoria.Codigo,il_guia)	


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
		vinf.dw_1.Modify("Cliente.text = '" + ls_cliente + "'")
		vinf.dw_1.Modify("Fruta.text = '" + ls_descri + "'")
		vinf.dw_1.Modify("Especie.text = '" + uo_selespecie.Nombre + "'")
		IF ii_tipoi = 1 THEN
			ls_entrada = '1 Ingreso desde Packing' 
		ELSEIF ii_tipoi = 2 THEN	
			ls_entrada = '2 InterPlanta Zonal' 
		ELSEIF	ii_tipoi = 3 THEN
			ls_entrada = '3 Devolución de Embarque'
		ELSEIF	ii_tipoi = 4 THEN
			ls_entrada = '4 Recepción Fruta Comercial'
		ELSEIF	ii_tipoi = 5 THEN
			ls_entrada = '5 Reembalajes'
		ELSEIF	ii_tipoi = 6 THEN
			ls_entrada = '6 Ingreso Interplanta'
		ELSEIF ii_tipoi = 7 THEN
			ls_entrada = '7 Sitio USDA'
		END IF	
		//ls_entrada = ddlb_tipoen.
		vinf.dw_1.Modify("entrada.text = '" + ls_entrada + "'")
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumido_recepciones
integer x = 3630
integer y = 1964
integer taborder = 170
end type

type st_4 from statictext within w_info_resumido_recepciones
integer x = 242
integer y = 604
integer width = 1545
integer height = 1168
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

type st_1 from statictext within w_info_resumido_recepciones
integer x = 302
integer y = 904
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
long backcolor = 33543637
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumido_recepciones
integer x = 306
integer y = 2004
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
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_resumido_recepciones
integer x = 686
integer y = 1984
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

type dw_cliente from datawindow within w_info_resumido_recepciones
integer x = 1467
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
	idwc_planta.Retrieve(1)
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(Integer(data))
	dw_productor.InsertRow(0)	
	idwc_etiqueta.Retrieve()
ELSE
	This.SetItem(1, "clie_codigo", Integer(ls_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_resumido_recepciones
integer x = 1125
integer y = 500
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_resumido_recepciones
integer x = 681
integer y = 888
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

type st_3 from statictext within w_info_resumido_recepciones
integer x = 302
integer y = 1500
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_resumido_recepciones
integer x = 306
integer y = 2116
integer width = 279
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_resumido_recepciones
integer x = 686
integer y = 2092
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

type st_8 from statictext within w_info_resumido_recepciones
integer x = 302
integer y = 1116
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
long backcolor = 33543637
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productor from datawindow within w_info_resumido_recepciones
integer x = 681
integer y = 1088
integer width = 974
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores_clientes"
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

type gb_3 from groupbox within w_info_resumido_recepciones
integer x = 270
integer y = 1944
integer width = 1262
integer height = 260
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_5 from statictext within w_info_resumido_recepciones
integer x = 1792
integer y = 604
integer width = 1714
integer height = 1168
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

type cbx_peso from checkbox within w_info_resumido_recepciones
integer x = 302
integer y = 1828
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

type tit_peso from statictext within w_info_resumido_recepciones
integer x = 951
integer y = 1836
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

type st_variedad from statictext within w_info_resumido_recepciones
integer x = 1870
integer y = 904
integer width = 302
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Variedad"
boolean focusrectangle = false
end type

type st_embalaje from statictext within w_info_resumido_recepciones
integer x = 1870
integer y = 1104
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
long backcolor = 33543637
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_resumido_recepciones
integer x = 2395
integer y = 1020
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
long backcolor = 33543637
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

type em_embalaje from editmask within w_info_resumido_recepciones
integer x = 2395
integer y = 1088
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
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_resumido_recepciones
integer x = 2706
integer y = 1092
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

type cbx_consembalaje from checkbox within w_info_resumido_recepciones
integer x = 2912
integer y = 1020
integer width = 539
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'1'
ELSE
	istr_mant.argumento[16]	=	'0'
END IF

end event

type st_11 from statictext within w_info_resumido_recepciones
integer x = 1870
integer y = 676
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
long backcolor = 33543637
boolean enabled = false
string text = "Etiqueta"
boolean focusrectangle = false
end type

type cbx_etiqueta from checkbox within w_info_resumido_recepciones
integer x = 2395
integer y = 620
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
long backcolor = 33543637
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

type cbx_consetiqueta from checkbox within w_info_resumido_recepciones
integer x = 2912
integer y = 620
integer width = 539
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[17]	=	'1'
ELSE
	istr_mant.argumento[17]	=	'0'
END IF

end event

type st_calidad from statictext within w_info_resumido_recepciones
integer x = 1870
integer y = 1304
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
long backcolor = 33543637
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type cbx_calidad from checkbox within w_info_resumido_recepciones
integer x = 2395
integer y = 1220
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
long backcolor = 33543637
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

type cbx_conscalidad from checkbox within w_info_resumido_recepciones
integer x = 2912
integer y = 1220
integer width = 539
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'1'
ELSE
	istr_mant.argumento[18]	=	'0'
END IF

end event

type gb_13 from groupbox within w_info_resumido_recepciones
integer x = 274
integer y = 1792
integer width = 1614
integer height = 132
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type cbx_productor from checkbox within w_info_resumido_recepciones
integer x = 686
integer y = 1020
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
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_productorcons.Enabled									=	True
	dw_productor.Enabled											=	False
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[4]									=	'0'
	istr_mant.argumento[14]									=	'0'
ELSE
	cbx_productorcons.Enabled									=	False
	cbx_productorcons.Checked									=	False
	dw_productor.Enabled											=	True
	dw_productor.Object.prod_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_productor.SetFocus()
	istr_mant.argumento[14]	=	'0'
END IF
end event

type cbx_productorcons from checkbox within w_info_resumido_recepciones
integer x = 1161
integer y = 1020
integer width = 562
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
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[14]	=	'1'
ELSE
	istr_mant.argumento[14]	=	'0'
END IF
	
end event

type cbx_planta from checkbox within w_info_resumido_recepciones
integer x = 686
integer y = 820
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
long backcolor = 33543637
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

type cbx_plantascons from checkbox within w_info_resumido_recepciones
integer x = 1161
integer y = 820
integer width = 562
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[13]	=	'1'
ELSE
	istr_mant.argumento[13]	=	'0'
END IF
	
end event

type st_18 from statictext within w_info_resumido_recepciones
integer x = 302
integer y = 1304
integer width = 247
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
boolean enabled = false
string text = "Packing"
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_resumido_recepciones
integer x = 686
integer y = 1224
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
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_packingcons.Enabled									=	True
	dw_packing.Enabled										=	False
	dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(192, 192, 192)
	istr_mant.argumento[23]									=	'0'
	istr_mant.argumento[33]									=	'0'
ELSE
	cbx_packingcons.Enabled									=	False
	cbx_packingcons.Checked									=	False
	dw_packing.Enabled										=	True
	dw_packing.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)

	dw_packing.SetFocus()
END IF
end event

type cbx_packingcons from checkbox within w_info_resumido_recepciones
integer x = 1166
integer y = 1220
integer width = 562
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
string text = "Consolidadas"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[33]	=	'1'
ELSE
	istr_mant.argumento[33]	=	'0'
END IF
	
end event

type dw_packing from datawindow within w_info_resumido_recepciones
integer x = 681
integer y = 1288
integer width = 969
integer height = 92
integer taborder = 50
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

IF ExistePacking(Integer(data)) THEN
	istr_mant.argumento[23]	=	data
ELSE
	This.SetItem(1, "plde_codigo", ls_null)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_9 from statictext within w_info_resumido_recepciones
integer x = 302
integer y = 688
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Zona"
boolean focusrectangle = false
end type

type cbx_zonas from checkbox within w_info_resumido_recepciones
integer x = 681
integer y = 624
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
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_zonascons.Enabled									=	True
	dw_zonas.Enabled											=	False
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(192, 192, 192)
	istr_mant.argumento[40]									=	'-1'
//	dw_productor.GetChild("prod_codigo", idwc_productor)
//	idwc_productor.SetTransObject(sqlca)
//	idwc_productor.Retrieve(Integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]),integer(istr_mant.argumento[3]))
//	dw_productor.InsertRow(0)
ELSE
	cbx_zonascons.Enabled									=	False
	cbx_zonascons.Checked									=	False
	dw_zonas.Enabled											=	True
	dw_zonas.Object.zona_codigo.BackGround.Color		=	RGB(255, 255, 255)
	dw_zonas.SetFocus()
END IF

end event

type dw_zonas from datawindow within w_info_resumido_recepciones
integer x = 681
integer y = 688
integer width = 873
integer height = 92
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[40]	=	data

//dw_productor.GetChild("prod_codigo", idwc_productor)
//idwc_productor.SetTransObject(sqlca)
//idwc_productor.Retrieve(Integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]),integer(istr_mant.argumento[3]))
//dw_productor.InsertRow(0)

end event

event itemerror;RETURN 1
end event

type cbx_zonascons from checkbox within w_info_resumido_recepciones
integer x = 1161
integer y = 624
integer width = 562
integer height = 68
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
	istr_mant.argumento[40]	=	'-9'
ELSE
	istr_mant.argumento[40]	=	'-1'
END IF
	
end event

type cbx_fecemb from checkbox within w_info_resumido_recepciones
integer x = 1079
integer y = 2052
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Consolidada"
boolean checked = true
end type

type gb_4 from groupbox within w_info_resumido_recepciones
integer x = 1915
integer y = 1796
integer width = 1563
integer height = 132
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_12 from statictext within w_info_resumido_recepciones
integer x = 247
integer y = 440
integer width = 3259
integer height = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_resumido_recepciones
integer x = 242
integer y = 1776
integer width = 3264
integer height = 160
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

type dw_pesoneto from datawindow within w_info_resumido_recepciones
integer x = 1115
integer y = 1820
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

type st_15 from statictext within w_info_resumido_recepciones
integer x = 1861
integer y = 1512
integer width = 526
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
boolean enabled = false
string text = "Caract.Recepción"
boolean focusrectangle = false
end type

type dw_etiqueta from datawindow within w_info_resumido_recepciones
integer x = 2395
integer y = 684
integer width = 891
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

type cbx_todosfru from checkbox within w_info_resumido_recepciones
integer x = 2395
integer y = 1424
integer width = 306
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
string text = "Todas"
end type

event clicked;Integer li_null
SetNull(li_null)

call super::clicked;IF This.Checked THEN
   cbx_consfru.Checked   =  false
	dw_frurecep.Enabled		=	False
	dw_frurecep.Object.frre_codigo[1] = li_null
ELSE
	dw_frurecep.Enabled		=	True
	dw_frurecep.SetFocus()
END IF
end event

type cbx_consfru from checkbox within w_info_resumido_recepciones
integer x = 2912
integer y = 1424
integer width = 539
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
string text = "Consolidadas"
boolean checked = true
end type

event clicked;Integer li_Null
SetNull(li_null)

IF This.Checked THEN
	cbx_todosfru.Checked		=	False       
	dw_frurecep.Enabled		=	False
	dw_frurecep.Object.frre_codigo[1] = li_null
ELSE
	cbx_todosfru.Checked		=	true      
	dw_frurecep.Enabled		=	false
END IF
end event

type dw_frurecep from datawindow within w_info_resumido_recepciones
integer x = 2395
integer y = 1488
integer width = 567
integer height = 100
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_frutarecep"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula

IF NOT iuo_frutarecepcion.existe(Integer(data),True,sqlca) THEN
	This.SetItem(1, "frre_codigo", li_nula)
	RETURN 1
ELSE
	istr_mant.argumento[9] = data
END IF
end event

type em_calidad from editmask within w_info_resumido_recepciones
integer x = 2395
integer y = 1284
integer width = 297
integer height = 96
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

type cbx_cliente from checkbox within w_info_resumido_recepciones
integer x = 2638
integer y = 496
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
long backcolor = 33543637
string text = "Todos"
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	dw_cliente.Enabled										=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(192, 192, 192)
   istr_mant.argumento[1]                          = '-1'
	dw_cliente.SetItem(1,"clie_codigo",li_null)
	
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(-1)
	dw_productor.InsertRow(0)	
	
ELSE
	dw_cliente.Enabled											=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_cliente.SetFocus()
END IF
end event

type uo_selespecie from uo_seleccion_especie within w_info_resumido_recepciones
event destroy ( )
integer x = 681
integer y = 1416
integer height = 164
integer taborder = 70
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

END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_resumido_recepciones
event destroy ( )
integer x = 2395
integer y = 820
integer height = 168
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_resumido_recepciones
integer x = 2491
integer y = 1824
integer width = 640
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

type cbx_1 from checkbox within w_info_resumido_recepciones
integer x = 1600
integer y = 1988
integer width = 288
integer height = 68
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
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -1
ELSE
	em_norden.Enabled = True
	ll_norden = Long(em_norden.Text)
END IF
end event

type em_norden from editmask within w_info_resumido_recepciones
integer x = 2030
integer y = 2080
integer width = 361
integer height = 84
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
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;ll_norden = Long(this.text)
end event

type st_21 from statictext within w_info_resumido_recepciones
integer x = 1591
integer y = 2092
integer width = 402
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Nro. Orden"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_info_resumido_recepciones
integer x = 1947
integer y = 1988
integer width = 443
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Consolidada"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -9
	cbx_1.Checked = True
	cbx_1.Enabled = False
ELSE
	em_norden.Enabled = False
	ll_norden = -1
	cbx_1.Checked = True
	cbx_1.Enabled = True
END IF
end event

type gb_5 from groupbox within w_info_resumido_recepciones
integer x = 1586
integer y = 1944
integer width = 827
integer height = 256
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_10 from statictext within w_info_resumido_recepciones
integer x = 242
integer y = 1936
integer width = 1326
integer height = 288
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

type ddlb_tipoen from dropdownlistbox within w_info_resumido_recepciones
integer x = 699
integer y = 1628
integer width = 1015
integer height = 664
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
string item[] = {"1 Ingreso desde Packing","2 InterPlanta Zonal","3 Devolución de Embarque","4 Recepción Fruta Comercial","5 Reembalajes","6 Ingreso Interplanta","7 Sitio USDA"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_tipoi = Integer(index)



end event

type st_16 from statictext within w_info_resumido_recepciones
integer x = 302
integer y = 1644
integer width = 384
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Tipo Entrada"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_17 from statictext within w_info_resumido_recepciones
integer x = 1861
integer y = 1656
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
long backcolor = 33543637
boolean enabled = false
string text = "Categoria"
boolean focusrectangle = false
end type

type dw_categorias from datawindow within w_info_resumido_recepciones
boolean visible = false
integer x = 2757
integer y = 2452
integer width = 896
integer height = 108
integer taborder = 150
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_categorias"
boolean border = false
boolean livescroll = true
end type

type cbx_categorias from checkbox within w_info_resumido_recepciones
integer x = 3607
integer y = 2468
integer width = 283
integer height = 80
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
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

event clicked;integer	li_null

SetNull(li_null)

IF This.Checked THEN
	dw_categorias.Enabled						=	False
	dw_categorias.Object.cate_codigo[1]		=	li_null
	
ELSE
	dw_categorias.Enabled						=	True
	
END IF
end event

type uo_selcategoria from uo_seleccion_categoria within w_info_resumido_recepciones
integer x = 2391
integer y = 1592
integer height = 160
integer taborder = 100
boolean bringtotop = true
end type

on uo_selcategoria.destroy
call uo_seleccion_categoria::destroy
end on

type gb_7 from groupbox within w_info_resumido_recepciones
integer x = 2423
integer y = 1944
integer width = 1051
integer height = 256
integer taborder = 110
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
end type

type st_13 from statictext within w_info_resumido_recepciones
integer x = 1568
integer y = 1936
integer width = 1938
integer height = 288
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

type st_19 from statictext within w_info_resumido_recepciones
integer x = 2455
integer y = 2092
integer width = 343
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Guia SII"
boolean focusrectangle = false
end type

type em_guia from editmask within w_info_resumido_recepciones
integer x = 2798
integer y = 2084
integer width = 361
integer height = 84
integer taborder = 140
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
string mask = "########"
end type

event modified;il_guia = Long(this.text)
end event

type cbx_4 from checkbox within w_info_resumido_recepciones
integer x = 2976
integer y = 1988
integer width = 443
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Consolidada"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_guia.Enabled = False
	em_guia.Text = ''
	il_guia = -9
	cbx_3.Checked = True
	cbx_3.Enabled = False
ELSE
	em_guia.Enabled = False
	il_guia = -1
	cbx_3.Checked = True
	cbx_3.Enabled = True
END IF
end event

type cbx_3 from checkbox within w_info_resumido_recepciones
integer x = 2587
integer y = 1988
integer width = 288
integer height = 68
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
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
	em_guia.Enabled = False
	em_guia.Text = ''
	il_guia = -1
ELSE
	em_guia.Enabled = True
	il_guia = Long(em_guia.Text)
END IF
end event

