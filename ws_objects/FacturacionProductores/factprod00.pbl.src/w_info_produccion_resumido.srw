$PBExportHeader$w_info_produccion_resumido.srw
forward
global type w_info_produccion_resumido from w_para_informes
end type
type st_4 from statictext within w_info_produccion_resumido
end type
type st_1 from statictext within w_info_produccion_resumido
end type
type st_2 from statictext within w_info_produccion_resumido
end type
type em_desde from editmask within w_info_produccion_resumido
end type
type dw_cliente from datawindow within w_info_produccion_resumido
end type
type st_6 from statictext within w_info_produccion_resumido
end type
type dw_planta from datawindow within w_info_produccion_resumido
end type
type st_3 from statictext within w_info_produccion_resumido
end type
type st_7 from statictext within w_info_produccion_resumido
end type
type em_hasta from editmask within w_info_produccion_resumido
end type
type st_8 from statictext within w_info_produccion_resumido
end type
type st_5 from statictext within w_info_produccion_resumido
end type
type cbx_peso from checkbox within w_info_produccion_resumido
end type
type tit_peso from statictext within w_info_produccion_resumido
end type
type st_variedad from statictext within w_info_produccion_resumido
end type
type st_embalaje from statictext within w_info_produccion_resumido
end type
type cbx_embalaje from checkbox within w_info_produccion_resumido
end type
type cb_buscaembalaje from commandbutton within w_info_produccion_resumido
end type
type cbx_consembalaje from checkbox within w_info_produccion_resumido
end type
type st_11 from statictext within w_info_produccion_resumido
end type
type cbx_etiqueta from checkbox within w_info_produccion_resumido
end type
type cbx_consetiqueta from checkbox within w_info_produccion_resumido
end type
type st_calidad from statictext within w_info_produccion_resumido
end type
type cbx_calidad from checkbox within w_info_produccion_resumido
end type
type cbx_conscalidad from checkbox within w_info_produccion_resumido
end type
type cbx_planta from checkbox within w_info_produccion_resumido
end type
type cbx_plantascons from checkbox within w_info_produccion_resumido
end type
type st_18 from statictext within w_info_produccion_resumido
end type
type cbx_packing from checkbox within w_info_produccion_resumido
end type
type cbx_packingcons from checkbox within w_info_produccion_resumido
end type
type dw_packing from datawindow within w_info_produccion_resumido
end type
type st_9 from statictext within w_info_produccion_resumido
end type
type cbx_zonas from checkbox within w_info_produccion_resumido
end type
type dw_zonas from datawindow within w_info_produccion_resumido
end type
type cbx_zonascons from checkbox within w_info_produccion_resumido
end type
type cbx_fecemb from checkbox within w_info_produccion_resumido
end type
type st_12 from statictext within w_info_produccion_resumido
end type
type cbx_infopacking from checkbox within w_info_produccion_resumido
end type
type st_14 from statictext within w_info_produccion_resumido
end type
type dw_pesoneto from datawindow within w_info_produccion_resumido
end type
type st_15 from statictext within w_info_produccion_resumido
end type
type dw_etiqueta from datawindow within w_info_produccion_resumido
end type
type cbx_todosfru from checkbox within w_info_produccion_resumido
end type
type cbx_consfru from checkbox within w_info_produccion_resumido
end type
type dw_frurecep from datawindow within w_info_produccion_resumido
end type
type em_calidad from editmask within w_info_produccion_resumido
end type
type cbx_cliente from checkbox within w_info_produccion_resumido
end type
type cbx_predio from checkbox within w_info_produccion_resumido
end type
type uo_selespecie from uo_seleccion_especie within w_info_produccion_resumido
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_resumido
end type
type cbx_varirotula from checkbox within w_info_produccion_resumido
end type
type cbx_1 from checkbox within w_info_produccion_resumido
end type
type em_norden from editmask within w_info_produccion_resumido
end type
type st_21 from statictext within w_info_produccion_resumido
end type
type cbx_2 from checkbox within w_info_produccion_resumido
end type
type st_10 from statictext within w_info_produccion_resumido
end type
type st_13 from statictext within w_info_produccion_resumido
end type
type em_guia from editmask within w_info_produccion_resumido
end type
type st_16 from statictext within w_info_produccion_resumido
end type
type cbx_3 from checkbox within w_info_produccion_resumido
end type
type cbx_4 from checkbox within w_info_produccion_resumido
end type
type st_17 from statictext within w_info_produccion_resumido
end type
type uo_seltipoproductor from uo_seleccion_tipoproductor within w_info_produccion_resumido
end type
type em_embalaje from singlelineedit within w_info_produccion_resumido
end type
type st_19 from statictext within w_info_produccion_resumido
end type
type uo_selcate from uo_seleccion_categoria within w_info_produccion_resumido
end type
type st_22 from statictext within w_info_produccion_resumido
end type
type st_29 from statictext within w_info_produccion_resumido
end type
type ddlb_calificacion from dropdownlistbox within w_info_produccion_resumido
end type
type cbx_todcalifi from checkbox within w_info_produccion_resumido
end type
type cbx_consolcalifi from checkbox within w_info_produccion_resumido
end type
type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_produccion_resumido
end type
type uo_selplanta from uo_seleccion_plantas within w_info_produccion_resumido
end type
type st_23 from statictext within w_info_produccion_resumido
end type
type cbx_produrotula from checkbox within w_info_produccion_resumido
end type
type st_20 from statictext within w_info_produccion_resumido
end type
end forward

global type w_info_produccion_resumido from w_para_informes
integer x = 14
integer y = 32
integer width = 3918
integer height = 2616
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
st_7 st_7
em_hasta em_hasta
st_8 st_8
st_5 st_5
cbx_peso cbx_peso
tit_peso tit_peso
st_variedad st_variedad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_consembalaje cbx_consembalaje
st_11 st_11
cbx_etiqueta cbx_etiqueta
cbx_consetiqueta cbx_consetiqueta
st_calidad st_calidad
cbx_calidad cbx_calidad
cbx_conscalidad cbx_conscalidad
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
st_12 st_12
cbx_infopacking cbx_infopacking
st_14 st_14
dw_pesoneto dw_pesoneto
st_15 st_15
dw_etiqueta dw_etiqueta
cbx_todosfru cbx_todosfru
cbx_consfru cbx_consfru
dw_frurecep dw_frurecep
em_calidad em_calidad
cbx_cliente cbx_cliente
cbx_predio cbx_predio
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
cbx_1 cbx_1
em_norden em_norden
st_21 st_21
cbx_2 cbx_2
st_10 st_10
st_13 st_13
em_guia em_guia
st_16 st_16
cbx_3 cbx_3
cbx_4 cbx_4
st_17 st_17
uo_seltipoproductor uo_seltipoproductor
em_embalaje em_embalaje
st_19 st_19
uo_selcate uo_selcate
st_22 st_22
st_29 st_29
ddlb_calificacion ddlb_calificacion
cbx_todcalifi cbx_todcalifi
cbx_consolcalifi cbx_consolcalifi
uo_selproductor uo_selproductor
uo_selplanta uo_selplanta
st_23 st_23
cbx_produrotula cbx_produrotula
st_20 st_20
end type
global w_info_produccion_resumido w_info_produccion_resumido

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_productor, idwc_pesoneto, &
						idwc_etiqueta, idwc_packing, idwc_zonas, idwc_fruta

String 	is_NomPlanta
Long		ll_norden, il_guia
Integer	ii_calificacion
Date		id_fechaini, id_fechafin

uo_frutarecepcion						iuo_frutarecepcion
uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor
//uo_seleccion_especie					iuo_selplanta
uo_calibre								iuo_calibre

end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean existeproductor (long ll_productor)
public function boolean existepacking (integer li_planta)
public function boolean noexisteetiqueta (integer li_etiqueta)
public function string buscdescfruta (integer codigo)
public function boolean rangotemporada ()
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

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

public function boolean existeproductor (long ll_productor);String	ls_Nombre

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

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dbo.plantadesp
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

public function string buscdescfruta (integer codigo);String	ls_descri


    SELECT frre_descri  
    INTO :ls_descri  
    FROM dbo.recfruprocee as re, dbo.frutarecibida as fr 
   WHERE re.frre_codigo = fr.frre_codigo and  
         re.frre_codigo = :codigo ;

		
RETURN ls_descri

end function

public function boolean rangotemporada ();
SELECT	pate_inicio,pate_termin
INTO		:id_fechaini, :id_fechafin
FROM		dbo.paramtemporada
WHERE		pate_vigent = 1;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla paramtemporada")
	RETURN TRUE
ELSE	
	RETURN FALSE	
END IF
end function

on w_info_produccion_resumido.create
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
this.st_5=create st_5
this.cbx_peso=create cbx_peso
this.tit_peso=create tit_peso
this.st_variedad=create st_variedad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_consembalaje=create cbx_consembalaje
this.st_11=create st_11
this.cbx_etiqueta=create cbx_etiqueta
this.cbx_consetiqueta=create cbx_consetiqueta
this.st_calidad=create st_calidad
this.cbx_calidad=create cbx_calidad
this.cbx_conscalidad=create cbx_conscalidad
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
this.st_12=create st_12
this.cbx_infopacking=create cbx_infopacking
this.st_14=create st_14
this.dw_pesoneto=create dw_pesoneto
this.st_15=create st_15
this.dw_etiqueta=create dw_etiqueta
this.cbx_todosfru=create cbx_todosfru
this.cbx_consfru=create cbx_consfru
this.dw_frurecep=create dw_frurecep
this.em_calidad=create em_calidad
this.cbx_cliente=create cbx_cliente
this.cbx_predio=create cbx_predio
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.cbx_1=create cbx_1
this.em_norden=create em_norden
this.st_21=create st_21
this.cbx_2=create cbx_2
this.st_10=create st_10
this.st_13=create st_13
this.em_guia=create em_guia
this.st_16=create st_16
this.cbx_3=create cbx_3
this.cbx_4=create cbx_4
this.st_17=create st_17
this.uo_seltipoproductor=create uo_seltipoproductor
this.em_embalaje=create em_embalaje
this.st_19=create st_19
this.uo_selcate=create uo_selcate
this.st_22=create st_22
this.st_29=create st_29
this.ddlb_calificacion=create ddlb_calificacion
this.cbx_todcalifi=create cbx_todcalifi
this.cbx_consolcalifi=create cbx_consolcalifi
this.uo_selproductor=create uo_selproductor
this.uo_selplanta=create uo_selplanta
this.st_23=create st_23
this.cbx_produrotula=create cbx_produrotula
this.st_20=create st_20
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
this.Control[iCurrent+12]=this.st_5
this.Control[iCurrent+13]=this.cbx_peso
this.Control[iCurrent+14]=this.tit_peso
this.Control[iCurrent+15]=this.st_variedad
this.Control[iCurrent+16]=this.st_embalaje
this.Control[iCurrent+17]=this.cbx_embalaje
this.Control[iCurrent+18]=this.cb_buscaembalaje
this.Control[iCurrent+19]=this.cbx_consembalaje
this.Control[iCurrent+20]=this.st_11
this.Control[iCurrent+21]=this.cbx_etiqueta
this.Control[iCurrent+22]=this.cbx_consetiqueta
this.Control[iCurrent+23]=this.st_calidad
this.Control[iCurrent+24]=this.cbx_calidad
this.Control[iCurrent+25]=this.cbx_conscalidad
this.Control[iCurrent+26]=this.cbx_planta
this.Control[iCurrent+27]=this.cbx_plantascons
this.Control[iCurrent+28]=this.st_18
this.Control[iCurrent+29]=this.cbx_packing
this.Control[iCurrent+30]=this.cbx_packingcons
this.Control[iCurrent+31]=this.dw_packing
this.Control[iCurrent+32]=this.st_9
this.Control[iCurrent+33]=this.cbx_zonas
this.Control[iCurrent+34]=this.dw_zonas
this.Control[iCurrent+35]=this.cbx_zonascons
this.Control[iCurrent+36]=this.cbx_fecemb
this.Control[iCurrent+37]=this.st_12
this.Control[iCurrent+38]=this.cbx_infopacking
this.Control[iCurrent+39]=this.st_14
this.Control[iCurrent+40]=this.dw_pesoneto
this.Control[iCurrent+41]=this.st_15
this.Control[iCurrent+42]=this.dw_etiqueta
this.Control[iCurrent+43]=this.cbx_todosfru
this.Control[iCurrent+44]=this.cbx_consfru
this.Control[iCurrent+45]=this.dw_frurecep
this.Control[iCurrent+46]=this.em_calidad
this.Control[iCurrent+47]=this.cbx_cliente
this.Control[iCurrent+48]=this.cbx_predio
this.Control[iCurrent+49]=this.uo_selespecie
this.Control[iCurrent+50]=this.uo_selvariedad
this.Control[iCurrent+51]=this.cbx_varirotula
this.Control[iCurrent+52]=this.cbx_1
this.Control[iCurrent+53]=this.em_norden
this.Control[iCurrent+54]=this.st_21
this.Control[iCurrent+55]=this.cbx_2
this.Control[iCurrent+56]=this.st_10
this.Control[iCurrent+57]=this.st_13
this.Control[iCurrent+58]=this.em_guia
this.Control[iCurrent+59]=this.st_16
this.Control[iCurrent+60]=this.cbx_3
this.Control[iCurrent+61]=this.cbx_4
this.Control[iCurrent+62]=this.st_17
this.Control[iCurrent+63]=this.uo_seltipoproductor
this.Control[iCurrent+64]=this.em_embalaje
this.Control[iCurrent+65]=this.st_19
this.Control[iCurrent+66]=this.uo_selcate
this.Control[iCurrent+67]=this.st_22
this.Control[iCurrent+68]=this.st_29
this.Control[iCurrent+69]=this.ddlb_calificacion
this.Control[iCurrent+70]=this.cbx_todcalifi
this.Control[iCurrent+71]=this.cbx_consolcalifi
this.Control[iCurrent+72]=this.uo_selproductor
this.Control[iCurrent+73]=this.uo_selplanta
this.Control[iCurrent+74]=this.st_23
this.Control[iCurrent+75]=this.cbx_produrotula
this.Control[iCurrent+76]=this.st_20
end on

on w_info_produccion_resumido.destroy
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
destroy(this.st_5)
destroy(this.cbx_peso)
destroy(this.tit_peso)
destroy(this.st_variedad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_consembalaje)
destroy(this.st_11)
destroy(this.cbx_etiqueta)
destroy(this.cbx_consetiqueta)
destroy(this.st_calidad)
destroy(this.cbx_calidad)
destroy(this.cbx_conscalidad)
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
destroy(this.st_12)
destroy(this.cbx_infopacking)
destroy(this.st_14)
destroy(this.dw_pesoneto)
destroy(this.st_15)
destroy(this.dw_etiqueta)
destroy(this.cbx_todosfru)
destroy(this.cbx_consfru)
destroy(this.dw_frurecep)
destroy(this.em_calidad)
destroy(this.cbx_cliente)
destroy(this.cbx_predio)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.cbx_1)
destroy(this.em_norden)
destroy(this.st_21)
destroy(this.cbx_2)
destroy(this.st_10)
destroy(this.st_13)
destroy(this.em_guia)
destroy(this.st_16)
destroy(this.cbx_3)
destroy(this.cbx_4)
destroy(this.st_17)
destroy(this.uo_seltipoproductor)
destroy(this.em_embalaje)
destroy(this.st_19)
destroy(this.uo_selcate)
destroy(this.st_22)
destroy(this.st_29)
destroy(this.ddlb_calificacion)
destroy(this.cbx_todcalifi)
destroy(this.cbx_consolcalifi)
destroy(this.uo_selproductor)
destroy(this.uo_selplanta)
destroy(this.st_23)
destroy(this.cbx_produrotula)
destroy(this.st_20)
end on

event open;Boolean lb_Cerrar

x	=	0
y	=	0

iuo_frutarecepcion		=	CREATE	uo_frutarecepcion			

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

iuo_calibre   						=	Create uo_calibre

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
IF IsNull(uo_SelCate.Codigo) THEN lb_Cerrar	=	True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCate.Seleccion(True, True)
END IF

// uo_seleccion_productor
IF IsNull(uo_selproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	//Close(This)
	lb_Cerrar = False
ELSE
	uo_selproductor.Seleccion(True,True)
END IF

// uo_seleccion_tipo productor
IF IsNull(uo_seltipoproductor.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_seltipoproductor.Seleccion(True,True)
END IF

// uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
END IF

// uo_seleccion_planta
IF IsNull(uo_selplanta.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selplanta.Seleccion(True,True)
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
ii_calificacion 			=  -9
rangotemporada()
em_desde.Text				=	String(id_fechaini)
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
il_guia						=	-9
end event

type pb_excel from w_para_informes`pb_excel within w_info_produccion_resumido
integer x = 3561
integer y = 1436
end type

type st_computador from w_para_informes`st_computador within w_info_produccion_resumido
end type

type st_usuario from w_para_informes`st_usuario within w_info_produccion_resumido
end type

type st_temporada from w_para_informes`st_temporada within w_info_produccion_resumido
end type

type p_logo from w_para_informes`p_logo within w_info_produccion_resumido
end type

type st_titulo from w_para_informes`st_titulo within w_info_produccion_resumido
integer y = 280
integer width = 3259
string text = "Informe de Producción Resumido"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_produccion_resumido
integer x = 3566
integer y = 1768
integer taborder = 160
string powertiptext = "Imprime Informe"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_cliente, li_planta,  li_etiqueta, li_consplanta,&
			li_consproductor, li_consembalaje, li_consetiqueta,li_conscalidad,&
			li_conspacking, li_packing, li_zona, li_fruta, li_Agru, li_varirotula, li_emba, li_emba2, li_prodrotula
Date		ld_desde, ld_hasta
String	texto_desde, texto_hasta, texto_fecha, ls_cajas, ls_embalaje, ls_calidad,&
         ls_null, ls_cliente, ls_descri, ls_lista, ls_construyelike1, ls_construye, ls_string, ls_construyelike
Long		ll_productor

SetNull(ls_null)

ls_embalaje	=	istr_mant.argumento[6]+','

IF cbx_infopacking.Checked = True THEN
	istr_info.titulo	= 'INFORME DE PRODUCCION RESUMIDO POR PACKING'

	OpenWithParm(vinf, istr_info)
	vinf.dw_1.DataObject = "dw_info_produccion_packing_enc"
ELSEIF cbx_predio.Checked = True THEN
	istr_info.titulo	= 'INFORME DE PRODUCCION RESUMIDO POR PREDIO'
	
	OpenWithParm(vinf, istr_info)
	vinf.dw_1.DataObject = "dw_info_produccion_resumido_precua"
	li_Agru	=	0
ELSE	
	istr_info.titulo	= 'INFORME DE PRODUCCION RESUMIDO'
	
	OpenWithParm(vinf, istr_info)
	vinf.dw_1.DataObject = "dw_info_produccion_resumido"
   li_Agru	=	1
END IF

li_emba = len(ls_embalaje)

FOR li_emba2 = 1 TO li_emba
	ls_string = mid(ls_embalaje,li_emba2,1)
	
	IF ls_string <> ',' THEN
		ls_construye = ls_construye+ls_string
	ELSE
		IF ls_construyelike1 = '' THEN
			ls_construyelike1 = ' emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			ls_construye = ''
		ELSE	
			IF ls_construyelike = '' THEN
				ls_construyelike = ls_construyelike1 +' or emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			ELSE
				ls_construyelike = ls_construyelike +' or emba_codigo like '+''+"'%"+ls_construye+"%'"+''
			END IF
			ls_construye = ''
		END IF	
	END IF	
NEXT	

IF ls_construyelike = '' THEN
	ls_construyelike = ls_construyelike1
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
Tipo Productor
*/
IF IsNull(uo_seltipoproductor.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_seltipoproductor.dw_Seleccion.SetFocus()
	RETURN
END IF

ls_lista = uo_selproductor.Lista

/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
planta
*/
IF IsNull(uo_selplanta.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Planta Previamente",Exclamation!)
	uo_selplanta.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
categoria
*/
IF IsNull(uo_selcate.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Categoria Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

IF cbx_produrotula.Checked THEN
	li_prodrotula = 1
ELSE
	li_prodrotula = 0
END IF

li_cliente 		=	Integer(istr_mant.argumento[1])
//li_planta		=	Integer(istr_mant.argumento[3])
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
    from dbo.clientesprod  
   where clie_codigo = :li_cliente ;

IF cbx_peso.Checked=False THEN
	ls_cajas = "Bultos"
	istr_mant.argumento[11]	=	"1"
ELSE
	istr_mant.argumento[11]	=	String(dw_pesoneto.Object.enva_pesone[1])
	ls_cajas						=	"Base " + istr_mant.argumento[11] 
END IF

IF uo_selplanta.Codigo = -9 THEN
	li_consplanta		=	1
ELSE	
	li_consplanta		=	0
END IF	

IF uo_selplanta.Codigo = -1 OR uo_selplanta.Codigo = -9 THEN 
	uo_selplanta.Codigo = 0
END IF	

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

IF cbx_embalaje.Checked OR cbx_consembalaje.Checked THEN
	ls_construyelike = "'Z' = 'Z'"
END IF

vinf.dw_1.SetTransObject(sqlca)

//ll_norden

IF cbx_infopacking.Checked = True THEN
	fila	=	vinf.dw_1.Retrieve(li_cliente, uo_selespecie.Codigo, uo_selplanta.Codigo, &
							uo_selvariedad.Codigo, ls_embalaje, li_etiqueta, ls_calidad,&
							ld_desde, ld_hasta, Dec(istr_mant.argumento[11]), li_consplanta,&
							li_consembalaje, li_consetiqueta, li_conscalidad,&
							li_packing, li_conspacking, li_zona,Integer(istr_mant.argumento[41]),&
							li_fruta, li_varirotula,ls_lista,ls_construyelike,uo_SelCate.Codigo)
ELSE
	fila	=	vinf.dw_1.Retrieve(li_cliente, uo_selespecie.Codigo, uo_selplanta.Codigo, &
							 uo_selvariedad.Codigo, ls_embalaje, li_etiqueta, ls_calidad,&
							 ld_desde, ld_hasta, Dec(istr_mant.argumento[11]), li_consplanta, &
							 li_consembalaje, li_consetiqueta, li_conscalidad,&
							 li_packing,li_conspacking, li_zona,Integer(istr_mant.argumento[41]),&
							 li_fruta,li_Agru, li_varirotula,ll_norden,il_guia,uo_seltipoproductor.Codigo,ls_lista,&
							 ls_construyelike,uo_SelCate.Codigo,ii_calificacion,li_prodrotula)	
END IF

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
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_produccion_resumido
integer x = 3566
integer y = 2040
integer taborder = 170
string powertiptext = "Salir de la Ventana"
end type

type st_4 from statictext within w_info_produccion_resumido
integer x = 238
integer y = 564
integer width = 1618
integer height = 1144
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

type st_1 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 800
integer width = 238
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

type st_2 from statictext within w_info_produccion_resumido
integer x = 302
integer y = 2000
integer width = 425
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_produccion_resumido
integer x = 681
integer y = 1980
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

event modified;Date	ld_Fecter, ld_FecIni

IF This.Text <> '' THEN
	IF	date(This.Text) < id_fechaini THEN
		MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
		This.Text = string(id_fechaini)
		RETURN
	END IF	
	
	IF	date(This.Text) > id_fechafin THEN
		MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término Temporada",Information!, Ok!)
		This.Text = string(id_fechaini)
		RETURN
	END IF			
			
	ld_Fecter	=	date(em_hasta.Text)
	ld_FecIni	=	date(This.Text)
	
	IF Not IsNull(ld_Fecter) THEN
		IF ld_FecIni > ld_Fecter THEN
			MessageBox("Error","Fecha NO puede ser Mayor a Fecha Término",Information!, Ok!)
			This.Text = string(id_fechaini)
			RETURN
		END IF
	END IF
END IF	

istr_mant.argumento[9]	=	This.Text
end event

type dw_cliente from datawindow within w_info_produccion_resumido
integer x = 1271
integer y = 460
integer width = 1143
integer height = 92
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
	uo_selproductor.Filtra(-1,-1,Integer(data))
	
ELSE
	This.SetItem(1, "clie_codigo", Integer(ls_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_produccion_resumido
integer x = 1038
integer y = 476
integer width = 233
integer height = 52
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_produccion_resumido
boolean visible = false
integer x = 3831
integer y = 772
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

type st_3 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 1608
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type st_7 from statictext within w_info_produccion_resumido
integer x = 302
integer y = 2112
integer width = 279
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_produccion_resumido
integer x = 681
integer y = 2088
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

event modified;Date	ld_Fecter, ld_FecIni

IF This.Text <> '' THEN
	IF	date(This.Text) > id_fechafin THEN
		MessageBox("Error","Fecha NO puede ser Mayor a Fecha Fin Temporada",Information!, Ok!)
		This.Text = String(id_fechafin)
		RETURN
	END IF
	
	IF	date(This.Text) < id_fechaini THEN
		MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio Temporada",Information!, Ok!)
		This.Text = String(id_fechafin)
		RETURN
	END IF
	
	ld_FecIni	=	Date(em_desde.Text)
	ld_FecTer	=	date(This.Text)
	
	IF Not IsNull(ld_FecIni) THEN
		IF ld_FecIni > ld_Fecter THEN
			MessageBox("Error","Fecha NO puede ser Menor a Fecha Inicio.",Information!, Ok!)
			This.Text = String(id_fechafin)
			RETURN
		END IF
	END IF			
END IF	
istr_mant.argumento[10]	=	This.Text


end event

type st_8 from statictext within w_info_produccion_resumido
integer x = 293
integer y = 1048
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_produccion_resumido
integer x = 1861
integer y = 568
integer width = 1641
integer height = 1060
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

type cbx_peso from checkbox within w_info_produccion_resumido
integer x = 297
integer y = 1776
integer width = 631
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
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

type tit_peso from statictext within w_info_produccion_resumido
integer x = 946
integer y = 1784
integer width = 155
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_produccion_resumido
integer x = 1893
integer y = 820
integer width = 315
integer height = 60
boolean bringtotop = true
integer textsize = -8
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

type st_embalaje from statictext within w_info_produccion_resumido
integer x = 1893
integer y = 988
integer width = 302
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

type cbx_embalaje from checkbox within w_info_produccion_resumido
integer x = 2391
integer y = 900
integer width = 416
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
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

type cb_buscaembalaje from commandbutton within w_info_produccion_resumido
integer x = 2702
integer y = 976
integer width = 110
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

type cbx_consembalaje from checkbox within w_info_produccion_resumido
integer x = 2907
integer y = 904
integer width = 535
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'1'
ELSE
	istr_mant.argumento[16]	=	'0'
END IF

end event

type st_11 from statictext within w_info_produccion_resumido
integer x = 1893
integer y = 628
integer width = 311
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

type cbx_etiqueta from checkbox within w_info_produccion_resumido
integer x = 2391
integer y = 572
integer width = 306
integer height = 72
boolean bringtotop = true
integer textsize = -8
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

type cbx_consetiqueta from checkbox within w_info_produccion_resumido
integer x = 2907
integer y = 572
integer width = 485
integer height = 72
boolean bringtotop = true
integer textsize = -8
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
	istr_mant.argumento[17]	=	'1'
ELSE
	istr_mant.argumento[17]	=	'0'
END IF

end event

type st_calidad from statictext within w_info_produccion_resumido
integer x = 1893
integer y = 1160
integer width = 270
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

type cbx_calidad from checkbox within w_info_produccion_resumido
integer x = 2391
integer y = 1076
integer width = 311
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
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

type cbx_conscalidad from checkbox within w_info_produccion_resumido
integer x = 2907
integer y = 1076
integer width = 485
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Consolidadas"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'1'
ELSE
	istr_mant.argumento[18]	=	'0'
END IF
 
end event 

type cbx_planta from checkbox within w_info_produccion_resumido
boolean visible = false
integer x = 3840
integer y = 704
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

type cbx_plantascons from checkbox within w_info_produccion_resumido
boolean visible = false
integer x = 4361
integer y = 776
integer width = 489
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

type st_18 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 1424
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Packing"
boolean focusrectangle = false
end type

type cbx_packing from checkbox within w_info_produccion_resumido
integer x = 777
integer y = 1352
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -8
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

type cbx_packingcons from checkbox within w_info_produccion_resumido
integer x = 1257
integer y = 1348
integer width = 471
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidadas"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[33]	=	'1'
ELSE
	istr_mant.argumento[33]	=	'0'
END IF
	
end event

type dw_packing from datawindow within w_info_produccion_resumido
integer x = 773
integer y = 1416
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

type st_9 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 636
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Zona"
boolean focusrectangle = false
end type

type cbx_zonas from checkbox within w_info_produccion_resumido
integer x = 773
integer y = 568
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -8
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

type dw_zonas from datawindow within w_info_produccion_resumido
integer x = 773
integer y = 628
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

type cbx_zonascons from checkbox within w_info_produccion_resumido
integer x = 1253
integer y = 568
integer width = 471
integer height = 68
boolean bringtotop = true
integer textsize = -8
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
	istr_mant.argumento[40]	=	'-9'
ELSE
	istr_mant.argumento[40]	=	'-1'
END IF
	
end event

type cbx_fecemb from checkbox within w_info_produccion_resumido
integer x = 1074
integer y = 2048
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidada"
boolean checked = true
end type

type st_12 from statictext within w_info_produccion_resumido
integer x = 242
integer y = 436
integer width = 3259
integer height = 124
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

type cbx_infopacking from checkbox within w_info_produccion_resumido
integer x = 2441
integer y = 1676
integer width = 1010
integer height = 80
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe por Packing (Crosstab)"
end type

event clicked;IF This.Checked THEN
	cbx_predio.Checked = False
	em_guia.Enabled = False
	em_guia.Text = ''
	il_guia = -9
	cbx_3.Checked = True
	cbx_3.Enabled = False
	cbx_4.Checked = True
	cbx_4.Enabled = False
	ddlb_calificacion.Enabled = False
	cbx_todcalifi.Enabled = False
	cbx_consolcalifi.Enabled = False
	ddlb_calificacion.SelectItem(0)
	cbx_todcalifi.Checked = True
	ddlb_calificacion.Enabled = False
	ii_calificacion = -9
	cbx_produrotula.Enabled = False
	cbx_produrotula.Checked = False
	cbx_varirotula.Enabled = False
	cbx_varirotula.Checked = False	
ELSE
	cbx_4.Enabled = True
	ddlb_calificacion.Enabled = True
	cbx_consolcalifi.Enabled = True	
	cbx_produrotula.Enabled = True
	cbx_varirotula.Enabled = True
END IF	
end event

type st_14 from statictext within w_info_produccion_resumido
integer x = 238
integer y = 1708
integer width = 1618
integer height = 224
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

type dw_pesoneto from datawindow within w_info_produccion_resumido
integer x = 1093
integer y = 1768
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

type st_15 from statictext within w_info_produccion_resumido
integer x = 1893
integer y = 1344
integer width = 539
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Caract.Recep."
boolean focusrectangle = false
end type

type dw_etiqueta from datawindow within w_info_produccion_resumido
integer x = 2391
integer y = 636
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

type cbx_todosfru from checkbox within w_info_produccion_resumido
integer x = 2391
integer y = 1256
integer width = 320
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
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

type cbx_consfru from checkbox within w_info_produccion_resumido
integer x = 2907
integer y = 1256
integer width = 485
integer height = 72
boolean bringtotop = true
integer textsize = -8
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

type dw_frurecep from datawindow within w_info_produccion_resumido
integer x = 2391
integer y = 1320
integer width = 581
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

type em_calidad from editmask within w_info_produccion_resumido
integer x = 2391
integer y = 1140
integer width = 311
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

type cbx_cliente from checkbox within w_info_produccion_resumido
integer x = 2441
integer y = 472
integer width = 293
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	dw_cliente.Enabled										=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(192, 192, 192)
   istr_mant.argumento[1]                          = '-1'
	dw_cliente.SetItem(1,"clie_codigo",li_null)
	
	uo_selproductor.Filtra(-1,-1,-1)
	
ELSE
	dw_cliente.Enabled											=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_cliente.SetFocus()
END IF
end event

type cbx_predio from checkbox within w_info_produccion_resumido
integer x = 1966
integer y = 1744
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Por Predio"
end type

event clicked;IF This.Checked THEN
	cbx_infopacking.Checked = False
END IF	
end event

type uo_selespecie from uo_seleccion_especie within w_info_produccion_resumido
event destroy ( )
integer x = 773
integer y = 1512
integer height = 180
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

type uo_selvariedad from uo_seleccion_variedad within w_info_produccion_resumido
event destroy ( )
integer x = 2391
integer y = 724
integer width = 910
integer taborder = 80
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_produccion_resumido
integer x = 2441
integer y = 1828
integer width = 677
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rotulada"
end type

type cbx_1 from checkbox within w_info_produccion_resumido
integer x = 1591
integer y = 1984
integer width = 288
integer height = 68
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
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

type em_norden from editmask within w_info_produccion_resumido
integer x = 1952
integer y = 2076
integer width = 416
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
string mask = "########"
end type

event modified;ll_norden = Long(this.text)
end event

type st_21 from statictext within w_info_produccion_resumido
integer x = 1586
integer y = 2088
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Nro. Orden"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_info_produccion_resumido
integer x = 1929
integer y = 1984
integer width = 443
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
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

type st_10 from statictext within w_info_produccion_resumido
integer x = 238
integer y = 1932
integer width = 1321
integer height = 288
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

type st_13 from statictext within w_info_produccion_resumido
integer x = 1559
integer y = 1932
integer width = 1943
integer height = 288
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

type em_guia from editmask within w_info_produccion_resumido
integer x = 2802
integer y = 2076
integer width = 430
integer height = 84
integer taborder = 130
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

type st_16 from statictext within w_info_produccion_resumido
integer x = 2505
integer y = 2088
integer width = 279
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Guía SII"
boolean focusrectangle = false
end type

type cbx_3 from checkbox within w_info_produccion_resumido
integer x = 2597
integer y = 1984
integer width = 288
integer height = 68
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
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

type cbx_4 from checkbox within w_info_produccion_resumido
integer x = 2981
integer y = 1984
integer width = 443
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
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

type st_17 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 1260
integer width = 475
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Productor"
boolean focusrectangle = false
end type

type uo_seltipoproductor from uo_seleccion_tipoproductor within w_info_produccion_resumido
integer x = 773
integer y = 1172
integer height = 184
integer taborder = 280
boolean bringtotop = true
end type

on uo_seltipoproductor.destroy
call uo_seleccion_tipoproductor::destroy
end on

type em_embalaje from singlelineedit within w_info_produccion_resumido
integer x = 2391
integer y = 980
integer width = 311
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
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
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
	
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
//	This.SetFocus()
//ELSEIF sqlca.SQLCode = 100 THEN
//	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
//		"Ingrese o seleccione otro Código.")
//	This.SetFocus()
//ELSE
	istr_mant.argumento[6]	=	ls_embalaje
	istr_mant.argumento[16]	=	'0'
//END IF
end event

type st_19 from statictext within w_info_produccion_resumido
integer x = 1893
integer y = 1508
integer width = 320
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Categoria"
boolean focusrectangle = false
end type

type uo_selcate from uo_seleccion_categoria within w_info_produccion_resumido
integer x = 2395
integer y = 1424
integer width = 910
integer taborder = 340
boolean bringtotop = true
end type

on uo_selcate.destroy
call uo_seleccion_categoria::destroy
end on

type st_22 from statictext within w_info_produccion_resumido
integer x = 238
integer y = 2224
integer width = 1627
integer height = 140
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

type st_29 from statictext within w_info_produccion_resumido
integer x = 297
integer y = 2260
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calificación"
boolean focusrectangle = false
end type

type ddlb_calificacion from dropdownlistbox within w_info_produccion_resumido
integer x = 672
integer y = 2244
integer width = 389
integer height = 400
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string item[] = {"1","2","3","4","5"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_calificacion	=	(index)
end event

type cbx_todcalifi from checkbox within w_info_produccion_resumido
integer x = 1102
integer y = 2256
integer width = 288
integer height = 76
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN	
	
	ii_calificacion = -1
	ddlb_calificacion.SelectItem(0)
	ddlb_calificacion.Enabled = False
ELSE
	ddlb_calificacion.Enabled = True
	ii_calificacion = 0
	
END IF
end event

type cbx_consolcalifi from checkbox within w_info_produccion_resumido
integer x = 1390
integer y = 2256
integer width = 411
integer height = 76
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked THEN	
	
	cbx_todcalifi.Enabled = False
	ddlb_calificacion.SelectItem(0)
	cbx_todcalifi.Checked = True
	ddlb_calificacion.Enabled = False
	ii_calificacion = -9
	
ELSE	
	cbx_todcalifi.Enabled = True
	ii_calificacion = -1
END IF
end event

type uo_selproductor from uo_seleccion_varios_productores_clientes within w_info_produccion_resumido
integer x = 773
integer y = 904
integer taborder = 40
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_varios_productores_clientes::destroy
end on

event ue_cambio;call super::ue_cambio;uo_selproductor.Filtra(-1,-1,dw_cliente.Object.clie_Codigo[1])
end event

type uo_selplanta from uo_seleccion_plantas within w_info_produccion_resumido
integer x = 773
integer y = 720
integer height = 184
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_23 from statictext within w_info_produccion_resumido
integer x = 1861
integer y = 1632
integer width = 1641
integer height = 296
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

type cbx_produrotula from checkbox within w_info_produccion_resumido
integer x = 2441
integer y = 1756
integer width = 677
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor Rotulada"
end type

type st_20 from statictext within w_info_produccion_resumido
integer x = 1865
integer y = 2220
integer width = 1637
integer height = 144
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

