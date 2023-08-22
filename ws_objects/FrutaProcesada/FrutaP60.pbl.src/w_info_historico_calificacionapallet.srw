$PBExportHeader$w_info_historico_calificacionapallet.srw
forward
global type w_info_historico_calificacionapallet from w_para_informes
end type
type gb_6 from groupbox within w_info_historico_calificacionapallet
end type
type st_1 from statictext within w_info_historico_calificacionapallet
end type
type em_desde from editmask within w_info_historico_calificacionapallet
end type
type st_6 from statictext within w_info_historico_calificacionapallet
end type
type cbx_etiqueta from checkbox within w_info_historico_calificacionapallet
end type
type st_4 from statictext within w_info_historico_calificacionapallet
end type
type st_especie from statictext within w_info_historico_calificacionapallet
end type
type st_nro2 from statictext within w_info_historico_calificacionapallet
end type
type st_variedad from statictext within w_info_historico_calificacionapallet
end type
type st_calidad from statictext within w_info_historico_calificacionapallet
end type
type em_calidad from editmask within w_info_historico_calificacionapallet
end type
type cbx_calidad from checkbox within w_info_historico_calificacionapallet
end type
type em_embalaje from editmask within w_info_historico_calificacionapallet
end type
type st_embalaje from statictext within w_info_historico_calificacionapallet
end type
type cbx_embalaje from checkbox within w_info_historico_calificacionapallet
end type
type cb_buscaembalaje from commandbutton within w_info_historico_calificacionapallet
end type
type st_5 from statictext within w_info_historico_calificacionapallet
end type
type st_productor from statictext within w_info_historico_calificacionapallet
end type
type em_productor from editmask within w_info_historico_calificacionapallet
end type
type sle_productor from singlelineedit within w_info_historico_calificacionapallet
end type
type cbx_productor from checkbox within w_info_historico_calificacionapallet
end type
type st_2 from statictext within w_info_historico_calificacionapallet
end type
type st_9 from statictext within w_info_historico_calificacionapallet
end type
type em_hasta from editmask within w_info_historico_calificacionapallet
end type
type st_3 from statictext within w_info_historico_calificacionapallet
end type
type cbx_planta from checkbox within w_info_historico_calificacionapallet
end type
type rb_controltodos from radiobutton within w_info_historico_calificacionapallet
end type
type rb_habilitado from radiobutton within w_info_historico_calificacionapallet
end type
type rb_rechazados from radiobutton within w_info_historico_calificacionapallet
end type
type rb_objetados from radiobutton within w_info_historico_calificacionapallet
end type
type dw_tipopallemba from datawindow within w_info_historico_calificacionapallet
end type
type cbx_tipoembalaje from checkbox within w_info_historico_calificacionapallet
end type
type dw_cliente from datawindow within w_info_historico_calificacionapallet
end type
type st_12 from statictext within w_info_historico_calificacionapallet
end type
type cbx_consolid from checkbox within w_info_historico_calificacionapallet
end type
type dw_stat from datawindow within w_info_historico_calificacionapallet
end type
type st_16 from statictext within w_info_historico_calificacionapallet
end type
type st_17 from statictext within w_info_historico_calificacionapallet
end type
type cb_buscaproductor from commandbutton within w_info_historico_calificacionapallet
end type
type st_14 from statictext within w_info_historico_calificacionapallet
end type
type cbx_pallet from checkbox within w_info_historico_calificacionapallet
end type
type dw_codpal from datawindow within w_info_historico_calificacionapallet
end type
type dw_etiqueta from datawindow within w_info_historico_calificacionapallet
end type
type dw_planta from datawindow within w_info_historico_calificacionapallet
end type
type st_11 from statictext within w_info_historico_calificacionapallet
end type
type st_15 from statictext within w_info_historico_calificacionapallet
end type
type st_19 from statictext within w_info_historico_calificacionapallet
end type
type uo_selcate from uo_seleccion_categoria within w_info_historico_calificacionapallet
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_historico_calificacionapallet
end type
type gb_7 from groupbox within w_info_historico_calificacionapallet
end type
type st_8 from statictext within w_info_historico_calificacionapallet
end type
type uo_selespecie from uo_seleccion_especie within w_info_historico_calificacionapallet
end type
type cbx_varirotula from checkbox within w_info_historico_calificacionapallet
end type
type st_20 from statictext within w_info_historico_calificacionapallet
end type
type st_21 from statictext within w_info_historico_calificacionapallet
end type
type em_lote from editmask within w_info_historico_calificacionapallet
end type
type cbx_1 from checkbox within w_info_historico_calificacionapallet
end type
type cbx_prdrot from checkbox within w_info_historico_calificacionapallet
end type
type cbx_calrot from checkbox within w_info_historico_calificacionapallet
end type
type st_7 from statictext within w_info_historico_calificacionapallet
end type
type ddlb_calificacion from dropdownlistbox within w_info_historico_calificacionapallet
end type
type cbx_califica from checkbox within w_info_historico_calificacionapallet
end type
type st_10 from statictext within w_info_historico_calificacionapallet
end type
type st_13 from statictext within w_info_historico_calificacionapallet
end type
type em_norden from editmask within w_info_historico_calificacionapallet
end type
type cbx_2 from checkbox within w_info_historico_calificacionapallet
end type
type st_18 from statictext within w_info_historico_calificacionapallet
end type
end forward

global type w_info_historico_calificacionapallet from w_para_informes
integer x = 14
integer y = 32
integer width = 3895
integer height = 2200
string title = "Histórico Calificación Control Calidad"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_6 gb_6
st_1 st_1
em_desde em_desde
st_6 st_6
cbx_etiqueta cbx_etiqueta
st_4 st_4
st_especie st_especie
st_nro2 st_nro2
st_variedad st_variedad
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
em_embalaje em_embalaje
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
cb_buscaembalaje cb_buscaembalaje
st_5 st_5
st_productor st_productor
em_productor em_productor
sle_productor sle_productor
cbx_productor cbx_productor
st_2 st_2
st_9 st_9
em_hasta em_hasta
st_3 st_3
cbx_planta cbx_planta
rb_controltodos rb_controltodos
rb_habilitado rb_habilitado
rb_rechazados rb_rechazados
rb_objetados rb_objetados
dw_tipopallemba dw_tipopallemba
cbx_tipoembalaje cbx_tipoembalaje
dw_cliente dw_cliente
st_12 st_12
cbx_consolid cbx_consolid
dw_stat dw_stat
st_16 st_16
st_17 st_17
cb_buscaproductor cb_buscaproductor
st_14 st_14
cbx_pallet cbx_pallet
dw_codpal dw_codpal
dw_etiqueta dw_etiqueta
dw_planta dw_planta
st_11 st_11
st_15 st_15
st_19 st_19
uo_selcate uo_selcate
uo_selvariedad uo_selvariedad
gb_7 gb_7
st_8 st_8
uo_selespecie uo_selespecie
cbx_varirotula cbx_varirotula
st_20 st_20
st_21 st_21
em_lote em_lote
cbx_1 cbx_1
cbx_prdrot cbx_prdrot
cbx_calrot cbx_calrot
st_7 st_7
ddlb_calificacion ddlb_calificacion
cbx_califica cbx_califica
st_10 st_10
st_13 st_13
em_norden em_norden
cbx_2 cbx_2
st_18 st_18
end type
global w_info_historico_calificacionapallet w_info_historico_calificacionapallet

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_cliente,idwc_planta,idwc_etiqueta,idwc_tipopallemba,idwc_Destino,&
						idwc_stat, idwc_copal, idwc_tipofrio

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad
uo_calibre								iuo_calibre

 
Integer	ii_variable, ii_tipoi, ii_calificacion
Long		ll_norden, il_lote
String	is_frio
end variables

forward prototypes
public function boolean noexisteplanta (string planta)
public function boolean noexisteetiqueta (string etiqueta)
public function boolean noexisteespecie (string especie)
public function boolean noexistecliente (string cliente)
public function boolean noexistestatus (integer ia_codigo)
public function boolean noexistecopall (integer ai_codigo)
public function boolean existetipofrio (string as_codigo)
end prototypes

public function boolean noexisteplanta (string planta);Integer		li_planta
String		ls_nombre

li_planta	=	Integer(planta)

SELECT	plde_nombre
	INTO	:ls_nombre
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla PlantaDesp")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	istr_mant.argumento[12]	=	String(li_planta)	
	RETURN False
END IF
end function

public function boolean noexisteetiqueta (string etiqueta);Integer		li_etiqueta
String		ls_nombre

li_etiqueta	=	Integer(etiqueta)

SELECT	etiq_nombre
	INTO	:ls_nombre
	FROM	dba.etiquetas
	WHERE	etiq_codigo	=	:li_etiqueta;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Etiquetas")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Etiqueta no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	istr_mant.argumento[6]	=	String(li_etiqueta)	
	RETURN False
END IF
end function

public function boolean noexisteespecie (string especie);Integer		li_especie
String		ls_nombre

li_especie	=	Integer(especie)

SELECT	espe_nombre
	INTO	:ls_nombre
	FROM	dba.especies
	WHERE	espe_codigo	=	:li_especie;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	istr_mant.argumento[3]	=	String(li_especie)	
	RETURN False
END IF
end function

public function boolean noexistecliente (string cliente);Integer		li_cliente
String		ls_nombre

li_cliente	=	Integer(cliente)

SELECT	clie_nombre
	INTO	:ls_nombre
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:li_cliente;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla ClientesProd")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	dw_cliente.SetItem(1, "clie_codigo", li_cliente)
	istr_mant.argumento[2]	=	String(li_cliente)	
	RETURN False
END IF
end function

public function boolean noexistestatus (integer ia_codigo);Integer	li_existe
boolean lb_retorno
SELECT	count(*)
	INTO	:li_existe
	FROM	dba.status
	WHERE	stat_codigo	= :ia_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla status")
	lb_retorno = TRUE
ELSEIF li_existe > 0 THEN	
	lb_retorno = FALSE
ELSE
	lb_retorno = TRUE
END IF

RETURN lb_retorno
end function

public function boolean noexistecopall (integer ai_codigo);Integer	li_existe
boolean lb_retorno

SELECT	count(*)
	INTO	:li_existe
	FROM	dba.codigopallet
	WHERE	copa_codigo	= :ai_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Codigopallet")
	lb_retorno = TRUE
ELSEIF li_existe > 0 THEN	
	lb_retorno = FALSE
ELSE
	lb_retorno = TRUE
END IF

RETURN lb_retorno


end function

public function boolean existetipofrio (string as_codigo);Integer	li_existe
boolean lb_retorno

SELECT	count(*)
	INTO	:li_existe
	FROM	dba.tipofrio
	WHERE	frio_codigo	= :as_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla tipofrio")
	lb_retorno = TRUE
ELSEIF li_existe > 0 THEN	
	lb_retorno = FALSE
ELSE
	lb_retorno = TRUE
END IF

RETURN lb_retorno
end function

on w_info_historico_calificacionapallet.create
int iCurrent
call super::create
this.gb_6=create gb_6
this.st_1=create st_1
this.em_desde=create em_desde
this.st_6=create st_6
this.cbx_etiqueta=create cbx_etiqueta
this.st_4=create st_4
this.st_especie=create st_especie
this.st_nro2=create st_nro2
this.st_variedad=create st_variedad
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.em_embalaje=create em_embalaje
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.st_5=create st_5
this.st_productor=create st_productor
this.em_productor=create em_productor
this.sle_productor=create sle_productor
this.cbx_productor=create cbx_productor
this.st_2=create st_2
this.st_9=create st_9
this.em_hasta=create em_hasta
this.st_3=create st_3
this.cbx_planta=create cbx_planta
this.rb_controltodos=create rb_controltodos
this.rb_habilitado=create rb_habilitado
this.rb_rechazados=create rb_rechazados
this.rb_objetados=create rb_objetados
this.dw_tipopallemba=create dw_tipopallemba
this.cbx_tipoembalaje=create cbx_tipoembalaje
this.dw_cliente=create dw_cliente
this.st_12=create st_12
this.cbx_consolid=create cbx_consolid
this.dw_stat=create dw_stat
this.st_16=create st_16
this.st_17=create st_17
this.cb_buscaproductor=create cb_buscaproductor
this.st_14=create st_14
this.cbx_pallet=create cbx_pallet
this.dw_codpal=create dw_codpal
this.dw_etiqueta=create dw_etiqueta
this.dw_planta=create dw_planta
this.st_11=create st_11
this.st_15=create st_15
this.st_19=create st_19
this.uo_selcate=create uo_selcate
this.uo_selvariedad=create uo_selvariedad
this.gb_7=create gb_7
this.st_8=create st_8
this.uo_selespecie=create uo_selespecie
this.cbx_varirotula=create cbx_varirotula
this.st_20=create st_20
this.st_21=create st_21
this.em_lote=create em_lote
this.cbx_1=create cbx_1
this.cbx_prdrot=create cbx_prdrot
this.cbx_calrot=create cbx_calrot
this.st_7=create st_7
this.ddlb_calificacion=create ddlb_calificacion
this.cbx_califica=create cbx_califica
this.st_10=create st_10
this.st_13=create st_13
this.em_norden=create em_norden
this.cbx_2=create cbx_2
this.st_18=create st_18
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_6
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.cbx_etiqueta
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.st_especie
this.Control[iCurrent+8]=this.st_nro2
this.Control[iCurrent+9]=this.st_variedad
this.Control[iCurrent+10]=this.st_calidad
this.Control[iCurrent+11]=this.em_calidad
this.Control[iCurrent+12]=this.cbx_calidad
this.Control[iCurrent+13]=this.em_embalaje
this.Control[iCurrent+14]=this.st_embalaje
this.Control[iCurrent+15]=this.cbx_embalaje
this.Control[iCurrent+16]=this.cb_buscaembalaje
this.Control[iCurrent+17]=this.st_5
this.Control[iCurrent+18]=this.st_productor
this.Control[iCurrent+19]=this.em_productor
this.Control[iCurrent+20]=this.sle_productor
this.Control[iCurrent+21]=this.cbx_productor
this.Control[iCurrent+22]=this.st_2
this.Control[iCurrent+23]=this.st_9
this.Control[iCurrent+24]=this.em_hasta
this.Control[iCurrent+25]=this.st_3
this.Control[iCurrent+26]=this.cbx_planta
this.Control[iCurrent+27]=this.rb_controltodos
this.Control[iCurrent+28]=this.rb_habilitado
this.Control[iCurrent+29]=this.rb_rechazados
this.Control[iCurrent+30]=this.rb_objetados
this.Control[iCurrent+31]=this.dw_tipopallemba
this.Control[iCurrent+32]=this.cbx_tipoembalaje
this.Control[iCurrent+33]=this.dw_cliente
this.Control[iCurrent+34]=this.st_12
this.Control[iCurrent+35]=this.cbx_consolid
this.Control[iCurrent+36]=this.dw_stat
this.Control[iCurrent+37]=this.st_16
this.Control[iCurrent+38]=this.st_17
this.Control[iCurrent+39]=this.cb_buscaproductor
this.Control[iCurrent+40]=this.st_14
this.Control[iCurrent+41]=this.cbx_pallet
this.Control[iCurrent+42]=this.dw_codpal
this.Control[iCurrent+43]=this.dw_etiqueta
this.Control[iCurrent+44]=this.dw_planta
this.Control[iCurrent+45]=this.st_11
this.Control[iCurrent+46]=this.st_15
this.Control[iCurrent+47]=this.st_19
this.Control[iCurrent+48]=this.uo_selcate
this.Control[iCurrent+49]=this.uo_selvariedad
this.Control[iCurrent+50]=this.gb_7
this.Control[iCurrent+51]=this.st_8
this.Control[iCurrent+52]=this.uo_selespecie
this.Control[iCurrent+53]=this.cbx_varirotula
this.Control[iCurrent+54]=this.st_20
this.Control[iCurrent+55]=this.st_21
this.Control[iCurrent+56]=this.em_lote
this.Control[iCurrent+57]=this.cbx_1
this.Control[iCurrent+58]=this.cbx_prdrot
this.Control[iCurrent+59]=this.cbx_calrot
this.Control[iCurrent+60]=this.st_7
this.Control[iCurrent+61]=this.ddlb_calificacion
this.Control[iCurrent+62]=this.cbx_califica
this.Control[iCurrent+63]=this.st_10
this.Control[iCurrent+64]=this.st_13
this.Control[iCurrent+65]=this.em_norden
this.Control[iCurrent+66]=this.cbx_2
this.Control[iCurrent+67]=this.st_18
end on

on w_info_historico_calificacionapallet.destroy
call super::destroy
destroy(this.gb_6)
destroy(this.st_1)
destroy(this.em_desde)
destroy(this.st_6)
destroy(this.cbx_etiqueta)
destroy(this.st_4)
destroy(this.st_especie)
destroy(this.st_nro2)
destroy(this.st_variedad)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.em_embalaje)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.st_5)
destroy(this.st_productor)
destroy(this.em_productor)
destroy(this.sle_productor)
destroy(this.cbx_productor)
destroy(this.st_2)
destroy(this.st_9)
destroy(this.em_hasta)
destroy(this.st_3)
destroy(this.cbx_planta)
destroy(this.rb_controltodos)
destroy(this.rb_habilitado)
destroy(this.rb_rechazados)
destroy(this.rb_objetados)
destroy(this.dw_tipopallemba)
destroy(this.cbx_tipoembalaje)
destroy(this.dw_cliente)
destroy(this.st_12)
destroy(this.cbx_consolid)
destroy(this.dw_stat)
destroy(this.st_16)
destroy(this.st_17)
destroy(this.cb_buscaproductor)
destroy(this.st_14)
destroy(this.cbx_pallet)
destroy(this.dw_codpal)
destroy(this.dw_etiqueta)
destroy(this.dw_planta)
destroy(this.st_11)
destroy(this.st_15)
destroy(this.st_19)
destroy(this.uo_selcate)
destroy(this.uo_selvariedad)
destroy(this.gb_7)
destroy(this.st_8)
destroy(this.uo_selespecie)
destroy(this.cbx_varirotula)
destroy(this.st_20)
destroy(this.st_21)
destroy(this.em_lote)
destroy(this.cbx_1)
destroy(this.cbx_prdrot)
destroy(this.cbx_calrot)
destroy(this.st_7)
destroy(this.ddlb_calificacion)
destroy(this.cbx_califica)
destroy(this.st_10)
destroy(this.st_13)
destroy(this.em_norden)
destroy(this.cbx_2)
destroy(this.st_18)
end on

event open;call super::open;Date	ld_fecha, ld_actual
Boolean	lb_Cerrar

ld_actual	=	Today()
ld_fecha	=	RelativeDate(ld_actual, -365)

em_desde.Text	=	String(ld_fecha)
em_hasta.Text	=	String(ld_actual)

iuo_calibre   						=	Create uo_calibre

ii_tipoi 			= 	-1
il_lote				=	-1
ii_calificacion	=	-1

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

IF IsNull(uo_SelCate.Codigo) THEN lb_Cerrar	=	True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCate.Seleccion(True, True)
END IF


IF lb_Cerrar THEN
	Close(This)
ELSE
	
	dw_cliente.GetChild("clie_codigo", idwc_cliente)
	idwc_cliente.SetTransObject(SQLCA)
	idwc_cliente.Retrieve()
	dw_cliente.InsertRow(0)
	dw_cliente.SetItem(1, "clie_codigo", gi_codexport)
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(SQLCA)
	idwc_planta.Retrieve(1)
	dw_planta.InsertRow(0)
	dw_planta.SetItem(1, "plde_codigo", gi_codplanta)
	
	dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
	idwc_etiqueta.SetTransObject(sqlca)
	idwc_etiqueta.Retrieve()
	dw_etiqueta.InsertRow(0)
	
	dw_tipopallemba.GetChild("tpem_codigo", idwc_tipopallemba)
	idwc_tipopallemba.SetTransObject(sqlca)
	idwc_tipopallemba.Retrieve(gi_codexport,'Z')
	dw_tipopallemba.InsertRow(0)
	dw_tipopallemba.Object.tpem_codigo.BackGround.Color	=	RGB(166,180,210)
	dw_tipopallemba.Enabled		=	False
	cbx_tipoembalaje.Enabled	=	False	
	
	dw_stat.GetChild("stat_codigo", idwc_stat)
	idwc_stat.SetTransObject(SQLCA)
	idwc_stat.Retrieve()
	dw_stat.InsertRow(0)
	
	dw_codpal.GetChild("copa_codigo", idwc_copal)
	idwc_copal.SetTransObject(SQLCA)
	idwc_copal.Retrieve()
	dw_codpal.InsertRow(0)
	
	istr_mant.argumento[21]	= "0"
	istr_mant.argumento[22]	= "Consolidado"
	dw_stat.Object.stat_codigo.background.color = RGB(166,180,210)
	
	istr_mant.argumento[1]	= 	'1'							//	Tipo de Pallet
	istr_mant.argumento[2]	= 	String(gi_codexport)		//	Cliente
//	istr_mant.argumento[4]	=	'0'							// Variedad
	istr_mant.argumento[5]	=	'0'							// Productor
	istr_mant.argumento[6]	=	'0'							// Etiqueta
	istr_mant.argumento[7]	=	'*'							// Calidad [Calibre]
	istr_mant.argumento[8]	=	'Z'							// Embalaje
	istr_mant.argumento[9]	=	String(ld_fecha)			// Fecha desde
	istr_mant.argumento[10]	=	String(ld_actual)			// Fecha Hasta
	istr_mant.argumento[11]	=	'0'							// Inspección
	istr_mant.argumento[12]	=	'0'							// Planta
	istr_mant.argumento[13]	=	'9'							// Condicion (Todos)
	istr_mant.argumento[20]	=	'-9'							// Destino(consolidado)
	istr_mant.argumento[24]	=	'-1'							// Código de Pallet	
	ll_norden					=	-1								// Orden de proceso
	is_frio						=  '*'							// Tipo de frio
END IF
end event

type st_computador from w_para_informes`st_computador within w_info_historico_calificacionapallet
end type

type st_usuario from w_para_informes`st_usuario within w_info_historico_calificacionapallet
end type

type st_temporada from w_para_informes`st_temporada within w_info_historico_calificacionapallet
end type

type p_logo from w_para_informes`p_logo within w_info_historico_calificacionapallet
end type

type st_titulo from w_para_informes`st_titulo within w_info_historico_calificacionapallet
integer width = 3099
string text = "Histórico Calificación Control Calidad Nro. Lote Pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_historico_calificacionapallet
string tag = "Imprimir Reporte"
integer x = 3483
integer y = 1272
integer taborder = 240
integer weight = 400
fontcharset fontcharset = ansi!
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_tipopallemba= -1, li_cliente, li_condicion, li_varirotula, li_prdrot, li_calrot
String	l_s_titulo, ls_tipopal = 'PALLETS COMPLETOS', ls_embalaje,ls_condicion, ls_revision, &
			ls_sort = "clie_codigo A, plde_codigo A, paen_numero A, paen_fecemb A",&
			ls_orden, ls_ccalidad, ls_fechas,lS_tipopallemba

li_cliente	=	Integer(istr_mant.argumento[2])
ls_embalaje	=	istr_mant.argumento[8]

/*
Categoria
*/
IF IsNull(uo_SelCate.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Categoria Previamente",Exclamation!)
	uo_SelCate.dw_Seleccion.SetFocus()
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

IF ll_norden = 0 OR isnull(ll_norden) THEN
	MessageBox("Atención","Debe Seleccionar Nº Orden de Proceso Previamente",Exclamation!)
	em_norden.SetFocus()
	RETURN
END IF	


IF rb_controltodos.Checked THEN
	ls_ccalidad = ''
ELSEIF rb_habilitado.Checked THEN
	ls_ccalidad = 'HABILITADOS'
ELSEIF rb_rechazados.Checked THEN
	ls_ccalidad = 'RECHAZADOS'
ELSE
	ls_ccalidad = 'OBJETADOS'
END IF
 
IF cbx_consolid.Checked THEN
	istr_mant.argumento[21]	= string(0)
	istr_mant.argumento[22] = "CONSOLIDADOS"
END IF	

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF	

//Código de Pallet
IF cbx_pallet.Checked THEN
	istr_mant.argumento[24]	= string(-1)
END IF

ls_fechas = 'Embalados Entre el : '+istr_mant.argumento[9]+' y el '+istr_mant.argumento[10]

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_historico_calificacionpallet"	

vinf.dw_1.SetTransObject(sqlca)

IF cbx_embalaje.Checked THEN
	li_tipopallemba = -1
ELSE
	IF cbx_tipoembalaje.Checked THEN
		li_tipopallemba = -1
	ELSE
		lS_tipopallemba = dw_tipopallemba.Object.tpem_codigo[1]		
		
		SELECT tpem_cancaj
		INTO :li_tipopallemba
		FROM dba.tipopallemba
		WHERE clie_codigo = :li_cliente
      AND   emba_codigo = :ls_embalaje
		AND   tpem_codigo = :ls_tipopallemba;
		
	END IF
END IF

IF cbx_prdrot.Checked THEN
	li_prdrot 	=	1
ELSE
	li_prdrot	=	0
END IF

IF cbx_calrot.Checked THEN
	li_calrot 	=	1
ELSE
	li_calrot	=	0
END IF

ii_tipoi = -1

fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
			Integer(istr_mant.argumento[12]), uo_selespecie.Codigo,uo_selvariedad.Codigo,&
			Long(istr_mant.argumento[5]), Integer(istr_mant.argumento[6]),istr_mant.argumento[7],&
			istr_mant.argumento[8], Date(istr_mant.argumento[9]),Date(istr_mant.argumento[10]),&
			Integer(istr_mant.argumento[11]),ii_variable,li_condicion, Integer(li_tipopallemba),&
			Integer(istr_mant.argumento[20]),Integer(istr_mant.argumento[21]),&
			Integer(istr_mant.argumento[24]),uo_SelCate.Codigo,li_varirotula,ll_norden,li_prdrot,li_calrot,&
			is_frio,ii_tipoi,ii_calificacion,il_lote)
	
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("revision.text = '" + ls_revision+ "'")
	vinf.dw_1.Modify("t_status.text = '" + Upper(istr_mant.argumento[22]) + "'")	
	vinf.dw_1.Modify("ccalidad.text = '" + ls_ccalidad+ "'")
	vinf.dw_1.Modify("fechas.text = '" + ls_fechas+ "'")	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_historico_calificacionapallet
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3479
integer y = 1560
integer taborder = 250
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_6 from groupbox within w_info_historico_calificacionapallet
integer x = 2007
integer y = 1240
integer width = 1312
integer height = 248
integer taborder = 130
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Control Calidad"
end type

type st_1 from statictext within w_info_historico_calificacionapallet
integer x = 343
integer y = 1256
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

type em_desde from editmask within w_info_historico_calificacionapallet
integer x = 2322
integer y = 876
integer width = 311
integer height = 84
integer taborder = 210
boolean bringtotop = true
integer textsize = -8
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

event modified;istr_mant.argumento[9]	=	String(this.text)
end event

type st_6 from statictext within w_info_historico_calificacionapallet
integer x = 343
integer y = 504
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type cbx_etiqueta from checkbox within w_info_historico_calificacionapallet
integer x = 1659
integer y = 1268
integer width = 270
integer height = 80
boolean bringtotop = true
integer textsize = -8
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
	dw_etiqueta.Enabled		=	False
	istr_mant.argumento[6]	=	'0'
ELSE
	dw_etiqueta.Enabled		=	True
	istr_mant.argumento[6]	=	String(dw_etiqueta.Object.etiq_codigo[1])
END IF
end event

type st_4 from statictext within w_info_historico_calificacionapallet
integer x = 251
integer y = 440
integer width = 1728
integer height = 348
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

type st_especie from statictext within w_info_historico_calificacionapallet
integer x = 347
integer y = 940
integer width = 238
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type st_nro2 from statictext within w_info_historico_calificacionapallet
integer x = 251
integer y = 796
integer width = 1728
integer height = 428
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

type st_variedad from statictext within w_info_historico_calificacionapallet
integer x = 347
integer y = 1116
integer width = 279
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type st_calidad from statictext within w_info_historico_calificacionapallet
integer x = 1998
integer y = 452
integer width = 256
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_historico_calificacionapallet
integer x = 2290
integer y = 452
integer width = 297
integer height = 84
integer taborder = 150
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
		istr_mant.argumento[7]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	




end event

type cbx_calidad from checkbox within w_info_historico_calificacionapallet
integer x = 2729
integer y = 452
integer width = 297
integer height = 80
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
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
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type em_embalaje from editmask within w_info_historico_calificacionapallet
integer x = 2290
integer y = 560
integer width = 297
integer height = 84
integer taborder = 180
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
string mask = "xxxxxxxxxx"
end type

event modified;Integer  li_cliente
String	ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[2]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dba.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
	
//	
//IF sqlca.SQLCode = -1 THEN
////	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
////	This.SetFocus()
//ELSEIF sqlca.SQLCode = 100 THEN
////	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
////		"Ingrese o seleccione otro Código.")
////	This.SetFocus()
//ELSE
IF ls_Nombre <> '' THEN
	istr_mant.argumento[8]	=	ls_embalaje
	dw_tipopallemba.GetChild("tpem_codigo", idwc_tipopallemba)
	idwc_tipopallemba.SetTransObject(sqlca)
	idwc_tipopallemba.Retrieve(Integer(istr_mant.argumento[2]),ls_embalaje)
	cbx_tipoembalaje.Enabled = TRUE
ELSE
	istr_mant.argumento[8]	=	ls_embalaje
	cbx_tipoembalaje.Checked = True
	cbx_tipoembalaje.Enabled = False
END IF	
//END IF
end event

type st_embalaje from statictext within w_info_historico_calificacionapallet
integer x = 1998
integer y = 564
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -8
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

type cbx_embalaje from checkbox within w_info_historico_calificacionapallet
integer x = 2729
integer y = 560
integer width = 283
integer height = 80
integer taborder = 160
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

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[8]		=	'Z'
   cbx_tipoembalaje.Enabled	=	False		
	
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
	
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_historico_calificacionapallet
integer x = 2601
integer y = 564
integer width = 96
integer height = 76
integer taborder = 190
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

lstr_busq.argum[1]	=	istr_mant.argumento[2] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[8]	=	lstr_busq.argum[2]

	dw_tipopallemba.GetChild("tpem_codigo", idwc_tipopallemba)
	idwc_tipopallemba.SetTransObject(sqlca)	
   idwc_tipopallemba.Retrieve(Integer(istr_mant.argumento[2]),lstr_busq.argum[2])	
END IF
end event

type st_5 from statictext within w_info_historico_calificacionapallet
integer x = 251
integer y = 1224
integer width = 1728
integer height = 348
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

type st_productor from statictext within w_info_historico_calificacionapallet
integer x = 343
integer y = 1448
integer width = 297
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type em_productor from editmask within w_info_historico_calificacionapallet
integer x = 658
integer y = 1444
integer width = 238
integer height = 84
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
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;String		ls_Nombre
Long			ll_productor

ll_productor	=	Long(This.Text)

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:ll_productor;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productores")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
ELSE
	sle_productor.Text		=	ls_nombre
	istr_mant.argumento[5]	=	String(ll_productor)
END IF
end event

type sle_productor from singlelineedit within w_info_historico_calificacionapallet
integer x = 1006
integer y = 1444
integer width = 613
integer height = 84
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

type cbx_productor from checkbox within w_info_historico_calificacionapallet
integer x = 663
integer y = 1380
integer width = 288
integer height = 68
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
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
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[5]		=	'0'
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type st_2 from statictext within w_info_historico_calificacionapallet
integer x = 2121
integer y = 884
integer width = 201
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_historico_calificacionapallet
integer x = 2720
integer y = 884
integer width = 201
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_historico_calificacionapallet
integer x = 2912
integer y = 876
integer width = 311
integer height = 84
integer taborder = 230
boolean bringtotop = true
integer textsize = -8
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

event modified;istr_mant.argumento[10] = String(this.text)
end event

type st_3 from statictext within w_info_historico_calificacionapallet
integer x = 343
integer y = 664
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_historico_calificacionapallet
integer x = 1659
integer y = 648
integer width = 293
integer height = 80
boolean bringtotop = true
integer textsize = -8
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
	dw_planta.Enabled			=	False
	istr_mant.argumento[12]	=	'0'
ELSE
	dw_planta.Enabled			=	True
	istr_mant.argumento[12]	=	String(dw_planta.GetItemNumber(1,"plde_codigo"))
END IF
end event

type rb_controltodos from radiobutton within w_info_historico_calificacionapallet
integer x = 2075
integer y = 1296
integer width = 283
integer height = 72
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

event clicked;IF This.checked=True THEN
	ii_variable=0
END IF
end event

type rb_habilitado from radiobutton within w_info_historico_calificacionapallet
integer x = 2747
integer y = 1292
integer width = 407
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Habilitados"
end type

event clicked;IF This.checked=True THEN
	ii_variable=1
END IF
end event

type rb_rechazados from radiobutton within w_info_historico_calificacionapallet
integer x = 2075
integer y = 1392
integer width = 434
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Rechazados"
end type

event clicked;IF This.checked=True THEN
	ii_variable=3
END IF
end event

type rb_objetados from radiobutton within w_info_historico_calificacionapallet
integer x = 2747
integer y = 1396
integer width = 407
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Objetados"
end type

event clicked;IF This.checked=True THEN
	ii_variable=2
END IF
end event

type dw_tipopallemba from datawindow within w_info_historico_calificacionapallet
integer x = 2286
integer y = 664
integer width = 320
integer height = 84
integer taborder = 200
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_tipopallemba"
boolean maxbox = true
boolean border = false
boolean livescroll = true
end type

type cbx_tipoembalaje from checkbox within w_info_historico_calificacionapallet
integer x = 2729
integer y = 664
integer width = 283
integer height = 80
integer taborder = 170
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_tipopallemba.Object.tpem_codigo.BackGround.Color	=	RGB(166,180,210)
	dw_tipopallemba.Enabled		=	False	
	
END IF
end event

type dw_cliente from datawindow within w_info_historico_calificacionapallet
integer x = 663
integer y = 492
integer width = 1157
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]	=	data

IF NoExisteCliente(istr_mant.argumento[2]) THEN
	dw_cliente.SetItem(1, "clie_codigo", gi_codexport)
	dw_cliente.SetFocus()
	RETURN 1
ELSE
	
   dw_tipopallemba.GetChild("tpem_codigo", idwc_tipopallemba)
   idwc_tipopallemba.SetTransObject(sqlca)
	idwc_tipopallemba.Retrieve(Integer(istr_mant.argumento[2]),'Z')

END IF

end event

event itemerror;RETURN 1
end event

type st_12 from statictext within w_info_historico_calificacionapallet
integer x = 2011
integer y = 1120
integer width = 215
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Status"
boolean focusrectangle = false
end type

type cbx_consolid from checkbox within w_info_historico_calificacionapallet
integer x = 2322
integer y = 1040
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidados"
boolean checked = true
end type

event clicked;IF This.checked THEN
	dw_stat.enabled = FALSE
	dw_stat.Object.stat_codigo.background.color = RGB(166,180,210)
ELSE
	dw_stat.enabled = TRUE
	dw_stat.Object.stat_codigo.background.color = rgb(255,255,255)
	istr_mant.argumento[21]	= "1"	
	istr_mant.argumento[22]	=  f_statnombre(1)
	dw_stat.Object.stat_codigo[1] = 1
END IF	
end event

type dw_stat from datawindow within w_info_historico_calificacionapallet
integer x = 2318
integer y = 1108
integer width = 983
integer height = 88
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF  NoExisteStatus(integer(data)) THEN
	MessageBox("informe","No Existe Status")
	dw_stat.SetItem(1,"stat_codigo",li_Null)
	RETURN 1
ELSE	
	istr_mant.argumento[21]	= data
	istr_mant.argumento[22]	=  f_statnombre(integer(data))
END IF

end event

type st_16 from statictext within w_info_historico_calificacionapallet
integer x = 251
integer y = 1792
integer width = 1728
integer height = 156
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

type st_17 from statictext within w_info_historico_calificacionapallet
integer x = 1979
integer y = 1224
integer width = 1371
integer height = 472
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

type cb_buscaproductor from commandbutton within w_info_historico_calificacionapallet
integer x = 905
integer y = 1444
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

lstr_busq.argum[1]	=	istr_mant.argumento[2] // Cliente

OpenWithParm(w_busc_productores, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	em_productor.SetFocus()
ELSE
	em_productor.Text			=	lstr_busq.argum[3]
	sle_productor.Text		=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[3]
END IF
end event

type st_14 from statictext within w_info_historico_calificacionapallet
integer x = 343
integer y = 1836
integer width = 311
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cód. Pallet"
boolean focusrectangle = false
end type

type cbx_pallet from checkbox within w_info_historico_calificacionapallet
integer x = 1422
integer y = 1828
integer width = 238
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked THEN
	dw_codpal.enabled = FALSE
	dw_codpal.Object.copa_codigo.background.color = RGB(166,180,210)
	istr_mant.argumento[24] = '-1'
	
	dw_codpal.Reset()
	idwc_copal.Retrieve()
	dw_codpal.InsertRow(0)
	
ELSE
	dw_codpal.enabled = TRUE
	dw_codpal.Object.copa_codigo.background.color = rgb(255,255,255)
	dw_codpal.Object.copa_codigo[1] = 1
	istr_mant.argumento[24] = '1'
END IF	
end event

type dw_codpal from datawindow within w_info_historico_calificacionapallet
integer x = 663
integer y = 1824
integer width = 704
integer height = 88
integer taborder = 260
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_codigopallet"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null
SetNull(li_Null)

IF NoExisteCopall(integer(data)) THEN
	MessageBox("informe","No Existe Código de Pallet")
	dw_codpal.SetItem(1,"copa_codigo",li_Null)
	RETURN 1
ELSE	
	istr_mant.argumento[24]	= data
END IF

end event

event itemerror;RETURN 1
end event

type dw_etiqueta from datawindow within w_info_historico_calificacionapallet
integer x = 658
integer y = 1256
integer width = 887
integer height = 96
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_etiquetas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[6]	=	data

IF NoExisteEtiqueta(istr_mant.argumento[6]) THEN
	dw_etiqueta.SetItem(1, "etiq_codigo", 1)
	dw_etiqueta.SetFocus()
	RETURN 1
END IF


end event

event itemerror;RETURN 1
end event

type dw_planta from datawindow within w_info_historico_calificacionapallet
integer x = 663
integer y = 636
integer width = 974
integer height = 104
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[12]	=	data
end event

type st_11 from statictext within w_info_historico_calificacionapallet
integer x = 251
integer y = 1576
integer width = 1728
integer height = 212
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

type st_15 from statictext within w_info_historico_calificacionapallet
integer x = 1979
integer y = 1028
integer width = 1371
integer height = 196
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

type st_19 from statictext within w_info_historico_calificacionapallet
integer x = 343
integer y = 1688
integer width = 261
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Categoria"
boolean focusrectangle = false
end type

type uo_selcate from uo_seleccion_categoria within w_info_historico_calificacionapallet
integer x = 663
integer y = 1592
integer taborder = 270
boolean bringtotop = true
end type

on uo_selcate.destroy
call uo_seleccion_categoria::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_historico_calificacionapallet
integer x = 663
integer y = 1016
integer taborder = 40
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type gb_7 from groupbox within w_info_historico_calificacionapallet
integer x = 2043
integer y = 800
integer width = 1248
integer height = 196
integer taborder = 220
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha Embalaje"
end type

type st_8 from statictext within w_info_historico_calificacionapallet
integer x = 1979
integer y = 784
integer width = 1371
integer height = 244
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

type uo_selespecie from uo_seleccion_especie within w_info_historico_calificacionapallet
event destroy ( )
integer x = 663
integer y = 820
integer height = 180
integer taborder = 30
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
		uo_selvariedad.Enabled		=	True
		uo_selvariedad.Filtra(This.Codigo)

END CHOOSE
end event

type cbx_varirotula from checkbox within w_info_historico_calificacionapallet
integer x = 1659
integer y = 1104
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Var. Rot."
end type

type st_20 from statictext within w_info_historico_calificacionapallet
integer x = 1979
integer y = 1696
integer width = 1371
integer height = 124
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

type st_21 from statictext within w_info_historico_calificacionapallet
integer x = 2066
integer y = 1856
integer width = 297
integer height = 68
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Nro. Lote"
boolean focusrectangle = false
end type

type em_lote from editmask within w_info_historico_calificacionapallet
integer x = 2386
integer y = 1840
integer width = 361
integer height = 84
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
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;il_lote = Long(this.text)
end event

type cbx_1 from checkbox within w_info_historico_calificacionapallet
integer x = 2871
integer y = 1844
integer width = 288
integer height = 68
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 400
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
	em_lote.Enabled = False
	em_lote.Text = ''
	il_lote = -1
ELSE
	em_lote.Enabled = True
	il_lote = Long(em_lote.Text)
END IF
end event

type cbx_prdrot from checkbox within w_info_historico_calificacionapallet
integer x = 1659
integer y = 1444
integer width = 315
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Rotulado"
end type

type cbx_calrot from checkbox within w_info_historico_calificacionapallet
integer x = 3017
integer y = 452
integer width = 315
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Rotulado"
end type

type st_7 from statictext within w_info_historico_calificacionapallet
integer x = 1979
integer y = 440
integer width = 1371
integer height = 340
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

type ddlb_calificacion from dropdownlistbox within w_info_historico_calificacionapallet
integer x = 2331
integer y = 1572
integer width = 951
integer height = 612
integer taborder = 280
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

type cbx_califica from checkbox within w_info_historico_calificacionapallet
integer x = 2331
integer y = 1492
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked THEN
	ii_calificacion = -1
	ddlb_calificacion.Enabled = False
	ddlb_calificacion.SelectItem(0)

ELSE
	ddlb_calificacion.Enabled = True
	//ii_calificacion = ddlb_calificacion.SelectItem(1)
	
END IF	
end event

type st_10 from statictext within w_info_historico_calificacionapallet
integer x = 2011
integer y = 1576
integer width = 293
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Calificación"
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_historico_calificacionapallet
integer x = 2066
integer y = 1728
integer width = 297
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Nro. Orden"
boolean focusrectangle = false
end type

type em_norden from editmask within w_info_historico_calificacionapallet
integer x = 2386
integer y = 1712
integer width = 361
integer height = 88
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

type cbx_2 from checkbox within w_info_historico_calificacionapallet
integer x = 2871
integer y = 1716
integer width = 288
integer height = 72
integer taborder = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 400
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
	em_norden.Enabled = False
	em_norden.Text = ''
	ll_norden = -1
ELSE
	em_norden.Enabled = True
	ll_norden = Long(em_norden.Text)
END IF
end event

type st_18 from statictext within w_info_historico_calificacionapallet
integer x = 1979
integer y = 1820
integer width = 1371
integer height = 128
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

