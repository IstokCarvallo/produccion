$PBExportHeader$w_info_estadisticas_cajas.srw
forward
global type w_info_estadisticas_cajas from w_para_informes
end type
type st_6 from statictext within w_info_estadisticas_cajas
end type
type st_1 from statictext within w_info_estadisticas_cajas
end type
type st_8 from statictext within w_info_estadisticas_cajas
end type
type st_3 from statictext within w_info_estadisticas_cajas
end type
type st_variedad from statictext within w_info_estadisticas_cajas
end type
type st_11 from statictext within w_info_estadisticas_cajas
end type
type st_embalaje from statictext within w_info_estadisticas_cajas
end type
type st_calidad from statictext within w_info_estadisticas_cajas
end type
type uo_selespecie from uo_seleccion_especie within w_info_estadisticas_cajas
end type
type uo_selplanta from uo_seleccion_plantas within w_info_estadisticas_cajas
end type
type uo_selproductor from uo_seleccion_productor within w_info_estadisticas_cajas
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_estadisticas_cajas
end type
type cbx_etiqueta from checkbox within w_info_estadisticas_cajas
end type
type cbx_consetiqueta from checkbox within w_info_estadisticas_cajas
end type
type dw_etiqueta from datawindow within w_info_estadisticas_cajas
end type
type cbx_embalaje from checkbox within w_info_estadisticas_cajas
end type
type em_embalaje from editmask within w_info_estadisticas_cajas
end type
type cbx_consembalaje from checkbox within w_info_estadisticas_cajas
end type
type cbx_conscalidad from checkbox within w_info_estadisticas_cajas
end type
type cbx_calidad from checkbox within w_info_estadisticas_cajas
end type
type em_calidad from editmask within w_info_estadisticas_cajas
end type
type cb_buscaembalaje from commandbutton within w_info_estadisticas_cajas
end type
type st_2 from statictext within w_info_estadisticas_cajas
end type
type em_desde from editmask within w_info_estadisticas_cajas
end type
type st_7 from statictext within w_info_estadisticas_cajas
end type
type em_hasta from editmask within w_info_estadisticas_cajas
end type
type cbx_fecemb from checkbox within w_info_estadisticas_cajas
end type
type em_orden from editmask within w_info_estadisticas_cajas
end type
type cbx_ordentodo from checkbox within w_info_estadisticas_cajas
end type
type st_17 from statictext within w_info_estadisticas_cajas
end type
type cbx_todosfecha from checkbox within w_info_estadisticas_cajas
end type
type gb_4 from groupbox within w_info_estadisticas_cajas
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_estadisticas_cajas
end type
type cbx_consproc from checkbox within w_info_estadisticas_cajas
end type
type cbx_exporta from checkbox within w_info_estadisticas_cajas
end type
type sle_estado from singlelineedit within w_info_estadisticas_cajas
end type
type dw_entidad from datawindow within w_info_estadisticas_cajas
end type
type cbx_todosrut from checkbox within w_info_estadisticas_cajas
end type
type cbx_consrut from checkbox within w_info_estadisticas_cajas
end type
type ddlb_tipo from dropdownlistbox within w_info_estadisticas_cajas
end type
type st_5 from statictext within w_info_estadisticas_cajas
end type
type sle_proceso from singlelineedit within w_info_estadisticas_cajas
end type
type cbx_todospro from checkbox within w_info_estadisticas_cajas
end type
type cbx_conspro from checkbox within w_info_estadisticas_cajas
end type
type ddlb_tipoproc from dropdownlistbox within w_info_estadisticas_cajas
end type
type st_9 from statictext within w_info_estadisticas_cajas
end type
type st_4 from statictext within w_info_estadisticas_cajas
end type
type uo_selcontratista from uo_seleccion_contratista within w_info_estadisticas_cajas
end type
end forward

global type w_info_estadisticas_cajas from w_para_informes
integer width = 2578
integer height = 2272
string title = "Informe Estadistico Cajas Procesadas"
boolean minbox = false
boolean maxbox = false
st_6 st_6
st_1 st_1
st_8 st_8
st_3 st_3
st_variedad st_variedad
st_11 st_11
st_embalaje st_embalaje
st_calidad st_calidad
uo_selespecie uo_selespecie
uo_selplanta uo_selplanta
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
cbx_etiqueta cbx_etiqueta
cbx_consetiqueta cbx_consetiqueta
dw_etiqueta dw_etiqueta
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cbx_consembalaje cbx_consembalaje
cbx_conscalidad cbx_conscalidad
cbx_calidad cbx_calidad
em_calidad em_calidad
cb_buscaembalaje cb_buscaembalaje
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
cbx_fecemb cbx_fecemb
em_orden em_orden
cbx_ordentodo cbx_ordentodo
st_17 st_17
cbx_todosfecha cbx_todosfecha
gb_4 gb_4
uo_selcliente uo_selcliente
cbx_consproc cbx_consproc
cbx_exporta cbx_exporta
sle_estado sle_estado
dw_entidad dw_entidad
cbx_todosrut cbx_todosrut
cbx_consrut cbx_consrut
ddlb_tipo ddlb_tipo
st_5 st_5
sle_proceso sle_proceso
cbx_todospro cbx_todospro
cbx_conspro cbx_conspro
ddlb_tipoproc ddlb_tipoproc
st_9 st_9
st_4 st_4
uo_selcontratista uo_selcontratista
end type
global w_info_estadisticas_cajas w_info_estadisticas_cajas

type variables
str_mant					istr_mant

Integer					ii_nroorden, ii_tipoorden, ii_cliente, ii_tipoproceso
Long						il_nroorden
String					is_rut, is_tipo, is_cajas

uo_embalajesprod		iuo_embalajes
uo_spro_ordenproceso	iuo_orden

DataWindowChild		idwc_etiqueta

DataWindowChild 	idwc_cont

uo_contratista		iuo_cont
end variables

forward prototypes
public function boolean existecalibre (string as_calibre)
public function boolean existeproceso ()
public function boolean existe (integer ai_planta, integer ai_tipoorden, long al_numero, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente)
public function boolean contratistaporproceso (integer ai_proceso)
end prototypes

public function boolean existecalibre (string as_calibre);Integer	li_cliente,	li_especie, li_variedad, li_cantid
String	ls_calibre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
li_especie	=	Integer(istr_mant.argumento[2]) // Especie
li_variedad	=	Integer(istr_mant.argumento[5]) // Variedad

SELECT	Count(*)
  INTO	:li_cantid
  FROM	dbo.variecalibre
 WHERE	espe_codigo	=	:li_especie
	AND	vari_codigo	=	:li_variedad
	AND	vaca_calibr	=	:as_calibre;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variecalibre")
	Return False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Calibre no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	Return False
ELSE
	Return True
END IF
end function

public function boolean existeproceso ();Return True

end function

public function boolean existe (integer ai_planta, integer ai_tipoorden, long al_numero, boolean ab_mensaje, transaction at_transaccion, integer ai_cliente);Boolean	lb_Retorno = True
Integer	li_cliente,li_contratista, li_existe
Long ll_row

li_cliente = ai_Cliente

SELECT	count(*),cont_codigo INTO :li_existe,:li_contratista
	FROM	dbo.spro_ordenproceso
	WHERE	plde_codigo =	:ai_Planta
	AND	orpr_tipord	=	:ai_TipoOrden
	AND	orpr_numero =	:al_Numero
	AND   :ai_cliente in (clie_codigo, -1)
	GROUP BY cont_codigo
	USING at_Transaccion;
	
	
IF at_Transaccion.SQLCode = -1 THEN
	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Ordenes de Proceso")

	lb_Retorno	=	False
	
ELSEIF at_Transaccion.SQLCode = 100 THEN
	 IF ab_Mensaje THEN
		MessageBox("Atención", "Orden de Proceso " + String(al_Numero, '00000000') + &
					", no ha sido registrado.~r~rIngrese o seleccione otro Número.")
	 END IF
	lb_Retorno	=	False
ELSE
	uo_SelContratista.Codigo = li_contratista
	uo_SelContratista.dw_Seleccion.Object.Codigo[1] = li_contratista
END IF

RETURN lb_retorno
end function

public function boolean contratistaporproceso (integer ai_proceso);Boolean	lb_Retorno = True
Integer	 li_existe


//
//SELECT	count(*) INTO :li_existe
//	FROM	dbo.spro_ordenproceso
//	WHERE	plde_codigo =	:ai_Planta
//	AND	orpr_tipord	=	:ai_TipoOrden
//	AND	orpr_numero =	:al_Numero
//	AND   :ai_cliente in (clie_codigo, -1)
//	USING at_Transaccion;
//
//IF at_Transaccion.SQLCode = -1 THEN
//	F_ErrorBaseDatos(at_Transaccion, "Lectura de Tabla Ordenes de Proceso")
//
//	lb_Retorno	=	False
//	
//ELSEIF at_Transaccion.SQLCode = 100 THEN
//	 IF ab_Mensaje THEN
//		MessageBox("Atención", "Orden de Proceso " + String(al_Numero, '00000000') + &
//					", no ha sido registrado.~r~rIngrese o seleccione otro Número.")
//	 END IF
//	lb_Retorno	=	False
//	
//END IF

RETURN lb_retorno
end function

on w_info_estadisticas_cajas.create
int iCurrent
call super::create
this.st_6=create st_6
this.st_1=create st_1
this.st_8=create st_8
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_11=create st_11
this.st_embalaje=create st_embalaje
this.st_calidad=create st_calidad
this.uo_selespecie=create uo_selespecie
this.uo_selplanta=create uo_selplanta
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
this.cbx_etiqueta=create cbx_etiqueta
this.cbx_consetiqueta=create cbx_consetiqueta
this.dw_etiqueta=create dw_etiqueta
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cbx_consembalaje=create cbx_consembalaje
this.cbx_conscalidad=create cbx_conscalidad
this.cbx_calidad=create cbx_calidad
this.em_calidad=create em_calidad
this.cb_buscaembalaje=create cb_buscaembalaje
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.cbx_fecemb=create cbx_fecemb
this.em_orden=create em_orden
this.cbx_ordentodo=create cbx_ordentodo
this.st_17=create st_17
this.cbx_todosfecha=create cbx_todosfecha
this.gb_4=create gb_4
this.uo_selcliente=create uo_selcliente
this.cbx_consproc=create cbx_consproc
this.cbx_exporta=create cbx_exporta
this.sle_estado=create sle_estado
this.dw_entidad=create dw_entidad
this.cbx_todosrut=create cbx_todosrut
this.cbx_consrut=create cbx_consrut
this.ddlb_tipo=create ddlb_tipo
this.st_5=create st_5
this.sle_proceso=create sle_proceso
this.cbx_todospro=create cbx_todospro
this.cbx_conspro=create cbx_conspro
this.ddlb_tipoproc=create ddlb_tipoproc
this.st_9=create st_9
this.st_4=create st_4
this.uo_selcontratista=create uo_selcontratista
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_6
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_8
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_variedad
this.Control[iCurrent+6]=this.st_11
this.Control[iCurrent+7]=this.st_embalaje
this.Control[iCurrent+8]=this.st_calidad
this.Control[iCurrent+9]=this.uo_selespecie
this.Control[iCurrent+10]=this.uo_selplanta
this.Control[iCurrent+11]=this.uo_selproductor
this.Control[iCurrent+12]=this.uo_selvariedad
this.Control[iCurrent+13]=this.cbx_etiqueta
this.Control[iCurrent+14]=this.cbx_consetiqueta
this.Control[iCurrent+15]=this.dw_etiqueta
this.Control[iCurrent+16]=this.cbx_embalaje
this.Control[iCurrent+17]=this.em_embalaje
this.Control[iCurrent+18]=this.cbx_consembalaje
this.Control[iCurrent+19]=this.cbx_conscalidad
this.Control[iCurrent+20]=this.cbx_calidad
this.Control[iCurrent+21]=this.em_calidad
this.Control[iCurrent+22]=this.cb_buscaembalaje
this.Control[iCurrent+23]=this.st_2
this.Control[iCurrent+24]=this.em_desde
this.Control[iCurrent+25]=this.st_7
this.Control[iCurrent+26]=this.em_hasta
this.Control[iCurrent+27]=this.cbx_fecemb
this.Control[iCurrent+28]=this.em_orden
this.Control[iCurrent+29]=this.cbx_ordentodo
this.Control[iCurrent+30]=this.st_17
this.Control[iCurrent+31]=this.cbx_todosfecha
this.Control[iCurrent+32]=this.gb_4
this.Control[iCurrent+33]=this.uo_selcliente
this.Control[iCurrent+34]=this.cbx_consproc
this.Control[iCurrent+35]=this.cbx_exporta
this.Control[iCurrent+36]=this.sle_estado
this.Control[iCurrent+37]=this.dw_entidad
this.Control[iCurrent+38]=this.cbx_todosrut
this.Control[iCurrent+39]=this.cbx_consrut
this.Control[iCurrent+40]=this.ddlb_tipo
this.Control[iCurrent+41]=this.st_5
this.Control[iCurrent+42]=this.sle_proceso
this.Control[iCurrent+43]=this.cbx_todospro
this.Control[iCurrent+44]=this.cbx_conspro
this.Control[iCurrent+45]=this.ddlb_tipoproc
this.Control[iCurrent+46]=this.st_9
this.Control[iCurrent+47]=this.st_4
this.Control[iCurrent+48]=this.uo_selcontratista
end on

on w_info_estadisticas_cajas.destroy
call super::destroy
destroy(this.st_6)
destroy(this.st_1)
destroy(this.st_8)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_11)
destroy(this.st_embalaje)
destroy(this.st_calidad)
destroy(this.uo_selespecie)
destroy(this.uo_selplanta)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
destroy(this.cbx_etiqueta)
destroy(this.cbx_consetiqueta)
destroy(this.dw_etiqueta)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cbx_consembalaje)
destroy(this.cbx_conscalidad)
destroy(this.cbx_calidad)
destroy(this.em_calidad)
destroy(this.cb_buscaembalaje)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.cbx_fecemb)
destroy(this.em_orden)
destroy(this.cbx_ordentodo)
destroy(this.st_17)
destroy(this.cbx_todosfecha)
destroy(this.gb_4)
destroy(this.uo_selcliente)
destroy(this.cbx_consproc)
destroy(this.cbx_exporta)
destroy(this.sle_estado)
destroy(this.dw_entidad)
destroy(this.cbx_todosrut)
destroy(this.cbx_consrut)
destroy(this.ddlb_tipo)
destroy(this.st_5)
destroy(this.sle_proceso)
destroy(this.cbx_todospro)
destroy(this.cbx_conspro)
destroy(this.ddlb_tipoproc)
destroy(this.st_9)
destroy(this.st_4)
destroy(this.uo_selcontratista)
end on

event open;call super::open;Boolean lb_cerrar


ii_TipoOrden				=	Integer(Message.StringParm)

IF ii_tipoOrden = 0 THEN
	st_titulo.text			=	'Informe Estadistico Cajas Procesadas'
ELSE
	st_titulo.text			=	'Informe Estadistico Etiquetas Impresas'
END IF


IF IsNull(uo_SelCliente.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo)THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelContratista.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
END IF

iuo_embalajes		=	Create uo_embalajesprod
iuo_orden			=	Create uo_spro_ordenproceso

iuo_cont	=	Create uo_contratista


uo_SelCliente.Seleccion(True, True)
uo_SelPlanta.Seleccion(False, False)
uo_SelContratista.Seleccion(True, True)

//uo_SelCliente.dw_seleccion.object.codigo[1] 		= 	gi_CodExport
uo_SelPlanta.dw_seleccion.object.codigo[1]	= 	gstr_paramplanta.codigoplanta

dw_etiqueta.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()
dw_etiqueta.InsertRow(0)

ddlb_tipo.SelectItem(1)

is_tipo 	= "F"
is_cajas = '-9'
is_rut 	= '-1'

dw_entidad.Reset()
dw_entidad.SetTransObject(SQLCA)
dw_entidad.InsertRow(0)

cbx_todosfecha.TriggerEvent("clicked")
cbx_todospro.TriggerEvent("clicked")

em_desde.Text 				= 	String(RelativeDate(Today(), -365))
em_hasta.Text 				= 	String(Today())


//uo_selcliente.Codigo	=	gi_CodExport
uo_SelPlanta.Codigo		= 	gstr_paramplanta.codigoplanta
istr_mant.argumento[1]	=	String(gi_CodExport)
ddlb_tipoproc.SelectItem(1)
ii_tipoproceso	=	-1
end event

type pb_excel from w_para_informes`pb_excel within w_info_estadisticas_cajas
end type

type st_computador from w_para_informes`st_computador within w_info_estadisticas_cajas
end type

type st_usuario from w_para_informes`st_usuario within w_info_estadisticas_cajas
end type

type st_temporada from w_para_informes`st_temporada within w_info_estadisticas_cajas
end type

type p_logo from w_para_informes`p_logo within w_info_estadisticas_cajas
end type

type st_titulo from w_para_informes`st_titulo within w_info_estadisticas_cajas
integer width = 1801
string text = "Informe Estadistico Cajas Procesadas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_estadisticas_cajas
integer x = 2153
integer y = 1376
integer taborder = 110
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer		li_etiqueta, 	li_consfecha
String			ls_embalaje, 	ls_calidad, 	ls_Archivo,	ls_ruta
Long			fila
Date			ld_desde, 		ld_hasta

SetPointer(HourGlass!)

istr_info.titulo	= "INFORME ESTADISTICO DE CAJAS PROCESADAS"
istr_info.copias	= 1

IF cbx_todospro.Checked THEN
	il_nroorden	= 	-1
	IF cbx_conspro.Checked THEN	il_nroorden	= 	-9
ELSE
	il_nroorden 	= 	Long(sle_proceso.Text)
END IF

IF cbx_embalaje.Checked THEN
	ls_embalaje 	= '-1'
	IF cbx_consembalaje.Checked THEN	ls_embalaje 	= '-9'
ELSE
	ls_embalaje 	= 	em_embalaje.Text
END IF

IF cbx_calidad.Checked THEN
	ls_calidad 	= '-1'
	IF cbx_conscalidad.Checked THEN	ls_calidad 	= '-9'
ELSE
	ls_calidad 	= 	em_calidad.Text
END IF

IF cbx_etiqueta.Checked THEN
	li_etiqueta 	= -1
	IF cbx_consetiqueta.Checked THEN	li_etiqueta 	= -9
ELSE
	li_etiqueta 	= 	dw_etiqueta.object.etiq_codigo[1]
END IF

IF cbx_fecemb.Checked THEN li_ConsFecha = 1
ld_desde			=	Date(em_desde.Text)
ld_hasta			=	Date(em_hasta.Text)

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_estadisticacajas"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_Selplanta.codigo, ls_embalaje, ls_calidad, is_rut,&
								  is_tipo, ld_desde, ld_hasta, ii_TipoOrden, ii_tipoproceso, il_nroorden,uo_SelContratista.Codigo) 
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_estadisticas_cajas
integer x = 2158
integer y = 1732
integer taborder = 120
fontcharset fontcharset = ansi!
end type

type st_6 from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 528
integer width = 425
integer height = 92
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

type st_1 from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 716
integer width = 425
integer height = 92
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

type st_8 from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 1272
integer width = 425
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
string text = "Personal"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 1432
integer width = 425
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
string text = "Tipo"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_estadisticas_cajas
boolean visible = false
integer x = 4421
integer y = 68
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

type st_11 from statictext within w_info_estadisticas_cajas
boolean visible = false
integer x = 4421
integer y = 200
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

type st_embalaje from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 1600
integer width = 425
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

type st_calidad from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 1772
integer width = 425
integer height = 92
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
string text = "Calibre"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_estadisticas_cajas
event destroy ( )
boolean visible = false
integer x = 1079
integer y = 68
integer height = 188
integer taborder = 130
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled		=	True
		uo_selvariedad.dw_Seleccion.Enabled		=	False		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
END CHOOSE
end event

type uo_selplanta from uo_seleccion_plantas within w_info_estadisticas_cajas
event destroy ( )
integer x = 805
integer y = 704
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_estadisticas_cajas
event destroy ( )
boolean visible = false
integer x = 2149
integer y = 660
integer taborder = 160
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_estadisticas_cajas
event destroy ( )
boolean visible = false
integer x = 4114
integer taborder = 150
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_etiqueta from checkbox within w_info_estadisticas_cajas
boolean visible = false
integer x = 4114
integer y = 192
integer width = 293
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

type cbx_consetiqueta from checkbox within w_info_estadisticas_cajas
boolean visible = false
integer x = 4421
integer y = 252
integer width = 471
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
string text = "Consolidadas"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[17]	=	'-9'
ELSE
	istr_mant.argumento[17]	=	'0'
END IF

end event

type dw_etiqueta from datawindow within w_info_estadisticas_cajas
boolean visible = false
integer x = 4101
integer y = 260
integer width = 905
integer height = 96
integer taborder = 170
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_etiquetas"
boolean border = false
boolean livescroll = true
end type

event itemerror;RETURN 1
end event

event itemchanged;istr_mant.argumento[7]	=	data

end event

type cbx_embalaje from checkbox within w_info_estadisticas_cajas
integer x = 800
integer y = 1536
integer width = 402
integer height = 60
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
	cbx_consembalaje.Enabled		=	True
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled		=	False
	em_embalaje.Text					=	''
	istr_mant.argumento[6]			=	'Z'
	istr_mant.argumento[16]		=	'0'
ELSE
	cbx_consembalaje.Enabled		=	False
	cbx_consembalaje.Checked		=	False
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled		=	True
END IF
end event

type em_embalaje from editmask within w_info_estadisticas_cajas
integer x = 800
integer y = 1596
integer width = 297
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
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

IF NOT iuo_embalajes.Existe(li_cliente, ls_embalaje, true, sqlca) THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido para el cliente " + String(li_cliente) + ".~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.Text					=	""
	This.SetFocus()
ELSE
	istr_mant.argumento[6]	=	ls_embalaje
END IF
end event

type cbx_consembalaje from checkbox within w_info_estadisticas_cajas
integer x = 1285
integer y = 1536
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
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[16]	=	'.9'
ELSE
	istr_mant.argumento[16]	=	'0'
END IF

end event

type cbx_conscalidad from checkbox within w_info_estadisticas_cajas
integer x = 1285
integer y = 1692
integer width = 471
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
string text = "Consolidados"
end type

event clicked;IF This.Checked = TRUE THEN
	istr_mant.argumento[18]	=	'-9'
ELSE
	istr_mant.argumento[18]	=	'0'
END IF

end event

type cbx_calidad from checkbox within w_info_estadisticas_cajas
integer x = 800
integer y = 1692
integer width = 297
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
string text = "Todos"
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

type em_calidad from editmask within w_info_estadisticas_cajas
integer x = 800
integer y = 1756
integer width = 297
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;String ls_calibre

ls_calibre	=	This.Text
ls_calibre	=	Trim(ls_calibre) + Fill(" ",3 - Len(ls_calibre))

IF NOT ExisteCalibre(ls_calibre) THEN
	This.Text					=	''
ELSE
	istr_mant.argumento[8]	=	ls_calibre
	em_calidad.Text			=	ls_calibre
END IF
end event

type cb_buscaembalaje from commandbutton within w_info_estadisticas_cajas
boolean visible = false
integer x = 4421
integer y = 456
integer width = 119
integer height = 100
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

type st_2 from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 1952
integer width = 425
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
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_estadisticas_cajas
integer x = 800
integer y = 1948
integer width = 421
integer height = 96
integer taborder = 90
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

event modified;istr_mant.argumento[9]	=	This.Text
end event

type st_7 from statictext within w_info_estadisticas_cajas
integer x = 1248
integer y = 1964
integer width = 279
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
string text = "Termino"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_estadisticas_cajas
integer x = 1531
integer y = 1948
integer width = 421
integer height = 96
integer taborder = 100
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

event modified;istr_mant.argumento[10]	=	This.Text
end event

type cbx_fecemb from checkbox within w_info_estadisticas_cajas
boolean visible = false
integer x = 4421
integer y = 1192
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidada"
end type

event clicked;IF THIS.Checked THEN
	cbx_todosfecha.Checked 	= 	True
	cbx_todosfecha.TriggerEvent("clicked")
	
	cbx_todosfecha.Enabled 	= 	False
ELSE
	cbx_todosfecha.Enabled 	= 	True
END IF
end event

type em_orden from editmask within w_info_estadisticas_cajas
boolean visible = false
integer x = 2149
integer y = 716
integer width = 375
integer height = 96
integer taborder = 200
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

event modified;ii_nroorden	=	Long(This.Text)
end event

type cbx_ordentodo from checkbox within w_info_estadisticas_cajas
boolean visible = false
integer x = 4114
integer y = 920
integer width = 293
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_orden.Enabled										=	False
	em_orden.Text 											=	''
	ii_nroorden												=	-1
ELSE
	em_orden.Enabled										=	True
	ii_nroorden												=	Long(em_orden.Text)
	em_orden.SetFocus()
END IF
end event

type st_17 from statictext within w_info_estadisticas_cajas
boolean visible = false
integer x = 2149
integer y = 628
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

type cbx_todosfecha from checkbox within w_info_estadisticas_cajas
integer x = 800
integer y = 1888
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_desde.Text 		= String(RelativeDate(Today(), -365))
	em_hasta.Text 		= String(Today())
	em_desde.enabled	= False
	em_hasta.enabled		= False
ELSE
	em_desde.enabled	= True
	em_hasta.enabled		= True
END IF
end event

type gb_4 from groupbox within w_info_estadisticas_cajas
boolean visible = false
integer x = 4041
integer y = 820
integer width = 1262
integer height = 196
integer taborder = 190
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_estadisticas_cajas
integer x = 805
integer y = 472
integer height = 172
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cbx_consproc from checkbox within w_info_estadisticas_cajas
boolean visible = false
integer x = 4411
integer y = 976
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolidada"
end type

event clicked;IF THIS.Checked THEN
	cbx_ordentodo.Checked 	= 	True
	cbx_ordentodo.TriggerEvent("clicked")
	
	cbx_ordentodo.Enabled 		= 	False
ELSE
	cbx_ordentodo.Enabled 		= 	True
END IF
end event

type cbx_exporta from checkbox within w_info_estadisticas_cajas
boolean visible = false
integer x = 942
integer y = 2084
integer width = 507
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Consolida Cajas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
//	sle_estado.Visible	=	True
//	sle_estado.Text		=	"En Espera"
	is_cajas = '-9'
ELSE
	is_cajas = ''
END IF
end event

type sle_estado from singlelineedit within w_info_estadisticas_cajas
boolean visible = false
integer x = 4329
integer y = 716
integer width = 699
integer height = 92
integer taborder = 180
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 10789024
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type dw_entidad from datawindow within w_info_estadisticas_cajas
integer x = 805
integer y = 1260
integer width = 896
integer height = 112
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_entidadespacking"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "enpa_rutper"
		is_rut	=	F_verrut(data, True)
		IF is_rut	=	""	THEN
			This.SetItem(row,ls_columna,String(ll_null))
			Return 1
		END IF
			
END CHOOSE
end event

type cbx_todosrut from checkbox within w_info_estadisticas_cajas
integer x = 809
integer y = 1208
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
	cbx_consrut.Enabled			=	True
	dw_entidad.Enabled			=	False
	dw_entidad.Reset()
	dw_entidad.SetTransObject(SQLCA)
	dw_entidad.InsertRow(0)
	is_rut 							= '-1'
ELSE
	cbx_consrut.Enabled			=	False
	cbx_consrut.Checked			=	False
	dw_entidad.Enabled			=	True
	dw_entidad.Reset()
	dw_entidad.SetTransObject(SQLCA)
	dw_entidad.InsertRow(0)
END IF
end event

type cbx_consrut from checkbox within w_info_estadisticas_cajas
integer x = 1285
integer y = 1208
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
end type

event clicked;IF This.Checked = TRUE THEN
	is_rut = '-9'
ELSE
	is_rut = '-1'
END IF

end event

type ddlb_tipo from dropdownlistbox within w_info_estadisticas_cajas
integer x = 805
integer y = 1416
integer width = 878
integer height = 400
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean allowedit = true
boolean sorted = false
string item[] = {"Completas","Control Calidad","Sin Pesaje"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
	CASE 1
		is_tipo = "F"
	CASE 2
		is_tipo = "Q"
	CASE 3
		is_tipo = "A"
		
END CHOOSE
end event

type st_5 from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 904
integer width = 425
integer height = 92
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
string text = "Proceso"
boolean focusrectangle = false
end type

type sle_proceso from singlelineedit within w_info_estadisticas_cajas
integer x = 805
integer y = 892
integer width = 297
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

event modified;IF IsNumber(This.Text) THEN
	il_nroorden	=	Long(This.Text)
	IF NOT Existe(uo_selplanta.codigo, 4, il_nroorden, True, SQLCA, uo_selcliente.codigo) THEN
		This.Text	=	''
		SetNull(il_nroorden)
	END IF
ELSE
	This.Text	=	''
	SetNull(il_nroorden)
END IF
end event

type cbx_todospro from checkbox within w_info_estadisticas_cajas
integer x = 805
integer y = 824
integer width = 402
integer height = 60
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
	sle_proceso.Enabled				=	False
	sle_proceso.Text					=	''
	il_nroorden							=	-1
	cbx_conspro.Checked				=	False
	cbx_conspro.TriggerEvent(Clicked!)
	uo_SElContratista.Todos(True)
ELSE
	sle_proceso.Enabled				=	True
	uo_SElContratista.Todos(False)
END IF
end event

type cbx_conspro from checkbox within w_info_estadisticas_cajas
integer x = 1280
integer y = 828
integer width = 471
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;IF This.Checked THEN
	il_nroorden							=	-1
	ii_nroorden							=	0
ELSE
	cbx_todospro.checked				=	True
	sle_proceso.Enabled				=	False
	sle_proceso.Text					=	''
	il_nroorden							=	-1
	ii_nroorden							=	1
END IF

end event

type ddlb_tipoproc from dropdownlistbox within w_info_estadisticas_cajas
integer x = 1111
integer y = 888
integer width = 571
integer height = 368
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
string text = "none"
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Todos","Proceso","Re-Proceso","Re-Embalaje","Pre-Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
		
	CASE 1
		ii_tipoproceso	=	-1			
				
	CASE 2
		ii_tipoproceso	=	4
		
	CASE 3
		ii_tipoproceso	=	5
		
	CASE 4
		ii_tipoproceso	=	7
		
	CASE 5
		ii_tipoproceso	=	8
		
END CHOOSE
end event

type st_9 from statictext within w_info_estadisticas_cajas
integer x = 297
integer y = 1076
integer width = 425
integer height = 92
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
string text = "Contratista"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_estadisticas_cajas
integer x = 242
integer y = 408
integer width = 1801
integer height = 1672
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcontratista from uo_seleccion_contratista within w_info_estadisticas_cajas
integer x = 805
integer y = 1008
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcontratista.destroy
call uo_seleccion_contratista::destroy
end on

